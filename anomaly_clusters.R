## Analyzing anomalous emissions results
require(cluster)
require(NbClust)
require(tidyverse)
{
 ## Read in the anomalous data.
  current_dir <- getwd()
  
  anomalous_data <- read.csv(paste0(current_dir,"/Anomalous_Emissions_Results/Anomalous_Emissions_Drewnick.csv"),
                             row.names = 1)

  anomalous_emissions <- anomalous_data %>%
    dplyr::select(BC,CO2,NOx,UFP) %>%
    mutate_all(scale)

  anom_list <- replicate(5,anomalous_emissions,simplify=FALSE)

  random_subsets <- lapply(anom_list,function(x) dplyr::sample_n(x,95000))

  nb_clust_res <- vector(mode = "list", length = length(random_subsets))
  
  start_time <- Sys.time()
  for(i in 1:length(random_subsets)){
    print(paste0("i: ", i))
    require(NbClust)
    nb_clust_res[[i]] <- NbClust::NbClust(random_subsets[[i]],method = "kmeans", distance = "euclidean", min.nc = 2, max.nc = 10)
    # require(cluster)
    # gap_stat <- clusGap(random_subsets[[i]], FUN = kmeans, nstart = 100, K.max = 10, B = 10)
    # print(factoextra::fviz_gap_stat(gap_stat) + ggtitle(paste0("Iter: ", i)))
  }
  
}

## From cluster evaluation results above, determine best number of kmeans cluster centers
## and visualize results.
{
  anom_kmeans <- kmeans(anomalous_emissions,centers = 3, nstart = 100)

  clustered_data <- cbind(anomalous_emissions, "Cluster" = as.factor(anom_kmeans$cluster))

  clustered_data_long <- clustered_data %>%
    pivot_longer(c(BC,CO2,NOx,UFP), names_to = "Pollutant", values_to = "Measurement")

  print(
    ggplot(data=clustered_data_long) +
    geom_boxplot(aes(x=Cluster,y=Measurement)) +
    facet_wrap(~Pollutant, scale="free")
  )
}