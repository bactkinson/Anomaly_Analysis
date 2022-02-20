## Analyzing anomalous emissions results
require(cluster)
require(NbClust)
require(tidyverse)

## Writing function that colors anomalous points red in time series data to evaluate
## algorithm performance

plot_time_series_anomalies <- function(windowed_data,polls_to_pull,title,
                                       save_graph = F, directory = getwd()){

  poll_data <- windowed_data %>%
    select(all_of(polls_to_pull),LST,Anomaly) %>%
    mutate(LST=as.POSIXct(LST,format = c("%Y-%m-%d %H:%M:%S"),tz = Sys.timezone()),.keep = "unused") %>%
    pivot_longer(cols = all_of(polls_to_pull),names_to = "Pollutant", values_to = "Value")

  poll_subset <- list()

  if(nrow(poll_data)>10000){
    poll_subset[[1]] <- poll_data[1:floor(nrow(poll_data)/2),]
    poll_subset[[2]] <- poll_data[(floor(nrow(poll_data)/2):nrow(poll_data)),]
  } else{
    poll_subset[[1]] <- poll_data
  }


  require(lattice)
  require(latticeExtra)
  require(grid)

  for(i in 1:length(poll_subset)){
    
    if(save_graph){png(paste0(directory,title,"_Part_",i,".png"))}
    
    print(
      xyplot(Value ~ LST | Pollutant,
             data = poll_subset[[i]],
             type = "p",
             grid = TRUE,
             group = Anomaly,
             col = c("black","red"),
             layout = c(1,length(polls_to_pull)),
             size = 0.3,
             scale = "free",
             strip = F,
             xlab = "Time (US Central)",
             ylab = "Concentration",
             main = paste0(title," Part ",i),
             panel = function(...) {
               panel.xyplot(...)
               draw.key(
                 list(col='black',text=list(polls_to_pull[panel.number()])),
                 draw=T,
                 vp=viewport(x=unit(.1,'npc'),y=unit(.9,'npc'))
               )
             }
      )


    )
    
    if(save_graph){dev.off()}
  }

  print("Plotting complete")

}

## For a given data set, find the fraction of points flagged as anomalies
## given percentile and poll
fraction_flagged <- function(percentile,poll,data){
  
  selected_poll <- data %>%
    select(all_of(poll)) %>%
    unlist(use.names = FALSE)
  
  threshold <- quantile(selected_poll,percentile)
  
  filtered_poll <- data %>%
    filter(selected_poll > threshold)
  
  ## With filtered poll, calculate percentage anomalies
  
  fraction_anomalies <- length(which(filtered_poll$Anomaly==2))/nrow(filtered_poll)
  
  return(fraction_anomalies*100)
}


{
 ## Read in the anomalous data. Preprocessing
  current_dir <- getwd()
  
  db_data <- read.csv(paste0(current_dir,"/Anomalous_Emissions_Results/Labeled_Emissions_DBSCAN_V01_run_1.csv"),
                             row.names = 1)
  
  # drew_data <- read.csv(paste0(current_dir,"/Anomalous_Emissions_Results/Labeled_Emissions_Drewnick.csv"),
  #                            row.names = 1)
  # 
  # qor_data <- read.csv(paste0(current_dir,"/Anomalous_Emissions_Results/Labeled_Emissions_Quantile_OR.csv"),
  #                            row.names = 1)
  # 
  # qand_data <- read.csv(paste0(current_dir,"/Anomalous_Emissions_Results/Labeled_Emissions_Quantile_AND.csv"),
  #                            row.names = 1)
  # 
  # drew_grouped_anomalies <- drew_data %>%
  #   group_split(Uniq_Fac,.keep = FALSE)

  db_grouped_anomalies <- db_data %>%
    group_split(Uniq_Fac,.keep = FALSE)

  # qor_grouped_anomalies <- qor_data %>%
  #   group_split(Uniq_Fac,.keep = FALSE)
  # 
  # qand_grouped_anomalies <- qand_data %>%
  #   group_split(Uniq_Fac,.keep = FALSE)
  
  anomalous_emissions <- db_data %>%
    filter(Anomaly==2) %>%
    dplyr::select(BC,CO2,NOx,UFP) %>%
    mutate_all(scale)
}

# Cluster validation
# {
#   anom_list <- replicate(1,anomalous_emissions,simplify=FALSE)
# 
#   random_subsets <- lapply(anom_list,function(x) dplyr::sample_n(x,62000))
# 
#   nb_clust_res <- vector(mode = "list", length = length(random_subsets))
# 
#   start_time <- Sys.time()
#   
#   # for(i in 1:length(random_subsets)){
#     # print(paste0("i: ", i))
#     # nb_clust_res[[i]] <- NbClust::NbClust(random_subsets[[i]],method = "kmeans", 
#     #                                       distance = "manhattan", 
#     #                                       min.nc = 2, max.nc = 10,
#     #                                       index = "gap")$Best.partition
#     # gap_stat <- clusGap(random_subsets[[i]], FUN = kmeans, nstart = 100, K.max = 10, B = 10)
#     # print(factoextra::fviz_gap_stat(gap_stat) + ggtitle(paste0("Iter: ", i)))
#     
#     
#   # }
#   
# }


## From cluster evaluation results above, determine best number of kmeans cluster centers
## and visualize results.
{
  set.seed(5)

  anom_kmeans <- kmeans(anomalous_emissions,centers = 3, nstart = 100)

  clustered_data <- cbind(anomalous_emissions, "Cluster" = as.factor(anom_kmeans$cluster))

  clustered_data_long <- clustered_data %>%
    pivot_longer(c(BC,CO2,NOx,UFP), names_to = "Pollutant", values_to = "Measurement")

  print(
    ggplot(data=clustered_data_long) +
    geom_boxplot(aes(x=Cluster,y=Measurement)) +
    facet_wrap(~Pollutant, scale="free")
  )

  require(factoextra)

  fviz_cluster(anom_kmeans,data=anomalous_emissions,
               geom = "point",
               main = "3 Cluster Solution")

  res_pca <- FactoMineR::PCA(anomalous_emissions,scale.unit = TRUE,ncp = 4) 
  
  fviz_pca_biplot(res_pca,
                  axes = c(3,4),
                  geom = c("point","point"))
}

# {
#   plot_time_series_anomalies(drew_grouped_anomalies[[8]], polls_to_pull = c("BC","CO2","NOx","UFP"),
#                              title = "Drewnick",
#                              save_graph = T,
#                              directory = paste0(getwd(),"/Miscellaneous_Figures/DB_Over_Drewnick/"))
# 
#   # UFP <- windowed_anomalies[[1]]$UFP
#   #
#   # TS <- as.POSIXct(windowed_anomalies[[1]]$LST,format=c("%Y-%m-%d %H:%M:%S"), tz = Sys.timezone())
#   #
#   # AS <- windowed_anomalies[[1]]$Anomaly
#   #
#   # plot(UFP~TS,col=AS)
# }

## Label validation
{
  valid_label_files <- list.files(path=paste0(getwd(),"/Manually_Flagged_Anomalies/"))
  
  load(paste0(current_dir,"/windowed_data.RData")) %>% as.list()
  
  db_flags <- vector(,)
  
  valid_flags <- vector(,)
  
  valid_data_windows <- vector(mode = "list", length = length(valid_label_files))
  
  for(j in seq_along(valid_label_files)){
    day_tag <- as.numeric(
      strsplit(
        strsplit(valid_label_files[j],"_")[[1]][3],
    "[.]")[[1]][1]
    )
    
    current_flags <- unlist(read.csv(paste0(getwd(),"/Manually_Flagged_Anomalies/",valid_label_files[j])),use.names = FALSE)
    
    print(head(current_flags))
    
    db_flags <- c(db_flags,db_grouped_anomalies[[day_tag]]$Anomaly)
    
    valid_flags <- c(valid_flags,current_flags)
    
    valid_data_windows[[j]] <- windowed_data[[day_tag]]
    
    valid_data_windows[[j]]$Anomaly <- current_flags
  }
  
  print(length(which(db_flags==valid_flags))/length(valid_flags))
  
  save(valid_data_windows,file=paste0(getwd(),"/valid_data.RData"))
}

## With drewnick data, db data, compare how many points in percentiles each method 
## flags as anomalies OVERALL
# {
#   qt_increments <- c(0.1,0.25,0.5,0.75,0.9,0.95,0.99)
# 
#   drew_NOx <- sapply(qt_increments, function(x) round(fraction_flagged(x,"NOx",drew_data),1))
#   drew_CO2 <- sapply(qt_increments, function(x) round(fraction_flagged(x,"CO2",drew_data),1))
#   drew_UFP <- sapply(qt_increments, function(x) round(fraction_flagged(x,"UFP",drew_data),1))
#   drew_BC <- sapply(qt_increments, function(x) round(fraction_flagged(x,"BC",drew_data),1))
#   db_NOx <- sapply(qt_increments, function(x) round(fraction_flagged(x,"NOx",db_data),1))
#   db_CO2 <- sapply(qt_increments, function(x) round(fraction_flagged(x,"CO2",db_data),1))
#   db_UFP <- sapply(qt_increments, function(x) round(fraction_flagged(x,"UFP",db_data),1))
#   db_BC <- sapply(qt_increments, function(x) round(fraction_flagged(x,"BC",db_data),1))
# 
#   quantile_tibble <- tibble("Drewnick_NOx"=drew_NOx,
#                             "DBSCAN_NOx"=db_NOx,
#                             "Drewnick_CO2"=drew_CO2,
#                             "DBSCAN_CO2"=db_CO2,
#                             "Drewnick_UFP"=drew_UFP,
#                             "DBSCAN_UFP"=db_UFP,
#                             "Drewnick_BC"=drew_BC,
#                             "DBSCAN_BC"=db_BC)
# 
#   rownames(quantile_tibble) <- c("0.1","0.25","0.5","0.75","0.9","0.95","0.99")
# 
#   require(kableExtra)
# 
#   quantile_tibble %>%
#     kbl() %>%
#     kable_classic(html_font = "Cambria")
# 
# }
# 
# ## Investigating joint behavior. Doing it on day-by-day basis.
# {
#   fraction_flagged_joint <- function(percentile,data){
# 
#     polls <- c("BC","CO2","NOx","UFP")
# 
#     selected_poll <- data %>%
#       select(all_of(polls))
# 
#     test <- apply(selected_poll,2,function(x) x > quantile(x,percentile))
# 
#     joint_truth <- apply(test,1,function(x) all(x))
# 
#     joint_greater <- data %>%
#       filter(joint_truth)
# 
#     if(length(which(joint_greater$Anomaly==2))==0){
#       return(0)
#     } else{
# 
#       fraction <- length(which(joint_greater$Anomaly==2))/length(joint_greater$Anomaly)
# 
#       return(round(fraction*100,1))
# 
#     }
# 
#   }
# 
#   drew_joint_percentages <- lapply(drew_grouped_anomalies,function(x) fraction_flagged_joint(0.90,x)) %>%
#     unlist(use.names = FALSE)
# 
#   db_joint_percentages <- lapply(db_grouped_anomalies,function(x) fraction_flagged_joint(0.90,x)) %>%
#     unlist(use.names = FALSE)
# 
#   # comparison_tibble <- tibble("Drewnick_Percentages"=drew_joint_percentages,
#   #                             "DB_Percentages"=db_joint_percentages)
# 
#   db_greater_drew <- which(db_joint_percentages>drew_joint_percentages)
# 
#   drew_greater_db <- which(drew_joint_percentages>db_joint_percentages)
# 
#   # comparison_tibble[db_greater_drew,] %>%
#   #   kbl() %>%
#   #   kable_classic()
#   # 
#   # comparison_tibble[drew_greater_db,] %>%
#   #   kbl() %>%
#   #   kable_classic()
# 
#   # for(i in 1:length(db_greater_drew)){
#   #   day_index <- db_greater_drew[i]
#   # 
#   #   plot_time_series_anomalies(db_grouped_anomalies[[day_index]],
#   #                              c("BC","CO2","NOx","UFP"),
#   #                              paste0("DBSCAN_Day_",day_index),
#   #                              save_graph = T,
#   #                              directory = paste0(getwd(),"/Miscellaneous_Figures/DB_Over_Drewnick/"))
#   # 
#   #   plot_time_series_anomalies(drew_grouped_anomalies[[day_index]],
#   #                              c("BC","CO2","NOx","UFP"),
#   #                              paste0("Drewnick_Day_",day_index),
#   #                              save_graph = T,
#   #                              directory = paste0(getwd(),"/Miscellaneous_Figures/DB_Over_Drewnick/"))
#   # }
# }

## Generate 20 random time series comparing flagged anomalies for Drewnick, DB
## quantile_or, and quantile_and methods
# {
#   set.seed(3)
#   
#   grouped_indices <- seq(1,277,1)
#   
#   random_indices <- sample(grouped_indices,20)
#   
#   for(j in 1:length(random_indices)){
#     print(j)
#     print("-------")
#     
#     current_index <- random_indices[j]
#     
#     plot_time_series_anomalies(db_grouped_anomalies[[current_index]],
#                                c("BC","CO2","NOx","UFP"),
#                                paste0("DBSCAN_Day_",current_index),
#                                save_graph = T,
#                                directory = paste0(getwd(),"/Miscellaneous_Figures/Comparing_All_Four/"))
# 
#     plot_time_series_anomalies(drew_grouped_anomalies[[current_index]],
#                                c("BC","CO2","NOx","UFP"),
#                                paste0("Drewnick_Day_",current_index),
#                                save_graph = T,
#                                directory = paste0(getwd(),"/Miscellaneous_Figures/Comparing_All_Four/"))
#     
#     plot_time_series_anomalies(qor_grouped_anomalies[[current_index]],
#                                c("BC","CO2","NOx","UFP"),
#                                paste0("QOR_Day_",current_index),
#                                save_graph = T,
#                                directory = paste0(getwd(),"/Miscellaneous_Figures/Comparing_All_Four/"))
#     
#     plot_time_series_anomalies(qand_grouped_anomalies[[current_index]],
#                                c("BC","CO2","NOx","UFP"),
#                                paste0("QAND_Day_",current_index),
#                                save_graph = T,
#                                directory = paste0(getwd(),"/Miscellaneous_Figures/Comparing_All_Four/"))
#     
#   }
# }
