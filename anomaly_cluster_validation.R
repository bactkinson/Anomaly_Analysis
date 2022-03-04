## Analyzing anomalous emissions results
require(cluster)
require(NbClust)

save_plot <- function(filename,width,height){
  
  ggsave(filename = filename,
         device = "png",
         units = "in",
         width = width,
         height = height,
         dpi = 300)
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


