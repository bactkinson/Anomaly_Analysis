## Load required packages
require(dbscan)
require(dplyr)
require(tidyverse)


## Required functions
find_the_knee <- function(poll_data,min_pts){
  t1_start <- Sys.time()
  tt <- kNNdist(poll_data,k=min_pts,approx = 1)
  print(Sys.time()-t1_start)
  print("knndists construction completed")
  t2_start <- Sys.time()
  dist_subset <- tt[1:30]
  for(i in 31:length(tt)){
    if(tt[i] > (mean(dist_subset) + 3*sd(dist_subset))){
      print(Sys.time()-t2_start)
      print("Knee identification completed")
      return(tt[i])
      break
    } else{
      dist_subset <- c(dist_subset,tt[i])
    }
  }
  print(Sys.time()-t2_start)
  print("Knee identification completed")
  return(tt[length(tt)])
}

core_cluster_compactness <- function(dbscan_mod,poll_data){
  clustering_results <- dbscan_mod$cluster
  
  core_data <- poll_data[clustering_results==1,]
  
  core_mean <- apply(core_data,2,mean)
  
  distances <- apply(core_data,1,function(x) return(sum((x-core_mean)^2)))

  avg_distance <- mean(distances)

  return(avg_distance)
}

return_anomalies <- function(windowed_data,min_pts_param){
  poll_data <- windowed_data %>%
      dplyr::select(BC,CO2,NOx,UFP) %>%
      mutate_all(scale)
    
  # min_dist <- find_the_knee(poll_data,min_pts = min_pts_param)
  
  # dbscan_res <- dbscan(poll_data,eps = min_dist, minPts = min_pts_param)
  # 
  # assignments <- dbscan_res$cluster
  # 
  # assignments[assignments==0] <- 2
  
  # return("Done")
  # return(cbind(windowed_data,"Anomaly"=assignments))
  
  eps_grid <- seq(0.5,5,by = 0.5)
  
  mod_storage <- vector(mode = "list", length = length(eps_grid))
  
  for(i in 1:length(eps_grid)){
    mod_storage[[i]] <- dbscan::dbscan(poll_data,eps = eps_grid[i], minPts = min_pts_param)
  }
  
  cluster_compactness <- unlist(
    lapply(mod_storage,function(x) core_cluster_compactness(x,poll_data)),
    use.names = FALSE)
  
  optimal_index <- which.min(cluster_compactness)
  
  optimal_mod <- mod_storage[[optimal_index]]
  
  assignments <- optimal_mod$cluster
  
  assignments[assignments==0] <- 2
  
  print(eps_grid[optimal_index])
  
  return(cbind(windowed_data,"Anomaly"=assignments))
}


{
  current_dir <- getwd()
  
  load(paste0(getwd(),"/windowed_subset.RData")) %>% as.list()
  
  windowed_subset <- lapply(windowed_subset,function(x)x %>%  dplyr::select(-c(Anomaly)))
  
  min_pts_to_use <- read.csv(paste0(current_dir,"/min_pts_storage.csv"))[1:6,2]
  
  aggregate_list <- vector(mode="list", length = length(min_pts_to_use))

  for(j in 1:length(aggregate_list)){
    aggregate_list[[j]] <- list(windowed_subset[[j]],min_pts_to_use[j])
  }

  dbOutput <- lapply(aggregate_list,function(x) return_anomalies(x[[1]],x[[2]]))
  
  # for(j in 1:30){
  #   print(paste0("Iteration: ",j))
  #   
  #   current_min_pts <- min_pts_to_use[j]
  #   
  #   
  # 
  #   # par(mfrow=c(3,2))
  #   if(F){
  #     file_string <- paste0("Iteration_",j,".png")
  #     png(paste0(current_dir,"/Anomaly_Analysis_Plots/",file_string))
  #     plot(NOx~CO2, data = poll_data,  col = assignments,pch = 20)
  #     dev.off()
  #   }
  # }
  

}
