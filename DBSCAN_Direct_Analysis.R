## Load required packages
require(dbscan)
require(dplyr)
require(tidyverse)
require(parallel)
## Required functions
find_the_knee <- function(poll_data,min_pts){
  
  tt <- kNNdist(poll_data,k=min_pts,approx = 1)
  
  t2_start <- Sys.time()
  dist_subset <- tt[1:30]
  for(i in 31:length(tt)){
    if(tt[i] > (mean(dist_subset) + 3*sd(dist_subset))){
      return(tt[i])
      break
    } else{
      dist_subset <- c(dist_subset,tt[i])
    }
  }
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

return_anomalies <- function(windowed_data,min_pts_param,no_cores = parallel::detectCores()-2){
  poll_data <- windowed_data %>%
      dplyr::select(BC,CO2,NOx,UFP) %>%
      mutate_all(scale)
    
  cl <- makeCluster(no_cores)
  
  eps_grid <- seq(0.1,3,by = 0.1)
  
  pre_mod_storage <- vector(mode = "list", length = length(eps_grid))
  
  for(i in 1:length(eps_grid)){
    pre_mod_storage[[i]] <- list(poll_data,eps_grid[i],min_pts_param)
  }
  
  clusterExport(cl=cl,varlist = c("pre_mod_storage"),envir = environment())
  
  mod_storage <- parallel::parLapply(cl,pre_mod_storage,function(x) dbscan::dbscan(x[[1]],eps = x[[2]],minPts = x[[3]]))
  
  cluster_compactness <- unlist(
    lapply(mod_storage,function(x) core_cluster_compactness(x,poll_data)),
    use.names = FALSE)
  
  optimal_index <- which.min(cluster_compactness)
  
  optimal_mod <- mod_storage[[optimal_index]]
  
  assignments <- optimal_mod$cluster
  
  assignments[assignments==0] <- 2
  
  print(eps_grid[optimal_index])
  
  stopCluster(cl=cl)
  
  return(cbind(windowed_data,"Anomaly"=assignments))
}


{
  start_time <- Sys.time()
  
  current_dir <- getwd()
  
  source(paste0(current_dir,"/send_myself_mail.R"))
  
  load(paste0(current_dir,"/windowed_data.RData")) %>% as.list()
  
  windowed_data <- lapply(windowed_data,function(x)x %>%  dplyr::select(-c(Delta_D)))
  
  min_pts_to_use <- read.csv(paste0(current_dir,"/min_pts_storage.csv"))[,2]
  
  aggregate_list <- vector(mode="list", length = length(min_pts_to_use))

  for(j in 1:length(aggregate_list)){
    aggregate_list[[j]] <- list(windowed_data[[j]],min_pts_to_use[j])
  }

  dbOutput <- lapply(aggregate_list,function(x) return_anomalies(x[[1]],x[[2]]))
  
  # for(j in 1:20){
  #     file_string <- paste0("Iteration_",j,"cv.png")
  #     png(paste0(current_dir,"/Anomaly_Analysis_Plots/",file_string))
  #     plot(NOx~CO2, data = dbOutput[[j]],  col = Anomaly,pch = 20)
  #     dev.off()
  # }
  
  # db_tibble <- list_to_tibble(dbOutput)
    
  
  
  send_message_to_myself("Routine Completed",paste0("Routine took", Sys.time()-start_time))

}

{
  list_to_tibble <- function(data_subset_list){
  # output_tibble <- unlist(data_subset_list[[1]],use.names=F)
  # for(i in 2:length(data_subset_list)) {output_tibble <- rbind(output_tibble,unlist(data_subset_list[[i]],use.names=F))}
  # return(output_tibble)
  
  output_tibble <- data_subset_list[[1]]
  for(i in 2:length(data_subset_list)) {output_tibble <- rbind(output_tibble,data_subset_list[[i]])}
  return(output_tibble)
  }
  
  db_tibble <- list_to_tibble(dbOutput)
  
  anomalous_emissions <- db_tibble %>%
    select(LST,BC,CO2,NOx,UFP,Anomaly) %>%
    filter(Anomaly==2)
  
  
}
