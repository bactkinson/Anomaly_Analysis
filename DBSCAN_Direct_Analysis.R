## Load required packages
require(dbscan)
require(dplyr)
require(tidyverse)


## Required functions
find_the_knee <- function(poll_data,min_pts){
    tt <- sort(kNNdist(poll_data,k=min_pts))

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

return_anomalies <- function(windowed_data,min_pts_param){
  poll_data <- windowed_data %>%
      dplyr::select(BC,CO2,NOx,UFP) %>%
      mutate_all(scale)
    
  min_dist <- find_the_knee(poll_data,min_pts = min_pts_param)

  dbscan_res <- dbscan(poll_data,eps = min_dist, minPts = min_pts_param)
  
  assignments <- dbscan_res$cluster
  
  assignments[assignments==0] <- 2
  
  return(cbind(windowed_data,"Anomaly"=assignments))
}


{
  current_dir <- getwd()
  
  load(paste0(getwd(),"/windowed_subset.RData")) %>% as.list()
  
  windowed_subset <- lapply(windowed_subset,function(x)x %>%  dplyr::select(-c(Anomaly)))
  
  min_pts_to_use <- read.csv(paste0(current_dir,"/min_pts_storage.csv"))[1:2,2]
  
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
