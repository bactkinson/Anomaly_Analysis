{
  current_dir <- getwd()
  
  load("~/Academic Work/Research/Houston_Mobile_Dynamic_PCA/windowed_data.RData")
  
  require(dbscan)
  require(dplyr)
  
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

  min_pts_to_use <- read.csv(paste0(current_dir,"/min_pts_storage.csv"))[,2]
  
  for(j in 1:30){
    print(paste0("Iteration: ",j))
    
    current_min_pts <- min_pts_to_use[j]
    
    poll_data <- windowed_data[[j]] %>%
      dplyr::select(BC,CO2,NOx,UFP) %>%
      mutate_all(scale)
    
    min_dist <- find_the_knee(poll_data,min_pts = current_min_pts)

    dbscan_res <- dbscan(poll_data,eps = min_dist, minPts = current_min_pts)

    print(dbscan_res)

    assignments <- dbscan_res$cluster

    assignments[assignments==0] <- 5

    # par(mfrow=c(3,2))
    if(T){
      file_string <- paste0("Iteration_",j,".png")
      png(paste0(current_dir,"/Anomaly_Analysis_Plots/",file_string))
      plot(NOx~CO2, data = poll_data,  col = assignments,pch = 20)
      dev.off()
    }


    # plot(BC~CO2, data = poll_data,  col = assignments,pch = 20)
    # plot(UFP~CO2, data = poll_data,  col = assignments,pch = 20)
    # plot(BC~NOx, data = poll_data,  col = assignments,pch = 20)
    # plot(UFP~NOx, data = poll_data,  col = assignments,pch = 20)
    # plot(UFP~BC, data = poll_data,  col = assignments,pch = 20)
  }
  

}
