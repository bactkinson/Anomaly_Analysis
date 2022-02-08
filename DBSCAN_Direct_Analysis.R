## Load required packages
require(dbscan)
require(dplyr)
require(tidyverse)
require(parallel)
## Required functions
find_the_knee <- function(poll_data,min_pts){
  
  tt <- kNNdist(poll_data,k=min_pts)
  
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

plot_knees <- function(poll_data,min_pts,title){
  
  png(paste0(getwd(),"/Miscellaneous_Figures/knee_analysis/",title,".png"))
  
  NN_plot <- kNNdist(poll_data,k=min_pts,main = title)
  
  change_in_NN <- diff(NN_plot)
  
  dist_subset <- NN_plot[1:30]
  for(i in 31:length(NN_plot)){
    if(NN_plot[i] > (mean(dist_subset) + 3*sd(dist_subset))){
      first_knee <- NN_plot[i]
      break
    } else{
      dist_subset <- c(dist_subset,NN_plot[i])
    }
  }
  
  slope_subset <- change_in_NN[1:30]
  for(j in 31:length(change_in_NN)){
    if(change_in_NN[j] > (mean(slope_subset)+3*sd(slope_subset))){
      second_knee <- NN_plot[j]
      break
    } else{
      slope_subset <- c(slope_subset,change_in_NN[j])
    }
  }
  
  abline(h=first_knee,lty = 2,col = "blue")
  
  abline(h=second_knee,lty = 2, col = "red")
  
  dev.off()
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
  
  # cl <- makeCluster(no_cores)
  
  # eps_grid <- seq(0.1,3,by = 0.1)
  
  # pre_mod_storage <- vector(mode = "list", length = length(eps_grid))
  
  # for(i in 1:length(eps_grid)){
  #   pre_mod_storage[[i]] <- list(poll_data,eps_grid[i],min_pts_param)
  # }
  
  # clusterExport(cl=cl,varlist = c("pre_mod_storage"),envir = environment())
  # 
  # mod_storage <- parallel::parLapply(cl,pre_mod_storage,function(x) dbscan::dbscan(x[[1]],eps = x[[2]],minPts = x[[3]]))
  # 
  # cluster_compactness <- unlist(
  #   lapply(mod_storage,function(x) core_cluster_compactness(x,poll_data)),
  #   use.names = FALSE)
  # 
  # optimal_index <- which.min(cluster_compactness)
  # 
  # optimal_mod <- mod_storage[[optimal_index]]
  # 
  # assignments <- optimal_mod$cluster
  # 
  # assignments[assignments==0] <- 2
  # 
  # print(eps_grid[optimal_index])
  # 
  # stopCluster(cl=cl)
  
  current_eps <- find_the_knee(poll_data,min_pts = floor(min_pts_param/2))
  
  db_clust <- dbscan::dbscan(poll_data,minPts = min_pts_param,eps = current_eps)
  
  assignments <- db_clust$cluster
  
  assignments[assignments==0] <- 2
  
  print("Execution complete")
  
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

  # dbOutput <- lapply(aggregate_list,function(x) return_anomalies(x[[1]],x[[2]]))
  
  dbOutput <- vector(mode = "list",length = length(aggregate_list))
  
  for(j in 31:length(aggregate_list)){
    print(j)
    dbOutput[[j]] <- return_anomalies(aggregate_list[[j]][[1]], aggregate_list[[j]][[2]])
  }
  
  # for(j in 1:20){
  #     file_string <- paste0("Iteration_",j,"cv.png")
  #     png(paste0(current_dir,"/Anomaly_Analysis_Plots/",file_string))
  #     plot(NOx~CO2, data = dbOutput[[j]],  col = Anomaly,pch = 20)
  #     dev.off()
  # }

  # db_tibble <- list_to_tibble(dbOutput)

}

{
  list_to_tibble <- function(data_subset_list){
  # output_tibble <- unlist(data_subset_list[[1]],use.names=F)
  # for(i in 2:length(data_subset_list)) {output_tibble <- rbind(output_tibble,unlist(data_subset_list[[i]],use.names=F))}
  # return(output_tibble)
  
    current_grp_index <- 1
 
    output_tibble <- cbind(data_subset_list[[1]],"Uniq_Fac"=rep(current_grp_index,nrow(data_subset_list[[1]])))
  
    for(i in 2:length(data_subset_list)) {
        current_grp_index <- i
      
        current_tibble <- cbind(data_subset_list[[i]],"Uniq_Fac"=rep(current_grp_index,nrow(data_subset_list[[i]])))
      
        output_tibble <- rbind(output_tibble,current_tibble)
      }
    return(output_tibble)
  }

  db_tibble <- list_to_tibble(dbOutput)

  # anomalous_emissions <- db_tibble %>%
  #   filter(Anomaly==2) %>%
  #   select(LST,BC,CO2,NOx,UFP)

  # write.csv(anomalous_emissions,paste0(current_dir,"/Anomalous_Emissions_Results/Anomalous_Emissions_cv.csv"))
}

## Finding the knee sensitivity analysis.
# {
#   memory.limit(size = 384000)
#   
#   start_time <- Sys.time()
#   
#   current_dir <- getwd()
#   
#   load(paste0(current_dir,"/windowed_data.RData")) %>% as.list()
#   
#   windowed_data <- lapply(windowed_data,function(x)x %>%  dplyr::select(-c(Delta_D)))
#   
#   min_pts_to_use <- read.csv(paste0(current_dir,"/min_pts_storage.csv"))[,2]
#   
#   no_subs <- 10
#   
#   knees_mat <- matrix(,nrow = 5,ncol = no_subs)
#   
#   
#   for(i in 1:no_subs){
#     
#     pts <- min_pts_to_use[i]
#     
#     poll_data <- windowed_data[[i]] %>%
#       dplyr::select(BC,CO2,NOx,UFP) %>%
#       mutate_all(scale)
#     
#     increments <- c(floor(pts/5),floor(pts/4),floor(pts/3),floor(pts/2),pts)
#     
#     knees_mat[,i] <- sapply(increments,function(x) find_the_knee(poll_data,x))
#   }
# 
#   print(Sys.time()-start_time)
# }
# 
# {
#   knees_tibble <- as_tibble(knees_mat) %>%
#     dplyr::rename_with(~sub("V","Window_",.x)) %>%
#     pivot_longer(cols = everything(),values_to = "Value", names_to = "Window")
#   
# 
#   ggplot(data=knees_tibble) + 
#     geom_boxplot(mapping=aes(x=Window,y=Value)) +
#     theme_classic() + 
#     ggtitle("Knee distributions for the first 10 data windows")
#   
#  percentage_diffs <- apply(knees_mat,2,function(x) abs(x[4]-x[5])/x[5]*100) 
#  
#  mean(percentage_diffs)
# }


## Knee analysis
{
  trimmed_data <- lapply(windowed_data,function(x) x %>% select(BC,CO2,NOx,UFP) %>% mutate_all(scale))
  
  for(i in 1:length(trimmed_data)){
    plot_knees(trimmed_data[[i]],min_pts_to_use[i],paste0("Day ",i))
  }
}