# options(error=recover)
## Load required packages
require(dbscan)
require(dplyr)
require(tidyverse)
require(parallel)
## Required functions
find_the_knee <- function(poll_data,min_pts){

  tt <- sort(dbscan::kNNdist(poll_data,k=min_pts))

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

find_the_knee_approx <- function(poll_data,min_pts){

  tt <- sort(kNNdist(poll_data,k=min_pts,approx = 1))

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

find_the_knee_quantile <- function(poll_data,min_pts){

  tt <- sort(kNNdist(poll_data,k=min_pts))

  dist_subset <- tt[1:30]
  for(i in 31:length(tt)){
    if(tt[i] > qnorm(0.95, mean = mean(dist_subset), sd = sd(dist_subset))){
      return(tt[i])
      break
    } else{
      dist_subset <- c(dist_subset,tt[i])
    }
  }
  return(tt[length(tt)])
}

plot_knees <- function(poll_data,min_pts,title,plot_bool = F,directory = getwd(), qt_check = F){
  
  if (plot_bool) {png(paste0(directory,title,".png"))}
  
  NNs <- sort(kNNdist(poll_data,k=min_pts))
  
  change_in_NN <- diff(NNs)
  
  dist_subset <- NNs[1:30]
  for(i in 31:length(NNs)){
    if(NNs[i] > (mean(dist_subset) + 3*sd(dist_subset))){
      first_knee <- NNs[i]
      break
    } else{
      dist_subset <- c(dist_subset,NNs[i])
    }
  }
  
  # slope_subset <- change_in_NN[1:30]
  # for(j in 31:length(change_in_NN)){
  #   if(change_in_NN[j] > (mean(slope_subset)+3*sd(slope_subset))){
  #     second_knee <- NNs[j]
  #     break
  #   } else{
  #     slope_subset <- c(slope_subset,change_in_NN[j])
  #   }
  # }
  
  if(qt_check){
  
    dist_subset <- NNs[1:30]
    for(j in 31:length(NNs)){
      if(NNs[j] > qnorm(0.95, mean=mean(dist_subset),sd = sd(dist_subset))){
        second_knee <- NNs[j]
        break
      } else{
        dist_subset <- c(dist_subset,NNs[j])
      }
    }
  
  }
  
  plot(NNs~1,main = title,type = "l")
  
  abline(h=first_knee,lty = 2,col = "blue")
  
  if(qt_check) {abline(h=second_knee,lty = 2, col = "red")}
  
  if(plot_bool) {dev.off()}
  
  return(list("NN_dists" = NNs, "Knee" = first_knee))
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
      dplyr::mutate_all(scale)
  
  current_eps <- find_the_knee(poll_data,min_pts = min_pts_param)
  
  db_clust <- dbscan::dbscan(poll_data,minPts = min_pts_param,eps = current_eps,borderPoints = FALSE)
  
  assignments <- db_clust$cluster
  
  assignments[assignments==0] <- 2
  
  print("Execution complete")
  
  return(cbind(windowed_data,"Anomaly"=assignments))
}


{
  # memory.limit(size = 384000)
  
  start_time <- Sys.time()

  current_dir <- getwd()

  load(paste0(current_dir,"/windowed_data.RData")) %>% as.list()

  windowed_data <- lapply(windowed_data,function(x) x %>%  dplyr::select(-c(Delta_D)))

  # min_pts_to_use <- read.csv(paste0(current_dir,"/min_pts_storage.csv"))[,2]
  
  ## Preprocess min_pts
  # percentage_differences <- read.csv(paste0(current_dir,"/one_half_percentage_diffs.csv"))[,2]
  # 
  # min_pts_modified <- ifelse(percentage_differences>15,floor(min_pts_to_use),floor(min_pts_to_use/2))

}

## Experimenting with select days.
# {
#   min_pts <- min_pts_to_use[203]
# 
#   poll_data <- windowed_data[[203]] %>%
#     select(BC,CO2,NOx,UFP) %>%
#     mutate_all(scale)
# 
#   # full_knee <- find_the_knee(poll_data,min_pts)
#   #  
#   # half_knee <- find_the_knee(poll_data,floor(min_pts/2))
#   # 
#   # plot_knees(poll_data,min_pts,title = "Day_269_Full")
# 
# }

## Knee plot for paper.
# {
#   selected_poll_data <- windowed_data[[60]] %>% dplyr::select(BC,CO2,NOx,UFP) %>% dplyr::mutate_all(scale)
#     
#   directory <- paste0(getwd(),"/Manuscript/Figs/")
#   
#   knee_output <- plot_knees(selected_poll_data, floor(0.03*nrow(selected_poll_data)), directory = directory, title = "Graphical Example of Eps Selection")
#   
#   require(ggplot2)
#   
#   knee_tibble <- tibble("Order" = seq(1,length(knee_output$NN_dists),1),
#                         "NN_Dists" = knee_output$NN_dists)
#   
#   ggplot(data = knee_tibble, aes(Order,NN_Dists))+
#     geom_line(size = 1,linetype = "dashed")+
#     geom_hline(yintercept = knee_output$Knee, color = "blue",size = 0.8)+
#     labs(x = "Order", y = "Nearest Neighbor Distance", title = "Epsilon Selection")+
#     annotate("text",x = 10000,y=knee_output$Knee+1.5, label = "Blue line is",fontface = 2)+
#     annotate("text",x = 10000,y=knee_output$Knee+1, label = "selected epsilon", fontface = 2)+
#     theme_classic()
#   
#   save_plot(paste0(getwd(),"/Manuscript/Figs/Epsilon_Selection_Example.png"),width = 6, height = 6)
# }


## Running the main DBSCAN routine
# {
#   aggregate_list <- vector(mode="list", length = length(windowed_data))
# 
#   for(j in 1:length(windowed_data)){
#     aggregate_list[[j]] <- list(windowed_data[[j]],floor(0.03*nrow(windowed_data[[j]])))
#   }
# 
#   require(parallel)
#   
#   no_cores <- detectCores()-2
#   
#   cls <- makeCluster(no_cores)
#   
#   clusterExport(cls, varlist = c("find_the_knee", "return_anomalies", "aggregate_list","%>%"), envir = .GlobalEnv)
#   
#   dbOutput <- parLapply(cls,aggregate_list,function(x) return_anomalies(x[[1]],x[[2]]))
# 
#   stopCluster(cls)
# 
# }
## Experimenting with approx parameter
# {
#   start_time <- Sys.time()
#   
#   set.seed(30)
#   
#   r_inds <- sample(seq(1,277),20)
#   
#   random_subset <- lapply(windowed_data[r_inds],function(x) x %>% select(BC,CO2,NOx,UFP) %>% mutate_all(scale))
#   
#   random_pts_to_use <- min_pts_to_use[r_inds]
#   
#   full_knee <- numeric(length(r_inds))
#   
#   approx_knee <- numeric(length(r_inds))
#   
#   for(i in 1:length(r_inds)){
#     # full_knee[i] <- find_the_knee(random_subset[[i]],floor(random_pts_to_use[i]))
#     
#     approx_knee[i] <- find_the_knee_approx(random_subset[[i]],random_pts_to_use[i])
#     
#   }
#   # knee_tibble <- tibble("No_Approx" = full_knee, "Approx"=approx_knee) %>%
#   #   kableExtra::kbl() %>%
#   #   kableExtra::kable_classic()
#   print(Sys.time()-start_time)
# }

# {
#   list_to_tibble <- function(data_subset_list){
#   # output_tibble <- unlist(data_subset_list[[1]],use.names=F)
#   # for(i in 2:length(data_subset_list)) {output_tibble <- rbind(output_tibble,unlist(data_subset_list[[i]],use.names=F))}
#   # return(output_tibble)
# 
#     current_grp_index <- 1
# 
#     output_tibble <- cbind(data_subset_list[[1]],"Uniq_Fac"=rep(current_grp_index,nrow(data_subset_list[[1]])))
# 
#     for(i in 2:length(data_subset_list)) {
#         current_grp_index <- i
# 
#         current_tibble <- cbind(data_subset_list[[i]],"Uniq_Fac"=rep(current_grp_index,nrow(data_subset_list[[i]])))
# 
#         output_tibble <- rbind(output_tibble,current_tibble)
#       }
#     return(output_tibble)
#   }
# 
#   db_tibble <- list_to_tibble(dbOutput)
# 
# #   # anomalous_emissions <- db_tibble %>%
# #   #   filter(Anomaly==2) %>%
# #   #   select(LST,BC,CO2,NOx,UFP)
# # 
#   write.csv(db_tibble,paste0(current_dir,"/Anomalous_Emissions_Results/Labeled_Emissions_DBSCAN_V05.csv"))
# }

## Finding the knee sensitivity analysis.
# {
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
#   no_subs <- 277
# 
#   knees_mat <- matrix(,nrow = 2,ncol = no_subs)
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
#     increments <- c(floor(pts/2),pts)
# 
#     knees_mat[,i] <- sapply(increments,function(x) find_the_knee(poll_data,x))
#     
#     print(paste0("Iteration ",i," Completed"))
#     print("--------------")
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
#  percentage_diffs <- apply(knees_mat,2,function(x) abs(x[1]-x[2])/x[2]*100)
# 
#  print(mean(percentage_diffs))
#  
#  print(median(percentage_diffs))
#  
#  num_obs <- sapply(windowed_data,function(x) nrow(x))
# }


## Knee analysis
# {
#   memory.limit(size = 384000)

#   trimmed_data <- lapply(windowed_data,function(x) x %>% select(BC,CO2,NOx,UFP) %>% mutate_all(scale))
# 
#   set.seed(10)
# 
#   indexes <- seq(1,length(trimmed_data),1)
# 
#   random_indexes <- sample(indexes,30)
# 
#   for(i in 1:length(random_indexes)){
#     cur_idx <- random_indexes[i]
# 
#     plot_knees(trimmed_data[[cur_idx]],floor(min_pts_to_use[cur_idx]/2),paste0("Day ",cur_idx))
# 
#     print(paste("Iteration",i,"Completed"))
# 
#     print("------------")
#   }
#   
#   # troubleshoot_indices <- c(8,18,23,25,26,33,40,44,48,62,68,71,81,84,90,93,96,105,109,116,127,131,146,157,158,187,191,194,209,228,238,243,269)
#   
#   # for(i in 1:length(trimmed_data)){
#   #   cur_idx <- i
#   # 
#   #   plot_knees(trimmed_data[[cur_idx]],floor(min_pts_to_use[cur_idx]/2),paste0("Day_",cur_idx))
#   # 
#   #   print(paste("Iteration",i,"Completed"))
#   # 
#   #   print("------------")
#   # }
# }

