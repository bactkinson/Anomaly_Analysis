# options(error=recover)
## Load required packages
require(dbscan)
require(dplyr)
require(tidyverse)
require(parallel)
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


plot_knees <- function(poll_data,min_pts,title){
  
  png(paste0(getwd(),"/Miscellaneous_Figures/Knee_Plots_Quantile_Comparison/",title,".png"))
  
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
  
  dist_subset <- NNs[1:30]
  for(j in 31:length(NNs)){
    if(NNs[j] > qnorm(0.95, mean=mean(dist_subset),sd = sd(dist_subset))){
      second_knee <- NNs[j]
      break
    } else{
      dist_subset <- c(dist_subset,NNs[j])
    }
  }
  
  plot(NNs~1,main = title,type = "l")
  
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
  
  current_eps <- find_the_knee(poll_data,min_pts = min_pts_param)
  
  db_clust <- dbscan::dbscan(poll_data,minPts = min_pts_param,eps = current_eps)
  
  assignments <- db_clust$cluster
  
  assignments[assignments==0] <- 2
  
  print("Execution complete")
  
  return(cbind(windowed_data,"Anomaly"=assignments))
}


{
  memory.limit(size = 384000)
  
  start_time <- Sys.time()

  current_dir <- getwd()

  load(paste0(current_dir,"/windowed_data.RData")) %>% as.list()

  windowed_data <- lapply(windowed_data,function(x) x %>%  dplyr::select(-c(Delta_D)))

  min_pts_to_use <- read.csv(paste0(current_dir,"/min_pts_storage.csv"))[,2]
  
  ## Preprocess min_pts
  percentage_differences <- read.csv(paste0(current_dir,"/one_half_percentage_diffs.csv"))[,2]
  
  min_pts_modified <- ifelse(percentage_differences>15,floor(min_pts_to_use),floor(min_pts_to_use/2))

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

## Running the main DBSCAN routine
{
  aggregate_list <- vector(mode="list", length = length(min_pts_to_use))

  for(j in 1:length(aggregate_list)){
    aggregate_list[[j]] <- list(windowed_data[[j]],min_pts_to_use[j])
  }

  dbOutput <- lapply(aggregate_list,function(x) return_anomalies(x[[1]],x[[2]]))


  # dbOutput <- vector(mode = "list",length = length(aggregate_list))
  # 
  # for(j in 1:length(aggregate_list)){
  #   print(j)
  #   dbOutput[[j]] <- return_anomalies(aggregate_list[[j]][[1]], aggregate_list[[j]][[2]])
  # }

  # for(j in 1:20){
  #     file_string <- paste0("Iteration_",j,"cv.png")
  #     png(paste0(current_dir,"/Anomaly_Analysis_Plots/",file_string))
  #     plot(NOx~CO2, data = dbOutput[[j]],  col = Anomaly,pch = 20)
  #     dev.off()
  # }


## Experimenting with approx parameter
{
  start_time <- Sys.time()
  
  set.seed(30)
  
  r_inds <- sample(seq(1,277),20)
  
  random_subset <- lapply(windowed_data[r_inds],function(x) x %>% select(BC,CO2,NOx,UFP) %>% mutate_all(scale))
  
  random_pts_to_use <- min_pts_to_use[r_inds]
  
  full_knee <- numeric(length(r_inds))
  
  approx_knee <- numeric(length(r_inds))
  
  for(i in 1:length(r_inds)){
    # full_knee[i] <- find_the_knee(random_subset[[i]],floor(random_pts_to_use[i]))
    
    approx_knee[i] <- find_the_knee_approx(random_subset[[i]],random_pts_to_use[i])
    
  }
  # knee_tibble <- tibble("No_Approx" = full_knee, "Approx"=approx_knee) %>%
  #   kableExtra::kbl() %>%
  #   kableExtra::kable_classic()
  print(Sys.time()-start_time)
}

# {
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

#   # anomalous_emissions <- db_tibble %>%
#   #   filter(Anomaly==2) %>%
#   #   select(LST,BC,CO2,NOx,UFP)
# 
  write.csv(db_tibble,paste0(current_dir,"/Anomalous_Emissions_Results/Labeled_Emissions_DBSCAN_V02_test.csv"))
# }



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
#   no_subs <- 277
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
# {
#   memory.limit(size = 384000)
# 
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
