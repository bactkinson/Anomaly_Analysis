require(dbscan)
require(parallel)
require(tidyverse)

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

quantile_flagged_individual <- function(percentile,windowed_data){
  
  selected_poll <- windowed_data %>%
    dplyr::select(BC,CO2,NOx,UFP) 
  
  ## For each poll in selected_poll
  ## Calculate the quantile
  ## Return whether each entry in poll is greater than calculated quantile
  ## If any is true, flag as anomaly
  ## Else, flag as normal point
  
  quantile_ids <- apply(selected_poll,2,function(x) {
    return(x > quantile(x,percentile))
  })
  
  anomalies <- apply(quantile_ids,1,function(x) any(x))
  ## With filtered poll, calculate percentage anomalies
  
  return(cbind(windowed_data,"Anomaly"=ifelse(anomalies,2,1)))
}

## Construct valid and testing datasets.
{
  valid_label_files <- list.files(path=paste0(getwd(),"/Manually_Flagged_Anomalies/"))
  
  testing_list <- vector(mode = "list", length = length(valid_label_files))
  
  load(paste0(getwd(),"/windowed_data.RData")) %>% as.list()
  
  windowed_data <- lapply(windowed_data,function(x) x %>%  dplyr::select(-c(Delta_D)))
  
  for(j in seq_along(valid_label_files)){
    day_tag <- as.numeric(
      strsplit(
        strsplit(valid_label_files[j],"_")[[1]][3],
    "[.]")[[1]][1]
    )
    
    print(day_tag)
    
    testing_list[[j]] <- windowed_data[[day_tag]]
  }
  
  load(paste0(getwd(),"/valid_data.RData")) %>% as.list()
  
  validated_data <- list_to_tibble(valid_data_windows)
}

# {
#   require(parallel)
# 
#   ## Construct eps grid to span increments of 0.05 between 0.1 and 0.5.
#   f_grid <- c(seq(0.01,0.10,by = 0.01),seq(0.15,0.5,by = 0.05))
#   ## Preallocate percentage agreement vector
#   f_valid_percents <- numeric(length(f_grid))
# 
#   ## For each value in f grid.
#   ## Run the DBSCAN routine flooring the multiplication of the number of rows in data by
#   ## the value in f_grid.
# 
#   for(i in seq_along(f_grid)){
# 
#     f_val <- f_grid[i]
# 
#     aggregate_list <- vector(mode="list", length = length(testing_list))
# 
#     for(j in 1:length(testing_list)){
#       aggregate_list[[j]] <- list(testing_list[[j]],floor(nrow(testing_list[[j]])*f_val))
#     }
# 
#     no_cores <- detectCores()-2
# 
#     cls <- makeCluster(no_cores)
# 
#     clusterExport(cls, varlist = c("find_the_knee", "return_anomalies", "aggregate_list","%>%"), envir = .GlobalEnv)
# 
#     dbOutput <- parLapply(cls,aggregate_list,function(x) return_anomalies(x[[1]],x[[2]]))
# 
#     stopCluster(cls)
# 
#     ## After finding dbOutput, calculate percentage agreement between labeled anomalies
#     ## and the validated data.
# 
#     db_tibble <- list_to_tibble(dbOutput)
# 
#     validation_percent <- length(which(db_tibble$Anomaly==validated_data$Anomaly))/nrow(validated_data)*100
# 
#     f_valid_percents[i] <- validation_percent
# 
#     print(paste0("Execution of f_grid value = ", f_val, " completed."))
#   }
# 
#   f_val_results <- tibble("F_value"=f_grid, "Validation_Percentage"=f_valid_percents)
# 
#   require(kableExtra)
#   require(knitr)
# 
#   print(
#     f_val_results %>%
#       round(.,2) %>%
#       kbl() %>%
#       kable_classic()
#   )
# }

## Impelment cross validation procedure.
# {
#   ## 5-fold cross validation
#   ## Partition data into 5 sets of 6 days randomly
#   ## Tune the f_val parameter to 4 of the 5 partitions. Training set.
#   ## Select the model with the best performance
#   ## Validate its performance on the remaining partition. Testing set.
#   ## Determine which f_value has best performance on the testing set.
#   
#   f_grid <- c(seq(0.01,0.10,by = 0.01),seq(0.15,0.5,by = 0.05))
#   # f_grid <- seq(0.01,0.03,by = 0.01)
#   
#   require(parallel)
#   
# 
#   ## For each value in f grid.
#   ## Run the DBSCAN routine flooring the multiplication of the number of rows in data by
#   ## the value in f_grid.
#   
#   k_fold_performance <- numeric(5)
#   
#   k_fold_f_values <- numeric(5)
#   
#   k_folds <- vector(mode = "list",length = 5)
#   
#   ## For reproducibility
#   set.seed(10)
#   
#   data_partitions <- sample(seq(1,30,1),30,replace = FALSE)
#   
#   current_value <- 1
#   for(k in 1:length(k_folds)){
#     k_folds[[k]] <- data_partitions[current_value:(current_value+5)]
#     current_value <- current_value+6
#     print(current_value)
#   }
#   for(k in 1:length(k_folds)){
#     
#     print(paste0("Executing fold ",k))
#     
#     ## Preallocate percentage agreement vector
#     f_valid_percents <- numeric(length(f_grid))
#     
#     testing_days <- k_folds[[k]]
#     
#     training_days <- unlist(k_folds[-k],use.names=F)
#     
#     training_set <- testing_list[training_days]
#     
#     valid_training_set <- valid_data_windows[training_days]
#     
#     testing_set <- testing_list[testing_days]
#     
#     valid_testing_set <- valid_data_windows[testing_days]
#     
#     print("Training Days")
#     print(training_days)
#     print("---------------")
#     print("Testing Days")
#     print(testing_days)
#     print("---------------")
#     
#     for(i in seq_along(f_grid)){
#       
#       f_val <- f_grid[i]
#       
#       aggregate_list <- vector(mode="list", length = length(training_set))
#       
#       for(j in 1:length(training_set)){
#         aggregate_list[[j]] <- list(training_set[[j]],floor(nrow(training_set[[j]])*f_val))
#       }
#       
#       no_cores <- detectCores()-2
#       
#       cls <- makeCluster(no_cores)
#       
#       clusterExport(cls, varlist = c("find_the_knee", "return_anomalies", "aggregate_list","%>%"), envir = .GlobalEnv)
#       
#       dbOutput <- parLapply(cls,aggregate_list,function(x) return_anomalies(x[[1]],x[[2]]))
#       
#       stopCluster(cls)
#       
#       ## Return the model with the highest percentage agreement based on f_val
#       
#       db_tibble <- list_to_tibble(dbOutput)
#       
#       valid_training_tibble <- list_to_tibble(valid_training_set)
#       
#       validation_percent <- length(which(db_tibble$Anomaly==valid_training_tibble$Anomaly))/nrow(valid_training_tibble)*100
#       
#       f_valid_percents[i] <- validation_percent
#       
#       print(paste0("Execution of f_grid value = ", f_val, " completed."))
#       print("-------------")
#       print(paste0("Validation percentage: ",validation_percent))
#     }
#     
#     ## Pick the best model.
#     trained_f_value <- f_grid[which.max(f_valid_percents)]
#     print(paste0("f_value to be tested: ", trained_f_value))
#     print("--------------")
#     
#     ## Perform DBSCAN clustering on remaining testing data
#     testing_aggregate_list <- vector(mode = "list", length = length(testing_set))
#     
#     for(j in 1:length(testing_aggregate_list)){
#       testing_aggregate_list[[j]] <- list(testing_set[[j]],floor(nrow(testing_set[[j]])*trained_f_value))
#     }
#     
#     cls <- makeCluster(no_cores)
#     
#     clusterExport(cls, varlist = c("find_the_knee","return_anomalies","testing_aggregate_list","%>%"), envir = .GlobalEnv)
#     
#     testingDbOutput <- parLapply(cls,testing_aggregate_list, function(x) return_anomalies(x[[1]],x[[2]]))
#     
#     stopCluster(cls)
#     
#     tested_db_tibble <- list_to_tibble(testingDbOutput)
#     
#     valid_testing_tibble <- list_to_tibble(valid_testing_set)
#     
#     performance <- length(which(tested_db_tibble$Anomaly==valid_testing_tibble$Anomaly))/nrow(valid_testing_tibble)*100
#     
#     print("Testing Performance")
#     print(performance)
#     print("---------------")
#     k_fold_performance[k] <- performance
#     k_fold_f_values[k] <- trained_f_value
#   }
#   
#   performance_results <- tibble("Fold"=seq(1,5,1),
#                                 "Trained F Value"=k_fold_f_values,
#                                 "Testing Performance (%)"=k_fold_performance) %>%
#     dplyr::mutate_at(3,~round(.,2)) %>%
#     kableExtra::kbl() %>%
#     kableExtra::kable_minimal()
#   
#   print(performance_results)  
# }

## Validate 90th quantile. See if better quantile can be obtained.
# quantile_flagged_individual <- function(percentile,windowed_data){
#   
#   selected_poll <- windowed_data %>%
#     dplyr::select(BC,CO2,NOx,UFP) 
#   
#   ## For each poll in selected_poll
#   ## Calculate the quantile
#   ## Return whether each entry in poll is greater than calculated quantile
#   ## If any is true, flag as anomaly
#   ## Else, flag as normal point
#   
#   quantile_ids <- apply(selected_poll,2,function(x) {
#     return(x > quantile(x,percentile))
#   })
#   
#   anomalies <- apply(quantile_ids,1,function(x) any(x))
#   ## With filtered poll, calculate percentage anomalies
#   
#   return(cbind(windowed_data,"Anomaly"=ifelse(anomalies,2,1)))
# }

## 5-fold cross validation for quantile values
## Same as above, but with the quantile value instead
{
  q_grid <- seq(0.85,0.98,by = 0.005)
  
  ## For each value in f grid.
  ## Run the DBSCAN routine flooring the multiplication of the number of rows in data by
  ## the value in f_grid.
  
  k_fold_performance <- numeric(5)
  
  k_fold_q_values <- numeric(5)
  
  k_folds <- vector(mode = "list",length = 5)
  
  ## For reproducibility
  set.seed(10)
  
  data_partitions <- sample(seq(1,30,1),30,replace = FALSE)
  
  current_value <- 1
  for(k in 1:length(k_folds)){
    k_folds[[k]] <- data_partitions[current_value:(current_value+5)]
    current_value <- current_value+6
    print(current_value)
  }
  for(k in 1:length(k_folds)){
    
    print(paste0("Executing fold ",k))
    
    ## Preallocate percentage agreement vector
    q_valid_percents <- numeric(length(q_grid))
    
    testing_days <- k_folds[[k]]
    
    training_days <- unlist(k_folds[-k],use.names=F)
    
    training_set <- testing_list[training_days]
    
    valid_training_set <- valid_data_windows[training_days]
    
    testing_set <- testing_list[testing_days]
    
    valid_testing_set <- valid_data_windows[testing_days]
    
    print("Training Days")
    print(training_days)
    print("---------------")
    print("Testing Days")
    print(testing_days)
    print("---------------")
    
    for(i in seq_along(q_grid)){
      
      q_val <- q_grid[i]
      
      q_output <- lapply(training_set,function(x) quantile_flagged_individual(q_val,x))
      
      qor_tibble <- list_to_tibble(q_output)
      
      valid_training_tibble <- list_to_tibble(valid_training_set)
      
      validation_percent <- length(which(qor_tibble$Anomaly==valid_training_tibble$Anomaly))/nrow(valid_training_tibble)*100
      
      q_valid_percents[i] <- validation_percent
      
      print(paste0("Execution of q_grid value = ", q_val, " completed."))
    }
    
    ## Pick the best model.
    trained_q_value <- q_grid[which.max(q_valid_percents)]
    print(paste0("q_value to be tested: ", trained_q_value))
    print("--------------")
    
    ## Perform DBSCAN clustering on remaining testing data
    testingQOutput <- lapply(testing_set, function(x) quantile_flagged_individual(trained_q_value,x))
    
    tested_q_tibble <- list_to_tibble(testingQOutput)
    
    valid_testing_tibble <- list_to_tibble(valid_testing_set)
    
    performance <- length(which(tested_q_tibble$Anomaly==valid_testing_tibble$Anomaly))/nrow(valid_testing_tibble)*100
    
    print("Testing Performance")
    print(performance)
    print("---------------")
    k_fold_performance[k] <- performance
    k_fold_q_values[k] <- trained_q_value
  }
  
  performance_results <- tibble("Fold"=seq(1,5,1),
                                "Trained Q Value"=k_fold_q_values,
                                "Testing Performance (%)"=k_fold_performance) %>%
    dplyr::mutate_at(3,~round(.,2)) %>%
    kableExtra::kbl() %>%
    kableExtra::kable_minimal()
  
  print(performance_results)
}
# {
#   require(parallel)
#   
#   ## Construct quantile grid to span increments of 0.05 between 0.1 and 0.5.
#   q_grid <- seq(.85,.98,0.005)
#   ## Preallocate percentage agreement vector
#   q_valid_percents <- numeric(length(q_grid))
#   
#   ## For each value in f grid.
#   ## Run the DBSCAN routine flooring the multiplication of the number of rows in data by
#   ## the value in f_grid.
#   
#   for(i in seq_along(q_grid)){
#     
#     q_val <- q_grid[i]
#     
#     q_output <- lapply(testing_list,function(x) quantile_flagged_individual(q_val,x))
#     
#     qor_tibble <- list_to_tibble(q_output)
#     
#     validation_percent <- length(which(qor_tibble$Anomaly==validated_data$Anomaly))/nrow(validated_data)*100
#     
#     q_valid_percents[i] <- validation_percent
#     
#     print(paste0("Execution of q_grid value = ", q_val, " completed."))
#   }
#  
#   f_val_results <- tibble("Q_value"=q_grid, "Validation_Percentage"=q_valid_percents)
#   
#   require(kableExtra)
#   require(knitr)
#   
#   print(
#     f_val_results %>%
#       kbl() %>%
#       kable_classic()
#   )
# }
