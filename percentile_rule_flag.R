## OR rule for quantile flagging
require(tidyverse)
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

## AND rule for quantile flagging
quantile_flagged_joint <- function(percentile,windowed_data){
  
  selected_poll <- windowed_data %>%
    dplyr::select(BC,CO2,NOx,UFP) 
  
  quantile_ids <- apply(selected_poll,2,function(x) x > quantile(x,percentile))
  
  anomalies <- apply(quantile_ids,1,function(x) all(x))
  
  return(cbind(windowed_data,"Anomaly"=ifelse(anomalies,2,1)))
}

## Read in the data
{
  current_dir <- getwd()

  load(paste0(current_dir,"/windowed_data.RData")) %>% as.list()

  windowed_data <- lapply(windowed_data,function(x) x %>% select(-c(Delta_D)))

  quantile_or <- lapply(windowed_data,function(x) quantile_flagged_individual(0.9,x))
  
  quantile_and <- lapply(windowed_data, function(x) quantile_flagged_joint(0.9,x))
}

## Save the results
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

  quantile_flagged_and_tibble <- list_to_tibble(quantile_and)
  
  quantile_flagged_or_tibble <- list_to_tibble(quantile_or)

  write.csv(quantile_flagged_and_tibble,paste0(getwd(),"/Anomalous_Emissions_Results/Labeled_Emissions_Quantile_AND.csv"))
  
  write.csv(quantile_flagged_or_tibble,paste0(getwd(),"/Anomalous_Emissions_Results/Labeled_Emissions_Quantile_OR.csv"))
}
