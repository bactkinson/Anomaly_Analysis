## R script to implement Drewnick flagging algorithm
require(tidyverse)
options(error=recover)
seconds_diff <- function(t1,t2){
  return(as.numeric(difftime(t1,t2,tz=Sys.timezone(),units = "secs")))
}

drewnick_flag <- function(windowed_data){
  poll_data <- windowed_data %>%
    select(CO2,UFP)
  
  current_timestamps <- windowed_data %>%
    select(LST) %>%
    unlist(use.names=F) %>%
    as.POSIXct(tz = Sys.timezone(), origin = "1970-01-01")
  
  ## Steps
  ## First, identify points below median of TS for given day
  ## Calculate standard deviation of points below median.
  ## Then, go through time series
  ## For each time series point, determine if it is greater than 3*sd than 
  ## previous point
  ## If it is, store previous point as background adjacent point
  ## Flag current point
  ## And then continue flagging subsequent points if those points are 3*sd + 
  ## sqrt(n)*sd greater than the stored adjacent background point
  ## Stop flagging otherwise.
  ## Do this process for CO2, UFP separately
  ## Then, for final anomaly flag step, flag the entire point directly
  ## If the point is flagged for CO2 OR UFP
  
  flag_storage <- matrix(,nrow=nrow(poll_data),ncol=ncol(poll_data))
  
  for(j in 1:ncol(poll_data)){
    current_data <- unlist(poll_data[,j],use.names=F)
    
    background_median <- median(current_data,na.rm = TRUE)
    
    below_background <- current_data[current_data<background_median]
    
    background_sd <- sd(below_background)
    
    pt_index <- 2
    
    flags <- numeric(length(current_data))
    
    flags[1] <- 1
    
    while(pt_index <= length(current_data)){
      if(current_data[pt_index]>(current_data[pt_index-1]+3*background_sd) & 
         seconds_diff(current_timestamps[pt_index],current_timestamps[pt_index-1])<5){
        
        # flags[pt_index] <- 2
        adjacent_background_point <- current_data[pt_index-1]
        adjacent_background_time <- current_timestamps[pt_index-1]
        # pt_index <- pt_index+1
        # n <- seconds_diff(current_timestamps[pt_index],adjacent_background_time)
        n <- 0
        while((pt_index <= length(current_data)) &
              (current_data[pt_index]>(adjacent_background_point+3*background_sd+sqrt(n)*background_sd))){
          

          flags[pt_index] <- 2
          pt_index <- pt_index+1
          n <- seconds_diff(current_timestamps[pt_index],adjacent_background_time)
        }
        if(pt_index < length(current_data)){flags[pt_index] <- 1; pt_index <- pt_index+1}
        
      } else{
        flags[pt_index] <- 1
        pt_index <- pt_index+1
      }
      
    }
  
    
    flag_storage[,j] <- flags
  }
  
  finalized_flags <- apply(flag_storage,1,function(x) ifelse(any(x==2),2,1))
  
  return(cbind(windowed_data,"Anomaly"=finalized_flags))
}

## Synthetic data testing
# {
#   synth_data <- c(1,1.1,1,0.9,1.2,1,8,8.2,8,1,1.5,3.4,1,1,8,9,8,8)  
#   start_date <- as.POSIXct("08-02-1994 08:00:00", tz = Sys.timezone(), format = c("%m-%d-%Y %H:%M:%S"))
#   second_start_date <- as.POSIXct("08-02-1994 08:00:14", tz = Sys.timezone(), format = c("%m-%d-%Y %H:%M:%S"))
#   first_time_seq <- seq.POSIXt(from = start_date,by="1 s",length.out=length(synth_data)/2)
#   sec_time_seq <- seq.POSIXt(from = second_start_date,by="1 s",length.out=length(synth_data)/2)
#   time_seq <- c(first_time_seq,sec_time_seq)
#   
#   
#   poll_data <- tibble("CO2"=synth_data,"UFP"=synth_data,"LST"=time_seq)
#   
#   qz <- drewnick_flag(poll_data)
#   plot(UFP~LST,data=qz,col=Anomaly)
# }


{
  current_dir <- getwd()

  load(paste0(current_dir,"/windowed_data.RData")) %>% as.list()

  windowed_data <- lapply(windowed_data,function(x) x %>% select(-c(Delta_D)))

  drewnick_flags <- lapply(windowed_data,function(x) drewnick_flag(x))
}
# 
# ## Save results
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

  drewnick_tibble <- list_to_tibble(drewnick_flags)

  write.csv(drewnick_tibble,paste0(getwd(),"/Anomalous_Emissions_Results/Labeled_Emissions_Drewnick.csv"))
  
  # anomalous_emissions <- drewnick_tibble %>%
  #   filter(Anomaly==2) %>%
  #   select(LST,BC,CO2,NOx,UFP)
  # 
  # write.csv(anomalous_emissions,paste0(current_dir,"/Anomalous_Emissions_Results/Anomalous_Emissions_Drewnick.csv"))
}

# {
#   for(k in 1:20){
#     plot(UFP~LST,data = drewnick_flags[[k]],col = Anomaly)
#     Sys.sleep(5)
#   }
# }
