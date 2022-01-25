require(zoo)
require(tidyverse)

## Hagler method
## Break campaign data up into days.
## For each day, calculate the rolling 5 s standard deviation
## Calculate the mean of the entire day.
## Divide rolling 5 s standard deviation by mean of entire day
## Flag values greater than or equal to 2.
## Return flagged values
hagler_cov <- function(windowed_data){
  poll_data <- windowed_data %>%
    select(UFP)

  timestamps <- windowed_data %>%
    select(LST) %>%
    unlist(use.names = FALSE) %>%
    as.POSIXct(tz = Sys.timezone(),origin = "1970-01-01")
  
  plume_flags <- matrix(,nrow=nrow(poll_data),ncol = ncol(poll_data))
  
  for(j in 1:ncol(poll_data)){
    ## Process, prepare irregular data.
    irregular_zoo <- zoo::zoo(poll_data[,j],order.by = timestamps)
    full_time_sequence <- seq.POSIXt(from = timestamps[1],to = timestamps[length(timestamps)],by = "sec")
    full_na_sequence <- zoo::zoo(rep(NA,length(full_time_sequence)),order.by = full_time_sequence)
    complete_zoo <- merge.zoo(full_na_sequence,irregular_zoo)[,2]
    ## Implement rolling statistics
    poll_sd <- zoo::rollapply(data = complete_zoo, width = 5, function(x) sd(x), fill = NA, partial = TRUE)
    poll_mean <- mean(coredata(complete_zoo),na.rm  = TRUE)
    poll_cov <- poll_sd/poll_mean
    poll_cov <- poll_cov[which(index(poll_cov) %in% timestamps)]
    
    ## Determine if poll is anomalous
    plume_flags[,j] <- (poll_cov >= 2 & !is.na(poll_cov))
  }
  
  anomalies <- apply(plume_flags,1,function(x) any(x))
  
  labeled_data <- windowed_data %>%
    mutate(Anomaly = ifelse(anomalies,2,1))
  
  return(labeled_data)
}

t_vec <- c(1,3,2,2.5,5,NA)

{
  current_dir <- getwd()
  
  load(paste0(current_dir,"/windowed_subset.RData")) %>% as.list() 
  
  windowed_subset <- lapply(windowed_subset,function(x) x %>% select(-c(Anomaly)))
  
  hagler_flags <- lapply(windowed_subset,function(x) hagler_cov(x))
  
}
