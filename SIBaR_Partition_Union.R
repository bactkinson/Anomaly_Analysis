## Script that determines whether points are anomalies by applying HMMs on 
## the 4 pollutants separately, then classifying them as anomaly if any one
## pollutant categorized as source
require(tidyverse)
require(depmixS4)

setwd("../")
source(paste0(getwd(),"/SIBaR_Background_Removal_and_Quantification/SIBaRPartitioningParallel.R"))
source(paste0(getwd(),"/SIBaR_Background_Removal_and_Quantification/SIBaRUtils.R"))
setwd(paste0(getwd(),"/Anomaly_Analysis"))


sibar_anomalies <- function(windowed_data,polls_to_pull){
  
  state_res <- matrix(,nrow=nrow(windowed_data),ncol=length(polls_to_pull))
  
  markov_timestamps <- windowed_data %>%
    dplyr::select(LST) %>%
    unlist(use.names = FALSE) %>%
    as.POSIXct(tz = Sys.timezone(),origin = "1970-01-01")
  
  for(k in 1:length(polls_to_pull)){
    
    current_poll <- polls_to_pull[k]
    
    markov_poll <- windowed_data %>%
      dplyr::select(current_poll) %>%
      unlist(use.names = FALSE)
    
    tryCatch(
      {
        current_partition <- partitionRoutine(markov_poll,markov_timestamps,
                                              bootstrap_iterations = 50,
                                              transform_string = "log",
                                              length_tolerance = 0.2)
        
        state_res[,k] <- current_partition$States
        
      },
      error = function(cond){
        print("Error condition executed")
        state_res[,k] <- NA
      }
    )
  }
  
  anomalies <- apply(state_res,1,function(x) ifelse(any(x==2),2,1))
  
  return(cbind(windowed_data,"Anomaly" = anomalies))
}

{
  current_dir <- getwd()
  
  load(paste0(current_dir,"/windowed_data.RData")) %>% as.list() 
  
  windowed_data <- lapply(windowed_data,function(x) x %>% dplyr::select(-c(Delta_D)))
  
  sibar_flags <- lapply(windowed_data,function(x) sibar_anomalies(x,polls_to_pull = c("CO2","NOx","UFP")))
}

## Post data processing
{
  list_to_tibble <- function(data_subset_list){

    output_tibble <- data_subset_list[[1]]
    for(i in 2:length(data_subset_list)) {output_tibble <- rbind(output_tibble,data_subset_list[[i]])}
    return(output_tibble)
  }

  sibar_tibble <- list_to_tibble(sibar_flags)

  anomalous_emissions <- sibar_tibble %>%
    filter(Anomaly==2) %>%
    select(LST,BC,CO2,NOx,UFP)

  write.csv(anomalous_emissions,paste0(current_dir,"/Anomalous_Emissions_Results/Anomalous_Emissions_SIBaR.csv"))
}
