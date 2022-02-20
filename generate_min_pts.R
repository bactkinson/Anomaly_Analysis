setwd("../")
source(paste0(getwd(),"/SIBaR_Background_Removal_and_Quantification/SIBaRPartitioningParallel.R"))
source(paste0(getwd(),"/SIBaR_Background_Removal_and_Quantification/SIBaRUtils.R"))
setwd(paste0(getwd(),"/Anomaly_Analysis/"))


# Determine number of points needed for DBSCAN routine.
find_min_pts <- function(windowed_data,polls_to_pull){
    ## Given windowed data frame
    ## For each poll in polls_to_pull
    ## Create data frame with poll, time stamps
    ## Run partitioning routine on subsetted data_frame
    ## From partitioning routine, determine the number of points
    ## Classified as background
    ## Store number of points classified as background
    ## Repeat for all polls
    ## Once iteration through all polls in polls_to_pull complete,
    ## compute average of number pts classified as background
    ## Return average.

    num_pts <- numeric(length(polls_to_pull))

    markov_timestamps <- windowed_data %>%
      dplyr::select(LST) %>%
      unlist(use.names = FALSE) %>%
      as.POSIXct(tz = Sys.timezone(),origin = "1970-01-01")

    for(k in 1:length(polls_to_pull)){
      current_poll <- polls_to_pull[k]

      markov_poll <- windowed_data %>%
        dplyr::select(current_poll) %>%
        unlist(.,use.names = FALSE)

      tryCatch(
        {
          current_partition <- partitionRoutine(markov_poll,markov_timestamps,
                                            bootstrap_iterations = 50,
                                            transform_string = "log",
                                            length_tolerance = 0.1,
                                            threshold = 10)

          num_pts[k] <- length(which(current_partition$States==1))
        },
        error = function(cond){
          print("Error condition executed")
          num_pts[k] <- NA
        }
      )
    }

    min_pts <- mean(num_pts,na.rm = TRUE)
    return(min_pts)
  }


{
  load(paste0(getwd(),"/windowed_data.RData"))
  
  min_pts_storage <- numeric(length(windowed_data))

  for(j in 1:length(windowed_data)){
    poll_data <- windowed_data[[j]] %>%
      dplyr::select(BC,CO2,NOx,UFP) %>%
      mutate_all(scale)

    min_pts <- find_min_pts(windowed_data[[j]],c("BC","CO2","NOx","UFP"))

    min_pts_storage[j] <- min_pts

    print(paste0("j: ",j))
    print(min_pts)
    print("------")
  }
  write.csv(min_pts_storage,paste0(getwd(),"/min_pts_storage_boot/min_pts_storage_V01_run_10.csv"))
}
