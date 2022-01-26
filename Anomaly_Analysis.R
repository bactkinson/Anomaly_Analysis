require(tidyverse)
require(dbscan)
require(pracma)
require(lubridate)
require(gmailr)
require(parallel)
options(error = recover)

current_dir <- getwd()
source(paste0(current_dir,"/send_myself_mail.R"))
source(paste0(getwd(),"/SIBaR_Background_Removal_and_Quantification/SIBaRPartitioningParallel.R"))
source(paste0(getwd(),"/SIBaR_Background_Removal_and_Quantification/SIBaRUtils.R"))

emissions_anomaly_extraction <- function(windowed_data){
  require(dbscan)
  
  current_data <- windowed_data %>%
    select(BC,NOx,CO2,UFP) %>%
    mutate_all(scale)

  dbscan_res <- dbscan(current_data,eps = 1.7, minPts = nrow(current_data)/3)
  dbs_assign <- dbscan_res$cluster
  dbs_assign[dbs_assign==0] = 2
  return(cbind(windowed_data,"Anomaly" = dbs_assign) %>%
           filter(Anomaly==2))
}

list_to_tibble <- function(data_subset_list){
  # output_tibble <- unlist(data_subset_list[[1]],use.names=F)
  # for(i in 2:length(data_subset_list)) {output_tibble <- rbind(output_tibble,unlist(data_subset_list[[i]],use.names=F))}
  # return(output_tibble)
  
  output_tibble <- data_subset_list[[1]]
  for(i in 2:length(data_subset_list)) {output_tibble <- rbind(output_tibble,data_subset_list[[i]])}
  return(output_tibble)
}

{
  data_dir <- paste0(current_dir,"/Raw_Data_Alt")
  
  all_files <- list.files(data_dir)
  
  poll_files <- list.files(data_dir,pattern = "Stage")
  
  loc_files <- list.files(data_dir)[3:6]
  
  poll_df <- data.frame("NAs"=rep(NA,9179794))
  
  for(file in 1:length(poll_files)){
    param_name <- strsplit(poll_files[file], "_")[[1]][1]
    current_data <- read.csv(paste0(data_dir,"/",poll_files[file]),na.strings = c("NAN","NaN")) %>%
      rename("{param_name}" := colnames(.))
    poll_df <- cbind(poll_df,current_data)
  }
  
  poll_df <- poll_df %>%
    dplyr::select(-c(NAs)) 
  
  # poll_df <- poll_df %>%
  #   select(-c(NAs)) %>%
  #   mutate_all(replace_zeros) %>%
  #   mutate_all(log1p)
  
  loc_df <- data.frame("NAs"=rep(NA,9179794))
  
  for(file in 1:length(loc_files)){
    param_name <- strsplit(loc_files[file], "_")[[1]][1]
    current_data <- read.csv(paste0(data_dir,"/",loc_files[file]),na.strings = c("NAN","NaN")) %>%
      rename("{param_name}" := colnames(.))
    loc_df <- cbind(loc_df,current_data)
  }
  
  loc_df <- loc_df %>%
    dplyr::select(-c(NAs)) 
  
  ## Interpolate missing values
  splined_df <- zoo::na.spline(poll_df, method='monoH.FC', maxgap=6, na.rm=F)
  
  valid_df <- cbind(loc_df,splined_df) %>%
    mutate(LST = as.POSIXct(as.character(LST), format = c("%m/%d/%Y %H:%M:%S"))) %>%
    drop_na() %>%
    mutate(NOx=NO+NO2, .keep = "unused")

  if(T) {
    c1 <- which(valid_df$Index==1)
    c2 <- which(valid_df$Index==2)
    smoothed_BC_c1 <- smoothData(valid_df$BC[c1],valid_df$LST[c1],10)
    smoothed_BC_c2 <- smoothData(valid_df$BC[c2],valid_df$LST[c2],10)
    smoothed_BC <- c(smoothed_BC_c1[[1]], smoothed_BC_c2[[1]])
    valid_df$BC <- smoothed_BC
  }
  
  valid_groups <- valid_df %>%
    mutate(Month = month(LST)) %>%
    mutate(Day = mday(LST)) %>%
    group_split(Month,Day,Index,.keep = FALSE)
  
  
  lens <- unlist(lapply(valid_groups, function(x) return(nrow(x))),use.names = F)
  
  windowed_data <- valid_groups[lens>600] %>% as.list()
}


# {
#   #Hidden Markov Model experiments
#   require(depmixS4)
#   
#   applyMultivariateDepmix <- function(poll_data,nstates){
#     fm.temp <- tryCatch(
#       {
#         model <- depmixS4::depmix(list(NOx~Timestamps,CO2~Timestamps,UFP~Timestamps), data = poll_data, nstates = nstates,
#                        family = list(gaussian(),gaussian(),gaussian()))
#         fm.temp <- depmixS4::fit(model)
#       },
#       error=function(cond) {
#         return(NA)
#       }
#     )
#     return(fm.temp)
#   }
#   
#   
#   return_best_state_number <- function(windowed_data,states_to_analyze,
#                                        num_iterations){
#     poll_series <- windowed_data %>%
#       dplyr::select(NOx,UFP,CO2) %>%
#       cbind("Timestamps" = windowed_data$LST)
#     
#     bic_values <- numeric(length(states_to_analyze))
#     
#     fits <- replicate(num_iterations,poll_series,simplify = FALSE)
#     
#     for(i in 1:length(states_to_analyze)){
#       print(paste0("State_Num: ",i))
#       
#       cl_num <- detectCores()-2
#       
#       cl <- parallel::makeCluster(cl_num)
#       
#       clusterExport(cl,varlist = list("fits","states_to_analyze"), envir = environment())
#       
#       clusterExport(cl,varlist = list("applyMultivariateDepmix"), envir = .GlobalEnv)
#       
#       generated_models <- parLapply(cl, fits, function(x) applyMultivariateDepmix(x,nstates = states_to_analyze[i]))
#       
#       if(any(!is.na(unlist(generated_models)))){ 
#         model_likes <- lapply(generated_models, function(x) try(logLik(x)))
#         best_model_loc <- which.max(unlist(model_likes, use.names = FALSE))
#         best_model <- generated_models[[best_model_loc]]
#         bic_values[i] <- BIC(best_model) 
#       } else {
#         bic_values[i] <- NA
#       }
#       
#       stopCluster(cl)
#     }
#     
#     best_bic <- which.min(bic_values)
#     return(bic_values[best_bic])
#   }
#   
#   # truncated_series <- poll_series[20000:24000,]
#   
#   # load("Trouble.RData")
#   # 
#   # a <- depmixS4::fb(init = init, A = trDens, B = dens, ntimes = 25440, homogeneous = TRUE)
#   
#   states_to_analyze <- seq(2,50,1)
#   best_states <- numeric(100)
#   start_time <- Sys.time()
#   
#   for(j in 18:100){
#     print(paste0("j: ", j))
#     state_number <- return_best_state_number(windowed_data[[j]],
#                                              states_to_analyze,
#                                              50)
#     
#     best_states[j] <- state_number
#   }
#   
#   
#   
#   
#   # classes <- unlist(lapply(fits,function(x) class(x)),use.names = FALSE)
#   # 
#   # print(classes)
#   # any(classes != "try-error")
#   # plot(bic_values~no_states, pch = 19)  
#   # axis(1, at = no_states, labels = no_states)
#   # test_model <- depmix(list(NOx~Timestamps,CO2~Timestamps,UFP~Timestamps,BC~Timestamps), data = poll_series, 9,
#   #                      family = list(gaussian(),gaussian(),gaussian(),gaussian()))
#   # 
#   # test_fit <- fit(test_model, verbose = TRUE)  
#   # 
#   # states <- viterbi(test_fit)$state
#   # 
#   # long_series <- cbind(poll_series,states) %>%
#   #   tidyr::pivot_longer(cols = c(CO2,NOx,UFP,BC),names_to = "Pollutant", values_to="Value")
#   # 
#   # require(lattice)
#   
#   # print(xyplot(
#   #   Value ~ Timestamps | Pollutant, data = long_series, col = states,
#   #   layout = c(1,4),
#   #   scales = "free",
#   #   pch = 19
#   # 
#   # 
#   # 
#   # ))
#   # 
#   # anomalous_emissions <- poll_series[states==2,]
#   #   
#   # data_to_cluster <- anomalous_emissions %>%
#   #   dplyr::select(CO2,NOx,UFP)
#   # 
#   # anom_kmeans <- kmeans(data_to_cluster,centers = 3, nstart = 300)
#   # 
#   # require(NbClust)
#   # 
#   # system.time(nb_res <- NbClust(data = data_to_cluster, min.nc = 2, max.nc = 10, method = "kmeans"))
#   # 
#   # data_to_cluster <- cbind(data_to_cluster, "Cluster" = as.factor(anom_kmeans$cluster))
#   # 
#   # clustered_data_long <- data_to_cluster %>%
#   #   pivot_longer(c(CO2,NOx,UFP), names_to = "Pollutant", values_to = "Measurement")
#   # 
#   # print(
#   #   ggplot(data=clustered_data_long) + 
#   #   geom_boxplot(aes(x=Cluster,y=Measurement)) +
#   #   facet_wrap(~Pollutant, scale="free")
#   # )
#   
# }

# Determine number of points needed for DBSCAN routine.

{
  min_pts_storage <- numeric(length(windowed_data))

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
                                            length_tolerance = 0.2,
                                            cores = 2)

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

  for(j in 1:length(windowed_data)){
    poll_data <- windowed_data[[j]] %>%
      dplyr::select(BC,CO2,NOx,UFP) %>%
      mutate_all(scale)

    # sizes <- lof(poll_data,minPts = 5)

    min_pts <- find_min_pts(windowed_data[[j]],c("BC","CO2","NOx","UFP"))

    min_pts_storage[j] <- min_pts

    print(paste0("j: ",j))
    print(min_pts)
    print("------")
  }

  send_message_to_myself("Routine completed", "routine completed")
}
   
## Analyzing anomalous emissions results
{
  anomalous_data <- read.csv("C:/Users/bwa2/Documents/Academic Work/Research/Anomaly_Analysis/Anomalous_Emissions_Results/Anomalous_Emissions_cv.csv",
                             row.names = 1)

  # print(qplot("NOx",NOx,data=full_data,geom = "boxplot"))

  anomalous_emissions <- anomalous_data %>%
    select(BC,CO2,NOx,UFP) %>%
    mutate_all(scale)

  anom_list <- replicate(5,anomalous_emissions,simplify=FALSE)

  random_subsets <- lapply(anom_list,function(x) dplyr::sample_n(x,60000))

  nb_clust_res <- vector(mode = "list", length = length(random_subsets))
  
  start_time <- Sys.time()
  for(i in 1:length(random_subsets)){
    print(paste0("i: ", i))
    # require(NbClust)
    # nb_clust_res <- NbClust::NbClust(random_subsets[[i]],method = "kmeans", distance = "euclidean", min.nc = 2, max.nc = 10)
    require(cluster)
    gap_stat <- clusGap(random_subsets[[i]], FUN = kmeans, nstart = 100, K.max = 10, B = 10)
    print(factoextra::fviz_gap_stat(gap_stat) + ggtitle(paste0("Iter: ", i)))
  }
  
  anom_kmeans <- kmeans(anomalous_emissions,centers = 2, nstart = 100)

  clustered_data <- cbind(anomalous_emissions, "Cluster" = as.factor(anom_kmeans$cluster))

  clustered_data_long <- clustered_data %>%
    pivot_longer(c(BC,CO2,NOx,UFP), names_to = "Pollutant", values_to = "Measurement")

  print(
    ggplot(data=clustered_data_long) +
    geom_boxplot(aes(x=Cluster,y=Measurement)) +
    facet_wrap(~Pollutant, scale="free")
  )
  send_message_to_myself("Routine Completed on GriffinLab", paste0("Routine completed at ", Sys.time()-start_time))
}





