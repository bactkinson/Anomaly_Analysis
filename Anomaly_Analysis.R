require(tidyverse)
require(dbscan)
require(pracma)
require(lubridate)
require(gmailr)
require(parallel)
options(error = recover)

setwd("../")
source(paste0(getwd(),"/SIBaR_Background_Removal_and_Quantification/SIBaRPartitioningParallel.R"))
source(paste0(getwd(),"/SIBaR_Background_Removal_and_Quantification/SIBaRUtils.R"))
setwd(paste0(getwd(),"/Anomaly_Analysis/"))

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

{
  current_dir <- getwd()
  
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
  
  if(T){
    splined_bc <- ifelse(!is.na(splined_df[,1]) & is.na(poll_df$BC),1,0)
    
    splined_co2 <- ifelse(!is.na(splined_df[,2]) & is.na(poll_df$CO2),1,0)
    
    splined_no <- ifelse(!is.na(splined_df[,3]) & is.na(poll_df$NO),1,0)
    
    splined_no2 <- ifelse(!is.na(splined_df[,4]) & is.na(poll_df$NO2),1,0)
    
    splined_ufp <- ifelse(!is.na(splined_df[,7]) & is.na(poll_df$UFP),1,0)
    
    splined_df <- cbind(splined_df,splined_bc,splined_co2,splined_no,splined_no2,splined_ufp)
  }
  
  
  valid_df <- cbind(loc_df,splined_df) %>%
    mutate(LST = as.POSIXct(as.character(LST), format = c("%m/%d/%Y %H:%M:%S"))) %>%
    drop_na() %>%
    mutate(NOx=NO+NO2, .keep = "unused")
  
  if(F){
    differences <- difftime(valid_df$LST,lag(valid_df$LST),units = c("mins"))
    length(which(differences>15))
    length(which(differences>20))
    length(which(differences>30))
    length(which(differences>60))
  }
  
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

## Create tibble from splined_indicators
{
  splined_indicators <- list_to_tibble(windowed_data)
  
  ## Go through shapefile processing.
  
  calculate_euclidean_distance <- function(geo_1,geo_2){
  ## For each entry in geo_1, geo_2
  ## Perform element-wise euclidean distance calculation and return result
  ## In vector
  
  ## To do this
  ## Unlist geo_1 entry
  ## Unlist geo_2 entry
  ## Calculate delta x, delta y 
  ## Square delta x, delta y
  ## Add them together
  ## Then take square root.
  
  euclid_distance <- function(x){
    c1 <- c(x[1],x[2])
    c2 <- c(x[3],x[4])
    diffs <- c1-c2
    return(sqrt(sum(diffs^2)))
    
  }
  
  print("Processing distance")
  
  coord_rows <- matrix(,nrow = length(geo_1),ncol=4)
  
  geo_coordinates_1 <- unlist(geo_1)
  
  geo_coordinates_2 <- unlist(geo_2)
  
  odds <- rep(c(TRUE,FALSE),length(geo_1))
  
  coord_rows[,1] <- geo_coordinates_1[odds]
  
  coord_rows[,2] <- geo_coordinates_1[!odds]
  
  coord_rows[,3] <- geo_coordinates_2[odds]
  
  coord_rows[,4] <- geo_coordinates_2[!odds]
  
  print(coord_rows)
  
  dists <- apply(coord_rows,1,function(x) euclid_distance(x))
  # for(i in seq_along(geo_1)){
  #   print(i)
  #   
  #   coord_1 <- unlist(geo_1[i])
  #   
  #   coord_2 <- unlist(geo_2[i])
  #   
  #   deltas <- coord_1-coord_2
  #   
  #   dists[i] <- sqrt(sum(deltas^2))
  # }
  # 
  print("Distance Processing Completed")
  return(dists)
  
  }
  
  shapefile_processing <- function(sf_df){
  # require(data.table)
  shp_points <- st_read(paste0(getwd(),"/Houston_Shape_Files/Houston_10mpts_seg50_90_final.shp"),
                                  query = 'SELECT selfsample, Longitude, Latitude, MTFCC FROM Houston_10mpts_seg50_90_final')
  
  nearest_points <- st_nearest_feature(sf_df, shp_points)
  
  sf_df_removed <- cbind(sf_df, "Self_Sample" = shp_points$selfsample[nearest_points],
                         "Road_Class"=shp_points$MTFCC[nearest_points]) %>%
    dplyr::mutate(NN_Dist=calculate_euclidean_distance(.$geometry,shp_points$geometry[nearest_points]))%>%
    filter(Self_Sample==0) %>%
    dplyr::filter(NN_Dist<30) %>%
    dplyr::filter(Road_Class=="S1100"|Road_Class=="S1200"|Road_Class=="S1400"|Road_Class=="S1630"|Road_Class=="S1640"|Road_Class=="S1740")%>%
    dplyr::select(-Self_Sample) %>%
    dplyr::select(-NN_Dist)

  return(sf_df_removed)
}
  
  final_splined_indicators <- splined_indicators %>% 
    dplyr::select(BC,CO2,NOx,UFP,splined_bc,splined_co2,splined_no,splined_no2,splined_ufp,Lat1,Long1,LST) %>% 
    st_as_sf(.,coords = c("Long1", "Lat1"), crs = "EPSG:4326") %>%
    st_transform("EPSG:32615") %>%
    shapefile_processing()
  
  if(T){
    no_bc <- final_splined_indicators %>%
      dplyr::filter(splined_bc==1) %>%
      nrow()
    
    print(no_bc)
    
    no_no <- final_splined_indicators %>%
      dplyr::filter(splined_no==1) %>%
      nrow()
    
    print(no_no)
    
    no_no2 <- final_splined_indicators %>%
      dplyr::filter(splined_no2==1) %>%
      nrow()
    
    print(no_no2)
    
    no_co2 <- final_splined_indicators %>%
      dplyr::filter(splined_co2==1) %>%
      nrow()
    
    print(no_co2)
    
    no_ufp <- final_splined_indicators %>%
      dplyr::filter(splined_ufp==1) %>%
      nrow()
    
    print(no_ufp)
    
    no_nox <- final_splined_indicators %>%
      dplyr::filter(splined_no==1 | splined_no2==1) %>%
      nrow()
    
    print(no_nox)

  }
}

## Saving data to send to Kathy.
# {
#   poll_data <- windowed_data[[1]] %>%
#     dplyr::select(LST,CO2,NOx,UFP)
#   
#   write.csv(poll_data,paste0(getwd(),"Poll_Day_1.csv"))
# }

## Preparing data to send to GriffinLab computer

# {
#   set.seed(7)
# 
#   indexes <- seq(1,length(windowed_data),1)
# 
#   rand_indexes <- sample(indexes,30)
# 
#   for(k in 1:length(rand_indexes)){
#     current_index <- rand_indexes[k]
# 
#     current_data <- cbind(windowed_data[[current_index]],"Anomaly"=rep(1,nrow(windowed_data[[current_index]])))
# 
#     write.csv(current_data,paste0(getwd(),"/Algorithm_Validation_Data/Day_",current_index,".csv"))
#   }
# }

## Create validation set.
{
  load(paste0(getwd(),"/windowed_data.RData")) %>% as.list()
  
  valid_label_files <- list.files(path=paste0(getwd(),"/Manually_Flagged_Anomalies/"))
  
  valid_data_windows <- vector(mode = "list", length = length(valid_label_files))
  
  valid_flags <- vector(,)
  
  for(j in seq_along(valid_label_files)){
    day_tag <- as.numeric(
      strsplit(
        strsplit(valid_label_files[j],"_")[[1]][3],
    "[.]")[[1]][1]
    )
    
    current_flags <- unlist(read.csv(paste0(getwd(),"/Manually_Flagged_Anomalies/",valid_label_files[j])),use.names = FALSE)
    
    valid_flags <- c(valid_flags,current_flags)
    
    valid_data_windows[[j]] <- windowed_data[[day_tag]]
    
    valid_data_windows[[j]]$Anomaly <- current_flags
  }
  
  save(valid_data_windows,file=paste0(getwd(),"/valid_data.RData"))  
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

