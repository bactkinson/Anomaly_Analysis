## Analyzing anomalous emissions results
require(cluster)
require(NbClust)
require(tidyverse)

## Writing function that colors anomalous points red in time series data to evaluate
## algorithm performance

plot_time_series_anomalies <- function(windowed_data,polls_to_pull,title,
                                       save_graph = F, directory = getwd()){

  poll_data <- windowed_data %>%
    select(all_of(polls_to_pull),LST,Anomaly) %>%
    mutate(LST=as.POSIXct(LST,format = c("%Y-%m-%d %H:%M:%S"),tz = Sys.timezone()),.keep = "unused") %>%
    pivot_longer(cols = all_of(polls_to_pull),names_to = "Pollutant", values_to = "Value")

  poll_subset <- list()

  if(nrow(poll_data)>10000){
    poll_subset[[1]] <- poll_data[1:floor(nrow(poll_data)/2),]
    poll_subset[[2]] <- poll_data[(floor(nrow(poll_data)/2):nrow(poll_data)),]
  } else{
    poll_subset[[1]] <- poll_data
  }


  require(lattice)
  require(latticeExtra)
  require(grid)

  for(i in 1:length(poll_subset)){
    
    if(save_graph){png(paste0(directory,title,"_Part_",i,".png"))}
    
    print(
      xyplot(Value ~ LST | Pollutant,
             data = poll_subset[[i]],
             type = "p",
             grid = TRUE,
             group = Anomaly,
             col = c("black","red"),
             layout = c(1,length(polls_to_pull)),
             size = 0.3,
             scale = "free",
             strip = F,
             xlab = "Time (US Central)",
             ylab = "Concentration",
             main = paste0(title," Part ",i),
             panel = function(...) {
               panel.xyplot(...)
               draw.key(
                 list(col='black',text=list(polls_to_pull[panel.number()])),
                 draw=T,
                 vp=viewport(x=unit(.1,'npc'),y=unit(.9,'npc'))
               )
             }
      )


    )
    
    if(save_graph){dev.off()}
  }

  print("Plotting complete")

}

## For a given data set, find the fraction of points flagged as anomalies
## given percentile and poll
fraction_flagged <- function(percentile,poll,data){
  
  selected_poll <- data %>%
    select(all_of(poll)) %>%
    unlist(use.names = FALSE)
  
  threshold <- quantile(selected_poll,percentile)
  
  filtered_poll <- data %>%
    filter(selected_poll > threshold)
  
  ## With filtered poll, calculate percentage anomalies
  
  fraction_anomalies <- length(which(filtered_poll$Anomaly==2))/nrow(filtered_poll)
  
  return(fraction_anomalies*100)
}


{
 ## Read in the anomalous data. Preprocessing
  current_dir <- getwd()
  
  db_data <- read.csv(paste0(current_dir,"/Anomalous_Emissions_Results/Labeled_Emissions_DBSCAN_V01_run_1.csv"),
                             row.names = 1)
  
  drew_data <- read.csv(paste0(current_dir,"/Anomalous_Emissions_Results/Labeled_Emissions_Drewnick.csv"),
                             row.names = 1)

  qor_data <- read.csv(paste0(current_dir,"/Anomalous_Emissions_Results/Labeled_Emissions_Quantile_OR.csv"),
                             row.names = 1)

  qand_data <- read.csv(paste0(current_dir,"/Anomalous_Emissions_Results/Labeled_Emissions_Quantile_AND.csv"),
                             row.names = 1)

  drew_grouped_anomalies <- drew_data %>%
    group_split(Uniq_Fac,.keep = FALSE)

  db_grouped_anomalies <- db_data %>%
    group_split(Uniq_Fac,.keep = FALSE)

  qor_grouped_anomalies <- qor_data %>%
    group_split(Uniq_Fac,.keep = FALSE)

  qand_grouped_anomalies <- qand_data %>%
    group_split(Uniq_Fac,.keep = FALSE)

  # anomalous_emissions <- anomalous_data %>%
  #   filter(Anomaly==2) %>%
  #   dplyr::select(BC,CO2,NOx,UFP) %>%
  #   mutate_all(scale)
}

# Cluster validation
# {
#   anom_list <- replicate(1,anomalous_emissions,simplify=FALSE)
# 
#   random_subsets <- lapply(anom_list,function(x) dplyr::sample_n(x,62000))
# 
#   nb_clust_res <- vector(mode = "list", length = length(random_subsets))
# 
#   start_time <- Sys.time()
#   
#   # for(i in 1:length(random_subsets)){
#     # print(paste0("i: ", i))
#     # nb_clust_res[[i]] <- NbClust::NbClust(random_subsets[[i]],method = "kmeans", 
#     #                                       distance = "manhattan", 
#     #                                       min.nc = 2, max.nc = 10,
#     #                                       index = "gap")$Best.partition
#     # gap_stat <- clusGap(random_subsets[[i]], FUN = kmeans, nstart = 100, K.max = 10, B = 10)
#     # print(factoextra::fviz_gap_stat(gap_stat) + ggtitle(paste0("Iter: ", i)))
#     
#     
#   # }
#   
# }


## From cluster evaluation results above, determine best number of kmeans cluster centers
## and visualize results.
# {
#   set.seed(5)
# 
#   anom_kmeans <- kmeans(anomalous_emissions,centers = 3, nstart = 100)
# 
#   clustered_data <- cbind(anomalous_emissions, "Cluster" = as.factor(anom_kmeans$cluster))
# 
#   clustered_data_long <- clustered_data %>%
#     pivot_longer(c(BC,CO2,NOx,UFP), names_to = "Pollutant", values_to = "Measurement")
# 
#   print(
#     ggplot(data=clustered_data_long) +
#     geom_boxplot(aes(x=Cluster,y=Measurement)) +
#     facet_wrap(~Pollutant, scale="free")
#   )
# 
#   require(factoextra)
# 
#   fviz_cluster(anom_kmeans,data=anomalous_emissions,
#                geom = "point",
#                main = "3 Cluster Solution")
# 
#   res_pca <- FactoMineR::PCA(anomalous_emissions,scale.unit = TRUE,ncp = 4) 
#   
#   fviz_pca_biplot(res_pca,
#                   axes = c(3,4),
#                   geom = c("point","point"))
# }


## Plotting DBSCAN results for each day.
# {
#   
#   for(k in 1:length(db_grouped_anomalies)){
#     plot_time_series_anomalies(db_grouped_anomalies[[k]], polls_to_pull = c("BC","CO2","NOx","UFP"),
#                                title = paste0("DBSCAN_Day_",k),
#                                save_graph = T,
#                                directory = paste0(getwd(),"/Miscellaneous_Figures/DB_Results/"))
#   }
# }

## Label validation. Assessing across entire dataset.
{
  require(caret)
  
  valid_label_files <- list.files(path=paste0(getwd(),"/Manually_Flagged_Anomalies/"))
  
  db_flags <- vector(,)
  
  drew_flags <- vector(,)
  
  qor_flags <- vector(,)
  
  qand_flags <- vector(,)
  
  valid_flags <- vector(,)
  
  load(paste0(getwd(),"/valid_data.RData")) %>% as.list()
  
  for(j in seq_along(valid_data_windows)){
    day_tag <- as.numeric(
      strsplit(
        strsplit(valid_label_files[j],"_")[[1]][3],
    "[.]")[[1]][1]
    )
    if(day_tag==218){
      next
    }
    
    print(day_tag)
    
    db_flags <- c(db_flags,db_grouped_anomalies[[day_tag]]$Anomaly)
    
    drew_flags <- c(drew_flags, drew_grouped_anomalies[[day_tag]]$Anomaly)
    
    qor_flags <- c(qor_flags, qor_grouped_anomalies[[day_tag]]$Anomaly)
    
    qand_flags <- c(qand_flags, qand_grouped_anomalies[[day_tag]]$Anomaly)
    
    valid_flags <- c(valid_flags,valid_data_windows[[j]]$Anomaly)
  }
  print("Percentage Agreeement between DBSCAN, validation set")  
  print(length(which(db_flags==valid_flags))/length(valid_flags))
  print("---------")
  
  print("Percentage Agreeement between Drewnick, validation set")  
  print(length(which(drew_flags==valid_flags))/length(valid_flags))
  print("---------")
  
  print("Percentage Agreeement between QOR, validation set")  
  print(length(which(qor_flags==valid_flags))/length(valid_flags))
  print("---------")
  
  print("Percentage Agreeement between QAND, validation set")  
  print(length(which(qand_flags==valid_flags))/length(valid_flags))
  print("---------")
  
  baseline_flags <- rep(1,length(valid_flags))
  
  print("Percentage Agreeement between baseline, validation set")  
  print(length(which(baseline_flags==valid_flags))/length(valid_flags))
  print("---------")
  
  db_cm <- caret::confusionMatrix(as.factor(db_flags), as.factor(valid_flags))
  
  drew_cm <- caret::confusionMatrix(as.factor(drew_flags), as.factor(valid_flags))
  
  qor_cm <- caret::confusionMatrix(as.factor(qor_flags), as.factor(valid_flags))
  
  qand_cm <- caret::confusionMatrix(as.factor(qand_flags), as.factor(valid_flags))
  
  # as.table(db_cm) %>%
  #   kbl() %>%
  #   kable_styling()
  # 
  # as.table(qor_cm) %>%
  #   kbl() %>%
  #   kable_classic()
  
  # db_table <- as.data.frame(db_cm$table) %>%
  #   mutate(Agreement = ifelse(Prediction==Reference, "Agree", "Disagree")) %>%
  #   group_by(Reference)
  # 
  # ggplot(data = db_table, aes(Prediction, Reference, fill = Agreement)) +
  #   geom_tile(aes(alpha = 0.1)) + 
  #   geom_text(aes(label=Freq),fontface = "bold", size = 5, alpha = 1) +
  #   scale_fill_manual(values = c(Agree = "blue", Disagree = "red")) +
  #   ylim(rev(levels(db_table$Reference)))+
  #   labs(title = "DBSCAN Confusion Matrix")+
  #   theme(legend.position = "none")
  
  plot_confusion_matrix <- function(data_flags, reference_flags,plot_title,reference_label,prediction_label){
    # data_labels <- ifelse(data_flags==1,"Non_Anomaly","Anomaly")
    # 
    # reference_labels <- ifelse(reference_flags==1,"Non_Anomaly","Anomaly")
    
    # res_cm <- caret::confusionMatrix(as.factor(data_labels), as.factor(reference_labels))
    
    res_cm <- caret::confusionMatrix(as.factor(data_flags), as.factor(reference_flags))
    
    res_table <- as.data.frame(res_cm$table) %>%
      mutate(Agreement = ifelse(Prediction==Reference, "Agree", "Disagree")) %>%
      group_by(Reference)
    
    print(
      ggplot(data = res_table, aes(Prediction, Reference, fill = Agreement)) +
        geom_tile(aes(alpha = 0.1)) + 
        geom_text(aes(label=Freq),fontface = "bold", size = 5, alpha = 1) +
        scale_fill_manual(values = c(Agree = "blue", Disagree = "red")) +
        labs(title = plot_title,x=prediction_label,y=reference_label)+
        theme(legend.position = "none")+
        ylim(rev(levels(res_table$Reference)))
      
    )

  }
  
  plot_confusion_matrix(db_flags, valid_flags, "DBSCAN Confusion Matrix")
  
  plot_confusion_matrix(qor_flags, valid_flags, "QOR Confusion Matrix")
  
  plot_confusion_matrix(qand_flags, valid_flags, "QAND Confusion Matrix")
  
  plot_confusion_matrix(drew_flags, valid_flags, "Drewnick Confusion Matrix")
    
}

# {
#   half_emissions <- read.csv(paste0(getwd(),"/Anomalous_Emissions_Results/Labeled_Emissions_DBSCAN_V01_one_half.csv"))
#   
#   full_emissions <- read.csv(paste0(getwd(),"/Anomalous_Emissions_Results/Labeled_Emissions_DBSCAN_V01_all.csv"))
#   
#   plot_confusion_matrix(half_emissions$Anomaly, full_emissions$Anomaly, "Floor(min_pts/2) vs min_pts as reference")
#   
#   qt <- ifelse(half_emissions$Anomaly==1,"No_Anomaly","Anomaly")
# }

## Assessing agreement between different approaches by comparing 
## results on daily basis

{
  eval_agreement <- function(valid_window,data_window){
    valid_flags <- valid_window$Anomaly
    
    data_flags <- data_window$Anomaly
    
    perc_agreement <- length(which(valid_flags==data_flags))/length(data_flags)*100
    
    return(round(perc_agreement,2))
  }
  
  ## Want: Tibble with overall agreement expressed as percentage for 
  ## each technique. Column 1 contains Day Tag, Column 2 contains DBSCAN
  ## Column 3 contains Drewnick, etc.
  valid_label_files <- list.files(path=paste0(getwd(),"/Manually_Flagged_Anomalies/"))
  
  load(paste0(getwd(),"/valid_data.RData")) %>% as.list()
  
  db_perc_agreement <- vector(mode = "numeric", length = length(valid_data_windows))
  
  drew_perc_agreement <- vector(mode = "numeric", length = length(valid_data_windows))
  
  qor_perc_agreement <- vector(mode = "numeric", length = length(valid_data_windows))
  
  qand_perc_agreement <- vector(mode = "numeric", length = length(valid_data_windows))
  
  days_tags <- vector(mode = "numeric", length = length(valid_data_windows))
  
  for(k in seq_along(valid_data_windows)){
    day_tag <- as.numeric(
      strsplit(
        strsplit(valid_label_files[k],"_")[[1]][3],
    "[.]")[[1]][1]
    )
    
    days_tags[k] <- day_tag
    
    db_perc_agreement[k] <- eval_agreement(valid_data_windows[[k]],db_grouped_anomalies[[day_tag]])
    
    drew_perc_agreement[k] <- eval_agreement(valid_data_windows[[k]],drew_grouped_anomalies[[day_tag]])
    
    qor_perc_agreement[k] <- eval_agreement(valid_data_windows[[k]],qor_grouped_anomalies[[day_tag]])
    
    qand_perc_agreement[k] <- eval_agreement(valid_data_windows[[k]],qand_grouped_anomalies[[day_tag]])
  }
  
  validation_results_by_day <- tibble("Day"=days_tags,
                                      "DBSCAN"=db_perc_agreement,
                                      "Drewnick"=drew_perc_agreement,
                                      "QOR"=qor_perc_agreement,
                                      "QAND"=qand_perc_agreement)
  
  maxes <- apply(validation_results_by_day,1,function(x) which.max(x[2:5]))
  
  length(which(maxes==1))
  length(which(maxes==2))
  length(which(maxes==3))
  
  ## Creating table of validation_results_by_day
  
  require(kableExtra)
  
  validation_results_by_day %>%
    kbl() %>%
    kable_classic()
}

## With drewnick data, db data, compare how many points in percentiles each method 
## flags as anomalies OVERALL
# {
#   qt_increments <- c(0.1,0.25,0.5,0.75,0.9,0.95,0.99)
# 
#   drew_NOx <- sapply(qt_increments, function(x) round(fraction_flagged(x,"NOx",drew_data),1))
#   drew_CO2 <- sapply(qt_increments, function(x) round(fraction_flagged(x,"CO2",drew_data),1))
#   drew_UFP <- sapply(qt_increments, function(x) round(fraction_flagged(x,"UFP",drew_data),1))
#   drew_BC <- sapply(qt_increments, function(x) round(fraction_flagged(x,"BC",drew_data),1))
#   db_NOx <- sapply(qt_increments, function(x) round(fraction_flagged(x,"NOx",db_data),1))
#   db_CO2 <- sapply(qt_increments, function(x) round(fraction_flagged(x,"CO2",db_data),1))
#   db_UFP <- sapply(qt_increments, function(x) round(fraction_flagged(x,"UFP",db_data),1))
#   db_BC <- sapply(qt_increments, function(x) round(fraction_flagged(x,"BC",db_data),1))
# 
#   quantile_tibble <- tibble("Drewnick_NOx"=drew_NOx,
#                             "DBSCAN_NOx"=db_NOx,
#                             "Drewnick_CO2"=drew_CO2,
#                             "DBSCAN_CO2"=db_CO2,
#                             "Drewnick_UFP"=drew_UFP,
#                             "DBSCAN_UFP"=db_UFP,
#                             "Drewnick_BC"=drew_BC,
#                             "DBSCAN_BC"=db_BC)
# 
#   rownames(quantile_tibble) <- c("0.1","0.25","0.5","0.75","0.9","0.95","0.99")
# 
#   require(kableExtra)
# 
#   quantile_tibble %>%
#     kbl() %>%
#     kable_classic(html_font = "Cambria")
# 
# }
# 
# ## Investigating joint behavior. Doing it on day-by-day basis.
# {
#   fraction_flagged_joint <- function(percentile,data){
# 
#     polls <- c("BC","CO2","NOx","UFP")
# 
#     selected_poll <- data %>%
#       select(all_of(polls))
# 
#     test <- apply(selected_poll,2,function(x) x > quantile(x,percentile))
# 
#     joint_truth <- apply(test,1,function(x) all(x))
# 
#     joint_greater <- data %>%
#       filter(joint_truth)
# 
#     if(length(which(joint_greater$Anomaly==2))==0){
#       return(0)
#     } else{
# 
#       fraction <- length(which(joint_greater$Anomaly==2))/length(joint_greater$Anomaly)
# 
#       return(round(fraction*100,1))
# 
#     }
# 
#   }
# 
#   drew_joint_percentages <- lapply(drew_grouped_anomalies,function(x) fraction_flagged_joint(0.90,x)) %>%
#     unlist(use.names = FALSE)
# 
#   db_joint_percentages <- lapply(db_grouped_anomalies,function(x) fraction_flagged_joint(0.90,x)) %>%
#     unlist(use.names = FALSE)
# 
#   # comparison_tibble <- tibble("Drewnick_Percentages"=drew_joint_percentages,
#   #                             "DB_Percentages"=db_joint_percentages)
# 
#   db_greater_drew <- which(db_joint_percentages>drew_joint_percentages)
# 
#   drew_greater_db <- which(drew_joint_percentages>db_joint_percentages)
# 
#   # comparison_tibble[db_greater_drew,] %>%
#   #   kbl() %>%
#   #   kable_classic()
#   # 
#   # comparison_tibble[drew_greater_db,] %>%
#   #   kbl() %>%
#   #   kable_classic()
# 
#   # for(i in 1:length(db_greater_drew)){
#   #   day_index <- db_greater_drew[i]
#   # 
#   #   plot_time_series_anomalies(db_grouped_anomalies[[day_index]],
#   #                              c("BC","CO2","NOx","UFP"),
#   #                              paste0("DBSCAN_Day_",day_index),
#   #                              save_graph = T,
#   #                              directory = paste0(getwd(),"/Miscellaneous_Figures/DB_Over_Drewnick/"))
#   # 
#   #   plot_time_series_anomalies(drew_grouped_anomalies[[day_index]],
#   #                              c("BC","CO2","NOx","UFP"),
#   #                              paste0("Drewnick_Day_",day_index),
#   #                              save_graph = T,
#   #                              directory = paste0(getwd(),"/Miscellaneous_Figures/DB_Over_Drewnick/"))
#   # }
# }

## Generate 20 random time series comparing flagged anomalies for Drewnick, DB
## quantile_or, and quantile_and methods
# {
#   set.seed(3)
#   
#   grouped_indices <- seq(1,277,1)
#   
#   random_indices <- sample(grouped_indices,20)
#   
#   for(j in 1:length(random_indices)){
#     print(j)
#     print("-------")
#     
#     current_index <- random_indices[j]
#     
#     plot_time_series_anomalies(db_grouped_anomalies[[current_index]],
#                                c("BC","CO2","NOx","UFP"),
#                                paste0("DBSCAN_Day_",current_index),
#                                save_graph = T,
#                                directory = paste0(getwd(),"/Miscellaneous_Figures/Comparing_All_Four/"))
# 
#     plot_time_series_anomalies(drew_grouped_anomalies[[current_index]],
#                                c("BC","CO2","NOx","UFP"),
#                                paste0("Drewnick_Day_",current_index),
#                                save_graph = T,
#                                directory = paste0(getwd(),"/Miscellaneous_Figures/Comparing_All_Four/"))
#     
#     plot_time_series_anomalies(qor_grouped_anomalies[[current_index]],
#                                c("BC","CO2","NOx","UFP"),
#                                paste0("QOR_Day_",current_index),
#                                save_graph = T,
#                                directory = paste0(getwd(),"/Miscellaneous_Figures/Comparing_All_Four/"))
#     
#     plot_time_series_anomalies(qand_grouped_anomalies[[current_index]],
#                                c("BC","CO2","NOx","UFP"),
#                                paste0("QAND_Day_",current_index),
#                                save_graph = T,
#                                directory = paste0(getwd(),"/Miscellaneous_Figures/Comparing_All_Four/"))
#     
#   }
# }
