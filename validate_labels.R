## Script which contains functions to validate anomaly detection labels.
require(caret)
require(tidyverse)
require(data.table)

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

save_plot <- function(filename,width,height,plot = last_plot()){
  
  ggsave(filename = filename,
         plot = plot,
         device = "png",
         units = "in",
         width = width,
         height = height,
         dpi = 300)
}

plot_confusion_matrix <- function(data_flags, reference_flags,plot_title,reference_label,prediction_label){
  
  res_cm <- caret::confusionMatrix(as.factor(data_flags), as.factor(reference_flags))
  
  res_table <- as.data.frame(res_cm$table) %>%
    mutate(Agreement = ifelse(Prediction==Reference, "Agree", "Disagree")) %>%
    mutate_at(1:2,~ as.factor(ifelse(.==1, "Normal", "Anomaly"))) %>%
    group_by(Prediction)
  
  
  print(res_table)
  
  print(levels(res_table$Reference))
  
  print(levels(res_table$Prediction))
  
  # print(rev(levels(res_table$Reference)))
  
  plt <- ggplot(data = res_table, aes(Prediction, Reference, fill = Agreement)) +
      geom_tile(aes(alpha = 0.4)) +
      geom_text(aes(label=Freq),fontface = "bold", size = 7,family = "serif") +
      xlim(rev(levels(res_table$Prediction)))+
      scale_fill_manual(values = c(Agree = munsell::mnsl("5BG 9/8"), Disagree = munsell::mnsl("5R 5/18"))) +
      labs(title = plot_title,x=prediction_label,y=reference_label,family = "mono")+
      theme_classic()+
      theme(legend.position = "none",plot.title = element_text(face = "bold", size = 14),
            text = element_text(family = "serif"),axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),axis.title = element_text(face = "bold"))

  
  print(
    plt        
  )
      
  return(plt)
  
}

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

eval_agreement <- function(valid_window,data_window){
    valid_flags <- valid_window$Anomaly
    
    data_flags <- data_window$Anomaly
    
    perc_agreement <- length(which(valid_flags==data_flags))/length(data_flags)*100
    
    return(round(perc_agreement,2))
}


validate_labels <- function(daily_predicted_labeled_data,confusion_matrix_title = "Confusion Matrix",
                            confusion_matrix_reference_label = "Reference",
                            confusion_matrix_prediction_label = "Prediction"){
  
  valid_label_files <- list.files(path=paste0(getwd(),"/Manually_Flagged_Anomalies/"))
  
  predicted_flags <- vector(,)
  
  valid_flags <- vector(,)
  
  load(paste0(getwd(),"/valid_data.RData")) %>% as.list()
  
  for(j in seq_along(valid_data_windows)){
    day_tag <- as.numeric(
      strsplit(
        strsplit(valid_label_files[j],"_")[[1]][3],
    "[.]")[[1]][1]
    )
    
    predicted_flags <- c(predicted_flags,daily_predicted_labeled_data[[day_tag]]$Anomaly)
    
    valid_flags <- c(valid_flags,valid_data_windows[[j]]$Anomaly)
  }
  print("Percentage Agreeement between prediction and validation sets")  
  print(length(which(predicted_flags==valid_flags))/length(valid_flags))
  print("---------")
  
  baseline_flags <- rep(1,length(valid_flags))
  
  print("Baseline percentage agreement")  
  print(length(which(baseline_flags==valid_flags))/length(valid_flags))
  print("---------")
  
  plt <- plot_confusion_matrix(predicted_flags, valid_flags, plot_title = confusion_matrix_title,
                        reference_label = confusion_matrix_reference_label, 
                        prediction_label = confusion_matrix_prediction_label)
  
  return(plt)
  
}

validate_labels_by_day <- function(daily_predicted_labeled_data,output_title = "Percentages"){
  
  valid_label_files <- list.files(path=paste0(getwd(),"/Manually_Flagged_Anomalies/"))
  
  percentage_agreement <- numeric(length(valid_label_files))
  
  days_used <- numeric(length(valid_label_files))
  
  load(paste0(getwd(),"/valid_data.RData")) %>% as.list()
  
  for(j in seq_along(valid_data_windows)){
    day_tag <- as.numeric(
      strsplit(
        strsplit(valid_label_files[j],"_")[[1]][3],
    "[.]")[[1]][1]
    )
    
    percentage_agreement[j] <- eval_agreement(valid_data_windows[[j]],daily_predicted_labeled_data[[day_tag]])
    
    days_used[j] <- day_tag
  }
  
  output_tibble <- tibble("Day"=days_used, "Validation Percentage"=percentage_agreement)
  
  require(kableExtra)
  
  print(
    output_tibble %>%
      kbl(caption =paste0('<b>',output_title,'</b>'),format = 'html') %>%
      kable_paper(bootstrap_options = c("striped", "condensed","hover","responsive"), full_width = F)
  )
  
  
  return(output_tibble)
}

plot_labeled_anomaly_scatter <- function(labeled_day,Var1,Var2,xlabel = NULL,ylabel = NULL,title = NULL){
  
  data_to_plot <- labeled_day %>%
    dplyr::select(Var1,Var2,Anomaly) %>%
    dplyr::mutate(Anomaly = as.factor(Anomaly))
  
  plt <- ggplot(data = data_to_plot,aes(group(factor(Anomaly))))+
    geom_point(aes_string(Var1,Var2,colour = "Anomaly"))+
    scale_color_manual(values = c("black","red"),labels=c("Normal","Anomaly"))+
    labs(x=xlabel,y=ylabel,title = title)+
    theme(legend.title = element_blank())+
    theme_classic()
  
  print(plt)
  
  return(plt)
  
}

## Load in data, run the analysis for all 4 methods.
{
  db_data <- fread(paste0(getwd(),"/Anomalous_Emissions_Results/Labeled_Emissions_DBSCAN_V05.csv"))
  
  drew_data <- fread(paste0(getwd(),"/Anomalous_Emissions_Results/Labeled_Emissions_Drewnick.csv"))
  
  qor_data <- fread(paste0(getwd(),"/Anomalous_Emissions_Results/Labeled_Emissions_Quantile_OR.csv"))
  
  qand_data <- fread(paste0(getwd(),"/Anomalous_Emissions_Results/Labeled_Emissions_Quantile_AND.csv"))
  
  db_grouped_anomalies <- db_data %>%
    group_split(Uniq_Fac,.keep = FALSE)
  
  drew_grouped_anomalies <- drew_data %>%
    group_split(Uniq_Fac,.keep = FALSE)
  
  qor_grouped_anomalies <- qor_data %>%
    group_split(Uniq_Fac,.keep = FALSE)
  
  qand_grouped_anomalies <- qand_data %>%
    group_split(Uniq_Fac,.keep = FALSE)
  
  p1 <- validate_labels(db_grouped_anomalies,confusion_matrix_title = NULL, confusion_matrix_prediction_label = "DBSCAN Labels",
                  confusion_matrix_reference_label = "Validation Labels")
  
  p2 <- validate_labels(drew_grouped_anomalies,confusion_matrix_title = NULL, confusion_matrix_prediction_label = "Drewnick Labels",
                  confusion_matrix_reference_label = "Validation Labels")
  
  p3 <- validate_labels(qor_grouped_anomalies,confusion_matrix_title = NULL, confusion_matrix_prediction_label = "QOR Labels",
                  confusion_matrix_reference_label = "Validation Labels")
  
  p4 <- validate_labels(qand_grouped_anomalies,confusion_matrix_title = NULL, confusion_matrix_prediction_label = "QAND Labels",
                  confusion_matrix_reference_label = "Validation Labels")
  
  ggarrange(p1,p2,p3,p4,labels = list("(a)","(b)","(c)","(d)"))
  
  save_plot(paste0(getwd(),"/Manuscript/Figs/Confusion_Matrix_Gridded_Results.png"),width = 10, height = 8)

  save_plot(paste0(getwd(),"/Manuscript/Figs/Confusion_Matrix_DBSCAN.png"),width = 5, height = 4,plot = p1)

  save_plot(paste0(getwd(),"/Manuscript/Figs/Confusion_Matrix_QOR.png"),width = 5, height = 4,plot=p3)
  
  db_by_day <- validate_labels_by_day(db_grouped_anomalies, output_title = NULL)  
  
  qor_by_day <- validate_labels_by_day(qor_grouped_anomalies, output_title = NULL)  
  
  both_by_day <- db_by_day %>%
    dplyr::rename("DBSCAN Validation Percentage (%)"=`Validation Percentage`) %>%
    dplyr::mutate("QOR Validation Percentage (%)"=qor_by_day$`Validation Percentage`) %>%
    kableExtra::kbl()%>%
    kableExtra::kable_paper(full_width=F)
  
  print(both_by_day)
}

## 2/15/2023
## Plotting time series of validated CO2 data for manuscript
## Recreating Figure S1 in R
{
  # load(paste0(getwd(),"/valid_data.RData")) %>% as.list()
  # 
  # ts_1 <- as.POSIXct("2018-03-26 07:00:00", format = c("%Y-%m-%d %H:%M:%S"), tz = "US/Central")
  # 
  # ts_2 <- as.POSIXct("2018-03-26 07:30:00", format = c("%Y-%m-%d %H:%M:%S"), tz = "US/Central")
  # 
  # validated_data_subset <- valid_data_windows[[29]] %>%
  #   mutate(fill_color = dplyr::case_when(Anomaly==1 ~ "black",
  #                                        Anomaly==2 ~ "red")) %>%
  #   mutate(indicator = ifelse(Anomaly==1,"Non-anomaly","Anomaly")) %>%
  #   filter(LST>ts_1 & LST<ts_2)
  # 
  # ggplot(data = validated_data_subset,aes(x = LST,y=CO2,color = indicator)) +
  #   geom_point()+
  #   labs(x = "Time (US/Central)", y = expression(paste(CO[2]," (ppm)"))) +
  #   scale_color_manual(name = "Labels", values = c("red","black")) +
  #   scale_y_continuous(limits = c(400,1200)) + 
  #   theme_classic()
  # 
  # save_plot(paste0(getwd(),"/Manuscript/Figs/Figure_S1.png"),width = 8.5, height = 4)
}

## Plot a daily time series
# {
#   CO2 <- db_grouped_anomalies[[1]] %>% select(LST,CO2) 
#   
#   lubridate::tz(CO2$LST) <- "America/Chicago"
#   
#   plt <- ggplot(data=CO2)+
#     geom_point(aes(LST,CO2))+
#     labs(x = "Time (US/Central)", y = expression(paste(CO[2],"(ppm)"))) + 
#     theme_classic()
#   
#   print(plt)
# 
#   save_plot(paste0(getwd(),"/Manuscript/Figs/Example_CO2_Daily_Time_Series.png"),width = 8.5,height = 4)
# }


{
  plot_labeled_anomaly_scatter(db_grouped_anomalies[[4]],"BC","CO2",
                               xlabel = expression(paste("BC (ng ", m^-3, ")")),
                               ylabel = expression(paste(CO[2],"(ppm)")),
                               title = "DBSCAN Labeled Anomaly Scatterplot")
  
  save_plot(paste0(getwd(),"/AMT_Manuscript/Final_Figures/Figure_2.png"),width = 8,height = 6)
}

## Plotting differences in labeling between QOR, DBSCAN
## In this case, want to determine instances that QOR flags something as anomaly
## That DBSCAN does not
## So identify instaces where QOR_Anomaly == 2 while DBSCAN_Anomaly == 1
{
  # validation_labels <- c(100,103,109,118,123,124,134,150,166,168,171,181,187,194,206,207,209,21,215,218,22,248,249,268,271,3,36,59,88,90)
  # 
  # for(label in validation_labels){
  #   print(label)
  # 
  #   different_labeled_data <- db_grouped_anomalies[[label]] %>%
  #     dplyr::mutate(QOR_Anomaly = qor_grouped_anomalies[[label]]$Anomaly) %>%
  #     dplyr::mutate(Different_Label = ifelse((Anomaly==1 & QOR_Anomaly==2),2,1)) %>%
  #     dplyr::select(-QOR_Anomaly) %>%
  #     dplyr::select(-Anomaly) %>%
  #     dplyr::rename("Anomaly"=Different_Label)
  # 
  #   plot_time_series_anomalies(different_labeled_data, polls_to_pull = c("BC","CO2","NOx","UFP"),
  #                              title = paste0("QOR_Anomaly_DBSCAN_Normal_Day_",label),
  #                              save_graph = T,
  #                              directory = paste0(getwd(),"/Miscellaneous_Figures/label_difference_time_series/"))
  # 
  # }
}

## In this, want to identify instances in which the DBSCAN anomalies agree with the validation set
## while the QOR disagrees. In particular, when QOR flags something as plume while DBSCAN flags as normal
{
  # validation_labels <- c(100,103,109,118,123,124,134,150,166,168,171,181,187,194,206,207,209,21,215,218,22,248,249,268,271,3,36,59,88,90)
  # 
  # for(j in seq_along(validation_labels)){
  #   label = validation_labels[j]
  #   
  #   print(label)
  # 
  #   different_labeled_data <- db_grouped_anomalies[[label]] %>%
  #     dplyr::mutate(QOR_Anomaly = qor_grouped_anomalies[[label]]$Anomaly) %>%
  #     dplyr::mutate(Correct_Label = valid_data_windows[[j]]$Anomaly) %>%
  #     dplyr::mutate(Different_Label = ifelse((Anomaly==1 & QOR_Anomaly==2 & Correct_Label==1),2,1)) %>%
  #     dplyr::select(-QOR_Anomaly) %>%
  #     dplyr::select(-Anomaly) %>%
  #     dplyr::rename("Anomaly"=Different_Label)
  # 
  #   plot_time_series_anomalies(different_labeled_data, polls_to_pull = c("BC","CO2","NOx","UFP"),
  #                              title = paste0("QOR_Wrong_Plume_DBSCAN_Right_Day_",label),
  #                              save_graph = T,
  #                              directory = paste0(getwd(),"/Miscellaneous_Figures/label_difference_time_series/"))
  # 
  # }
}



## In this, want to identify instances in which the DBSCAN anomalies disagree with the validation set
## while the QOR agrees. In particular, identify instances in which DBSCAN incorrectly labels something as
## background while QOR correctly labels it as plume.
{
  # validation_labels <- c(100,103,109,118,123,124,134,150,166,168,171,181,187,194,206,207,209,21,215,218,22,248,249,268,271,3,36,59,88,90)
  # 
  # for(j in seq_along(validation_labels)){
  #   label = validation_labels[j]
  #   
  #   print(label)
  # 
  #   different_labeled_data <- db_grouped_anomalies[[label]] %>%
  #     dplyr::mutate(QOR_Anomaly = qor_grouped_anomalies[[label]]$Anomaly) %>%
  #     dplyr::mutate(Correct_Label = valid_data_windows[[j]]$Anomaly) %>%
  #     dplyr::mutate(Different_Label = ifelse((Anomaly==1 & QOR_Anomaly==2 & Correct_Label==2),2,1)) %>%
  #     dplyr::select(-QOR_Anomaly) %>%
  #     dplyr::select(-Anomaly) %>%
  #     dplyr::rename("Anomaly"=Different_Label)
  # 
  #   plot_time_series_anomalies(different_labeled_data, polls_to_pull = c("BC","CO2","NOx","UFP"),
  #                              title = paste0("DBSCAN_Wrong_Background_QOR_Right_Day_",label),
  #                              save_graph = T,
  #                              directory = paste0(getwd(),"/Miscellaneous_Figures/label_difference_time_series/"))
  # 
  # }
}

## In this, want to identify instances in which the DBSCAN anomalies disagree with the validation set
## while the QOR agrees. In particular, identify instances in which DBSCAN incorrectly labels something as
## plume while QOR correctly labels it as background
{
  # validation_labels <- c(100,103,109,118,123,124,134,150,166,168,171,181,187,194,206,207,209,21,215,218,22,248,249,268,271,3,36,59,88,90)
  # 
  # for(j in seq_along(validation_labels)){
  #   label = validation_labels[j]
  #   
  #   print(label)
  # 
  #   different_labeled_data <- db_grouped_anomalies[[label]] %>%
  #     dplyr::mutate(QOR_Anomaly = qor_grouped_anomalies[[label]]$Anomaly) %>%
  #     dplyr::mutate(Correct_Label = valid_data_windows[[j]]$Anomaly) %>%
  #     dplyr::mutate(Different_Label = ifelse((Anomaly==2 & QOR_Anomaly==1 & Correct_Label==1),2,1)) %>%
  #     dplyr::select(-QOR_Anomaly) %>%
  #     dplyr::select(-Anomaly) %>%
  #     dplyr::rename("Anomaly"=Different_Label)
  # 
  #   plot_time_series_anomalies(different_labeled_data, polls_to_pull = c("BC","CO2","NOx","UFP"),
  #                              title = paste0("DBSCAN_Wrong_Plume_QOR_Right_Day_",label),
  #                              save_graph = T,
  #                              directory = paste0(getwd(),"/Miscellaneous_Figures/label_difference_time_series/"))
  # 
  # }
}

## In this, want to identify instances in which the DBSCAN anomalies disagree with the validation set
## while the QOR agrees. In particular, identify instances in which DBSCAN correctly labels plume while
## QOR incorrectly labels background
{
  # validation_labels <- c(100,103,109,118,123,124,134,150,166,168,171,181,187,194,206,207,209,21,215,218,22,248,249,268,271,3,36,59,88,90)
  # 
  # for(j in seq_along(validation_labels)){
  #   label = validation_labels[j]
  #   
  #   print(label)
  # 
  #   different_labeled_data <- db_grouped_anomalies[[label]] %>%
  #     dplyr::mutate(QOR_Anomaly = qor_grouped_anomalies[[label]]$Anomaly) %>%
  #     dplyr::mutate(Correct_Label = valid_data_windows[[j]]$Anomaly) %>%
  #     dplyr::mutate(Different_Label = ifelse((Anomaly==2 & QOR_Anomaly==1 & Correct_Label==2),2,1)) %>%
  #     dplyr::select(-QOR_Anomaly) %>%
  #     dplyr::select(-Anomaly) %>%
  #     dplyr::rename("Anomaly"=Different_Label)
  # 
  #   plot_time_series_anomalies(different_labeled_data, polls_to_pull = c("BC","CO2","NOx","UFP"),
  #                              title = paste0("QOR_Wrong_Background_DBSCAN_Right_Day_",label),
  #                              save_graph = T,
  #                              directory = paste0(getwd(),"/Miscellaneous_Figures/label_difference_time_series/"))
  # 
  # }
}

## Calculate number of instances in which the situations occur.
## QOR Plume wrong, DBSCAN background right
## QOR plume right, DBSCAN background wrong
## DBSCAN plume wrong, QOR background right
## DBSCAN plume right, QOR background wrong
{
  # validation_labels <- c(100,103,109,118,123,124,134,150,166,168,171,181,187,194,206,207,209,21,215,218,22,248,249,268,271,3,36,59,88,90)
  # 
  # validated_data_tibble <- list_to_tibble(valid_data_windows)
  # 
  # corresponding_db_tibble <- list_to_tibble(db_grouped_anomalies[validation_labels])  
  # 
  # corresponding_qor_tibble <- list_to_tibble(qor_grouped_anomalies[validation_labels])
  # 
  # labels_tibble <- validated_data_tibble %>%
  #   dplyr::select(LST,Anomaly) %>%
  #   dplyr::rename("Correct_Label"=Anomaly) %>%
  #   dplyr::mutate("DBSCAN_Label"=corresponding_db_tibble$Anomaly) %>%
  #   dplyr::mutate("QOR_Label"=corresponding_qor_tibble$Anomaly)
  # 
  # qor_anom_wrong_db_correct <- nrow(labels_tibble %>% dplyr::filter(Correct_Label==1 & DBSCAN_Label==1 & QOR_Label==2))
  # db_anom_wrong_qor_correct <- nrow(labels_tibble %>% dplyr::filter(Correct_Label==1 & DBSCAN_Label==2 & QOR_Label==1))
  # qor_background_wrong_db_correct <- nrow(labels_tibble %>% dplyr::filter(Correct_Label==2 & DBSCAN_Label==2 & QOR_Label==1))
  # db_background_wrong_qor_correct <- nrow(labels_tibble %>% dplyr::filter(Correct_Label==2 & DBSCAN_Label==1 & QOR_Label==2))
  # 
  # results <- tibble("Labels"=c("QOR Anomaly Wrong DBSCAN Background Correct","DBSCAN Anomaly Wrong QOR Background Correct",
  #                              "QOR Background Wrong DBSCAN Anomaly Correct","DBSCAN Background Wrong QOR Anomaly Correct"),
  #                   "Counts"=c(qor_anom_wrong_db_correct,db_anom_wrong_qor_correct,qor_background_wrong_db_correct,db_background_wrong_qor_correct))
  # 
  # results %>%
  #   kableExtra::kbl() %>%
  #   kableExtra::kable_minimal()
}

## Generate time series of correctly labeled data.
{
  # validation_labels <- c(100,103,109,118,123,124,134,150,166,168,171,181,187,194,206,207,209,21,215,218,22,248,249,268,271,3,36,59,88,90)
  # 
  # for(j in 1:length(valid_data_windows)){
  #   
  #   label = validation_labels[j]
  #   
  #   plot_time_series_anomalies(valid_data_windows[[j]], polls_to_pull = c("BC","CO2","NOx","UFP"),
  #                              title = paste0("Validated_Data_Day_",label),
  #                              save_graph = T,
  #                              directory = paste0(getwd(),"/Miscellaneous_Figures/label_difference_time_series/"))
  #   
  # }
}

## Creating datasets used in validation testing for upload to Zenodo.
{
  # validation_labels <- c(100,103,109,118,123,124,134,150,166,168,171,181,187,194,206,207,209,21,215,218,22,248,249,268,271,3,36,59,88,90)
  # 
  # db_to_be_validated <- db_grouped_anomalies[validation_labels]
  # 
  # db_to_be_validated_data <- list_to_tibble(db_to_be_validated) %>%
  #   dplyr::select(-c(V1,O3,PM25))
  # 
  # write.csv(db_to_be_validated_data,file = paste0(getwd(),"/Anomalous_Emissions_Results/DB_To_Be_Validated.csv"))
  # 
  # qor_to_be_validated <- qor_grouped_anomalies[validation_labels]
  # 
  # qor_to_be_validated_data <- list_to_tibble(qor_to_be_validated) %>%
  #   dplyr::select(-c(V1,O3,PM25))
  # 
  # write.csv(qor_to_be_validated_data,file = paste0(getwd(),"/Anomalous_Emissions_Results/QOR_To_Be_Validated.csv"))
  # 
  # qand_to_be_validated <- qand_grouped_anomalies[validation_labels]
  # 
  # qand_to_be_validated_data <- list_to_tibble(qand_to_be_validated) %>%
  #   dplyr::select(-c(V1,O3,PM25))
  # 
  # write.csv(qand_to_be_validated_data,file = paste0(getwd(),"/Anomalous_Emissions_Results/QAND_To_Be_Validated.csv"))
  # 
  # drew_to_be_validated <- drew_grouped_anomalies[validation_labels]
  # 
  # drew_to_be_validated_data <- list_to_tibble(drew_to_be_validated) %>%
  #   dplyr::select(-c(V1,O3,PM25))
  # 
  # write.csv(drew_to_be_validated_data,file = paste0(getwd(),"/Anomalous_Emissions_Results/Drew_To_Be_Validated.csv"))
}
v