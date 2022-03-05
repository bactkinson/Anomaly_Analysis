## Script which contains functions to validate anomaly detection labels.
require(caret)
require(tidyverse)

save_plot <- function(filename,width,height){
  
  ggsave(filename = filename,
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
      kable_styling(bootstrap_options = c("striped", "condensed","hover","responsive"), full_width = F)
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

## Load in data, run the analysis.
{
  db_data <- fread(paste0(getwd(),"/Anomalous_Emissions_Results/Labeled_Emissions_DBSCAN_V05.csv"))
  
  db_grouped_anomalies <- db_data %>%
    group_split(Uniq_Fac,.keep = FALSE)
  
  
  validate_labels(db_grouped_anomalies,confusion_matrix_title = "Test", confusion_matrix_prediction_label = "Test2",
                  confusion_matrix_reference_label = "Test3")
  
  validate_labels_by_day(db_grouped_anomalies, output_title = "DBSCAN Validation Percentages by Day")  
}

## Plot a daily time series
{
  CO2 <- db_grouped_anomalies[[1]] %>% select(LST,CO2) 
  
  lubridate::tz(CO2$LST) <- "America/Chicago"
  
  plt <- ggplot(data=CO2)+
    geom_point(aes(LST,CO2))+
    labs(x = "Time (US/Central)", y = expression(paste(CO[2],"(ppm)"))) + 
    theme_classic()
  
  print(plt)

  save_plot(paste0(getwd(),"/Manuscript/Figs/Example_CO2_Daily_Time_Series.png"),width = 8.5,height = 4)
}


{
  # plot_labeled_anomaly_scatter(db_grouped_anomalies[[4]],"BC","CO2",
#                              xlabel = expression(paste("BC",(ng/m^3))),
#                              ylabel = expression(paste(CO[2],"(ppm)")),
#                              title = "DBSCAN Labeled Anomaly Scatterplot")
# 
# save_plot(paste0(getwd(),"/Manuscript/Figs/DBSCAN_Labeled_Example.png"),width = 8,height = 6)
}
