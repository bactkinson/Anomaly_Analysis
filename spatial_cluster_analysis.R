require(sf)
require(tidyverse)
require(ggpubr)
## FUNCTIONS ##
census_tract_cluster_statplots <- function(cluster_stat_df, cluster_type, title = "Title",
                                           xlabel = FALSE,
                                           ylabel = "Value",
                                           fill_color = munsell::mnsl("5YR 7/12")){
  ## Produce ordered barplot containing statistics for each cluster_name in cluster_stat_df
  ## For a given cluster_type, or Cluster_Label designation
  require(ggpubr)
  
  print(
    ggbarplot(
      cluster_stat_df, x = "Name", y = cluster_type,
      sort.val = "desc",
      x.text.angle = 45,
      xlab = xlabel,
      fill = fill_color,
      ylab = ylabel,
      title = title,
      ggtheme = theme_pubclean(flip = TRUE)
    ) + labs_pubr(base_size = 14, base_family = "serif")
  )
}

save_plot <- function(filename,width,height){
  
  ggsave(filename = filename,
         device = "png",
         units = "in",
         width = width,
         height = height,
         dpi = 300)
}

key_conversion <- function(houston_polygons){
  houston_polygons$Name[houston_polygons$Name == "510800"] <- "Memorial Park"
  houston_polygons$Name[houston_polygons$Name == "3308"] <- "South Beltway Central"
  houston_polygons$Name[houston_polygons$Name == "520300"] <- "North Spring Branch"
  houston_polygons$Name[houston_polygons$Name == "520200"] <- "South Spring Branch"
  houston_polygons$Name[houston_polygons$Name == "411200"] <- "North River Oaks"
  houston_polygons$Name[houston_polygons$Name == "411400"] <- "South River Oaks"
  houston_polygons$Name[houston_polygons$Name == "510200"] <- "Washington Corridor"
  houston_polygons$Name[houston_polygons$Name == "412000"] <- "North Rice"
  houston_polygons$Name[houston_polygons$Name == "412200"] <- "South Rice"
  houston_polygons$Name[houston_polygons$Name == "511500"] <- "North Heights"
  houston_polygons$Name[houston_polygons$Name == "432400"] <- "Westchase"
  houston_polygons$Name[houston_polygons$Name == "432901"] <- "Sharpstown North"
  houston_polygons$Name[houston_polygons$Name == "432802"] <- "Sharpstown"
  houston_polygons$Name[houston_polygons$Name == "433300"] <- "Sharpstown South"
  houston_polygons$Name[houston_polygons$Name == "422701"] <- "Bayland Park"
  houston_polygons$Name[houston_polygons$Name == "233600"] <- "Clinton"
  houston_polygons$Name[houston_polygons$Name == "233701"] <- "West Galena Park"
  houston_polygons$Name[houston_polygons$Name == "233702"] <- "East Galena Park"
  houston_polygons$Name[houston_polygons$Name == "324200"] <- "Manchester"
  houston_polygons$Name[houston_polygons$Name == "311400"] <- "Harrisburg"
  houston_polygons$Name[houston_polygons$Name == "320200"] <- "Milby Park"
  houston_polygons$Name[houston_polygons$Name == "210800"] <- "West Eastex"
  houston_polygons$Name[houston_polygons$Name == "520500"] <- "Cypress"
  houston_polygons$Name[houston_polygons$Name == "530400"] <- "Independence Heights"
  houston_polygons$Name[houston_polygons$Name == "211000"] <- "Kashmere"
  houston_polygons$Name[houston_polygons$Name == "211900"] <- "East Fifth Ward"
  houston_polygons$Name[houston_polygons$Name == "320500"] <- "Pasadena Northwest"
  houston_polygons$Name[houston_polygons$Name == "342700"] <- "Deer Park"
  houston_polygons$Name[houston_polygons$Name == "253600"] <- "Baytown North"
  houston_polygons$Name[houston_polygons$Name == "254500"] <- "Baytown West"
  houston_polygons$Name[houston_polygons$Name == "550100"] <- "Aldine Northwest"
  houston_polygons$Name[houston_polygons$Name == "240100"] <- "Aldine Northeast"
  houston_polygons$Name[houston_polygons$Name == "222600"] <- "Aldine Southeast"
  houston_polygons$Name[houston_polygons$Name == "421201"] <- "Westpark West"
  houston_polygons$Name[houston_polygons$Name == "421102"] <- "Westpark East"
  
  return(houston_polygons)
}

cluster_anomalies <- function(anomalous_emissions,centers=3,nstart=200){
  ## Extract pollutants to be clustered
  pollutants <- anomalous_emissions %>%
    as_tibble() %>%
    dplyr::select(BC,CO2,NOx,UFP) %>%
    dplyr::mutate_all(scale)

  ## Perform the clustering and visualize the results. Perform PCA analysis
  ## to help interpret each cluster.
  km_clusters <- kmeans(pollutants, centers = centers, nstart = nstart)
  
  ## Reassign cluster labeled centers based on the following convention:
  # 1: CO2 cluster. Assign cluster label 1 to center with highest scaled CO2 value
  # 3: BC/UFP cluster. Assign cluster label 3 to center with highest scaled BC value
  # 2: Transition cluster. Assign cluster label 2 to remaining center.
  mirror_centers <- km_clusters$centers
  mirror_cluster <- km_clusters$cluster
  
  CO2_label <- which.max(km_clusters$centers[,colnames(km_clusters$centers)=="CO2"])
  BC_label <- which.max(km_clusters$centers[,colnames(km_clusters$centers)=="BC"])
  transition_label <- which(!(c(1,2,3) %in% c(CO2_label,BC_label)))
  
  ## Map CO2_label to 1, BC_label to 3, transition_label to 2.
  km_clusters$centers[1,] <- mirror_centers[CO2_label,]
  km_clusters$centers[3,] <- mirror_centers[BC_label,]
  km_clusters$centers[2,] <- mirror_centers[transition_label,]
  
  km_clusters$cluster[mirror_cluster==CO2_label] <- 1
  km_clusters$cluster[mirror_cluster==BC_label] <- 3
  km_clusters$cluster[mirror_cluster==transition_label] <- 2
  
  ## Based on above results, want to analyze DTRKVMT differences between cluster labels 2 and 3
  ## Cbind the clustering label results to labeled anomalous_emissions.
  labeled_anomalous_emissions <- cbind("Cluster_Label"=as.factor(km_clusters$cluster),anomalous_emissions)
  
  return(list(km_clusters,labeled_anomalous_emissions))
  
}

## Mapping anomaly distributions by cluster.
map_anomalies <- function(labeled_anomalous_emissions,cluster_label){
  
  isolated_anomalies <- labeled_anomalous_emissions %>%
    filter(Cluster_Label==cluster_label)
  
  tmap_mode("view")
  
  map <- tm_basemap(leaflet::providers$OpenStreetMap) +
    tm_shape(isolated_anomalies) +
    tm_dots()
  
  map
  
}

## Create function that processes self-sampling points.
## Take in point geometry, removes point geometries that are associated
## with labeled lat/long coordinate in sf_df_50_90m.csv as self-sample
## Association == k nearest neighbor distance.
self_sample_removal <- function(sf_df){
  # require(data.table)
  self_sampling_points <- st_read(paste0(getwd(),"/Houston_Shape_Files/Houston_10mpts_seg50_90_final.shp"),
                                  query = 'SELECT selfsample, Longitude, Latitude FROM Houston_10mpts_seg50_90_final')
  
  nearest_points <- st_nearest_feature(sf_df, self_sampling_points)

  sf_df_removed <- cbind(sf_df, "Self_Sample" = self_sampling_points$selfsample[nearest_points]) %>%
    filter(Self_Sample==0)

  return(sf_df_removed)
}


## Find nearest feature in txdot_joint_inventory to labeled anomalies
## Possible roadway features to plot:
## DTRKVMT: Daily Truck VMT
## TRK_AADT_PCT: Percent of trucks in AADT
## PCT_SADT: Percent of single unit trucks in AADT
## PCT_CADT: Percent of combo unit trucks in AADT
## AADT_TRUCKS: Number of all trucks in AADT
## AADT_SINGLE_UNIT: Number of Single Unit Trucks in AADT
## AADT_COMBINATION: Number of Combination Trucks in AADT

return_clustered_attributes <- function(anomalous_emissions, txdot_joint_inventory,attribute_name){
  
  ## Find the nearest line to each point in anomalous emissions
  nearest_lines <- st_nearest_feature(anomalous_emissions$geometry,st_zm(txdot_joint_inventory$geometry))
  print("Nearest_neighbor search complete")
  ## Determine which column corresponds to given traffic attribute
  col_idx <- which(colnames(txdot_joint_inventory)==attribute_name)
  
  traffic_attributes <- as_tibble(txdot_joint_inventory[,col_idx]) %>%
    dplyr::select(-geometry)
  
   
  clustered_attributes <- cbind(anomalous_emissions,traffic_attributes[nearest_lines,]) %>%
    dplyr::select(attribute_name,Cluster_Label) %>%
    dplyr::rename("Traffic_Attribute"=attribute_name) %>%
    as_tibble()
  
  return(clustered_attributes)
  
  
}

plot_clustered_roadway_features <- function(clustered_attributes, title, xlabel, ylabel){
    
  print(
    ggplot(data=clustered_attributes)+
      geom_boxplot(aes(x = Cluster_Label,y=Traffic_Attribute))+
      labs(title = title, x = xlabel, y = ylabel)+
      theme_pubclean()+
      labs_pubr()
  )
}

visualize_cluster_results <- function(plot_type,labeled_emissions_results, Var1 = NULL, Var2 = NULL, 
                                      title = "Plot", xlabel=aes_string(Var1),ylabel=aes_string(Var2)){
  require(ggpubr)
  
  tibble_to_plot <- as_tibble(labeled_emissions_results) 
  
  if(plot_type=="fast_scatter"){
    require(scattermore)
    
    print(
      ggplot(tibble_to_plot,aes_string(Var1,Var2))+
        geom_scattermore(aes(colour = Cluster_Label))+
        labs(x=xlabel,y=ylabel,title=title)+
        theme_classic()
      
      
      
    )
  }
  
  if(plot_type=="nice_scatter"){
    require(munsell)
    
    no_colors <- length(unique(tibble_to_plot$Cluster_Label))
    
    print(
      ggscatter(data=tibble_to_plot,x=Var1,y=Var2,
                color="Cluster_Label",
                shape = 19,
                palette = colorRampPalette(c(mnsl('5B 8/8'),mnsl('5YR 7/12')))(no_colors),
                xlab=xlabel,
                ylab = ylabel,
                repel = TRUE,
                title=title,
                legend = "right",
                legend.title = "Cluster",
                theme = theme_pubclean())+
        labs_pubr()
    )
  }
  
  if(plot_type=="box_plot"){
    
    clustered_wide <- tibble_to_plot %>%
      pivot_longer(cols=c("BC","CO2","NOx","UFP"), names_to = "Pollutant", values_to = "Measurement")
  
    bp <- ggplot(data=clustered_wide)+ 
      geom_boxplot(aes(x=Cluster_Label,y=Measurement))+
      theme_pubclean()+
      labs_pubr(base_family = "serif")+
      labs(x = xlabel, title=title)+
      theme(strip.background = element_rect(fill = munsell::mnsl('5BG 9/8')))
    
    poll_labeller <- as_labeller(c(BC="BC (ng/m^3)",CO2="CO[2] (ppm)", NOx = "NO[x] (ppb)",UFP="UFP (p/cc)"),
                                 default = label_parsed)
    
    print(
      bp + facet_wrap(vars(Pollutant),nrow = 2,ncol = 2,scales = "free",
                      labeller = poll_labeller)
    )
    
  }
}

## For each polygon_name in Houston polygon names
## Return points intersecting polygon with polygon_name
## Return number of points with cluster labels 1,2,3 for row in df with name polygon name
## From df, plot bars in decreasing order with most number of anomalies on left going
## to least number of anomalies on the right.

count_anomalies_in_polygons <- function(clustered_anomalous_emissions, polygon_sf_df){
  
  raw_anoms_output <- tibble("Name" = polygon_sf_df$Name, 
                             "CO2_Cluster" = rep(0,length(polygon_sf_df$Name)),
                             "Transition_Cluster" = rep(0,length(polygon_sf_df$Name)),
                             "BC_UFP_Cluster" = rep(0,length(polygon_sf_df$Name)))
  
  poly_names <- polygon_sf_df$Name
  
  current_emissions <- clustered_anomalous_emissions
  
  for(k in 1:length(polygon_sf_df$Name)){
    current_poly_name <- poly_names[k]
    
    print(current_poly_name)
    
    current_poly <- polygon_sf_df[polygon_sf_df$Name==current_poly_name, ]
    
    intersections <- which(
                      unlist(
      lapply(st_intersects(current_emissions, current_poly),function(x) length(x)),
                      use.names = F) !=0)
    
    intersecting_points <- current_emissions[intersections,]
    
    raw_anoms_output$CO2_Cluster[k] <- length(which(intersecting_points$Cluster_Label==1))
    raw_anoms_output$Transition_Cluster[k] <- length(which(intersecting_points$Cluster_Label==2))
    raw_anoms_output$BC_UFP_Cluster[k] <- length(which(intersecting_points$Cluster_Label==3))
    
    current_emissions <- current_emissions[-intersections,]
  }
  
  return(raw_anoms_output)
}

## Function which returns total number of points intersecting polygon_name
## in sf_hou_polygon
return_total_number_inpolygon <- function(sf_df, sf_hou_polygon){
  
  total_points <- tibble("Name"=sf_hou_polygon$Name, "Total_Points"=rep(0,nrow(sf_hou_polygon)))
  
  poly_names <- sf_hou_polygon$Name
  
  current_emissions <- sf_df
  
  for(k in 1:length(poly_names)){
    current_poly_name <- poly_names[k]
    
    print(current_poly_name)
    
    current_poly <- sf_hou_polygon[sf_hou_polygon$Name==current_poly_name, ]
    
    intersections <- which(
                      unlist(
      lapply(st_intersects(current_emissions, current_poly),function(x) length(x)),
                      use.names = F) !=0)
    
    total_points$Total_Points[k] <- length(intersections)
    
    print(length(intersections))
    
    current_emissions <- current_emissions[-intersections,]
  }
  
  
  return(total_points)
  
}

norm_by_anoms <- . %>% mutate(total = rowSums(select(.,2:4))) %>%
    mutate_at(2:4, ~./total*100) %>%
    select(-total) 

norm_by_total <- function(raw_anoms_output, total_points){
  
  anoms_norm_by_total <- raw_anoms_output %>%
    mutate_at(2:4, ~./total_points*100)
  
  return(anoms_norm_by_total)
    
}

inverted_cluster_means <- function(cluster_anomalies_object){
  
  centers <- cluster_anomalies_object[[1]]$centers
  
  pollutants <- cluster_anomalies_object[[2]] %>%
    dplyr::select(BC,CO2,NOx,UFP) %>%
    as_tibble() %>%
    dplyr::select(-c(geometry))
  
  column_means <- colMeans(pollutants)
  
  column_sds <- apply(pollutants,2,sd)
  
  print(column_means)
  
  print(column_sds)
  
  print(centers)
  
  transformed_centers <- matrix(,nrow = nrow(centers), ncol = ncol(centers))
  
  for(k in 1:ncol(centers)){
    transformed_centers[,k] <- (centers[,k]*column_sds[k])+column_means[k]  
  }
  
  
  require(knitr)
  
  
  print(
    transformed_centers %>%
      kable(col.names = c(expression(paste("BC(ng/",m^3)),
                                     expression(paste(CO[2],"(ppm)")),
                                     expression(paste(NO[x],"(ppb)")),
                                     expression(paste("UFP(p/cc")))) %>%
      kable_styling()
  )
  return(transformed_centers)
  
}

inverted_cluster_means(cluster_output)

## Data preprocessing
{
  load(paste0(getwd(),"/txdot_roadways/txdot_joint_inventory.RData"))
  
  ## Load in anomalous emissions. For now, we use DBSCAN_v01
  labeled_anomalous_emissions <- read.csv(paste0(getwd(),
                                         "/Anomalous_Emissions_Results/Labeled_Emissions_Quantile_OR.csv")) %>%
    select(BC,CO2,NOx,UFP,Lat1,Long1,Anomaly,Uniq_Fac) %>%
    filter(Anomaly==2) %>%
    st_as_sf(.,coords = c("Long1", "Lat1"), crs = "EPSG:4326") %>%
    st_transform("EPSG:32615") %>%
    self_sample_removal()
  
  ## Going to transform txdot roadway coordinates to WGS84 datum for now.
  txdot_joint_inventory <- st_transform(txdot_joint_inventory, "EPSG:32615")
  
  ## Now, want to determine spatial intersection between points, lines in txdot_joint_inventory.
  
  houston_lays <- st_layers(paste0(getwd(),"/mobile_polygons_20m_buffer.kml"))

  houston_polygons <- st_read(paste0(getwd(),"/mobile_polygons_20m_buffer.kml"),layer = houston_lays$name[1]) %>%
    st_transform("EPSG:32615")
  
  ct_num <- 1
  
  houston_polygons <- cbind(houston_polygons,"ct_num" = as.factor(rep(ct_num,nrow(houston_polygons))))
  
  
  for(k in 2:length(houston_lays)){
    ct_num <- k
    
    temp_polygons <- st_read(paste0(getwd(),"/mobile_polygons_20m_buffer.kml"),layer = houston_lays$name[k]) %>%
      st_transform("EPSG:32615")
    
    temp_polygons <- cbind(temp_polygons,"ct_num" = as.factor(rep(ct_num,nrow(temp_polygons))))
    
    houston_polygons <- rbind(houston_polygons,temp_polygons)
  }
  
  houston_polygons <- key_conversion(houston_polygons)
}

{
  set.seed(10)
  
  cluster_output <- cluster_anomalies(labeled_anomalous_emissions, centers = 3, nstart = 200)
  
  clustered_anomalous_emissions <- cluster_output[[2]]
}

## Producing visualizations of clustered results.
{
  visualize_cluster_results(plot_type = "fast_scatter",
                            clustered_anomalous_emissions,
                            "BC", "CO2",
                            title = "Cluster Visualization After",
                            xlabel = "BC",
                            ylabel = "CO2")
  
  # visualize_cluster_results(plot_type = "box_plot",
  #                           clustered_anomalous_emissions,
  #                           title = "Clustered QOR Anomaly Boxplots",
  #                           xlabel = "Cluster Label"
  #                           )
  # save_plot(paste0(getwd(),"/Manuscript/Figs/QOR_Anomaly_Cluster_Boxplot.png"),
  #           height = 6, width =4)
  # 
  # visualize_cluster_results(plot_type = "nice_scatter",
  #                           clustered_anomalous_emissions,
  #                           Var1 = "BC", Var2 = "CO2",
  #                           title = expression(paste("QOR Anomaly Cluster Visualization BC v ", CO[2])),
  #                           xlabel = expression(BC(ng/m^3)),
  #                           ylabel = expression(CO[2](ppm)))
  # 
  # save_plot(paste0(getwd(),"/Manuscript/Figs/QOR_Anomaly_Cluster_BC_CO2_Scatterplot.png"),
  #           height = 6, width =4)
  # 
  # visualize_cluster_results(plot_type = "nice_scatter",
  #                           clustered_anomalous_emissions,
  #                           Var1 = "UFP", Var2 = "CO2",
  #                           title = expression(paste("QOR Anomaly Cluster Visualization UFP v ", CO[2])),
  #                           xlabel = expression(UFP(p/cc)),
  #                           ylabel = expression(CO[2](ppm)))
  # 
  # save_plot(paste0(getwd(),"/Manuscript/Figs/QOR_Anomaly_Cluster_UFP_CO2_Scatterplot.png"),
  #           height = 6, width =4)
}


## Want to determine the number of anomalies in each polygon as well as anomaly
## type.
{
  require(data.table)
  
  raw_anoms_output <- count_anomalies_in_polygons(clustered_anomalous_emissions,houston_polygons)
  
  anoms_norm_by_anoms <- raw_anoms_output %>% norm_by_anoms(.)
  
  total_polygon_points <- fread(paste0(getwd(),"/total_polygon_points.csv"), select = c("Name","Total_Points"))
  
  anoms_norm_by_total <- norm_by_total(raw_anoms_output = raw_anoms_output, total_polygon_points$Total_Points)
}



{
  census_tract_cluster_statplots(raw_anoms_output, "CO2_Cluster",
                                 title = expression(paste("Total QOR ", CO[2], " Anomaly Detections by Census Tract")),
                                 ylabel = "Count")
  
  # save_plot(paste0(getwd(),"/Manuscript/Figs/QOR_Total_CO2_Anomaly_CT_Bar.png"),height = 4, width = 8)
  
  census_tract_cluster_statplots(raw_anoms_output, "BC_UFP_Cluster",
                               title = "Total QOR BC/UFP Anomaly Detections by Census Tract",
                               ylabel = "Count")
  
  # save_plot(paste0(getwd(),"/Manuscript/Figs/QOR_Total_BC_UFP_Anomaly_CT_Bar.png"),height = 4, width = 8)

  census_tract_cluster_statplots(anoms_norm_by_anoms, "CO2_Cluster",
                               title = expression(paste("QOR ",CO[2]," Anomalies Normalized by Census Tract Anomaly Total")),
                               ylabel = expression(paste("% ", CO[2], " Anomaly")))

  # save_plot(paste0(getwd(),"/Manuscript/Figs/QOR_CO2_Anomalies_Over_Anomalies_CT_Bar.png"),height = 4, width = 8)
  
  census_tract_cluster_statplots(anoms_norm_by_anoms, "Transition_Cluster",
                               title = expression(paste("QOR Transition Anomalies Normalized by Census Tract Anomaly Total")),
                               ylabel = "% Transition Anomaly")
  
  # save_plot(paste0(getwd(),"/Manuscript/Figs/QOR_Transition_Anomalies_Over_Anomalies_CT_Bar.png"),height = 4, width = 8)
  
  census_tract_cluster_statplots(anoms_norm_by_anoms, "BC_UFP_Cluster",
                               title = expression(paste("QOR BC/UFP Anomalies Normalized by Census Tract Anomaly Total")),
                               ylabel = "% BC/UFP Anomaly")
  
  # save_plot(paste0(getwd(),"/Manuscript/Figs/QOR_BC_UFP_Anomalies_Over_Anomalies_CT_Bar.png"),height = 4, width = 8)
  
  census_tract_cluster_statplots(anoms_norm_by_total, "CO2_Cluster",
                               title = expression(paste("QOR ", CO[2], " Anomalies Normalized by Census Tract Total Points")),
                               ylabel = expression(paste("% ", CO[2], " Anomaly")))
  
  # save_plot(paste0(getwd(),"/Manuscript/Figs/QOR_CO2_Anomalies_Over_Total_CT_Bar.png"),height = 4, width = 8)
  
  # census_tract_cluster_statplots(anoms_norm_by_anoms, "BC_UFP_Cluster",
  #                              title = expression(paste("QOR BC/UFP Anomalies Normalized by Anomaly Total")),
  #                              ylabel = "% BC/UFP Anomaly")
  # 
  # save_plot(paste0(getwd(),"/Manuscript/Figs/QOR_BC_UFP_Anomalies_Over_Anomalies_CT_Bar.png"),height = 4, width = 8)
  
  census_tract_cluster_statplots(anoms_norm_by_total, "BC_UFP_Cluster",
                               title = expression(paste("QOR BC/UFP Anomalies Normalized by Census Tract Total Points")),
                               ylabel = "% BC/UFP Anomaly")
  
  # save_plot(paste0(getwd(),"/Manuscript/Figs/QOR_BC_UFP_Anomalies_Over_Total_CT_Bar.png"),height = 4, width = 8)
}
