require(sf)
require(tidyverse)
require(ggpubr)
require(data.table)
## FUNCTIONS ##
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

generate_poly_map <- function(polygon_sf_df,fill_by=NA,bins = NA,legend_title=NA){
  
  polygon_sf_df <- st_transform(st_zm(polygon_sf_df),"EPSG:4326")
  require(leafem)
  require(leaflet)
  
  if(is.na(fill_by)){
    m <- leaflet(polygon_sf_df) %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolygons(fillColor = )
  
    return(m)
  } else{
    
    selected_values <- polygon_sf_df %>% as_tibble() %>% dplyr::select(fill_by) %>% unlist(use.names=F) 
    
    # colors_to_use <- rev(RColorBrewer::brewer.pal(length(bins),"RdBu"))
    # pal <- colorBin(colors_to_use,domain = selected_values, bins = bins)
    
    pal <- colorBin("YlOrRd",domain = selected_values, bins = bins)
    
    m <- leaflet(polygon_sf_df) %>%
      addProviderTiles(providers$Stamen.TonerBackground) %>%
      addPolygons(fillColor = ~pal(selected_values),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  fillOpacity = 0.7) %>%
      addStaticLabels(.,label = polygon_sf_df$Name, style = list("color" = "black","font-weight" = "bold","font-size"="14px")) %>%
      addLegend(pal=pal,values = selected_values, opacity = 0.7, title = legend_title, position = "bottomright")
    
    return(m)
  }
  # return(polygon_sf_df)
}

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

census_tract_cluster_statplots <- function(cluster_stat_df, cluster_type, title = "Title",
                                           xlabel = FALSE,
                                           ylabel = "Value",
                                           fill_color = munsell::mnsl("5YR 7/12"),
                                           bar_groups = NULL){
  ## Produce ordered barplot containing statistics for each cluster_name in cluster_stat_df
  ## For a given cluster_type, or Cluster_Label designation
  require(ggpubr)
  
  if(is.null(bar_groups)){
  
    plt <- ggbarplot(
      cluster_stat_df, x = "Name", y = cluster_type,
      sort.val =  "desc",
      x.text.angle = 60,
      xlab = xlabel,
      fill = fill_color,
      ylab = ylabel,
      title = title,
      ggtheme = theme_pubclean(flip = TRUE)
    ) + labs_pubr(base_size = 14, base_family = "serif")
    
    print(plt)
  } else{
    
    if(length(fill_color)==1){stop("Need to select more than one fill color for multiple groups")}
    
    plt <- ggbarplot(
      cluster_stat_df, x = "Name", y = cluster_type,
      x.text.angle = 60,
      xlab = xlabel,
      fill = bar_groups,
      palette = fill_color,
      ylab = ylabel,
      title = title,
      legend.title = "",
      position = position_dodge(),
      ggtheme = theme_pubclean()
    ) + labs_pubr(base_size = 14, base_family = "serif")
    
    print(plt)
    
    
  }
  
  return(plt)
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
  
  require(tmap)
  
  isolated_anomalies <- labeled_anomalous_emissions %>%
    filter(Cluster_Label==cluster_label)
  
  tmap_mode("view")
  
  map <- tm_basemap(leaflet::providers$OpenStreetMap) +
    tm_shape(isolated_anomalies) +
    tm_dots()
  
  map
  
}

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

## Create function that processes labeled emissions by removing self-sample points
## and assigning road-class.
## Take in point geometry, removes point geometries that are associated
## with labeled lat/long coordinate in sf_df_50_90m.csv as self-sample
## Additionally, assign the TigerLINE roadclass to that point.
## Association == k nearest neighbor distance.
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
    
  plt <- ggplot(data=clustered_attributes)+
    geom_boxplot(aes(x = Cluster_Label,y=Traffic_Attribute))+
    labs(title = title, x = xlabel, y = ylabel)+
    theme_pubclean()+
    labs_pubr()
  
  print(
    plt
  )
  
  return(plt)
}

visualize_cluster_results <- function(plot_type,labeled_emissions_results, Var1 = NULL, Var2 = NULL, 
                                      title = "Plot", xlabel=aes_string(Var1),ylabel=aes_string(Var2),
                                      kmeans_obj = NULL){
  
  tibble_to_plot <- as_tibble(labeled_emissions_results) 
  
  if(plot_type=="PCA_Clust"){
    
    require(scattermore)
    
    clustered_polls <- tibble_to_plot %>%
      dplyr::select(BC,CO2,NOx,UFP)%>%
      dplyr::mutate_all(scale)
    
    clustered_pca <- prcomp(clustered_polls,scale. = TRUE)
    
    clustered_pca_scores <- as_tibble(clustered_pca$x[,1:2]) %>%
      cbind("Cluster"=tibble_to_plot$Cluster_Label)
    
    clustered_pca_vars <- clustered_pca$rotation[,1:2]
    
    var_labels <- c("BC",expression(CO[2]),expression(NO[x]),"UFP")
    
    plt <- ggplot(data = clustered_pca_scores,mapping = aes(PC1,PC2))+
      geom_scattermore(aes(colour = Cluster))+
      geom_segment(aes(x = 0, y = 0, xend = 10*clustered_pca_vars[1,1],yend = 10*clustered_pca_vars[1,2]),
                   arrow = arrow(length=unit(0.4,"cm")))+
      geom_segment(aes(x = 0, y = 0, xend = 10*clustered_pca_vars[2,1],yend = 10*clustered_pca_vars[2,2]),
                   arrow = arrow(length=unit(0.4,"cm")))+
      geom_segment(aes(x = 0, y = 0, xend = 10*clustered_pca_vars[3,1],yend = 10*clustered_pca_vars[3,2]),
                   arrow = arrow(length=unit(0.4,"cm")))+
      geom_segment(aes(x = 0, y = 0, xend = 10*clustered_pca_vars[4,1],yend = 10*clustered_pca_vars[4,2],),
                   arrow = arrow(length=unit(0.4,"cm")))+
      annotate("text",x=10*clustered_pca$rotation[,1], y=10*clustered_pca$rotation[,2],label=var_labels,size=5,fontface=2)+
      labs(title = title)+
      theme_classic()
  }
  
  if(plot_type=="fast_scatter"){
    require(scattermore)
    
    plt <- ggplot(tibble_to_plot,aes_string(Var1,Var2))+
        geom_scattermore(aes(colour = Cluster_Label))+
        labs(x=xlabel,y=ylabel,title=title)+
        theme_classic()
    print(plt)
  }
  
  if(plot_type=="nice_scatter"){
    require(ggpubr)
    require(munsell)
    
    no_colors <- length(unique(tibble_to_plot$Cluster_Label))
    
    plt <- ggscatter(data=tibble_to_plot,x=Var1,y=Var2,
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
    print(
      plt
    )
  }
  
  if(plot_type=="box_plot"){
    # require(ggbreak)
    # 
    # bc_p <- ggplot(data=tibble_to_plot,aes(Cluster_Label,BC))+
    #   geom_boxplot()+
    #   theme_classic()+
    #   scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
    #   theme(axis.line.x = element_blank(),axis.text.x = element_blank(),
    #         axis.ticks.x = element_blank(),axis.title.x = element_blank()
    #         )+
    #   annotate("rect",xmin = 0.5, xmax = 3.5,ymin = 0,ymax = 25000,alpha = 0.01)
    #   # scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
    #   # scale_y_break(c(2e4,8e4))
    # 
    # co2_p <- ggplot(data=tibble_to_plot,aes(Cluster_Label,CO2))+
    #   geom_boxplot()+
    #   theme_classic()+
    #   theme(axis.line.x = element_blank(),axis.text.x = element_blank(),
    #         axis.ticks.x = element_blank(),axis.title.x = element_blank()
    #         )
    #   # scale_y_break(c(700,1800))
    # 
    # nox_p <- ggplot(data=tibble_to_plot,aes(Cluster_Label,NOx))+
    #   geom_boxplot()+
    #   theme_classic()
    #   # theme(axis.line.y = element_blank())
    # 
    # ufp_p <- ggplot(data=tibble_to_plot,aes(Cluster_Label,UFP))+
    #   geom_boxplot()+
    #   theme_classic()
    # 
    # plt <- ggarrange(bc_p,co2_p,nox_p,ufp_p,nrow=2,ncol=2,labels = c("(a)","(b)","(c)","(d)"))
    # 
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

    plt <-   bp + facet_wrap(vars(Pollutant),nrow = 2,ncol = 2,scales = "free",
                      labeller = poll_labeller)
      
    print(plt)

  }
  
  if(plot_type=="box_plot_with_inset"){
    require(gridExtra)
    
    
    zoomed_polls <- tibble_to_plot %>%
      dplyr::select(BC,CO2,Cluster_Label)
    
    bc_bp_inset <- ggplot(data=zoomed_polls)+
      geom_boxplot(aes(Cluster_Label,BC),fill=munsell::mnsl("5R 6/14"))+
      scale_y_continuous(limits = c(-7500,20000),labels = function(x) format(x,scientific = TRUE))+
      theme_classic()+
      labs(x=xlabel,y=bquote("BC (ng/"~m^3*")"))+
      theme(panel.background = element_rect(colour="red",size = 1.5),
            axis.line.y = element_blank())
    
    co2_bp_inset <- ggplot(data=zoomed_polls)+
      geom_boxplot(aes(Cluster_Label,CO2),fill=munsell::mnsl("5R 6/14"))+
      scale_y_continuous(limits = c(380,920))+
      theme_classic()+
      labs(x=xlabel,y=bquote(""~CO[2]*" (ppm)"))+
      theme(panel.background = element_rect(colour = "red",size = 1.5),
            axis.line.y = element_blank())
    
    gbcinset <- ggplotGrob(bc_bp_inset)
    gco2inset <- ggplotGrob(co2_bp_inset)
    
    insetWidth = grid::unit.pmax(gbcinset$widths[2:5],gco2inset$widths[2:5])
    
    gbcinset$widths[2:5] <- as.list(insetWidth)
    gco2inset$widths[2:5] <- as.list(insetWidth)
    
    # inset <- ggarrange(bc_bp_inset,co2_bp_inset,nrow=2)
    
    bc_p <- ggplot(data=tibble_to_plot,aes(Cluster_Label,BC))+
      geom_boxplot(fill=munsell::mnsl("5R 6/14"))+
      theme_classic()+
      scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
      theme(axis.line.x = element_blank(),axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),axis.title.x = element_blank()
            )+
      labs(y=bquote("BC (ng/"~m^3*")"))+
      annotate("rect",xmin = 0.5, xmax = 3.5,ymin = -7500,ymax = 20000,alpha = 0,colour = "red",size=1)

    co2_p <- ggplot(data=tibble_to_plot,aes(Cluster_Label,CO2))+
      geom_boxplot(fill=munsell::mnsl("5R 6/14"))+
      theme_classic()+
      theme(axis.line.x = element_blank(),axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),axis.title.x = element_blank()
            )+
      labs(y=bquote(""~CO[2]*" (ppm)"))+
      annotate("rect",xmin = 0.5,xmax = 3.5,ymin = 380,ymax = 920,alpha = 0, colour = "red",size=1)

    nox_p <- ggplot(data=tibble_to_plot,aes(Cluster_Label,NOx))+
      geom_boxplot(fill=munsell::mnsl("5R 6/14"))+
      theme_classic()+
      labs(x=xlabel,y=bquote(""~NO[x]*" (ppb)"))
      # theme(axis.line.y = element_blank())

    ufp_p <- ggplot(data=tibble_to_plot,aes(Cluster_Label,UFP))+
      geom_boxplot(fill=munsell::mnsl("5R 6/14"))+
      scale_y_continuous(labels = function(x) format(x,scientific = TRUE))+
      theme_classic()+
      labs(x=xlabel,y="UFP (p/cc)")


    gbc <- ggplotGrob(bc_p)
    gco2 <- ggplotGrob(co2_p)
    gnox <- ggplotGrob(nox_p)
    gufp <- ggplotGrob(ufp_p)
    
    maxWidth = grid::unit.pmax(gbc$widths[2:5],gco2$widths[2:5],gnox$widths[2:5],gufp$widths[2:5])
    
    gbc$widths[2:5] <- as.list(maxWidth)
    gco2$widths[2:5] <- as.list(maxWidth)
    gnox$widths[2:5] <- as.list(maxWidth)
    gufp$widths[2:5] <- as.list(maxWidth)
    
    orig_plt <- arrangeGrob(gbc,gco2,gnox,gufp,ncol=2,nrow=2)
      
    # title_grob <- grid::textGrob(label = title,
    #                              gp = grid::gpar(fontface=2,fontsize=14))
    
    plt <- arrangeGrob(
      grobs = list(orig_plt,gbcinset,gco2inset),
      top = grid::textGrob(label = title,gp = grid::gpar(fontface = 2,fontsize=14),hjust=0,x=0.05),
      layout_matrix = rbind(c(1,1,1,NA),
                            c(1,1,1,2),
                            c(1,1,1,3),
                            c(1,1,1,NA))
    )
    
    save_plot(filename = paste0(getwd(),"/Manuscript/Figs/",gsub(" ","_",title),".png"),
              width = 8.5, height = 7,
              plot = plt
              )
    
    
  }
  
  return(plt)
  
  dev.off()
}

## For each polygon_name in Houston polygon names
## Return points intersecting polygon with polygon_name
## Return number of points with cluster labels 1,2,3 for row in df with name polygon name
## From df, plot bars in decreasing order with most number of anomalies on left going
## to least number of anomalies on the right.

assign_anomalies_to_polygons <- function(clustered_anomalous_emissions, polygon_sf_df,by_anomaly_type=T){
  
  nearest_centroid <- function(anom_point, intersecting_vector){
  
    queried_centroids <- polygon_sf_df$Centroid[intersecting_vector]
    
    nearest_pt <- st_nearest_feature(anom_point,queried_centroids)
    
    return(intersecting_vector[[nearest_pt]])
  }
  
  raw_anoms_output <- tibble("Name" = polygon_sf_df$Name, 
                             "CO2_Cluster" = rep(0,length(polygon_sf_df$Name)),
                             "Transition_Cluster" = rep(0,length(polygon_sf_df$Name)),
                             "BC_UFP_Cluster" = rep(0,length(polygon_sf_df$Name)))
  
  poly_names <- polygon_sf_df$Name
  
  polygon_intersection <- vector(mode = "list", length = nrow(polygon_sf_df))
  
  initial_intersections <- st_intersects(clustered_anomalous_emissions,polygon_sf_df)
  
  final_intersections <- initial_intersections
  
  multiple_intersects_bool <- sapply(initial_intersections,function(x)length(x)>1)
  
  multiple_intersections <- initial_intersections[multiple_intersects_bool]
  
  multiple_intersecting_anomalies <- clustered_anomalous_emissions[multiple_intersects_bool,]
  
  for(i in 1:length(multiple_intersections)){
    
    list_entry <- which(multiple_intersects_bool)[i]
    
    resolution <- nearest_centroid(clustered_anomalous_emissions[list_entry,],multiple_intersections[[i]])
    
    final_intersections[list_entry] <- resolution
  }
  
  finalized_names <- sapply(final_intersections,function(x){
    ifelse(length(x)==0,"None",poly_names[x])
  })
  
  clustered_anomalous_emissions <- cbind(clustered_anomalous_emissions,"Intersecting_Poly"=finalized_names)
  
  for(k in 1:length(polygon_sf_df$Name)){
    current_poly_name <- poly_names[k]
    
    print(current_poly_name)
    
    intersecting_points <- clustered_anomalous_emissions %>% dplyr::filter(Intersecting_Poly==current_poly_name)
    
    if(by_anomaly_type){
      raw_anoms_output$CO2_Cluster[k] <- nrow(intersecting_points %>% dplyr::filter(Cluster_Label==1))
      raw_anoms_output$Transition_Cluster[k] <- nrow(intersecting_points %>% dplyr::filter(Cluster_Label==2))
      raw_anoms_output$BC_UFP_Cluster[k] <- nrow(intersecting_points %>% dplyr::filter(Cluster_Label==3))
    }
    
    polygon_intersection[[k]][[1]] <- current_poly_name
    polygon_intersection[[k]][[2]] <- intersecting_points
    
  }
  
  return(list("Raw_Anomaly_Output"=raw_anoms_output, "Points_In_Polygon"=polygon_intersection))
}

## Function which returns total number of points intersecting polygon_name
## in sf_hou_polygon
return_total_number_inpolygon <- function(sf_df, sf_hou_polygon){
  
  nearest_centroid <- function(anom_point, intersecting_vector){
  
    queried_centroids <- sf_hou_polygon$Centroid[intersecting_vector]
    
    nearest_pt <- st_nearest_feature(anom_point,queried_centroids)
    
    return(intersecting_vector[[nearest_pt]])
  }
  
  total_points <- tibble("Name"=sf_hou_polygon$Name, "Total_Points"=rep(0,nrow(sf_hou_polygon)))
  
  poly_names <- sf_hou_polygon$Name
  
  total_intersecting_list <- vector(mode = "list", length = nrow(sf_hou_polygon))
  
  initial_intersections <- st_intersects(sf_df,sf_hou_polygon)
  
  final_intersections <- initial_intersections
  
  multiple_intersects_bool <- sapply(initial_intersections,function(x)length(x)>1)
  
  multiple_intersections <- initial_intersections[multiple_intersects_bool]
  
  for(i in 1:length(multiple_intersections)){
    
    list_entry <- which(multiple_intersects_bool)[i]
    
    resolution <- nearest_centroid(sf_df[list_entry,],multiple_intersections[[i]])
    
    final_intersections[list_entry] <- resolution
  }
  
  finalized_names <- sapply(final_intersections,function(x){
    ifelse(length(x)==0,"None",poly_names[x])
  })
  
  sf_df <- cbind(sf_df,"Intersecting_Poly"=finalized_names)
  
  for(k in 1:length(poly_names)){
    current_poly_name <- poly_names[k]
    
    print(current_poly_name)
    
    intersecting_points <- sf_df %>% dplyr::filter(Intersecting_Poly==current_poly_name)
    
    # current_poly <- sf_hou_polygon[sf_hou_polygon$Name==current_poly_name, ]
    # 
    # intersections <- which(
    #                   unlist(
    #   lapply(st_intersects(current_emissions, current_poly),function(x) length(x)),
    #                   use.names = F) !=0)
    # 
    total_points$Total_Points[k] <- nrow(intersecting_points)
    
    total_intersecting_list[[k]][[1]] <- current_poly_name
    
    total_intersecting_list[[k]][[2]] <- intersecting_points
  }
  
  
  return(list("Total_Points" = total_points, "Intersecting_List"=total_intersecting_list))
}


norm_by_anoms <- . %>% mutate(total = rowSums(select(.,2:4))) %>%
    mutate_at(2:4, ~./total) %>%
    mutate_at(2:4, ~ round(.,3)*100) %>%
    select(-total) 

norm_by_total <- function(raw_anoms_output, total_points){
  anoms_norm_by_total <- raw_anoms_output %>%
    mutate_at(2:4, ~./total_points) %>%
    mutate_at(2:4, ~ round(.,3)*100)
  
  return(anoms_norm_by_total)
    
}

count_anomaly_type_in_polygon <- function(intersecting_list_anomaly){
  
  CO2_Anomalies <- nrow(intersecting_list_anomaly[[2]] %>%
                          dplyr::filter(Cluster_Label==1))
  
  Transition_Anomalies <- nrow(intersecting_list_anomaly[[2]] %>%
                                 dplyr::filter(Cluster_Label==2))
  
  BC_UFP_Anomalies <- nrow(intersecting_list_anomaly[[2]] %>%
                             dplyr::filter(Cluster_Label==3))
  
  return(list("CO2_Anomalies"=CO2_Anomalies,"Transition_Anomalies"=Transition_Anomalies,
              "BC_UFP_Anomalies"=BC_UFP_Anomalies))
}

count_total_points_in_polygon <- function(intersecting_list_total){
  
  total_points <- nrow(intersecting_list_total[[2]])
  
  return(total_points)
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
  
  transformed_centers <- as.data.frame(transformed_centers) %>%
    round(2)
  
  rownames(transformed_centers) <- c("Cluster 1", "Cluster 2", "Cluster 3")
  
  # require(knitr)
  
  # print(
  #   transformed_centers %>%
  #     kable(format = "html",
  #         col.names = c("BC",
  #                     "CO<sub>2</sub>(ppm)",
  #                     "NO<sub>x</sub>(ppb)",
  #                     "UFP (p/cc)")) %>%
  #     kable_styling()
  # )
  # 
  return(transformed_centers)
  
}

## Cluster_type designations:
## 1: CO2 Cluster
## 2: Transition Cluster
## 3: BC/UFP Cluster

create_census_tract_histogram <- function(intersecting_list,  poly_name,
                                          cluster_type = NA,
                                          title = "Title",
                                          xlabel = "Hours",
                                          ylabel = "Frequency",
                                          polygon_sf_df=houston_polygons){
  
  names_in_list <- unlist(
    lapply(intersecting_list,function(x) return(x[[1]])),
    use.names = F)
  
  selected_entry <- which(names_in_list==poly_name)
  ## Extract data corresponding to selected entry.
  queried_data <- intersecting_list[[selected_entry]][[2]]
  
  ## Filter clustered emissions by cluster_type
  
  print(is.na(cluster_type))
  if(!is.na(cluster_type)){
    queried_data <- queried_data %>%
      dplyr::filter(Cluster_Label %in% cluster_type)
  }
  
  ## Determine the hours measured in the time series.
  hours_measured <- lubridate::hour(queried_data$LST)
  
  first_hour <- min(hours_measured)
  
  last_hour <- max(hours_measured)
  
  poly_hist <- ggplot()+
    geom_histogram(aes(hours_measured), alpha=0.1, binwidth=1,color="black")+
    labs(title=title,x=xlabel,y=ylabel)+
    scale_x_continuous(breaks = seq(first_hour,last_hour,1),labels = seq(first_hour,last_hour,1))+
    theme_classic()
  

  
  out_hist <- hist(hours_measured,breaks=seq(first_hour,last_hour,1))$counts
  
  print(poly_hist)
  
  # return(out_hist)
  return(poly_hist)
}

remove_weekend_points <- function(intersecting_list_entry){
    
  intersecting_list_entry[[2]] <- intersecting_list_entry[[2]] %>%
    as_tibble() %>%
    dplyr::mutate(Day_Of_Week = lubridate::wday(LST)) %>%
    dplyr::filter(!(Day_Of_Week==1|Day_Of_Week==7))
  
  return(intersecting_list_entry)
  
}

trim_hourly_tails <- function(intersecting_total_df_entry, intersecting_anomaly_df_entry){
    
  trimmed_totals <- intersecting_total_df_entry %>%
    as_tibble() %>%
    dplyr::mutate(H = lubridate::hour(LST)) 
  
  low_bound <- 8
  
  upper_bound <- 15
  
  trimmed_totals <- trimmed_totals %>%
    dplyr::filter(H>=low_bound & H<=upper_bound) %>%
    dplyr::select(-H)
  
  trimmed_anomalies <- intersecting_anomaly_df_entry %>%
    as_tibble() %>%
    dplyr::mutate(H = lubridate::hour(LST)) %>%
    dplyr::filter(H>=low_bound & H<=upper_bound) %>%
    dplyr::select(-H)
  
  
  return(list("Trimmed_Totals"=trimmed_totals, "Trimmed_Anomalies"=trimmed_anomalies))
}

normalize_ct <- function(intersecting_list_anomaly, intersecting_list_total,remove_weekends=T,trim_cts=T){
  
  ## For each polygon, need to calculate the total measurements
  ## Determine the number of unique sampling hours
  ## Divide total measurements by num unique sampling hours
  ## For each hour, divide each anomaly type by number of total measurements
  ## Multiply each hourly fraction by the average sample/hr and add together
  
  count_hourly_total_points <- function(total_intersecting_list_entry){
    
    sf_tibble <- total_intersecting_list_entry[[2]] %>%
      as_tibble() %>%
      dplyr::mutate(H=lubridate::hour(LST))
    
    unique_hours <- unique(sf_tibble$H)
    
    hourly_totals <- sapply(unique_hours,function(x) return(nrow(sf_tibble %>% dplyr::filter(H==x))))
    
    total_hourly_points <- tibble("Hour"=unique_hours,"Totals"=hourly_totals)
    
    return(total_hourly_points)
  }
  
  count_hourly_anomaly_type <- function(intersecting_list_anomaly_entry){
    
    sf_tibble <- intersecting_list_anomaly_entry[[2]] %>%
      as_tibble() %>%
      dplyr::mutate(H=lubridate::hour(LST))
    
    unique_hours <- unique(sf_tibble$H)
    
    cluster_types <- unique(sf_tibble$Cluster_Label)
    
    hourly_anomaly_entries <- tibble("Hour"=unique_hours,"CO2_Cluster"=rep(0,length(unique_hours)),
                                     "Transition_Cluster"=rep(0,length(unique_hours)),
                                     "BC_UFP_Cluster"=rep(0,length(unique_hours)))
    
    for(h in 1:length(unique_hours)){
      for(j in 1:length(cluster_types)){
        given_anomaly_entries <- nrow(sf_tibble %>% dplyr::filter(H==unique_hours[h] & Cluster_Label==j))
        hourly_anomaly_entries[h,(j+1)] <- given_anomaly_entries
      }
    }
      
    return(hourly_anomaly_entries)
  }
  
  extract_avg_samples <- function(intersecting_list_entry){
    
    sf_tibble <- intersecting_list_entry[[2]] %>%
      as_tibble()
    
    num_pts <- nrow(sf_tibble)
    
    unique_hours <- unique(lubridate::hour(sf_tibble$LST))
    
    return(num_pts/length(unique_hours))
  }
  
  if(remove_weekends){
    intersecting_list_total <- lapply(intersecting_list_total,function(x) remove_weekend_points(x))
      
    intersecting_list_anomaly <- lapply(intersecting_list_anomaly, function(x) remove_weekend_points(x))
  }
  
  if(trim_cts){
    # browser()
    
    for(k in 1:length(intersecting_list_total)){
      trimmed_output <- trim_hourly_tails(intersecting_list_total[[k]][[2]], intersecting_list_anomaly[[k]][[2]])
      
      intersecting_list_total[[k]][[2]] <- trimmed_output$Trimmed_Totals
      
      intersecting_list_anomaly[[k]][[2]] <- trimmed_output$Trimmed_Anomalies
    }
  }
  
  avg_samples <- sapply(intersecting_list_total,function(x) extract_avg_samples(x))
  
  total_measurement_by_hour <- lapply(intersecting_list_total,function(x) count_hourly_total_points(x))
  
  total_anomaly_by_hour <- lapply(intersecting_list_anomaly, function(x) count_hourly_anomaly_type(x))
  
  # return(list(total_anomaly_by_hour,total_measurement_by_hour))
  
  ## Calculate fraction anomaly_type in hour/total measurements in hour
  polygon_names <- sapply(intersecting_list_anomaly,function(x) return(x[[1]]))
  
  reweighted_ct_anomaly_counts <- tibble("Name"=polygon_names,"CO2_Cluster"=rep(0,length(polygon_names)),
                                         "Transition_Cluster"=rep(0,length(polygon_names)),
                                         "BC_UFP_Cluster"=rep(0,length(polygon_names)))

  for(i in 1:nrow(reweighted_ct_anomaly_counts)){
    fraction_anomaly_type_per_hour <- dplyr::left_join(total_measurement_by_hour[[i]],total_anomaly_by_hour[[i]],by = "Hour") %>%
      dplyr::mutate_at(3:5,~./Totals) %>%
      replace_na(list(CO2_Cluster=0,Transition_Cluster=0,BC_UFP_Cluster=0)) %>%
      dplyr::select(CO2_Cluster,Transition_Cluster,BC_UFP_Cluster)

    rescaled_anomalies <- fraction_anomaly_type_per_hour*avg_samples[i]

    total_anomaly_by_type <- apply(rescaled_anomalies,2,sum)

    reweighted_ct_anomaly_counts[i,2] <- total_anomaly_by_type[1]
    reweighted_ct_anomaly_counts[i,3] <- total_anomaly_by_type[2]
    reweighted_ct_anomaly_counts[i,4] <- total_anomaly_by_type[3]
  }

  reweighted_ct_anomaly_norm_by_anomaly <- reweighted_ct_anomaly_counts %>%
    dplyr::mutate(Total_Anoms=CO2_Cluster+Transition_Cluster+BC_UFP_Cluster) %>%
    dplyr::mutate_at(2:4, ~./Total_Anoms)%>%
    dplyr::mutate_at(2:4, ~ round(.,3)*100)
  
  Total_Measurements <- unlist(lapply(intersecting_list_total,function(x) nrow(x[[2]])))
  
  reweighted_ct_anomaly_norm_by_total <- reweighted_ct_anomaly_counts %>% 
    cbind("Total_Measurements"=Total_Measurements) %>%
    dplyr::mutate_at(2:4,~./Total_Measurements) %>%
    dplyr::mutate_at(2:4, ~round(.,3)*100)
    
  
  return(list("Rescaled_Anomaly_Totals"=reweighted_ct_anomaly_counts,
              "Rescaled_Anomaly_Normed_By_Anomaly"=reweighted_ct_anomaly_norm_by_anomaly,
              "Rescaled_Anomaly_Normed_By_Total"=reweighted_ct_anomaly_norm_by_total))
}


## Data preprocessing - loading the txdot shapefile
{
  
  load(paste0(getwd(),"/txdot_roadways/txdot_joint_inventory.RData"))  
  
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
  
  houston_polygons <- key_conversion(houston_polygons) %>%
    dplyr::mutate(Centroid=st_centroid(geometry))
  
  
  
}


## Data preprocessing - DBSCAN anomalies
{
  require(data.table)
  
  ## Load in anomalous emissions. For now, we use DBSCAN_v05
  system.time(db_labeled_emissions <- fread(paste0(getwd(),
                                         "/Anomalous_Emissions_Results/Labeled_Emissions_DBSCAN_V05.csv")) %>%
    select(BC,CO2,NOx,UFP,Lat1,Long1,Anomaly,Uniq_Fac,LST) %>%
    st_as_sf(.,coords = c("Long1", "Lat1"), crs = "EPSG:4326") %>%
    st_transform("EPSG:32615") %>%
    shapefile_processing()
  )
  db_anomalous_emissions <- db_labeled_emissions %>%
    dplyr::filter(Anomaly==2)
   
  ## Take out weekends
  if(F){
    db_labeled_emissions <- db_labeled_emissions %>%
      dplyr::mutate(DayOfWeek=lubridate::wday(LST)) %>%
      dplyr::filter(DayOfWeek!=1 | DayOfWeek!=7) %>%
      dplyr::select(-DayOfWeek)
    
    db_anomalous_emissions <- db_anomalous_emissions %>%
      dplyr::mutate(DayOfWeek=lubridate::wday(LST)) %>%
      dplyr::filter(DayOfWeek!=1 | DayOfWeek!=7) %>%
      dplyr::select(-DayOfWeek)
  }
}

## Load in data - QOR preprocessing
{
  qor_labeled_emissions <- fread(paste0(getwd(),
                                         "/Anomalous_Emissions_Results/Labeled_Emissions_Quantile_OR.csv")) %>%
    select(BC,CO2,NOx,UFP,Lat1,Long1,Anomaly,Uniq_Fac,LST) %>%
    st_as_sf(.,coords = c("Long1", "Lat1"), crs = "EPSG:4326") %>%
    st_transform("EPSG:32615") %>%
    shapefile_processing()
  
  qor_anomalous_emissions <- qor_labeled_emissions %>%
    filter(Anomaly==2) 
  
  if(F){
    qor_labeled_emissions <- qor_labeled_emissions %>%
      dplyr::mutate(DayOfWeek=lubridate::wday(LST)) %>%
      dplyr::filter(DayOfWeek!=1 | DayOfWeek!=7) %>%
      dplyr::select(-DayOfWeek)
    
    qor_anomalous_emissions <- qor_anomalous_emissions %>%
      dplyr::mutate(DayOfWeek=lubridate::wday(LST)) %>%
      dplyr::filter(DayOfWeek!=1 | DayOfWeek!=7) %>%
      dplyr::select(-DayOfWeek)
  }
}

## Performing the clustering - DBSCAN
{
  set.seed(10)

  db_cluster_output <- cluster_anomalies(db_anomalous_emissions, centers = 3, nstart = 200)

  db_clust_anoms <- db_cluster_output[[2]] 
  
  if(F){
    db_clust_anoms <- db_clust_anoms %>%
      dplyr::mutate(DayOfWeek=lubridate::wday(LST)) %>%
      dplyr::filter(DayOfWeek!=1|DayOfWeek!=7) %>%
      dplyr::select(-DayOfWeek)
  }

  inverted_cluster_means(db_cluster_output)
}

## Performing the clustering - QOR

{
  set.seed(10)
  
  qor_cluster_output <- cluster_anomalies(qor_anomalous_emissions, centers = 3, nstart = 200)

  qor_clust_anoms <- qor_cluster_output[[2]]
  
  inverted_cluster_means(qor_cluster_output)
}

## Visualizing DBSCAN anomaly clustering results - projection onto principal component axes,
## boxplots, and scatterplots
{
  dbs_scatter <- visualize_cluster_results("nice_scatter", db_clust_anoms,Var1 = "BC",Var2 = "CO2",
                            title = "K-means visualized for all DBSCAN labeled anomalies",
                            xlabel = expression(paste("BC(ng/",m^3,")")),
                            ylabel = expression(paste(CO[2],"(ppm)")))

  save_plot(paste0(getwd(),"/Manuscript/Figs/DBSCAN_Kmeans_Clustered_Anomalies.png"), width =6, height = 6)

  dbs_pca_clust <- visualize_cluster_results(plot_type = "PCA_Clust",db_clust_anoms,
                                  title = "Visualizing DBSCAN Anomalies on Principal Axes")

  print(dbs_pca_clust)
  
  dbs_bxp <- visualize_cluster_results(plot_type = "box_plot_with_inset",
                                  db_clust_anoms,
                                  title = "Clustered DBSCAN Anomaly Boxplots",
                                  xlabel = "Cluster",
                                  ylabel = "Measurement")

  save_plot(paste0(getwd(),"/Manuscript/Figs/DBSCAN_Kmeans_Two_Principal_Axes.png"), width = 8, height = 8)
  
  print(dbs_bxp)
  
  grid::grid.draw(dbs_bxp)
  
  save_plot(paste0(getwd(),"/Manuscript/Figs/DBSCAN_Anomaly_Cluster_Boxplot.png"), width = 8, height = 8,plot = dbs_bxp)
  
  if(T){
    polls_to_rotate <- db_clust_anoms %>% 
      as_tibble() %>%
      dplyr::select(BC,CO2,NOx,UFP) 
    
    psych::principal(polls_to_rotate,nfactors = 2,rotate = "varimax")
  }
}

## Producing boxplots of clustered traffic attributes for DBSCAN anomalies
{
  clustered_aadt_df <- return_clustered_attributes(db_clust_anoms, txdot_joint_inventory,
                                                      "ADT_CUR")

  clustered_trkaadtpct_df <- return_clustered_attributes(db_clust_anoms, txdot_joint_inventory,
                                                      "TRK_AADT_PCT")

  p1 <- plot_clustered_roadway_features(clustered_aadt_df, title = NULL,
                                        xlabel = "Cluster", ylabel = "AADT")

  p2 <- plot_clustered_roadway_features(clustered_trkaadtpct_df, title = NULL,
                                        xlabel = "Cluster", ylabel = "AADT % Truck")

  ggarrange(p1,p2,labels = list("(a)","(b)"))

  save_plot(paste0(getwd(),"/Manuscript/Figs/DBSCAN_Clustered_Traffic_Attributes.png"),width = 8, height = 4)
}


## Producing other visualizations of clustered results.
{
  # visualize_cluster_results(plot_type = "fast_scatter",
  #                           clustered_anomalous_emissions,
  #                           "BC", "CO2",
  #                           title = "DBSCAN_V05",
  #                           xlabel = "BC",
  #                           ylabel = "CO2")
  
  # visualize_cluster_results(plot_type = "box_plot",
  #                           clustered_anomalous_emissions,
  #                           title = "Clustered DSBCAN Anomaly Boxplots",
  #                           xlabel = "Cluster Label"
  #                           )
  # 
  # save_plot(paste0(getwd(),"/Manuscript/Figs/DBSCAN_Anomaly_Cluster_Boxplot.png"),
  #           height = 6, width = 8.5)
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


## Assigning DBSCAN anomaly type distributions by census tract.
{
  total_polygon_points <- fread(paste0(getwd(),"/total_polygon_points_updated.csv"), select = c("Name","Total_Points"))
  
  db_anoms_output <- assign_anomalies_to_polygons(db_clust_anoms,houston_polygons)
  
  print("DBSCAN anomalies output computed")
  # load(file=paste0(getwd(),"/db_anoms_output.RData"))
  
  # db_anoms_output_total <- db_anoms_output$Raw_Anomaly_Output
  # 
  # db_anoms_norm_by_anoms <- db_anoms_output$Raw_Anomaly_Output %>% norm_by_anoms(.)
  # 
  # db_anoms_norm_by_total <- norm_by_total(db_anoms_output$Raw_Anomaly_Output, total_polygon_points$Total_Points)
  
  # db_anoms_norm_by_total %>%
  #   mutate_at(2:4,~ round(.,2))%>%
  #   kableExtra::kbl() %>%
  #   kableExtra::kable_classic_2(bootstrap_options = c("condensed"),full_width = F) %>%
  #   kableExtra::row_spec(c(18,26), bold = T, background = "red") %>%
  #   kableExtra::column_spec(1:3, bold = F, background = "white")
  
  total_output <- return_total_number_inpolygon(db_labeled_emissions,houston_polygons)
}

## Rescaling temporal effects for DBSCAN anomalies. All CTs
{
  load(paste0(getwd(),"/db_anoms_output.RData"))  
  
  load(paste0(getwd(),"/t_points_by_poly.RData"))  
  
  rescaled_values <- normalize_ct(db_anoms_output$Points_In_Polygon,t_points_by_poly$Intersecting_List)  
  
  rescaled_anomaly_totals <- rescaled_values$Rescaled_Anomaly_Totals 
  rescaled_anomaly_by_anom <- rescaled_values$Rescaled_Anomaly_Normed_By_Anomaly
  rescaled_anomaly_by_total <- rescaled_values$Rescaled_Anomaly_Normed_By_Total
}

## Determining DBSCAN anomaly type distributions by census tract. Tier 1, part of Tier 2, and SBC selected.
##  North River Oaks, South River Oaks, Kashmere, and East Fifth Ward excluded
## Temporal rescaling implemented.
{
  total_polygon_points <- fread(paste0(getwd(),"/total_polygon_points_updated.csv"), select = c("Name","Total_Points"))
  
  load(paste0(getwd(),"/db_anoms_output.RData"))
  
  load(paste0(getwd(),"/t_output.RData"))  
  
  polys_2_extract <- (!(houston_polygons$ct_num==1 | houston_polygons$ct_num==2 | houston_polygons$Name=="North River Oaks" | houston_polygons$Name=="South River Oaks"))
  
  anomaly_all_roads_list <- db_anoms_output$Points_In_Polygon[polys_2_extract]
  
  total_all_roads_list <- total_output$Intersecting_List[polys_2_extract]
  
  rescaled_values <- normalize_ct(anomaly_all_roads_list,
                                  total_all_roads_list,
                                  remove_weekends = T,
                                  trim_cts = T)  
  
  rescaled_anomaly_totals <- rescaled_values$Rescaled_Anomaly_Totals 
  rescaled_anomaly_by_anom <- rescaled_values$Rescaled_Anomaly_Normed_By_Anomaly
  rescaled_anomaly_by_total <- rescaled_values$Rescaled_Anomaly_Normed_By_Total
  
  # raw_anoms_output <- db_anoms_output$Raw_Anomaly_Output
  # 
  # raw_anoms_over_total <- norm_by_total(raw_anoms_output,total_polygon_points$Total_Points)
  
}

## Same analysis as above, but with highway related points (Road_Classes S1100, S1630, S1640)
## excluded.
{
  total_polygon_points <- fread(paste0(getwd(),"/total_polygon_points_updated.csv"), select = c("Name","Total_Points"))
  
  load(paste0(getwd(),"/db_anoms_output.RData"))
  
  load(paste0(getwd(),"/t_output.RData"))  
  
  polys_2_extract <- (!(houston_polygons$ct_num==1 | houston_polygons$ct_num==2 | houston_polygons$Name=="North River Oaks" | houston_polygons$Name=="South River Oaks"))
  
  remove_highway_roads <- function(intersecting_list_entry){
    
    intersecting_list_entry[[2]] <- intersecting_list_entry[[2]] %>%
      dplyr::filter(!(Road_Class=="S1100" | Road_Class=="S1630" | Road_Class=="S1640"))
    
    return(intersecting_list_entry)
  }
  
  anomaly_neighborhood_list <- lapply(db_anoms_output$Points_In_Polygon[polys_2_extract],function(x) remove_highway_roads(x))
  
  total_neighborhood_list <- lapply(total_output$Intersecting_List[polys_2_extract],function(x) remove_highway_roads(x))
  
  rescaled_neighborhood_values <- normalize_ct(anomaly_neighborhood_list,
                                  total_neighborhood_list,
                                  remove_weekends = T,
                                  trim_cts = T)  
  
  rescaled_neighborhood_anomaly_totals <- rescaled_neighborhood_values$Rescaled_Anomaly_Totals 
  rescaled_neighborhood_anomaly_by_anom <- rescaled_neighborhood_values$Rescaled_Anomaly_Normed_By_Anomaly
  rescaled_neighborhood_anomaly_by_total <- rescaled_neighborhood_values$Rescaled_Anomaly_Normed_By_Total
}

## Same analysis as above, but on the raw data with no temporal rescaling
## Weekends, hours trimmed performed
{
  total_polygon_points <- fread(paste0(getwd(),"/total_polygon_points_updated.csv"), select = c("Name","Total_Points"))
  
  load(paste0(getwd(),"/db_anoms_output.RData"))
  
  load(paste0(getwd(),"/t_output.RData"))  
  
  polys_2_extract <- (!(houston_polygons$ct_num==1 | houston_polygons$ct_num==2 | houston_polygons$Name=="North River Oaks" | houston_polygons$Name=="South River Oaks"))
  
  anomaly_all_roads_list <- db_anoms_output$Points_In_Polygon[polys_2_extract]
  
  total_all_roads_list <- total_output$Intersecting_List[polys_2_extract]
  
  anomaly_all_roads_list <- lapply(anomaly_all_roads_list, function(x) remove_weekend_points(x))
  
  total_all_roads_list <- lapply(total_all_roads_list, function(x) remove_weekend_points(x))
  
  for (j in seq(from=1,to=length(anomaly_all_roads_list),by=1)){
    
    trimmed_output <- trim_hourly_tails(anomaly_all_roads_list[[j]][[2]],total_all_roads_list[[j]][[2]])
    
    anomaly_all_roads_list[[j]][[2]] <- trimmed_output[[1]]
      
    total_all_roads_list[[j]][[2]] <- trimmed_output[[2]]  
  }
  
  co2_output <- sapply(anomaly_all_roads_list,function(x) count_anomaly_type_in_polygon(x)$CO2_Anomalies)
  
  transition_output <- sapply(anomaly_all_roads_list,function(x) count_anomaly_type_in_polygon(x)$Transition_Anomalies)
  
  bc_ufp_output <- sapply(anomaly_all_roads_list,function(x) count_anomaly_type_in_polygon(x)$BC_UFP_Anomalies)
  
  trimmed_raw_anoms_output <- tibble("Name"=sapply(anomaly_all_roads_list,function(x)x[[1]]),
                                     "CO2_Cluster"=co2_output,
                                     "Transition_Cluster"=transition_output,
                                     "BC_UFP_Cluster"=bc_ufp_output)
  
  total_raw_trimmed_points <- sapply(total_all_roads_list,function(x) count_total_points_in_polygon(x))
  
  trimmed_anoms_by_total <- norm_by_total(trimmed_raw_anoms_output,total_raw_trimmed_points)
}

## Bar plots of anomaly types per census tract. DBSCAN.
{
  # p1 <- census_tract_cluster_statplots(db_anoms_output_total, "CO2_Cluster",
  #                                title = expression(paste("Total DBSCAN ", CO[2], " Anomaly Detections by Census Tract")),
  #                                ylabel = "Count")
  # 
  # p2 <- census_tract_cluster_statplots(db_anoms_output_total, "BC_UFP_Cluster",
  #                              title = expression(paste("Total DBSCAN BC/UFP Anomaly Detections by Census Tract")),
  #                              ylabel = "Count")
  # 
  # ggarrange(p1,p2,nrow = 2)
  # 
  # save_plot(paste0(getwd(),"/Manuscript/Figs/DBSCAN_Total_Anomaly_Bar_Stacked_Post_Intersection.png"),height = 11, width = 8)
  # 
  # 
  # p3 <- census_tract_cluster_statplots(db_anoms_norm_by_anoms, "CO2_Cluster",
  #                              title = expression(paste("DBSCAN ",CO[2]," Anomalies Normalized by Census Tract Anomaly Total")),
  #                              ylabel = expression(paste("% ", CO[2], " Anomaly")))
  
  # census_tract_cluster_statplots(db_anoms_norm_by_anoms, "Transition_Cluster",
  #                              title = expression(paste("DBSCAN Transition Anomalies Normalized by Census Tract Anomaly Total")),
  #                              ylabel = "% Transition Anomaly")
  # 
  # p4 <- census_tract_cluster_statplots(db_anoms_norm_by_anoms, "BC_UFP_Cluster",
  #                              title = expression(paste("DBSCAN BC/UFP Anomalies Normalized by Census Tract Anomaly Total")),
  #                              ylabel = expression(paste("% BC/UFP Anomaly")))
  # 
  # ggarrange(p3,p4,nrow = 2)
  # 
  # 
  p5 <- census_tract_cluster_statplots(raw_anoms_over_total, "CO2_Cluster",
                               title = expression(paste("Normalized DBSCAN ", CO[2], " Anomalies")),
                               ylabel = expression(paste("% ", CO[2])))
  
  p6 <- census_tract_cluster_statplots(raw_anoms_over_total, "BC_UFP_Cluster",
                               title = expression(paste("Normalized BC/UFP Anomalies")),
                               ylabel = expression(paste("% BC/UFP")))
  
  ggarrange(p5,p6,nrow=2)
  
  save_plot(paste0(getwd(),"/Manuscript/Figs/DBSCAN_Raw_Anomaly_Over_Total.png"),height = 11, width = 8)
}

## Comparing CTs from reweighted schemes, original schemes with trimmed measurements
## Probability of selecting anomaly type in given census tract.
{
  rescaled_anomaly_by_total_total_removed <- rescaled_anomaly_by_total %>%
    dplyr::select(-c(Total_Measurements))
  
  combined_totals <- rbind(trimmed_anoms_by_total,rescaled_anomaly_by_total_total_removed) %>%
    dplyr::mutate(IsScaled=ifelse(as.numeric(rownames(.))<20,"Unscaled","Scaled"))
  
  bc_effects <- census_tract_cluster_statplots(combined_totals,"BC_UFP_Cluster",
                                       title = expression(paste("Effects of Scaling on Normalized DBSCAN BC/UFP Anomalies")),
                                       ylabel = expression(paste("% BC/UFP")),
                                       fill_color = c(munsell::mnsl("5G 6/8"),munsell::mnsl("5YR 7/12")),
                                       bar_groups = "IsScaled")
  
  save_plot(paste0(getwd(),"/Manuscript/Figs/Effects_Of_Scaling_DBSCAN_BC_UFP_Over_Total.png"),width = 8,height = 5.5)
   
  co2_effects <- census_tract_cluster_statplots(combined_totals,"CO2_Cluster",
                                       title = expression(paste("Effects of Scaling on Normalized DBSCAN ", CO[2], " Anomalies")),
                                       ylabel = expression(paste("% ",CO[2])),
                                       fill_color = c(munsell::mnsl("5G 6/8"),munsell::mnsl("5YR 7/12")),
                                       bar_groups = "IsScaled")
  
  save_plot(paste0(getwd(),"/Manuscript/Figs/Effects_Of_Scaling_DBSCAN_CO2_Over_Total.png"),width = 8,height = 5.5)
}

## Comparing CTs from reweighted schemes, reweighted schemes without highway
## Comparing probabilities of detection, not the raw totals themselves for manu
{
  wp1 <- census_tract_cluster_statplots(rescaled_anomaly_by_total,"CO2_Cluster",
                                        title = expression(paste(CO[2], " Anomaly Detections Normalized by Census Tract Total Measurements")),
                                        ylabel = expression(paste("% ", CO[2])),
                                        fill_color = munsell::mnsl("5G 6/8"))
  
  wp2 <- census_tract_cluster_statplots(rescaled_anomaly_by_total,"BC_UFP_Cluster",
                                        title = expression(paste("BC/UFP Anomaly Detections by Normalized Census Tract Total Measurements")),
                                        ylabel = expression(paste("% BC/UFP")),
                                        fill_color = munsell::mnsl("5G 6/8"))
  

  output <- gridExtra::arrangeGrob(wp1,wp2)
  
  save_plot(filename = paste0(getwd(),"/Manuscript/Figs/Rescaled_Anomaly_Over_Total.png"),
            width = 8,height = 11,plot = output)
  
  wp3 <- census_tract_cluster_statplots(rescaled_anomaly_by_total,"Transition_Cluster",
                                        title = expression(paste("Transition Anomaly Detections by Normalized Census Tract Total Measurements")),
                                        ylabel = expression(paste("% Transition")),
                                        fill_color = munsell::mnsl("5G 6/8"))
  
  save_plot(filename = paste0(getwd(),"/Manuscript/Figs/Rescaled_Transition_Anomaly_Over_Total.png"),
            width = 8,height = 5.5,plot = output)
  
  # np1 <- census_tract_cluster_statplots(rescaled_neighborhood_anomaly_by_total,"CO2_Cluster",
  #                                      title = expression(paste("Rescaled DBSCAN ", CO[2], " Total Anomaly Detections in Neighborhood of Census Tract")),
  #                                      ylabel = "Count",
  #                                      fill_color = munsell::mnsl("5PB 5/12"))
  # 
  # np2 <- census_tract_cluster_statplots(rescaled_neighborhood_anomaly_totals,"BC_UFP_Cluster",
  #                                      title = expression(paste("Rescaled DBSCAN BC/UFP Total Anomaly Detections in Neighborhood of  Census Tract")),
  #                                      ylabel = "Count",
  #                                      fill_color = munsell::mnsl("5PB 5/12"))
  
  combined_totals <- rbind(rescaled_anomaly_by_total,rescaled_neighborhood_anomaly_by_total) %>%
    dplyr::mutate(ContainsHighway=ifelse(as.numeric(rownames(.))<20 ,"With Highway","No Highway"))
  
  bc_effects <- census_tract_cluster_statplots(combined_totals,"BC_UFP_Cluster",
                                       title = expression(paste("Effects of Removing Highways on Normalized DBSCAN BC/UFP Anomalies")),
                                       ylabel = expression(paste("% BC/UFP")),
                                       fill_color = c(munsell::mnsl("5PB 5/12"),munsell::mnsl("5G 6/8")),
                                       bar_groups = "ContainsHighway")
  
  save_plot(filename = paste0(getwd(),"/Manuscript/Figs/Effects_of_Highway_Removal_BC_UFP_Over_Total.png"),
            width = 8,height = 5.5,plot = bc_effects)
  
  co2_effects <- census_tract_cluster_statplots(combined_totals,"CO2_Cluster",
                                       title = expression(paste("Effects of Removing Highways on Normalized ",CO[2], " Anomalies")),
                                       ylabel = expression(paste("% ", CO[2])),
                                       fill_color = c(munsell::mnsl("5PB 5/12"),munsell::mnsl("5G 6/8")),
                                       bar_groups = "ContainsHighway")
  
  save_plot(filename = paste0(getwd(),"/Manuscript/Figs/Effects_of_Highway_Removal_CO2_Over_Total.png"),
            width = 8,height = 5.5,plot = co2_effects)
  
  ## Create table containing probabilities of detecting each anomaly type
  if(T) {
    rescaled_anomaly_by_total %>%
      kableExtra::kbl() %>%
      kableExtra::kable_paper(full_width = F)
  }
}

## Determining QOR anomaly type distributions by census tract
{
  qor_anoms_output <- assign_anomalies_to_polygons(qor_clust_anoms,houston_polygons)

  qor_anoms_norm_by_anoms <- qor_anoms_output[[1]] %>% norm_by_anoms(.)

  qor_anoms_norm_by_total <- norm_by_total(qor_anoms_output[[1]], total_polygon_points$Total_Points)
  
  # qor_anoms_norm_by_total %>%
  #   mutate_at(2:4,~ round(.,2))%>%
  #   kableExtra::kbl() %>%
  #   kableExtra::kable_classic_2(bootstrap_options = c("condensed"),full_width = F) %>%
  #   kableExtra::row_spec(c(18,26), bold = T, background = "red") %>%
  #   kableExtra::column_spec(1:3, bold = F, background = "white")
}

## Bar plots of QOR anomaly type by census tract.
{
  p7 <- census_tract_cluster_statplots(qor_anoms_norm_by_total, "CO2_Cluster",
                               title = expression(paste("QOR ", CO[2], " Anomalies Normalized by Census Tract Total Points")),
                               ylabel = expression(paste("% ", CO[2], " Anomaly")),
                               fill_color = munsell::mnsl('5B 8/8'))
  
  p8 <- census_tract_cluster_statplots(qor_anoms_norm_by_total, "BC_UFP_Cluster",
                               title = expression(paste("QOR BC/UFP Anomalies Normalized by Census Tract Total Points")),
                               ylabel = expression(paste("% BC/UFP Anomaly")),
                               fill_color = munsell::mnsl('5B 8/8'))
  
  ggarrange(p7,p8,nrow=2)
  
  # save_plot(paste0(getwd(),"/Manuscript/Figs/QOR_Anomaly_Normed_By_Total_Bar_Stacked.png"),height = 11, width = 8)
}

## Determining QOR anomaly type distributions by census tract. Tier 1, part of Tier 2, and SBC selected.
##  North River Oaks, South River Oaks, Kashmere, and East Fifth Ward excluded
## Temporal rescaling implemented.
{
  total_polygon_points <- fread(paste0(getwd(),"/total_polygon_points_updated.csv"), select = c("Name","Total_Points"))
  
  load(paste0(getwd(),"/qor_anoms_output.RData"))
  
  load(paste0(getwd(),"/t_output.RData"))  
  
  polys_2_extract <- (!(houston_polygons$ct_num==1 | houston_polygons$ct_num==2 | houston_polygons$Name=="North River Oaks" | houston_polygons$Name=="South River Oaks"))
  
  qor_anomaly_all_roads_list <- qor_anoms_output$Points_In_Polygon[polys_2_extract]
  
  qor_total_all_roads_list <- total_output$Intersecting_List[polys_2_extract]
  
  qor_rescaled_values <- normalize_ct(qor_anomaly_all_roads_list,
                                  qor_total_all_roads_list,
                                  remove_weekends = T,
                                  trim_cts = T)  
  
  qor_rescaled_anomaly_totals <- qor_rescaled_values$Rescaled_Anomaly_Totals 
  qor_rescaled_anomaly_by_anom <- qor_rescaled_values$Rescaled_Anomaly_Normed_By_Anomaly
  qor_rescaled_anomaly_by_total <- qor_rescaled_values$Rescaled_Anomaly_Normed_By_Total
  
  # qor_raw_anoms_output <-  qor_anoms_output$Raw_Anomaly_Output
  # 
  # raw_anoms_over_total <- norm_by_total(raw_anoms_output,total_polygon_points$Total_Points)
  
}

## Performing CT by CT comparison of normalized QOR, DBSCAN anomalies
{
  combined_totals <- rbind(rescaled_anomaly_by_total,qor_rescaled_anomaly_by_total) %>%
    dplyr::mutate(Algorithm=ifelse(as.numeric(rownames(.))<20,"DBSCAN","QOR"))  
  
  bc_effects <- census_tract_cluster_statplots(combined_totals,"BC_UFP_Cluster",
                                       title = expression(paste("DBSCAN v QOR Normalized DBSCAN BC/UFP Anomalies")),
                                       ylabel = expression(paste("% BC/UFP")),
                                       fill_color = c(munsell::mnsl("5G 6/8"),munsell::mnsl("5B 8/8")),
                                       bar_groups = "Algorithm")
  
  save_plot(filename = paste0(getwd(),"/Manuscript/Figs/DBSCAN_And_QOR_BC_UFP_Over_Total.png"),height = 5.5,width = 8)
  
  co2_effects <- census_tract_cluster_statplots(combined_totals,"CO2_Cluster",
                                       title = expression(paste("DBSCAN v QOR Normalized DBSCAN ", CO[2], " Anomalies")),
                                       ylabel = expression(paste("% ", CO[2])),
                                       fill_color = c(munsell::mnsl("5G 6/8"),munsell::mnsl("5B 8/8")),
                                       bar_groups = "Algorithm")
  
  save_plot(filename = paste0(getwd(),"/Manuscript/Figs/DBSCAN_And_QOR_CO2_Over_Total.png"),height = 5.5,width = 8)
  
  transition_effects <- census_tract_cluster_statplots(combined_totals,"Transition_Cluster",
                                       title = expression(paste("DBSCAN v QOR Normalized DBSCAN Transition Anomalies")),
                                       ylabel = expression(paste("% Transition")),
                                       fill_color = c(munsell::mnsl("5G 6/8"),munsell::mnsl("5B 8/8")),
                                       bar_groups = "Algorithm")
  
  save_plot(filename = paste0(getwd(),"/Manuscript/Figs/DBSCAN_And_QOR_Transition_Over_Total.png"),height = 5.5,width = 8)
}

## Creating histograms of sampling times for anomaly types and total measurements. By polygon
{
  # t_points_by_poly <- return_total_number_inpolygon(db_labeled_emissions,houston_polygons)
  
  load(paste0(getwd(),"/db_anoms_output.RData"))
  
  load(paste0(getwd(),"/t_output.RData"))  
  
  ## North Rice
  # nr_anomaly_histogram <- create_census_tract_histogram(db_anoms_output$Points_In_Polygon,"North Rice",
  #                                                       cluster_type = c(1,2,3),
  #                                                       title = NULL)
  # 
  # nr_histogram <- create_census_tract_histogram(t_points_by_poly$Intersecting_List, "North Rice",
  #                                               title = NULL)
  
  # save_plot(paste0(getwd(),"/Manuscript/Figs/Sampling_Distribution_Histogram_North_Rice.png"),width = 8, height = 6)
  
  # save_plot(paste0(getwd(),"/Manuscript/Figs/Frequency_Distribution_North_Rice_Anomalies.png"),width = 8,height=6)  
  
  # ggarrange(nr_anomaly_histogram,nr_histogram,labels=c("(a)","(b)"),ncol = 1)
  
  # save_plot(paste0(getwd(),"/Manuscript/Figs/North_Rice_Histograms.png"),width = 8, height = 8)
  
  ## Milby Park
  milby_anomaly_histogram <- create_census_tract_histogram(db_anoms_output$Points_In_Polygon,"Milby Park",
                                                           cluster_type = c(1,2,3),
                                                           title = NULL)
  
  save_plot(paste0(getwd(),"/Manuscript/Figs/Milby_Park_BC_Anomaly_Histogram.png"),width = 8, height = 5.5)
  
  milby_total_histogram <- create_census_tract_histogram(total_output$Intersecting_List,"Milby Park",
                                                           title = NULL)
  
  ggarrange(milby_anomaly_histogram,milby_total_histogram,labels=c("(a)","(b)"),ncol = 1)
  
  save_plot(paste0(getwd(),"/Manuscript/Figs/Milby_Park_Histograms.png"),width = 8, height = 11)
}

## Checking road classes of grouped anomalies.
{
  print_unique_road_classes <- function(intersecting_list_entry){
    print(intersecting_list_entry[[1]])
    print("-----")
    print(unique(intersecting_list_entry[[2]]$Road_Class))
    print("----")
  }
  
  lapply(total_neighborhood_list,function(x) print_unique_road_classes(x))
}

## Comparing DBSCAN labeling to QOR labeling by coloring points
## in validation time series that QOR labels as anomaly that DBSCAN does not
{
  db_grouped_anomalies <- db_labeled_emissions %>%
    dplyr::group_split(Uniq_Fac,.keep = FALSE)
  
  qor_grouped_anomalies <- qor_labeled_emissions %>%
    dplyr::group_split(Uniq_Fac,.keep = FALSE)
  
  validation_labels <- c(100,103,109,118,123,124,134,150,166,168,171,181,187,194,206,207,209,21,215,218,22,248,249,268,271,3,36,59,88,90)
  
  for(label in validation_labels){
    print(label)
    
    plot_time_series_anomalies(db_grouped_anomalies[[label]], polls_to_pull = c("BC","CO2","NOx","UFP"),
                               title = paste0("DBSCAN_Day_",label),
                               save_graph = T,
                               directory = paste0(getwd(),"/Miscellaneous_Figures/validation_time_series/"))

    plot_time_series_anomalies(qor_grouped_anomalies[[label]], polls_to_pull = c("BC","CO2","NOx","UFP"),
                               title = paste0("QOR_Day_",label),
                               save_graph = T,
                               directory = paste0(getwd(),"/Miscellaneous_Figures/validation_time_series/"))

  }    
}

## Determining weekend effects.
{
  ## Count number of measurements made on weekends vs. weekdays.
  weekend_emissions <- db_labeled_emissions %>%
    dplyr::mutate("Day_of_week"=lubridate::wday(LST)) %>%
    dplyr::filter(Day_of_week==1 | Day_of_week == 7)
  
  weekend_ct <- count_anomalies_in_polygons(weekend_emissions, houston_polygons, by_anomaly_type = F)
  
  weekend_ct <- weekend_ct$Points_In_Polygon
  
  ## Total number of points.
  total_polygon_points <- fread(paste0(getwd(),"/total_polygon_points_updated.csv"), select = c("Name","Total_Points"))
  
  ## Calculate percentage of points measured in census tract on weekends
  weekend_percentages <- tibble("Census_Tract"=sapply(weekend_ct,function(x)x[[1]]), 
                                "Number_of_Measurements(%)"=sapply(weekend_ct, function(x) nrow(x[[2]]))) %>%
    mutate_at(2, ~ round(./total_polygon_points$Total_Points*100,2)) %>%
    kableExtra::kbl() %>%
    kableExtra::kable_classic(full_width = F) %>%
    print()
  
  
}

## Counting unique number of drive days in polygons. Unique drive day defined as
## unique combination of month number and mday number
{
  polygonsForAnalysis <- total_output$Intersecting_List[polys_2_extract]
  
  count_unique_drive_days <- function(df){
    df <- df %>%
      dplyr::mutate("Day"=lubridate::mday(LST)) %>%
      dplyr::mutate("Month"=lubridate::month(LST)) %>%
      group_split(Day,Month,.keep = FALSE)
    
    return(length(df))
  }
  
  unique_dds <- sapply(polygonsForAnalysis,function(x) count_unique_drive_days(x[[2]]))
}

## Determine 5th, 95th percentiles of time stamps for given census tract 
## in an intersecting list.
{
  return_hourly_quantiles <- function(intersecting_list_entry){
    
    intersecting_list_entry[[2]] <- intersecting_list_entry[[2]] %>%
      dplyr::mutate(H=lubridate::hour(LST))
    
    low_quant <- quantile(intersecting_list_entry[[2]]$H,0.05)
    
    high_quant <- quantile(intersecting_list_entry[[2]]$H,0.95)
    
    return(c(low_quant,high_quant))
  }
  
  return_trimmed_measurements <- function(intersecting_list_entry){
    
    quants <- return_hourly_quantiles(intersecting_list_entry)
    
    trimmed_data <- intersecting_list_entry[[2]] %>%
      dplyr::mutate(H = lubridate::hour(LST)) %>%
      dplyr::filter(H<quants[1] | H>quants[2])
    
    return(list("Trimmed_Measurements" = nrow(trimmed_data),
                "Total_Measurements"=nrow(intersecting_list_entry[[2]])))
  }
  polys_2_extract <- (!(houston_polygons$ct_num==1 | houston_polygons$ct_num==2) | houston_polygons$Name=="Memorial Park")
  ## Preparing table for neighborhood + highway
  hourly_quants <- sapply(total_output$Intersecting_List[polys_2_extract],function(x) return_hourly_quantiles(x))
  
  extracted_measurements <- lapply(total_output$Intersecting_List[polys_2_extract], function(x) return_trimmed_measurements(x))
  
  extracted_trimmed <- sapply(extracted_measurements, function(x) return(x$Trimmed_Measurements))
  
  extracted_total <- sapply(extracted_measurements, function(x) return(x$Total_Measurements))
  
  census_sampling_snapshot <- tibble("Census Tract"=sapply(total_neighborhood_list, function(x) return(x[[1]])),
                                     "5th Quantile"=hourly_quants[1,],
                                     "95th Quantile"=hourly_quants[2,],
                                     "Total Trimmed Measurements"=extracted_trimmed,
                                     "Total Measurements" = extracted_total) %>%
    dplyr::mutate("Percentage Extracted (%)" = `Total Trimmed Measurements`/`Total Measurements`*100) %>%
    dplyr::mutate_at(6, ~round(.,2)) %>%
    kableExtra::kbl() %>%
    kableExtra::kable_minimal(full_width = F)
  
  print(census_sampling_snapshot)
  
  ## In neighborhood ONLY
  
  hourly_quants <- sapply(total_neighborhood_list,function(x) return_hourly_quantiles(x))
  
  extracted_measurements <- lapply(total_neighborhood_list, function(x) return_trimmed_measurements(x))
  
  extracted_trimmed <- sapply(extracted_measurements, function(x) return(x$Trimmed_Measurements))
  
  extracted_total <- sapply(extracted_measurements, function(x) return(x$Total_Measurements))
  
  in_neighborhood_census_sampling_snapshot <- tibble("Census Tract"=sapply(total_neighborhood_list, function(x) return(x[[1]])),
                                     "5th Quantile"=hourly_quants[1,],
                                     "95th Quantile"=hourly_quants[2,],
                                     "Total Trimmed Measurements"=extracted_trimmed,
                                     "Total Measurements" = extracted_total) %>%
    dplyr::mutate("Percentage Extracted (%)" = `Total Trimmed Measurements`/`Total Measurements`*100) %>%
    dplyr::mutate_at(6, ~round(.,2)) %>%
    kableExtra::kbl() %>%
    kableExtra::kable_minimal(full_width = F)
  
  print(in_neighborhood_census_sampling_snapshot)
}

## Generate polygon map with polygons colored with probabilities of detection
{
  polygons_to_map <- houston_polygons %>%
    dplyr::filter(Name %in% rescaled_anomaly_by_total$Name) %>%
    dplyr::mutate("CO2_Probability" = rescaled_anomaly_by_total$CO2_Cluster) %>%
    dplyr::mutate("BC_UFP_Probability" = rescaled_anomaly_by_total$BC_UFP_Cluster)
  
  ## CO2 Probs
  CO2_map <- generate_poly_map(polygons_to_map,fill_by = "CO2_Probability",bins = seq(0,6,1),
                    legend_title = glue::glue("CO<sub>{2}</sub> Probabilities (%)"))
  
  mapview::mapshot(CO2_map,file = paste0(getwd(),"/Manuscript/Figs/CO2_Probabilities_Polygon_Map.png"),
                   remove_controls = c("zoomControl"),vwidth = 1050)
  
  BC_map <- generate_poly_map(polygons_to_map,fill_by = "BC_UFP_Probability",bins = 7,
                    legend_title = "BC/UFP Probabilities (%)")
  
  mapview::mapshot(BC_map,file = paste0(getwd(),"/Manuscript/Figs/BC_UFP_Probabilities_Polygon_Map.png"),
                   remove_controls = c("zoomControl"),vwidth = 1050)
}

## Count number of points taken in hour for given polygon and create
## table from it
{
   return_hour_total <- function(intersecting_list_entry,selected_hour){
    
    intersecting_list_entry[[2]] <- intersecting_list_entry[[2]] %>%
      dplyr::mutate(H=lubridate::hour(LST)) %>%
      dplyr::filter(H==selected_hour)
    
    
    
    return(nrow(intersecting_list_entry[[2]]))
  }
  
  
  polys_2_extract <- (!(houston_polygons$ct_num==1 | houston_polygons$ct_num==2))
  ## Preparing table for neighborhood + highway
  eight_am_totals <- sapply(total_output$Intersecting_List[polys_2_extract],function(x) return_hour_total(x,8))
  
  one_pm_totals <- sapply(total_output$Intersecting_List[polys_2_extract],function(x) return_hour_total(x,13))
  
  ct_extremes_snapshot <- tibble("Census Tract"=sapply(total_neighborhood_list, function(x) return(x[[1]])),
                                     "Number of 8 AM Measurements"=eight_am_totals,
                                     "Number of 1 PM Measurements"=one_pm_totals) %>%
    kableExtra::kbl() %>%
    kableExtra::kable_minimal(full_width = F)
  
  print(ct_extremes_snapshot)
  
  ## Calculating minimum number of measurements in each polygon
  for(h in seq(from=8, to = 15, by =1)){
    hour_totals <- sapply(total_output$Intersecting_List[polys_2_extract],function(x) return_hour_total(x,h))
    
    print(h)
    
    print(min(hour_totals))
  }
}

## Calculating bootstrap confidence intervals for normalized probability calculation.

{
  ## Load in the data
  total_polygon_points <- fread(paste0(getwd(),"/total_polygon_points_updated.csv"), select = c("Name","Total_Points"))
  
  load(paste0(getwd(),"/db_anoms_output.RData"))
  
  load(paste0(getwd(),"/t_output.RData"))  
  
  ## Extract target polygons
  polys_2_extract <- (!(houston_polygons$ct_num==1 | houston_polygons$ct_num==2 | houston_polygons$Name=="North River Oaks" | houston_polygons$Name=="South River Oaks"))
  
  anomaly_all_roads_list <- db_anoms_output$Points_In_Polygon[polys_2_extract]
  
  total_all_roads_list <- total_output$Intersecting_List[polys_2_extract]
  
  ## Remove weekend points
  anomaly_all_roads_list <- lapply(anomaly_all_roads_list, function(x) remove_weekend_points(x))
  
  total_all_roads_list <- lapply(total_all_roads_list, function(x) remove_weekend_points(x))
  
  ## Constrict hours of analysis to be between 8 AM, 3 PM inclusive.
  for (j in seq(from=1,to=length(anomaly_all_roads_list),by=1)){
    
    trimmed_output <- trim_hourly_tails(anomaly_all_roads_list[[j]][[2]],total_all_roads_list[[j]][[2]])
    
    anomaly_all_roads_list[[j]][[2]] <- trimmed_output[[1]]
      
    total_all_roads_list[[j]][[2]] <- trimmed_output[[2]]  
  }
  
  ## For each polygon, will need to create 100 synthetic total sampling distributions
  ## From the 100 synthetic total sampling distributions, create the corresponding anomaly distributions
  ## Input that into normalize_ct.
  
  generate_synthetic_distributions <- function(anomaly_intersecting_list_entry,total_intersecting_list_entry,bootstrap_iterations=100){
    
    synthetic_sampling_list <- vector(mode = "list",length = bootstrap_iterations)
  
    synthetic_anomaly_list <- vector(mode = "list", length = bootstrap_iterations)  
    
    sample_df <- total_intersecting_list_entry[[1]][[2]]
    
    anomaly_df <- anomaly_intersecting_list_entry[[1]][[2]]
    
    for(i in 1:bootstrap_iterations){
      synthetic_sampling_list[[i]][[1]] <- total_intersecting_list_entry[[1]][[1]]
      
      synthetic_anomaly_list[[i]][[1]] <- anomaly_intersecting_list_entry[[1]][[1]]
      
      rows_to_sample <- seq(1,nrow(sample_df),1)
      
      sampled_indices <- sample(rows_to_sample,nrow(sample_df),replace = T)
      
      synthetic_total_sampling_distro <- sample_df[sampled_indices,]  
      
      synthetic_anomaly_distro <- dplyr::left_join(anomaly_df,synthetic_total_sampling_distro,by = c("BC","CO2","NOx","UFP","LST","geometry","Road_Class","Day_Of_Week","Intersecting_Poly","Uniq_Fac")) %>%
        drop_na() %>%
        select(-Anomaly.y) %>%
        rename("Anomaly"=Anomaly.x)
      
      synthetic_sampling_list[[i]][[2]] <- synthetic_total_sampling_distro
      
      synthetic_anomaly_list[[i]][[2]] <- synthetic_anomaly_distro
    }
  
    rescaled_output <- normalize_ct(synthetic_anomaly_list,synthetic_sampling_list,
                                    remove_weekends = F,
                                    trim_cts = F)
    
    rescaled_probabilities <- rescaled_output$Rescaled_Anomaly_Normed_By_Total  
    
    return(rescaled_probabilities)
  }
  
  bootstrap_results <- vector(mode = "list",length = length(total_all_roads_list))
  
  for(k in 1:length(total_all_roads_list)){
    bootstrap_results[[k]] <- generate_synthetic_distributions(anomaly_all_roads_list[k],total_all_roads_list[k],bootstrap_iterations = 100)
  }
  
}

## Analyze synthetic distributions
{
  load(file = paste0(getwd(),"/bootstrapped_cts.RData"))
  
  ## Create tibble from bootstrapped results.
  bootstrapped_df <- list_to_tibble(bootstrap_results) %>%
    dplyr::select(-Uniq_Fac)
  
  ## Create barplot.
  # ggbarplot(bootstrapped_df,x="Name",y="CO2_Cluster",add = "mean_ci",error.plot = "linerange")  
  
  ## Alternative way of creating barplot.
  bootstrapped_df_streamlined <- bootstrapped_df %>%
    dplyr::group_by(Name) %>%
    dplyr::summarize("CO2_Mean" = mean(CO2_Cluster),"CO2_Lower" = quantile(CO2_Cluster,0.05),"CO2_Upper"=quantile(CO2_Cluster,0.95),
                     "BC_UFP_Mean" = mean(BC_UFP_Cluster), "BC_UFP_Lower" = quantile(BC_UFP_Cluster,0.05), "BC_UFP_Upper"= quantile(BC_UFP_Cluster,0.95))
  
  
  
}
