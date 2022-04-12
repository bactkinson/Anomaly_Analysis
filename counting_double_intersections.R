ll_points <- db_anomalous_emissions %>% dplyr::select(LST,geometry) %>% dplyr::mutate(Intersections=rep(0,nrow(.)))

total_points <- tibble("Name"=houston_polygons$Name, "Total_Points"=rep(0,nrow(houston_polygons)))
  
poly_names <- houston_polygons$Name

current_emissions <- ll_points

total_intersecting_list <- vector(mode = "list", length = nrow(houston_polygons))

for(k in 1:length(poly_names)){
  current_poly_name <- poly_names[k]
  
  print(current_poly_name)
  
  current_poly <- houston_polygons[houston_polygons$Name==current_poly_name, ]
  
  intersections <- which(
    unlist(
      lapply(st_within(current_emissions, current_poly),function(x) length(x)),
      use.names = F) !=0)
  
  ll_points$Intersections[intersections] <- ll_points$Intersections[intersections]+1
}

print("No Withins")

print(length(which(ll_points$Intersections==0))/length(ll_points$Intersections)*100)

print("-------------------")

print("1 Within")

print(length(which(ll_points$Intersections==1))/length(ll_points$Intersections)*100)

print("-------------------")
print("2 Within")

print(length(which(ll_points$Intersections==2))/length(ll_points$Intersections)*100)

print("-------------------")
print("3 Within")

print(length(which(ll_points$Intersections==3))/length(ll_points$Intersections)*100)

print("-------------------")
