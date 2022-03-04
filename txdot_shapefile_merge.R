require(sf)
require(tidyverse)
require(tmap)

current_dir <- getwd()
txdot_inventory <- read.delim(paste0(getwd(),"/txdot_roadways/TxDOT_Roadway_Inventory.txt"),header=TRUE,
                              sep = "|") %>%
  select(GID,CO,FRM_DFO, TO_DFO, DTRKVMT, TRK_AADT_PCT, PCT_SADT, PCT_CADT, AADT_TRUCKS, AADT_SINGLE_UNIT, AADT_COMBINATION) %>%
  filter(CO==102) %>%
  group_by(GID) %>%
  mutate(Attribute_Weights = TO_DFO-FRM_DFO) %>%
  summarise(across(c(DTRKVMT, TRK_AADT_PCT, PCT_SADT, PCT_CADT, AADT_TRUCKS, AADT_SINGLE_UNIT, AADT_COMBINATION), ~ weighted.mean(.x, Attribute_Weights)))

# tt <- read.delim(paste0(getwd(),"/txdot_roadways/TxDOT_Roadway_Inventory.txt"),header=TRUE,
#                               sep = "|")%>%
#   filter(CO==102) %>%
#   select(GID, FRM_DFO, TO_DFO, DTRKVMT, PCT_SADT) %>%
#   filter(GID==1599249310)

txdot_linework <- st_read(
  paste0(getwd(),"/txdot_roadways/TxDOT_Roadway_Linework.shp")
) %>%
  select(GID, geometry)

# which(duplicated(txdot_linework$geometry))
# 
# which(duplicated(txdot_inventory$RTE_GRID))

## Join the inventory with matching GID records.
txdot_joint_inventory <- dplyr::inner_join(txdot_linework, txdot_inventory,
                                           by = "GID")

## Save the txdot joint inventory to file.
## For now, save this to R object. May need to revisit later
## Coudl just save the txdot linework to its own shapefile.
save(txdot_joint_inventory, file = paste0(getwd(),"/txdot_roadways/txdot_joint_inventory.RData"))


## Determine whether Harris County is county number 102
## Filter txdot to be just GID and CO; filter txdot_linework to be just
## GID and geometry

# txdot_linework_filtered <- txdot_linework %>% dplyr::select(GID,geometry)
# 
# txdot_inventory_filtered <- txdot_inventory %>% 
#   dplyr::select(GID,CO) %>%
#   dplyr::filter(CO==102)
# 
# output <- tt %>% filter(GID==1599249310) %>%
#   kableExtra::kbl() %>%
#   kableExtra::kable_classic()
# 
# output

## Do a join in which only records present in each dataframe are returned

# txdot_joined <- dplyr::inner_join(txdot_linework_filtered, txdot_inventory_filtered,
#                                   by = "GID")

mapview::mapview(txdot_joint_inventory)




