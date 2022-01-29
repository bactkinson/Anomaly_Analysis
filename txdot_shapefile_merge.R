require(sf)
current_dir <- getwd()
txdot_inventory <- read.delim(paste0(getwd(),"/txdot_roadways/TxDOT_Roadway_Inventory.txt"),header=TRUE,
                              sep = "|")

txdot_linework <- st_read(
  paste0(getwd(),"/txdot_roadways/TxDOT_Roadway_Linework.shp")
)


ggplot() +
  geom_sf(data = st_zm(txdot_linework)) +
  coord_sf()


## Determine whether Harris County is county number 102
## Filter txdot to be just GID and CO; filter txdot_linework to be just
## GID and geometry

txdot_linework_filtered <- txdot_linework %>% dplyr::select(GID,geometry)

txdot_inventory_filtered <- txdot_inventory %>% 
  dplyr::select(GID,CO) %>%
  dplyr::filter(CO==102)

## Do a join in which only records present in each dataframe are returned

txdot_joined <- dplyr::inner_join(txdot_linework_filtered, txdot_inventory_filtered,
                                  by = "GID")

ggplot() +
  geom_sf(data = st_zm(txdot_joined)) +
  coord_sf()

