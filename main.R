library('devtools')
library('pacman')

pacman::p_load(
  chmloader,
  terra,
  sf,
  stars,
  maptiles,
  classInt,
  tidyverse,
  tidyterra,
  leaflet,
  htmlwidgets,
  raster,
  lwgeom,
  parallel,
  future,
  furrr
)

source('run_for_sa1.R')
source('mapping_functions.R')
source('run.R')


dwelling_data <-read_sf('Melbourne dwelling data.gpkg', query = "SELECT geom,lat,lon,zone_short,sa1_code_2021,
                            dwellings_est,sa2_code_2021,sa4_code_2021,cbd_dist,
                              lga_name_2022,feature_preventing_development,zoning_permits_housing,zone_short,prox_walk_time_s_tram,
                            prox_walk_time_s_train,prox_dist_m_tram,prox_dist_m_train,traffic_pollution,lot_size,zone_short,sa3_code_2021,heritage_status,heritage,vacant_in_2016 FROM 'Melbourne dwelling data'") %>% 
  mutate(lga_name_2022 = str_remove_all(lga_name_2022, "\\s*\\(.*?\\)\\s*")) %>% 
  st_set_geometry('geom')


sa1_sf <- read_sf('SA1_2021/SA1_2021_AUST_GDA2020.shp')
sa3_sf <- read_sf('SA3_2021/SA3_2021_AUST_GDA2020.shp')

#not needed because it only had the big roads 
# state_level_zoning <- read_sf('Order_A723U7/ll_gda2020/esrishape/whole_of_dataset/victoria/VMPLAN/PLAN_ZONE.shp')
# state_level_zoning = state_level_zoning %>% filter(ZONE_DESC == 'TRANSPORT ZONE 2 - PRINCIPAL ROAD NETWORK')

road_network <- read_sf('Order_SHQC22/ll_gda2020/esrishape/user_polygon/user_polygon-0/VMTRANS/TR_ROAD_ALL.shp') %>%
  st_transform(., 7844) %>%
  dplyr::select(c('ROAD_TYPE', 'geometry'))

#settings for canopy
threshold <- 0
union <- TRUE
res <- 5

sf_use_s2(FALSE)

results_df <- data.frame()
robust_df <- data.frame()

run()



