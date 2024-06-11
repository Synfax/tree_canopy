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
  furrr,
  quarto,
  rmarkdown,
  knitr,
  reactablefmtr
)

#source files
source('r/run_for_sa1.R')
source('r_alt/run_for_sa1_alt.R')
source('r/mapping_functions.R')
source('r/run.R')
source('r_alt/alt_analysis.R')
source('markdown/webpage.qmd')


#load dwelling Data
dwelling_data <-read_sf('Melbourne dwelling data.gpkg', query = "SELECT geom,lat,lon,zone_short,sa1_code_2021,
                            dwellings_est,sa2_code_2021,sa4_code_2021,cbd_dist,feature_type,
                              lga_name_2022,feature_preventing_development,sa2_name_2021,zoning_permits_housing,zone_short,address,lot_size,zone_short,sa3_code_2021,heritage_status,heritage FROM 'Melbourne dwelling data'") %>% 
  mutate(lga_name_2022 = str_remove_all(lga_name_2022, "\\s*\\(.*?\\)\\s*")) %>% 
  st_set_geometry('geom')

#load shapefiles for SA1 and SA3 regions - download from ABS or my Google Drive
sa1_sf <- read_sf('SA1_2021/SA1_2021_AUST_GDA2020.shp')
sa2_sf <- read_sf('SA2_2021/SA2_2021_AUST_GDA2020.shp')
sa3_sf <- read_sf('SA3_2021/SA3_2021_AUST_GDA2020.shp')

#not needed because it only had the big roads 
# state_level_zoning <- read_sf('Order_A723U7/ll_gda2020/esrishape/whole_of_dataset/victoria/VMPLAN/PLAN_ZONE.shp')
# state_level_zoning = state_level_zoning %>% filter(ZONE_DESC == 'TRANSPORT ZONE 2 - PRINCIPAL ROAD NETWORK')

#load the road network LINESTRINGS - download from Google Drive
road_network <- read_sf('Order_SHQC22/ll_gda2020/esrishape/user_polygon/user_polygon-0/VMTRANS/TR_ROAD_ALL.shp') %>%
  st_transform(., 7844) %>%
  dplyr::select(c('ROAD_TYPE', 'geometry')) %>%
  filter(!is.na(ROAD_TYPE)) %>%
  st_union()

#settings for canopy model
threshold <- 0
union <- TRUE
res <- 5

#turn off spherical geo
sf_use_s2(FALSE)

#initialise global dataframes ()
results_df <- data.frame()
robust_df <- data.frame()

#run the model
#run()

#if the model has already been run, then you can just run the analysis part.




