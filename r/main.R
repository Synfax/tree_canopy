library('devtools')
library('pacman')
devtools::install_github("dmurdoch/leaflet@crosstalk4")
library(leaflet)

pacman::p_load(
  chmloader,
  terra,
  ggtext,
  sf,
  stars,
  maptiles,
  classInt,
  tidyverse,
  tidyterra,
  htmlwidgets,
  raster,
  lwgeom,
  parallel,
  future,
  furrr,
  quarto,
  rmarkdown,
  knitr,
  reactablefmtr,
  crosstalk,
  htmltools,
  DT,
  update = TRUE
)

#source files
source('r/run_for_sa1_alt.R')
source('r/mapping_functions.R')
source('r/run.R')
#source('r_alt/alt_analysis.R')
#source('markdown/webpage.qmd')


#load dwelling Data
dwelling_data <-read_sf('Melbourne dwelling data.gpkg') %>% 
  dplyr::select(!starts_with('prox_')) %>%
  dplyr::select(!starts_with('dev_')) %>%
  mutate(lga_name_2022 = str_remove_all(lga_name_2022, "\\s*\\(.*?\\)\\s*")) %>% 
  st_set_geometry('geom')

#load shapefiles for SA1 and SA3 regions - download from ABS or my Google Drive
sa1_sf <- read_sf('SA1_2021/SA1_2021_AUST_GDA2020.shp')
sa2_sf <- read_sf('SA2_2021/SA2_2021_AUST_GDA2020.shp')
sa3_sf <- read_sf('SA3_2021/SA3_2021_AUST_GDA2020.shp')
sal_sf <- read_sf('SAl_2021/SAL_2021_AUST_GDA2020.shp') %>%
  mutate(SAL_NAME21 = gsub(" \\(Vic\\.\\)", "", SAL_NAME21))

lga_sf <- read_sf('LGA_2022/LGA_2022_AUST_GDA2020.shp') %>%
  filter(STE_NAME21 == 'Victoria') %>%
  mutate(LGA_NAME22 = gsub(" \\(Vic\\.\\)", "", LGA_NAME22))

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
res <- 1

#turn off spherical geo
sf_use_s2(FALSE)

#initialise global dataframes ()
results_df <- data.frame()
robust_df <- data.frame()

#run the model
run(parallel = F)

#if the model has already been run, then you can just run the analysis part.


saveRDS(sa1_sf, 'data/sa1_sf.Rdata')
saveRDS(road_network, 'data/road_network.Rdata')
saveRDS(dwelling_data, 'data/dwelling_data.Rdata')
