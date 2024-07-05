directory = 'data/Order_VW6MMD/ll_gda2020/esrishape/whole_of_dataset/victoria/PLANNING/HEAT_URBAN_HEAT_2018.shp'

uhi = read_sf(directory)

uhi_df = uhi %>%
  st_drop_geometry() %>%
  rename(MB_CODE21 = 'MB_CODE16')

mesh_blocks = read_sf('mesh_blocks/MB_2021_AUST_GDA2020.shp')

sa1s_downloaded = list.files('rasters/5res_sa1') %>%
  gsub(".tif", "", .)

mb = mesh_blocks %>%
  filter(SA1_CODE21 %in% sa1s_downloaded) %>%
  select(c(MB_CODE21, SA3_CODE21,  AREASQKM21)) %>%
  left_join(uhi_df, by = 'MB_CODE21') %>%
  st_drop_geometry() %>%
  rowwise() %>%
  mutate(area_anytree = ((PERSHRBTRE)/100) *  AREASQKM21)

mb = mb %>%
  group_by(SA1_MAIN16) %>%
  summarise( total_uhi_cov = sum(area_anytree) / sum(AREASQKM21) ) %>%
  rename(sa1 = 'SA1_MAIN16')

sa1_summary <- agg_df %>%
  group_by(sa1) %>%
  summarise( ttl_cov = sum(coverage) / sum(total_area)  )

tt <- mb %>%
  left_join(sa1_summary, by = 'sa1')

tt = tt %>% left_join(sa1_sf %>%
  select(SA1_CODE21, CHG_FLAG21) %>%
  st_drop_geometry() %>%
    rename(sa1 = 'SA1_CODE21'), by = 'sa1')

unch = tt %>%
  filter(CHG_FLAG21 == 0) %>%
  mutate(diff = ttl_cov - total_uhi_cov) %>%
  select(diff) 

hist(unlist(unch), xlab = "Difference (Paul - Vic)", main = "Difference between Paul's estimates and Victoria's for each SA1")

