directory = 'data/Order_VW6MMD/ll_gda2020/esrishape/whole_of_dataset/victoria/PLANNING/HEAT_URBAN_HEAT_2018.shp'
uhi = read_sf(directory)

uhi_df = uhi %>%
  st_drop_geometry() %>%
  rename(MB_CODE21 = 'MB_CODE16')

mesh_blocks = read_sf('mesh_blocks/MB_2021_AUST_GDA2020.shp')

sa1s_downloaded = list.files('rasters/1res_sa1') %>%
  gsub(".tif", "", .)

mb = mesh_blocks %>%
  filter(SA1_CODE21 %in% sa1s_downloaded) %>%
  dplyr::select(c(MB_CODE21, SA2_CODE21,  AREASQKM21)) %>%
  left_join(uhi_df, by = 'MB_CODE21') %>%
  st_drop_geometry() %>%
  rowwise() %>%
  mutate(area_anytree = ((PERSHRBTRE)/100) *  AREASQKM21) %>%
  group_by(SA1_MAIN16) %>%
  summarise( total_uhi_cov = sum(area_anytree) / sum(AREASQKM21), uhi = first(UHI18_M), sa2 = first(SA2_CODE21) ) %>%
  rename(sa1 = 'SA1_MAIN16')

sa1_summary <- agg_df %>%
  group_by(sa1) %>%
  summarise( ttl_cov = sum(coverage) / sum(total_area), ttl_cov_a = sum(coverage), ttl_area = sum(total_area)  )

tt <- mb %>%
  left_join(sa1_summary, by = 'sa1') %>% 
  left_join(sa1_sf %>%
  dplyr::select(sa1_code_2021, CHG_FLAG21, SA2_CODE21, AREASQKM21) %>%
  st_drop_geometry() %>%
  rename(sa1 = 'sa1_code_2021'), by = 'sa1') %>%
  mutate(ttl_uhi_cov_a = total_uhi_cov * ttl_area ) %>%
  filter(!is.na(ttl_cov ))

#agg to SA2 and then check diffs? sa1s gonna be noisy regardless

s2_agg <- tt %>% group_by(SA2_CODE21) %>%
  summarise( sa2_cov = sum(ttl_cov_a) / sum(ttl_area), sa2_uhi_cov = sum(ttl_uhi_cov_a) / sum(ttl_area) ) %>%
  mutate(diff = sa2_cov - sa2_uhi_cov)

ggplot(tt %>% filter(CHG_FLAG21 == 0), mapping = aes(x = ttl_cov, y = uhi ) ) + 
  geom_point() +
  xlab('Tree Coverage (%)') +
  ylab('Urban Heat Island (deg)') +
  labs(title = "Urban heat island against coverage per SA1") +
  theme_minimal()

# unch = tt %>%
#   filter(CHG_FLAG21 == 0) %>%
#   mutate(diff = ttl_cov - total_uhi_cov) %>%
#   dplyr::select(diff) 
# 
# hist(unlist(unch), xlab = "Difference (Paul - Vic)", main = "Difference between Paul's estimates and Victoria's for each SA1")


