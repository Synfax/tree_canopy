
file_list <- list.files('sa3_results/')
agg_results <- data.frame()

for(file in file_list) {
  agg_results <<- rbind(agg_results, readRDS(paste0('sa3_results/',file)))
}

agg_sf <- agg_results %>%
  as.data.frame() %>%
  dplyr::left_join(dwelling_data_2, by = c('lat','lon')) %>%
  rowwise() %>%
  mutate(coverage_pc = ifelse(is.nan(coverage / total_area), 0, (coverage / total_area)*100)) %>%
  mutate(coverage_pc = ifelse(total_area < 0.001 && coverage_pc > 1, 1, coverage_pc)) %>%
  select(!(starts_with('prox'))) %>%
  st_set_geometry('geom')

agg_df <- agg_sf %>%
  st_drop_geometry() 

sa1_sa2_map =  with(agg_df, setNames(sa2_code_2021, sa1_code_2021))
sa1_lga_map = with(agg_df, setNames(lga_name_2022, sa1_code_2021))

agg_df <- agg_df %>%
  rowwise() %>%
  mutate(sa2_code_2021 = ifelse(is.na(sa2_code_2021), sa1_sa2_map[as.character(sa1)], sa2_code_2021),
         lga_name_2022 = ifelse(is.na(lga_name_2022), sa1_lga_map[as.character(sa1)], lga_name_2022))

st_write(agg_sf, 'sf_exports/test.gpkg', driver = "GPKG", append = FALSE)

#mapCoverage(agg_sf, unlist(agg_sf %>% st_drop_geometry() %>% select(coverage_pc)), 'Greens', 'Tree coverage per dwelling (%)')

# melbourne <- st_sfc(st_point(c(144.963115,-37.814175)), 
#                     crs = 7844) %>% #Lat longs have a 'web' crs, so start with that  
#   st_sfc()# turn it into an sf dataframe rather than just a point
# 
# sa2_sf = sa2_sf %>%
#   filter(SA2_CODE21 %in% na.omit(unique(dwelling_data$sa2_code_2021)))  %>%
#   mutate(centroid = st_centroid(geometry)) %>%
#   mutate(distance = as.vector(st_distance(melbourne,centroid))) %>%
#   dplyr::arrange(distance) %>%
#   rename( sa2_code_2021 = SA2_CODE21)
# 
# sa2_within_distance = unique(sa2_sf %>%
#                                st_drop_geometry() %>%
#                                filter(distance < 20000) %>%
#                                select(sa2_code_2021)) %>% unlist()

mm_lgas <- c('Brimbank', 'Merri-bek', 'Banyule', 'Darebin', 'Yarra', 'Moonee Valley', 'Manningham', 'Maribyrnong', 'Melbourne', 'Hobsons Bay', 'Port Phillip', 'Boroondara', 'Stonnington', 'Glen Eira', 'Bayside', 'Monash', 'Whitehorse', 'Maroondah', 'Manningham', 'Kingston')

agg_df <- agg_df %>% filter(lga_name_2022 %in% mm_lgas)

# calculate street tree coverage by sa2
street_tree_per_sa2 <- street_tree_coverage() %>%
  as.data.frame() %>%
  left_join(sa2_sf, by = 'SA2_NAME21') %>%
  st_set_geometry('geometry')

mapCoverage(street_tree_per_sa2, unlist(street_tree_per_sa2 %>%
                                          st_drop_geometry() %>%
                                          select(tree_percentage)), 'Greens', 'Street Tree Coverage (%)')

tibble(street_tree_per_sa2)






  
