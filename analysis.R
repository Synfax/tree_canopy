
file_list <- list.files('sa3_results/')
agg_results <- data.frame()

for(file in file_list) {
  agg_results <<- rbind(agg_results, readRDS(paste0('sa3_results/',file)))
}

#fix oddities 
agg_results = agg_results %>%
  as.data.frame() %>%
  rowwise() %>%
  mutate(tree_coverage_percent = ifelse(total_area < 0.001 && tree_coverage_percent > 1, 1, tree_coverage_percent))


agg_results = agg_results %>%
  group_by(land_type) %>%
  summarise(across(tree_coverage_area, sum),
            across(total_area, sum)) %>%
  mutate(total_land = sum(total_area)) %>%
  rowwise() %>%
  mutate(pc_trees = tree_coverage_area / total_area,
         pc_land = total_area / total_land ) 

pretty_results <- agg_results %>%
  dplyr::select(c('land_type', 'pc_trees', 'pc_land'))


##map total tree coverage by SA1
gy <- agg_results %>%
  st_drop_geometry() %>%
  group_by(sa1) %>%
  summarise(across(c(total_area, tree_coverage_area),sum)) %>%
  mutate(tree_pc = tree_coverage_area/total_area) %>%
  rename("SA1_CODE21" = sa1) %>%
  dplyr::left_join(sa1_sf, by = 'SA1_CODE21') %>%
  st_as_sf()

mapCoverage(gy, gy$tree_pc, 'Greens')

#map private tree coverage by SA1

agg_sf <- agg_results %>%
  st_drop_geometry() %>%
  rename("SA1_CODE21" = sa1) %>%
  dplyr::left_join(sa1_sf, by = 'SA1_CODE21') %>%
  st_as_sf()

mapCoverage(agg_sf, unlist(agg_sf %>%
                         st_drop_geometry() %>%
                         filter(land_type == 'private') %>%
                         select(tree_coverage_percent)), 'viridis', title = 'private tree coverage')
