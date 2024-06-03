
file_list <- list.files('sa3_results/')
agg_results <- data.frame()

for(file in file_list) {
  agg_results <<- rbind(agg_results, readRDS(paste0('sa3_results/',file)))
}

agg_sf <- agg_results %>%
  as.data.frame() %>%
  dplyr::left_join(dwelling_data, by = c('lat','lon')) %>%
  rowwise() %>%
  mutate(coverage_pc = ifelse(is.nan(coverage / total_area), 0, (coverage / total_area)*100)) %>%
  mutate(coverage_pc = ifelse(total_area < 0.001 && coverage_pc > 1, 1, coverage_pc)) %>%
  select(!(starts_with('prox'))) %>%
  st_set_geometry('geom')

st_write(agg_sf, 'sf_exports/test.gpkg', driver = "GPKG", append = FALSE)

mapCoverage(agg_sf, unlist(agg_sf %>% st_drop_geometry() %>% select(coverage_pc)), 'Greens', 'Tree coverage per dwelling (%)')
