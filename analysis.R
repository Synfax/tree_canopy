
gz <- results_df

file_list <- list.files('sa3_results/')
agg_results <- data.frame()

for(file in file_list) {
  agg_results <<- rbind(agg_results, readRDS(paste0('sa3_results/',file)))
}


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


##map tree coverage by SA1

gy <- results_df %>%
  st_drop_geometry() %>%
  group_by(sa1) %>%
  summarise(across(c(total_area, tree_coverage_area),sum)) %>%
  mutate(tree_pc = tree_coverage_area/total_area) %>%
  rename("SA1_CODE21" = sa1)

gyz <- gy %>%
  dplyr::left_join(sa1_sf, by = 'SA1_CODE21') %>%
  st_as_sf()

treePal <- colorNumeric(palette = "Greens", domain = gyz$tree_pc)

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(data = gyz,
              color = ~treePal(tree_pc),
              fillOpacity = 0.8) %>%
  addLegend(position = "bottomright",
            pal = treePal,
            values = gyz$tree_pc,
            title = "Tree Coverage (%)")
