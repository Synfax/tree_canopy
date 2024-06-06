street_tree_coverage = function() {
  street_tree_df <- agg_df %>%
    rowwise() %>%
    mutate(zone_short = ifelse(is.na(zone_short), land_type, zone_short)) %>%
    filter(zone_short == 'roads') %>%
    group_by(sa2_code_2021) %>%
    summarise(across(c(coverage,total_area), sum)) %>%
    mutate(tree_percentage = (coverage/total_area)*100 ) %>%
    left_join(sa2_sf %>%
                st_drop_geometry() %>%
                select(sa2_code_2021, distance, SA2_NAME21), by = 'sa2_code_2021') %>%
    select(SA2_NAME21, tree_percentage, distance) %>%
    arrange(-tree_percentage) 
  
  return(street_tree_df)
}
