coverage = function(exp = expression(!is.na(zone_short)), type = 'tree', group = 'sa2_code_2021') {
  return_df <- agg_df %>%
    rowwise() %>%
#   mutate(zone_short = ifelse(is.na(zone_short), land_type, zone_short)) %>%
    dplyr::filter(eval(exp)) %>%
    group_by(!!as.name(group)) %>%
    summarise(across(c(coverage,total_area), sum)) %>%
    mutate( "{type}_percentage" := (coverage/total_area)*100 )
  
  # if(group == 'sa2_code_2021') {
  #   return_df = return_df %>%
  #     left_join(sa2_sf %>%
  #                 st_drop_geometry() %>%
  #                 select(sa2_code_2021, distance, SA2_NAME21), by = 'sa2_code_2021') %>%
  #     select(SA2_NAME21, !!paste0(type,'_percentage'), distance) 
  #     
  # }
  
  return(return_df %>% arrange(!!paste0(type,'_percentage')) )
}

lga_targets <- function(lga_name, home_target) {
  
  area = home_target / (6 / 4*240 / 10000)
  
  #now need to calculate tree coverage per square metre on average in this LGA, restricted to the places we could actually build houses
  coverage_per_sqm = agg_df %>%
    as.data.frame() %>%
    filter(lga_name_2022 == lga_name) %>%
    filter(zoning_permits_housing == 'Housing permitted', feature_preventing_development == F, dwellings_est < 2) %>%
    summarise(across(c(coverage, total_area), sum)) %>%
    mutate(tree_percentage = (coverage/total_area) ) %>%
    select(tree_percentage) %>%
    unlist() %>%
    as.vector()
  
  #find total lost coverage
  lost_tree_coverage = coverage_per_sqm * area
  
  #find road network coverage
  road_summary = agg_df %>%
    as.data.frame() %>%
    filter(lga_name_2022 == lga_name) %>%
    filter(zone_short == 'roads') %>%
    summarise(across(c(coverage, total_area), sum))
  
  #calculate percentages
  road_percentage = road_summary$coverage / road_summary$total_area
  new_road_percentage = (road_summary$coverage + lost_tree_coverage) / road_summary$total_area
  percent_growth_needed <- ((new_road_percentage/road_percentage)-1)*100
  
  #print(paste0('Original coverage was ', round(road_percentage * 100, 3), '%. ',
              #'This needs to rise to ', round(new_road_percentage * 100,3), '%'))
  
  #print(paste0('This is a ', round(new_road_percentage - road_percentage, 3), ' percentage point rise, or a ',
              #round(percent_growth_needed,3), ' % change'))
 
  return(c(lga_name, round(road_percentage * 100, 3), round(new_road_percentage * 100, 3)))
}