coverage = function(exp = expression(!is.na(zone_short)), type = 'tree', group = 'sa2_code_2021', df = agg_df_filtered) {
  return_df <- df %>%
    rowwise() %>%
    dplyr::filter(eval(exp)) %>%
    dplyr::filter(!is.na(!!as.name(group))) %>%
    group_by(!!as.name(group)) %>%
    summarise(across(c(coverage,total_area), sum), n = n()) %>%
    mutate( "{type}_percentage" := (coverage/total_area)*100 )
  
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
    dplyr::select(tree_percentage) %>%
    unlist() %>%
    as.vector()
  
  #find total lost coverage
  lost_tree_coverage = coverage_per_sqm * area
  
  #find road network coverage
  public_summary = agg_df %>%
    as.data.frame() %>%
    filter(lga_name_2022 == lga_name) %>%
    filter(zone_short == 'roads' | feature_preventing_development == TRUE) %>%
    summarise(across(c(coverage, total_area), sum))
  
  #calculate percentages
  # road_percentage = road_summary$coverage / road_summary$total_area
  # new_road_percentage = (road_summary$coverage + lost_tree_coverage) / road_summary$total_area
  # percent_growth_needed <- ((new_road_percentage/road_percentage)-1)*100
  # 
  #print(paste0('Original coverage was ', round(road_percentage * 100, 3), '%. ',
              #'This needs to rise to ', round(new_road_percentage * 100,3), '%'))
  
  #print(paste0('This is a ', round(new_road_percentage - road_percentage, 3), ' percentage point rise, or a ',
              #round(percent_growth_needed,3), ' % change'))
 
  #return( c(lga_name, round(road_percentage * 100, 3), round(new_road_percentage * 100, 3)) )
  
  return( c(lga_name, coverage_per_sqm , round(lost_tree_coverage,1), round(public_summary$coverage - lost_tree_coverage , 1), round((public_summary$coverage), 1)) )
}

manipulateRents <- function(rents, grouping) {
  
  return(rents %>%
    group_by(!!as.name(grouping)) %>%
    select(!c('Not stated', 'Not applicable', 'Total')) %>%
    mutate(across( starts_with("$"), ~ as.numeric(.) )) %>%
    mutate(total = sum(across(where(is.numeric)))) %>%
    mutate(across(starts_with('$'), ~ .x / total )) %>%
    select(!total) %>%
    pivot_longer(cols = starts_with('$')) %>%
    mutate(csum = cumsum(value)) %>%
    filter(csum > 0.5) %>%
    arrange(csum) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    rename(median_band = name) %>%
    rowwise() %>%
    mutate( "{grouping}" := gsub(" \\(Vic\\.\\)", "", !!as.name(grouping))) )

}

loadFiles <- function(...) {
  
  filename = paste0('../city_comparisons/',...,'.Rdata')
  
  city_data <- readRDS(filename) %>%
    st_set_geometry('geometry') %>%
    st_transform(4326)
  
  leaflet(city_data) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons()
  
  city_data = city_data %>%
    st_drop_geometry() %>%
    rowwise() %>%
    mutate(city = as.character(...) )
  
  # %>%
  #   mutate(distance = round(distance, 0))
  
  # city_data = city_data %>%
  #   group_by(distance) %>%
  #   summarise(mean_cov = mean(cov), med_cov = median(cov) ) %>%
  #   
  
  #plot(city_data$distance, city_data$med_cov, type = 'l')
  
  #print(tibble(city_data))
  
  big_df <<- rbind(big_df, city_data)
}

suburbHousePriceData = function() {
  
}

v = function(x) {View(x)}
adf = function(x) {as.data.frame(x)}

wgs = function(x) {return(x %>% st_transform('wgs84'))}
