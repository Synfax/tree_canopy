run_for_sa1 <- function(sa) {
  #pbcounter = pbcounter + 1
  #setTxtProgressBar(pb, pbcounter)
  
  print(sa)
  
  current_sa_sf <- sa1_sf %>% filter(SA1_CODE21 == sa)
  
  dwellings_in_sa <- dwelling_data_current %>% filter(sa1_code_2021 == sa)
  
  dwellings_in_sa = st_intersection(dwellings_in_sa, current_sa_sf)
  
  bounding = st_as_sfc(st_bbox(current_sa_sf)) %>% st_transform(crs = 4326)
  
  #st_as_sf(bounding) %>% leaflet() %>% addProviderTiles('CartoDB.Positron') %>% addPolygons()
  
  file_name = paste0('rasters/',res,'res_sa1/',sa,'.tif')
  
  if(file.exists(file_name)) {
    print('running from file')
    
    chm <- raster(file_name)
    var = paste0('X',sa)
    
  } else {
    print('downloading from api')
    chm <- chmloader::download_chm(bounding, filename = file_name, res = res)
    var = paste0(sa,'.tif')
  }
  print('downloaded')
  
  chm_stars = st_as_stars(chm, nx = chm@ncols, ny = chm@nrow, n = (chm@ncols * chm@nrows), downsample = 0)
  
  chm_simplified = st_as_sf(chm_stars, merge = TRUE) 
  
  #TODO: 'cut out' actual SA1 from its extent (bounding box) currently if two bbox's overlap (they always will)
  # data will get lost or double counted
  
  #TODO: tweak this value
  chm_simplified = chm_simplified %>% filter( !!as.name(var) > threshold) 
  chm_simplified <- st_transform(st_as_sf(chm_simplified), 7844)
  
  
  clusterExport(cl, "current_sa_sf")
  
  #cut it out
  #chm_simplified <- st_intersection(chm_simplified, current_sa_sf)
  
  chm_is_empty = ifelse(nrow(chm_simplified) > 0, F, T) 
  
  if(!chm_is_empty) {
    
    if(nrow(chm_simplified) > 4) {
      
      chm_simplified <- st_parallel(chm_simplified, st_intersection, 4, y = current_sa_sf)
      #union into one
      if(union ){
        if(nrow(chm_simplified) > 4) {
          chm_simplified = st_parallel(chm_simplified, st_union, 4)
        }
        else {
          chm_simplified = chm_simplified %>% st_union()
        }
      }
    } else {
      chm_simplified <- st_intersection(chm_simplified, current_sa_sf)
      
      if(union && nrow(chm_simplified) > 0){
        #union into one
        chm_simplified = chm_simplified %>% st_union()
        #chm_simplified = st_parallel(chm_simplified, st_union, 4)
      }
    }
  }
  

  sf_use_s2(FALSE)
  
  print('simplified')
  
  #find road network
  road_network_in_sa = st_intersects( (road_network), st_buffer(current_sa_sf, 0.00035))
  road_network_in_sa = road_network[lengths(road_network_in_sa) > 0 ,]
  road_network_in_sa = road_network_in_sa %>% filter(!is.na(ROAD_TYPE))
  #make it wider
  buffered_roads = st_buffer(road_network_in_sa, 0.00035)
  buffered_roads = st_union(buffered_roads)
  
  #guess where roads are (between lots)
  #there was an st_union before dwellings here - changing it with removing s2
  guess_of_roads <- st_difference(st_union(current_sa_sf), st_union(dwellings_in_sa)) 
  
  #get the overlap between the guess and the buffer
  roads <- st_intersection(guess_of_roads, buffered_roads) %>%
    st_union()
  
  roads <- st_collection_extract(roads, "POLYGON") %>%
    st_union() %>%
    st_as_sf() %>%
    mutate(land_type = 'roads')
  
  #now need to trim guess_of_roads to only within x metres of an actual road line.
  
  pal <- colorFactor("viridis", dwellings_in_sa$feature_preventing_development) 
  
  map <- leaflet() %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons(data = dwellings_in_sa, color = ~pal(feature_preventing_development)) %>% 
    addPolygons(data = roads, color = 'darkgray', fillColor = 'grey', fillOpacity = 1) %>% 
    addPolygons(data = chm_simplified, color = 'green', fillColor = 'darkgreen', fillOpacity = 1) %>%
    addPolygons(data = current_sa_sf, color = 'black', fillOpacity = 0)
  
  print(map)
  
  private_land <- dwellings_in_sa %>%
    filter(!feature_preventing_development) %>%
    st_union() %>%
    st_as_sf() %>%
    mutate(land_type = 'private')
  
  public_land <- dwellings_in_sa %>%
    filter(feature_preventing_development) %>%
    st_union() %>%
    st_as_sf() %>%
    mutate(land_type = 'public')
  
  other_land = st_geometry(current_sa_sf) %>%
    st_difference(st_geometry(roads)) %>%
    st_difference(st_geometry(st_union(dwellings_in_sa)))
  
  if(class(other_land)[1] == "sfc_GEOMETRYCOLLECTION") {
    other_land = st_collection_extract(other_land, "POLYGON")
  }
  
  other_land = other_land %>%
    st_union() %>%
    st_as_sf() %>%
    mutate(land_type = 'other')
  
  { 
    # leaflet() %>%
    #   addProviderTiles('CartoDB.Positron') %>%
    #   addPolygons(data = current_sa_sf, color = 'darkgray', fillColor = 'blue', fillOpacity = 0.5) %>%
    #   addPolygons(data = roads, color = 'darkgray', fillColor = 'grey', fillOpacity = 0.7) %>%
    #   addPolygons(data = private_land, color = 'darkgray', fillColor = 'red', fillOpacity = 0.7) %>%
    #   addPolygons(data = public_land, color = 'darkgray', fillColor = 'green', fillOpacity = 0.7)
    }
  
  land_types <- other_land %>%
    bind_rows(public_land) %>%
    bind_rows(private_land) %>%
    bind_rows(roads) 
  
  land_types <- land_types %>%
    rowwise() %>%
    mutate(total_area = st_area(x) %>%
             as.numeric() %>%
             ifelse(purrr::is_empty(.), 0, .),
           
           tree_coverage_area = st_intersection(x, chm_simplified) %>%
             st_union() %>%
             st_area() %>%
             as.numeric() %>%
             ifelse(purrr::is_empty(.), 0, .),
           
           tree_coverage_percent = ifelse(is.nan(tree_coverage_area / total_area),
                                          0,
                                          tree_coverage_area / total_area),
           sa1 = sa
    )
  
  if( !(sa %in% results_df$sa1) ) {
    results_df <<- rbind(results_df, land_types)
  } 
  else {
    print('already in df')
  }
  
  #robustness
  lga_area = st_area(current_sa_sf)
  total_area = sum(land_types$total_area)
  
  print(total_area/lga_area)
  print(land_types %>% st_drop_geometry())
  
  robust_df <<- rbind(robust_df, c(sa,(total_area/lga_area)))

}
