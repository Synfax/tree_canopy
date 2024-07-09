run_for_sa1_alt_parallel <- function(sa , return_type = 'df') {
  
  #if( !(sa %in% results_df$sa1) ) {
    
    print(sa)
    
    current_sa_sf <- sa1_sf %>%
      filter(SA1_CODE21 == sa)
    
    dwellings_in_sa <- dwelling_data_current %>%
      st_intersection(current_sa_sf)
    
    clusterExport(cl, "current_sa_sf")
    clusterExport(cl, "road_network_in_sa3")
    clusterExport(cl, "dwellings_in_sa")
    
    bounding = st_as_sfc(st_bbox(current_sa_sf)) %>%
      st_transform(crs = 4326)
    
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
    
    #cut it out
    #chm_simplified <- st_intersection(chm_simplified, current_sa_sf)
    
    chm_is_empty = ifelse(nrow(chm_simplified) > 0, F, T) 
    
    if(!chm_is_empty) {
      
      chm_simplified <- st_parallel(chm_simplified, st_intersection, 4, y = current_sa_sf)
      #union into one
      if(union && nrow(chm_simplified) > 0){
        chm_simplified = st_parallel(chm_simplified, st_union, 4)
      }
    }
    
    
    sf_use_s2(FALSE)
    
    print('simplified')
    
    #find road network
    road_network_in_sa = st_parallel(road_network_in_sa3, st_intersection, 4 , y= st_buffer(current_sa_sf, 0.00035))
    #road_network_in_sa = road_network_in_sa3[lengths(road_network_in_sa) > 0 ,] 
    
    #make it wider
    buffered_roads = st_buffer(road_network_in_sa, 0.00035)
    #buffered_roads = st_parallel(buffered_roads, st_union)
    
    #guess where roads are (between lots)
    #there was an st_union before dwellings here - changing it with removing s2
    guess_of_roads <- st_difference(current_sa_sf %>% st_union(), st_union(dwellings_in_sa)) 
    
    #get the overlap between the guess and the buffer
    roads <- st_intersection(guess_of_roads, buffered_roads) %>%
      st_union()
    
    roads <- st_collection_extract(roads, "POLYGON") %>%
      st_union() %>%
      st_as_sf() %>%
      mutate(land_type = 'roads') %>%
      rename(geom = 'x')
    
    other_land = st_geometry(current_sa_sf) %>%
      st_difference(st_geometry(roads)) %>%
      st_difference(st_geometry(st_union(dwellings_in_sa)))
    
    if(class(other_land)[1] == "sfc_GEOMETRYCOLLECTION") {
      other_land = st_collection_extract(other_land, "POLYGON")
    }
    
    other_land = other_land %>%
      st_union() %>%
      st_as_sf() %>%
      mutate(land_type = 'other') %>%
      rename(geom = 'x')
    
    #now need to trim guess_of_roads to only within x metres of an actual road line.
    
    pal <- colorFactor("viridis", dwellings_in_sa$feature_preventing_development) 
    
    map <- leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      addPolygons(data = dwellings_in_sa, color = ~pal(feature_preventing_development)) %>% 
      addPolygons(data = roads, color = 'darkgray', fillColor = 'grey', fillOpacity = 1) %>% 
      addPolygons(data = chm_simplified, color = 'green', fillColor = 'darkgreen', fillOpacity = 1) %>%
      addPolygons(data = current_sa_sf, color = 'black', fillOpacity = 0)
    
    print(map)
    
    
    test_df <- dwellings_in_sa %>%
      bind_rows(roads) %>%
      bind_rows(other_land) %>%
      rowwise() %>%
      mutate(total_area = st_area(geom) %>%
               as.numeric() %>%
               ifelse(purrr::is_empty(.), 0, .),
             coverage = st_intersection(geom, chm_simplified) %>%
               st_union() %>%
               st_area() %>%
               as.numeric() %>%
               ifelse(purrr::is_empty(.), 0, .),
             sa1 = sa) %>%
      select(c(lat, lon, coverage, total_area, sa1, land_type))
    z
    if(return_type == 'df') {
      results_df <<- rbind(results_df, test_df)
    }
    if(return_type == 'ret') {
      
      return( 'hello' )
    }
  #}
  # else {
  #   print('already in df')
  # }
  # 
}
