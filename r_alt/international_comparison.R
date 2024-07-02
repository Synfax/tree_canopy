
coordinates <- list( c(2.3514, 48.8575) , c(16.3713,48.2081 ) , c(0.1276,51.5072), c(74.0060, 40.7128) )

crss <- c(27561, 31287, 29901, 32118  )

cities <- c('paris', 'vienna', 'london', 'newyork') 

radius <- 10000

union <- TRUE

cell_size <- 1000

res <- 5
threshold <- 0

city_df = tibble(coords = coordinates, coordinateref = crss, city = cities) %>%
  as.data.frame() %>%
  rowwise() %>%
  mutate(lng = coords[1], lat = coords[2])

city_df$coordinateref = as.numeric(city_df$coordinateref)

city_df = city_df[-c(1:2),]

city_df = city_df %>%
  rowwise() %>%
  mutate(res = runCity(lng, lat, coordinateref, city) )

runCity <- function(longit, latit, local_crs, cur_city) {
  
  local_coords = c(as.numeric(longit),as.numeric(latit))

  print(paste(cur_city, local_crs))
  print(local_coords)
  
  centre = st_point(local_coords) %>%
    st_sfc(crs = 4326) 
  
  bounding = centre %>%
    st_transform(crs = local_crs) %>%
    st_buffer(dist = radius) %>%
    st_bbox() %>%
    st_as_sfc(crs = 4326)
  
  bounding %>%
    st_transform("wgs84") %>%
    leaflet() %>%
    addProviderTiles('CartoDB.Positron') %>%
    addMeasure(primaryLengthUnit = 'meters') %>%
    addPolygons()
  
  grid <- st_make_grid(bounding, cellsize = c(cell_size,cell_size), square = TRUE)
  n_cells <- grid %>% length()
  
  
  grid %>%
    st_transform(crs = "wgs84") %>%
    leaflet() %>%
    addMeasure(primaryLengthUnit = 'meters') %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons()
  
  g_df <- data.frame()
  
  g_df <- data.frame(grid) %>% 
    mutate(ind = 1:length(grid) )
  
  print(nrow(g_df))
  
  g_df <- g_df %>%
    rowwise() %>%
    mutate(testx = list(analyseGridItem(geometry, ind, cur_city, centre)) )
  
  colnames(g_df)[3] <- 'Returns'
  
  g_df <- g_df %>%
    rowwise() %>%
    mutate(distance = Returns[2], cov = Returns[1]) 
  
  ggplot(g_df, mapping = aes(x = distance, y = cov)) + labs(title = "Tree Coverage In Paris") +xlab("Distance to Centre (m)") + ylab("Tree Coverage (%)") + geom_point() + theme_minimal() 
  
  saveRDS(g_df,paste0('data/',cur_city,'.Rdata'))
  
  return('NA')
}


analyseGridItem <- function(bbox, index, city, centre) {
  
  print(index)
  
  distance_to_centre <- st_distance( centre, st_centroid( st_transform( bbox , 4326) ) )
  
  print(distance_to_centre)
  print(city)
  
  item_bbox <- bbox %>%
    st_transform(4326)
  
  file_name = paste0('rasters/international/',city, '/',  'rad', radius, 'cellsize',cell_size, res,'res/',index,'.tif')
  
  print(file_name)
  
  if(file.exists(file_name)) {
    print('running from file')
    
    chm <- raster(file_name)
    var = paste0('X',index)
    
  } else {
    print('downloading from api')
    chm <- chmloader::download_chm(item_bbox, filename = file_name, res = res)
    var = paste0(index,'.tif')
  }
  
  
  chm_stars = st_as_stars(chm, proxy = T )
  chm_simplified = st_as_sf(chm_stars, merge = TRUE) 
  
  chm_simplified = chm_simplified %>% filter( !!as.name(var) > threshold) 
  chm_simplified <- st_transform(st_as_sf(chm_simplified), 7844)
  
  chm_is_empty = ifelse(nrow(chm_simplified) > 0, F, T) 
  
  if(!chm_is_empty) {
    if(union && nrow(chm_simplified) > 0){
      chm_simplified = st_union(chm_simplified)
    }
  }
  
  leaflet(chm_simplified) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons()
  
  coverage_pc = (chm_simplified %>% st_area()) / st_area(item_bbox)
  
  results <- c(as.numeric(coverage_pc), as.numeric(distance_to_centre))
  
  
  return(results)
}
