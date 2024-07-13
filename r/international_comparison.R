#build dataframes

library(stars)
library(raster)

coordinates <- list( c(2.3514, 48.8575) 
                     , c(16.3713,48.2081 ) 
                     , c(0.1276,51.5072),
                     c(-74.0060, 40.7128),
                     c(144.9631,-37.8136),
                     c(139.752825,35.684874),
                     c(151.2093,-33.8688),
                     c (12.5683, 55.6761),
                     c(8.5417, 47.3769),
                     c(-114.0719, 51.0447),
                     c(6.1432.46.2044))

crss <- c(27561,
          31287,
          29901,
          32118,
          28355, 
          30169, 
          28355,
          23032,
          21781,
          3402,
          21781)

cities <- c('paris', 
            'vienna',
            'london',
            'newyork',
            'melbourne',
            'tokyo',
            'sydney',
            'copenhagen',
            'zurich',
            'calgary',
            'geneva') 




#settings
radius <- 10000
union <- TRUE
cell_size <- 1000
res <- 5
threshold <- 0

#start actual code
city_df = tibble(coords = coordinates, coordinateref = crss, city = cities) %>%
  as.data.frame() %>%
  rowwise() %>%
  mutate(lng = coords[1], lat = coords[2])

city_df = city_df[-c(1:(nrow(city_df) -1 ) ),]

city_df = city_df %>%
  rowwise() %>%
  mutate(res = runCity(lng, lat, coordinateref, city) )

#get water layer ready

water_bodies_global <- read_sf('data/water_valid.shp')

runCity <- function(longit, latit, local_crs, cur_city) {
  
  longit = city_df$lng[1]
  latit = city_df$lat[1]
  local_crs = city_df$coordinateref[1]
  cur_city = city_df$city[1]

  local_coords = c(as.numeric(longit),as.numeric(latit))

  print(paste(cur_city, local_crs))
  print(local_coords)
  
  centre = st_point(local_coords) %>%
    st_sfc(crs = 4326) 
  
  sf_use_s2(F)

  bounding = centre %>%
    st_transform(crs = local_crs) %>%
    st_buffer(dist = radius) %>%
    st_bbox() %>%
    st_as_sfc(crs = 4326)
  
  bounding %>%
    st_transform('wgs84') %>%
    leaflet() %>%
    addProviderTiles('CartoDB.Positron') %>%
    addMeasure(primaryLengthUnit = 'meters') %>%
    addPolygons()

  water_bodies = st_transform(water_bodies_global, local_crs)

  #water_bodies = water_bodies %>% filter(TYPE == 'Ocean or Sea')

  wbm = st_is_valid(water_bodies)
  wb = water_bodies %>% filter( wbm )

  sf_use_s2(F)

  ww <- st_intersection(wb, bounding)
  
  ww <- st_as_sf(ww) %>%
    dplyr::select(SHAPE_Area)
  
  ww = distinct(ww) %>%
    rowwise() %>%
    mutate(area = st_area(geometry)) %>%
    ungroup() %>%
    mutate(bounding_area = st_area(bounding)) %>%
    mutate(SHAPE_Area = round(SHAPE_Area, 5)) %>%
    rowwise() %>%
    mutate(isNotEqlToBounding = !(bounding_area == area )) %>%
    filter(isNotEqlToBounding) %>%
    ungroup() %>%
    filter(SHAPE_Area != 0.06180)
  
  wwu = st_union(ww)

  water_map <- wwu %>%
    st_transform('wgs84') %>%
    leaflet() %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons()
  
  print(water_map)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # water_bodies = st_transform(water_bodies, local_crs)
  # 
  # water_bodies_wgs = st_transform(water_bodies, 'wgs84')
  # bounding_wgs = st_transform(bounding, 'wgs84')
  # 
  # bounding = st_transform(bounding, st_crs(water_bodies))
  # 
  # wb <- st_difference(water_bodies_wgs, bounding_wgs) %>% st_as_sf() %>% st_transform('wgs84')
  #  
  # wb %>% leaflet() %>% addProviderTiles('CartoDB.Positron') %>% addPolygons()
  # 
  
  
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
    mutate(testx = list(analyseGridItem(geometry, ind, cur_city, centre, wwu)) )
  
  colnames(g_df)[3] <- 'Returns'
  
  g_df <- g_df %>%
    rowwise() %>%
    mutate(distance = Returns[2], cov = Returns[1]) 
  
  ggplot(g_df, mapping = aes(x = distance, y = cov)) + labs(title = "Tree Coverage In Paris") +xlab("Distance to Centre (m)") + ylab("Tree Coverage (%)") + geom_point() + theme_minimal() 
  
  saveRDS(g_df,paste0('city_comparisons/',cur_city,'.Rdata'))
  
  return('NA')
}


analyseGridItem <- function(bbox, index, city, centre, water) {

  # index = 1
  # bbox = grid[index,]
  # city = 'copenhagen'
  # water = wwu
   
  if( !dir.exists( paste0('rasters/international/', city)) ) {
    dir.create( paste0('rasters/international/', city) )
  }
  
  print(index)
  
  distance_to_centre <- st_distance( centre, st_centroid( st_transform( bbox , 4326) ) )
  
  print(distance_to_centre)
  print(city)
  
  item_bbox <- bbox %>%
    st_transform(4326)
  
  mp(item_bbox)
  
  
  if( !dir.exists( paste0('rasters/international/',city, '/',  'rad', radius, 'cellsize',cell_size, res,'res/')) ) {
    dir.create( paste0('rasters/international/',city, '/',  'rad', radius, 'cellsize',cell_size, res,'res/') )
  }
  
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
  
  
  bbox_no_water = st_difference(bbox, water)
  
  chm_simplified = st_intersection(wgs(chm_simplified), wgs(bbox_no_water))
  
  leaflet(chm_simplified) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons()
  
  
  no_water_area = st_area(bbox_no_water) %>%
    as.vector()
  
  if( is_empty(no_water_area) | ifelse(!is_empty(no_water_area), no_water_area < 0.01, T ) ) {
    return( c(NA, as.numeric(distance_to_centre)))
  }
  else {
    coverage_pc <- ifelse(chm_is_empty, 0, (chm_simplified %>% st_area()) / no_water_area )
    
    results <- c(as.numeric(coverage_pc), as.numeric(distance_to_centre))
    
    
    return(results)
  }
 
}

