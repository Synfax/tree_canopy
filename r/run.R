run <- function(parallel = F) {
  
  melbourne <- st_sfc(st_point(c(144.963115,-37.814175)), 
                      crs = 7844) %>% #Lat longs have a 'web' crs, so start with that  
    st_sfc()# turn it into an sf dataframe rather than just a point

  sa3_sf = sa3_sf %>%
    filter(SA3_CODE21 %in% na.omit(unique(dwelling_data$sa3_code_2021)))  %>%
    mutate(centroid = st_centroid(geometry)) %>%
    mutate(distance = as.vector(st_distance(melbourne,centroid))) %>%
    dplyr::arrange(distance)
  
  sa3_list = sa3_sf$SA3_CODE21
  
  if(parallel) {
    cl <<- makeCluster(4)
    clusterEvalQ(cl, { library(sf) })
    clusterEvalQ(cl, sf_use_s2(FALSE) )
    clusterExport(cl, "sa3_sf")
  }
  
  for(sa3 in sa3_list) {
    
    sa3_name <- sa3_sf %>%
      filter(SA3_CODE21 == sa3) %>%
      st_drop_geometry() %>% 
      select(SA3_NAME21) %>%
      unlist()
    
    #cutting out road network
    #road_network_in_sa3 <- st_parallel(road_network, st_intersection, 4, y=sa3_sf )
    road_network_in_sa3 <- st_intersection(road_network, sa3_sf) %>%
      st_as_sf()
    print('road network cut')
    
    if(!file.exists(paste0('sa3_results/',res,'res/',sa3,'.Rdata'))) {
      
      print(paste('file doesnt exist for', sa3_name, 'continuing'))
      
      #can i put logic in here such that it removes saved stuff from SA list, otherwise resets it?
      
      dwelling_data_current <<- dwelling_data %>%
        filter(sa3_code_2021 == sa3)
      
      sa_list <- na.omit(unique(dwelling_data_current$sa1_code_2021))
      existing_results <- unique(results_df$sa1)
      
      sa_list <- setdiff(sa_list, existing_results)
      
      for(sa in sa_list) {
        
        if(parallel) {
          run_for_sa1_alt_parallel(sa)
        }
        if(!parallel) {
          run_for_sa1_alt(sa)
        }
      
      }
      
      file_path <- paste0('sa3_results/',res,'res/',sa3,'.Rdata')
      
      saveRDS(results_df %>% st_drop_geometry(), file_path)
      
      results_df <<- data.frame()
      robust_df <<- data.frame()
      
      
    } else {
      print('file already exists, skipping')
    }
    
  }
  
  if(parallel) {
    stopCluster(cl)
  }
}
