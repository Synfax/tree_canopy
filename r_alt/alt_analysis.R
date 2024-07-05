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
  dplyr::select(!(starts_with('prox'))) %>%
  st_set_geometry('geom') %>%
  st_join(sal_sf %>% select(c(SAL_CODE21, SAL_NAME21, geometry)))

agg_df <- agg_sf %>%
  st_drop_geometry() %>%
  as.data.frame()

xy <- sa1_sf %>%
  rename( sa2_code_2021 = SA2_CODE21) %>%
  st_join(sal_sf, largest = TRUE) %>%
  filter(!is.na(SAL_CODE21))

#create dictionaries between sa1 and sa2/lga to fill in missing values for 'roads' and 'other' land uses
sa1_sa2_map =  with(agg_df, setNames(sa2_code_2021, sa1_code_2021))
sa1_lga_map = with(agg_df, setNames(lga_name_2022, sa1_code_2021))
sa2_sa2_map = with(agg_df, setNames(sa2_name_2021, sa2_code_2021))
sa1_sal_map = with(xy, setNames(SAL_NAME21, SA1_CODE21))

#fill in these missing values
agg_df <- agg_df %>%
  rowwise() %>%
  mutate(zone_short = ifelse(is.na(zone_short), land_type, zone_short),
         sa2_code_2021 = ifelse(is.na(sa2_code_2021), sa1_sa2_map[as.character(sa1)], sa2_code_2021),
         lga_name_2022 = ifelse(is.na(lga_name_2022), sa1_lga_map[as.character(sa1)], lga_name_2022),
         sa2_name_2021 = ifelse(is.na(sa2_name_2021), sa2_sa2_map[as.character(sa2_code_2021)], sa2_name_2021),
         SAL_NAME21 = ifelse(is.na(SAL_NAME21), sa1_sal_map[as.character(sa1)], SAL_NAME21) )

#st_write(agg_sf, 'sf_exports/test.gpkg', driver = "GPKG", append = FALSE)

#mapCoverage(agg_sf, unlist(agg_sf %>% st_drop_geometry() %>% select(coverage_pc)), 'Greens', 'Tree coverage per dwelling (%)')

melbourne <- st_sfc(st_point(c(144.963115,-37.814175)),
                    crs = 7844) %>% #Lat longs have a 'web' crs, so start with that
  st_sfc()# turn it into an sf dataframe rather than just a point

sa2_sf = sa2_sf %>%
  filter(SA2_CODE21 %in% na.omit(unique(agg_df$sa2_code_2021)))  %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(distance = as.vector(st_distance(melbourne,centroid))) %>%
  dplyr::arrange(distance) %>%
  rename( sa2_code_2021 = SA2_CODE21)

sal_sf = sal_sf %>%
  filter(SAL_CODE21 %in% na.omit(unique(agg_df$SAL_CODE21)))  %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(distance = as.vector(st_distance(melbourne,centroid))) %>%
  dplyr::arrange(distance) 

lga_sf = lga_sf %>%
  filter(LGA_NAME22 %in% na.omit(unique(agg_df$lga_name_2022)))  %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(distance = as.vector(st_distance(melbourne,centroid))) %>%
  dplyr::arrange(distance) 

# 
# sa2_within_distance = unique(sa2_sf %>%
#                                st_drop_geometry() %>%
#                                filter(distance < 20000) %>%
#                                select(sa2_code_2021)) %>% unlist()

mm_lgas <- c('Brimbank', 'Merri-bek', 'Banyule', 'Darebin', 'Yarra', 'Moonee Valley', 'Manningham', 'Maribyrnong', 'Melbourne', 'Hobsons Bay', 'Port Phillip', 'Boroondara', 'Stonnington', 'Glen Eira', 'Bayside', 'Monash', 'Whitehorse', 'Maroondah', 'Manningham', 'Kingston')

metro_lgas <- c('Melbourne','Boroondara','Merri-bek',
                'Brimbank',
                'Greater Dandenong',
                'Kingston',
                'Darebin',
                'Frankston',
                'Port Phillip',
                'Monash',
                'Moonee Valley',
                'Knox',
                'Stonnington',
                'Whitehorse',
                'Glen Eira',
                'Yarra',
                'Hobsons Bay',
                'Manningham',
                'Maribyrnong',
                'Banyule',
                'Bayside',
                'Maroondah')

#filter to MM LGAS
agg_df <- agg_df %>%
  filter(lga_name_2022 %in% metro_lgas)

#saving objects for quarto to reach them
saveRDS(agg_df, 'r_objects/agg_df.Rdata')
saveRDS(sa2_sf, 'r_objects/sa2_sf.Rdata')
saveRDS(sal_sf, 'r_objects/sal_sf.Rdata')
saveRDS(lga_sf, 'r_objects/lga_sf.Rdata')

