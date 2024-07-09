files = c('paris', 'vienna', 'london', 'newyork', 'melbourne', 'sydney', 'tokyo' )

big_df <- data.frame()

walk(files, loadFiles)


loadFiles <- function(...) {
  
  filename = paste0('city_comparisons/',...,'.Rdata')
  
  city_data <- readRDS(filename) %>%
    st_set_geometry('geometry') %>%
    st_transform(4326)
  
  leaflet(city_data) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons()
  
  city_data = city_data %>%
    st_drop_geometry() %>%
    rowwise() %>%
    mutate(distance = round(distance, 0))
  
  city_data = city_data %>%
    group_by(distance) %>%
    summarise(mean_cov = mean(cov), med_cov = median(cov) ) %>%
    mutate(city = as.character(...) )
  
  plot(city_data$distance, city_data$med_cov, type = 'l')
  
  print(tibble(city_data))
  
  big_df <<- rbind(big_df, city_data)
}


big_df = big_df %>%
  mutate(med_cov = 100 * med_cov, mean_cov = 100 * mean_cov)
  

ggplot(big_df, mapping = aes(distance, mean_cov, colour = city, group = city)) +
  xlab("Distance to Centre (m)") +
  ylab("Mean Tree Coverage (%)") +
  labs(title = "Tree Coverage of Major Cities") +
  geom_smooth(method = 'loess') +
  theme_minimal()

ggplot(big_df, mapping = aes(distance, med_cov, colour = city, group = city)) +
  xlab("Distance to Centre (m)") +
  ylab("Median Tree Coverage (%)") +
  labs(title = "Tree Coverage of Major Cities") +
  geom_smooth(method = 'loess') +
  theme_minimal()


