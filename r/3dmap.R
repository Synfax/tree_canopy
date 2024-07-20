library(tidyverse)
library(sf)
library(RColorBrewer)
remotes::install_github("walkerke/mapgl")
library(mapgl)

sf_use_s2(F)

# sa1_sf <- readRDS("r_objects/sa1_sf.Rdata") %>%
#   st_set_geometry("geometry")

agg_df <- readRDS("r_objects/agg_df_filtered.Rdata") %>%
  mutate(sa1_code_2021 = sa1)

grouping = 'sa1_code_2021'
grouping_sf = sa1_sf 

sa1_sf = sa1_sf %>% rename(sa1_code_2021 = SA1_CODE21)

trees_grouped <- coverage(group = grouping) %>%
  left_join(grouping_sf, by = (grouping)) %>%
  st_set_geometry('geometry') %>%
  rowwise() %>%
  mutate(tree_percentage = round(tree_percentage, 1)) %>%
  filter(!st_is_empty(geometry))
  

trees_grouped = trees_grouped %>% mutate(x10 = tree_percentage * 100)

colValues = seq(min(trees_grouped$tree_percentage), max(trees_grouped$tree_percentage), len = 9)
colCols = c(brewer.pal(9, 'Greens'))

maplibre(style = carto_style('positron'), center = c(144.963115,-37.814175), zoom = 9, pitch = 40) %>%
  add_fill_extrusion_layer(id = 'trees_grouped', source = trees_grouped,
                           fill_extrusion_opacity = 0.85,
                           fill_extrusion_color = interpolate(
                             column = 'tree_percentage',
                             values = colValues,
                             stops = colCols
                           ),
                           fill_extrusion_height = c("get", "x10") )

