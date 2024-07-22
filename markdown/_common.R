# Setup for all markdown files
library(devtools)
devtools::install_github("dmurdoch/leaflet@crosstalk4")
library(sf)
library(leaflet)
library(stringr)
library(knitr)
library(reactablefmtr)
library(plotly)
library(fixest)
library(htmltools)
library(crosstalk)
library(readxl)
library(patchwork)
library(ggtext)
library(DT)
library(RColorBrewer)
library(showtext)
devtools::install_github("walkerke/mapgl")
library(mapgl)
library(shiny)
library(shinyWidgets)
library(reactable)
library(crosstalk)
library(scales)
library(tidyverse)

agg_df <- readRDS("../r_objects/agg_df.Rdata") %>%
  mutate(sa1_code_2021 = sa1)

agg_df_filtered <- readRDS("../r_objects/agg_df_filtered.Rdata") %>%
  mutate(sa1_code_2021 = sa1)

mm_lgas <- c("Brimbank", "Merri-bek", "Banyule", "Darebin", "Yarra", "Moonee Valley", "Manningham", "Maribyrnong", "Melbourne", "Hobsons Bay", "Port Phillip", "Boroondara", "Stonnington", "Glen Eira", "Bayside", "Monash", "Whitehorse", "Maroondah", "Manningham", "Kingston")


sa2_sf <- readRDS("../r_objects/sa2_sf.Rdata") %>%
  # rename(sa2_code_2021 = "SA2_CODE21") %>%
  st_set_geometry("geometry")

sal_sf <- readRDS("../r_objects/sal_sf.Rdata") %>%
  st_set_geometry("geometry")

lga_sf <- readRDS("../r_objects/lga_sf.Rdata") %>%
  st_set_geometry("geometry") %>%
  rename("lga_name_2022" = LGA_NAME22)

sa1_sf <- readRDS("../r_objects/sa1_sf.Rdata") %>%
  st_set_geometry("geometry")

uhi <- readRDS("../r_objects/uhi.Rdata")

mesh_blocks <- readRDS("../r_objects/mesh_blocks.Rdata")


source("../r/functions.r")
source("../r/mapping_functions.r")
source("../themes/theme.r")

rents <- readRDS("../r_objects/rents.Rdata") %>%
  manipulateRents(grouping = "sa2") %>%
  rowwise() %>%
  mutate(median_band = gsub("\\$", "", median_band)) %>%
  rename(sa2_code_2021 = "sa2")

public_coverage <- coverage(exp = expression(zone_code %in% c("PPRZ", "PCRZ") | zone_short == "roads"), type = "public", group = "SAL_NAME21") %>% select("SAL_NAME21", "public_percentage")

public_coverage_sa2 <- coverage(exp = expression(zone_code %in% c("PPRZ", "PCRZ") | zone_short == "roads"), type = "public", group = "sa2_code_2021") %>% select("sa2_code_2021", "public_percentage")


sa2_prices <- readRDS("../r_objects/yimby.Rdata") %>%
  filter(gcc_name == "GREATER MELBOURNE") %>%
  rename(sa2_code_2021 = "sa2_code") %>%
  mutate(sa2_code_2021 = as.character(sa2_code_2021)) %>%
  left_join(public_coverage_sa2, by = "sa2_code_2021") %>%
  left_join(rents, by = "sa2_code_2021") %>%
  left_join(sa2_sf, by = "sa2_code_2021") %>%
  mutate(within15 = ifelse(distance < 15000, T, F))

lot_sizes <- agg_df %>%
  filter(zoning_permits_housing == "Housing permitted") %>%
  group_by(SAL_NAME21) %>%
  summarise(med_lot = median(lot_size, na.rm = TRUE), n = n()) %>%
  rowwise()



create_combined_df <- function(grouping, grouping_sf) {
  
  
  street_trees_grouped <- coverage(exp = street_exp, type = 'street', group = grouping) %>%
  rename("street_tree_coverage" = coverage, "total_street_area" = total_area )

  reserve_trees_grouped <- coverage(exp = public_exp, type = 'reserve', group = grouping)  %>%
    rename("reserve_tree_coverage" = coverage, "total_reserve_area" = total_area ) 
 
  resi_trees_grouped <- coverage(exp = resi_exp , type = 'residential', group = grouping)  %>%
    rename("residential_tree_coverage" = coverage, "total_residential_area" = total_area )
  
  rural_trees_grouped <- coverage(exp = rural_exp , type = 'rural', group = grouping)  %>%
    rename("rural_tree_coverage" = coverage, "total_rural_area" = total_area )
  
  
  other_trees_grouped <- coverage(exp = other_exp, group = grouping, type = 'other') %>%
    rename('other_tree_coverage' = coverage, 'total_other_area' = total_area)
  
  #combine into one and clean
  combined_df <- street_trees_grouped %>%
    left_join(reserve_trees_grouped, by = grouping) %>%
    left_join(resi_trees_grouped, by = grouping) %>%
    left_join(other_trees_grouped, by = grouping) %>%
    left_join(rural_trees_grouped, by = grouping) %>%
    replace(is.na(.), 0) %>%
    group_by(!!as.name(grouping)) %>%
    mutate(total_area =  sum(across(ends_with('_area'))))  %>%
    mutate(total_coverage_area = sum(across(ends_with('_coverage')))) %>%
    mutate(total_coverage_pc = total_coverage_area / total_area ) %>%
    mutate( weighted_street_pc = (street_tree_coverage / total_coverage_area) * total_coverage_pc,
            weighted_reserve_pc = (reserve_tree_coverage / total_coverage_area) * total_coverage_pc,
            weighted_rural_pc = (rural_tree_coverage / total_coverage_area) * total_coverage_pc,
            weighted_resi_pc = (residential_tree_coverage / total_coverage_area) * total_coverage_pc,
            weighted_other_pc = (other_tree_coverage / total_coverage_area) * total_coverage_pc,
          ) %>%
    dplyr::select(c(grouping,'street_percentage', 'rural_percentage', 'reserve_percentage', 'residential_percentage', 'weighted_street_pc', 'weighted_reserve_pc', 'weighted_rural_pc', 'weighted_resi_pc', 'total_coverage_pc', 'other_percentage', 'weighted_other_pc')) %>%
    left_join(grouping_sf %>% st_drop_geometry() %>% select(grouping, distance), by = grouping) %>%
    rowwise() %>%
    # mutate(distance = distance_map[as.character(as.name(grouping))]) %>%
    mutate(distance = distance / 1000) %>%
    ungroup() %>%
    as.data.frame() %>%
    return()
    # mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
    # mutate(across(ends_with('percentage'), ~ ifelse(is.na(.x), 0, .x) )) %>%
    
}

# Custom labeling function
percent_format_no_multiply <- function(x) {
  sprintf("%d%%", x)
}

# GGPlot theme
theme_yimby <- theme_minimal() +
  theme(
    axis.line = element_blank(),
    panel.background = element_rect(fill = "#fdffee"),
    plot.background = element_rect(fill = "#fdffee", color = NA),
    plot.title = element_text(
      face = "bold",
      color = "#10461B",
      hjust = 0.5,
    ),
    # plot.margin = unit(-10,"points"),
    # axis.text.x = element_text(margin = margin(t = 10, b = 10)),  # Add buffer
    # axis.text.y = element_text(margin = margin(l = 10, r = 10)),  # Add buffer
    # plot.margin = unit(c(20, 20, 20, 20), "pt"),  # Add overall plot margin
    legend.position = "bottom"
  )