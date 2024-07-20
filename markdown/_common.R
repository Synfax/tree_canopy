# Setup for all markdown files
devtools::install_github("dmurdoch/leaflet@crosstalk4")
library(tidyverse)
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
library(shiny)
library(shinyWidgets)
library(reactable)
library(crosstalk)
library(scales)


agg_df <- readRDS("../r_objects/agg_df.Rdata") %>%
  mutate(sa1_code_2021 = sa1)

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


# GGPlot theme
theme_yimby <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#fdffee"),
    plot.background = element_rect(fill = "#fdffee"),
    plot.title = element_text(
      face = "bold",
      color = "#10461B",
      family = "Inter",
      hjust = 0.5,
    ),
    axis.title = element_text(face = "medium", family = "Inter"),
    axis.text = element_text(family = "Inter"),
    plot.margin = unit(c(50, 50, 50, 50), "pt")
  )
