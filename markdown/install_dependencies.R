# Function to install packages if not already installed
install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

# List of packages to install
packages <- c(
  "devtools",
  "tidyverse",
  "sf",
  "leaflet",
  "stringr",
  "knitr",
  "reactablefmtr",
  "plotly",
  "htmltools",
  "crosstalk",
  "readxl",
  "patchwork",
  "ggtext",
  "DT",
  "RColorBrewer",
  "showtext",
  "shiny",
  "shinyWidgets",
  "lwgeom",
  "units",
  "fixest",
  "mapgl",
  "crosstalk",
  "reactable",
  "pacman",
  "stars", 
  "pak"
)

# Install packages
lapply(packages, install_if_missing)

# Install specific GitHub version of leaflet
devtools::install_github("dmurdoch/leaflet@crosstalk4")

# Install chmloader
devtools::install_github("TESS-Laboratory/chmloader")

# Print message when done
cat("All required packages have been installed.\n")
