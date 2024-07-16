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
  "patchwork",
  "ggtext",
  "DT",
  "RColorBrewer",
  "showtext",
  "lwgeom",
  "units",
  "stars"
)

# Install packages
lapply(packages, install_if_missing)

# Install specific GitHub version of leaflet
devtools::install_github("dmurdoch/leaflet@crosstalk4")

# Print message when done
cat("All required packages have been installed.\n")