#################################
# installerar och läser in paket#
#################################


install_and_load <- function() {
  # CRAN-paket
  cran_packages <- c(
    "pxweb",
    "dplyr",
    "ggplot2",
    "tidyr",
    "cowplot",
    "readxl",
    "stringr",
    "leaflet",
    "sf",
    "mapview",
    "showtext",
    "gt",
    "plotly",
    "remotes", 
    'htmltools',
    'sf',
    'reactable',
    'forcats',
    'scales',
    'RColorBrewer',
    'magick',
    'GGally',
    'kableExtra',
    'jsonlite',
    'httr',
    'rKolada',
    'checkmate',
    'utils',
    'zoo',
    'svglite',
    'rKolada',
    'rvest',
    'purrr',
    'xml2',
    'leaflet.extras2',
    'lubridate',
    'reactablefmtr',
    'yyjsonr'
  )
  
  # Installera och ladda CRAN-paket
  for (pkg in cran_packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }

}




