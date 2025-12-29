########################################
## Ställer in kommunnamn, färger etc####
########################################

get_settings <- function() {
  kommunkod <- c("0330", "0331", "0360", "0380", "0381", "0382", "0305", "0319")
  kommuner <- c("Knivsta", "Heby", "Tierp", "Uppsala", "Enköping", "Östhammar", "Håbo", "Älvkarleby")
  
  lan <- "Uppsala län"
  lanskod <- "03"
  
  # Fonts
  try(showtext::font_add_google("Source Sans Pro", "sourcesanspro"), silent = TRUE)
  font_add("Arial", "path/to/arial.ttf")
  showtext::showtext_auto()
  
  # Theme
  region_theme_pdf <- ggplot2::theme(
    text = element_text(family = "sourcesanspro", size = 12),
    plot.title = element_text(family = "Arial", face = "bold", size = 20, hjust = 0.5, color = "#B81867"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(family = "Arial", face = "bold", size = 16),
    strip.text = element_text(family = "Arial", face = "bold", size = 14),
    plot.margin =grid::unit(c(15, 30, 15, 15), "pt")
  )
  ggplot2::theme_set(ggplot2::theme_minimal() + region_theme_pdf)
  
  kommun_colors <- c(
    "Enköping" = "#D57667",
    "Heby" = "#F9B000", 
    "Håbo" = "#019CD7",
    "Knivsta" = "#D0342C",
    "Tierp" = "#4AA271",
    "Uppsala" = "#6F787E",
    "Älvkarleby" = "#8B4A9C",
    "Östhammar" = "#E67E22"
  )
  
  riket_narliggande <- c('00','03',"04", "05", "18", "19" )
  
  # Returnera allt som lista
  list(
    kommunkod = kommunkod,
    kommuner = kommuner,
    kommun_colors = kommun_colors,
    riket_narliggande=riket_narliggande,
    lanskod = lanskod,
    lan = lan
  )
}
