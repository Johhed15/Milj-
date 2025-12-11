## skapa tabeller

# Ställer in settings paket mm
{
  source("Script/install_load_packages.R")
  source("Script/settings.R")
  install_and_load()
  settings <- get_settings()
  
  kommunkod <- settings$kommunkod
  kommuner <- settings$kommuner
  kommun_colors <- settings$kommun_colors
  lanskod <- settings$lanskod
  lan <- settings$lan
  
}

rus_lansstyrelse <- function(lan = 'uppsala-lan', year= '2024'){
  # Läser in data
  df <- read.csv(paste0('Data/miljomal_',lan,"_",year,'.csv' ))
  
  #Skapa en ny kolumn med pilar istället för vissa trendtexter
  df <- df %>%
    dplyr::mutate(
      Trend = dplyr::case_when(
        Trend == "Positiv" ~ "Positiv 🡅",
        Trend  == "Negativ" ~"Negativ 🡇",
        Trend  == "Negativ utveckling" ~"Negativ utveckling 🡇",
        Trend  == "Neutral" ~ "Neutral 🡆",
        Trend  ==  "Neutral utveckling" ~ "Neutral utveckling 🡆",
        TRUE ~ Trend  # Behåll "Oklar utveckling" och "Ingen regional bedömning"
      )
    )
  
  # Skapar tabell
  df %>% gt() %>%
    tab_header(
      title = paste("Länsstyrelsens regionala årliga uppföljning av miljökvalitetsmål"),
      subtitle = paste("År", year)
    ) %>%
    cols_label(
      Miljömål = "Miljömål",
      Målbedömning = "Målbedömning",
      Trend = "Trend"
    )  %>%
    data_color(
      columns = Målbedömning,
      colors = scales::col_factor(
        palette = c("#4AA271", "#D0342C", "#6F787E","#DBECE3"),  
        levels = c("Uppnås", "Uppnås ej", "Ingen regional bedömning","Nära")
      )
    ) %>%
    data_color(
      columns = Trend,
      colors = scales::col_factor(
        palette = c("#4AA271", "#D0342C","#D0342C","#6F787E","#6F787E", "#6F787E","#6F787E"),  
        levels = c("Positiv 🡅","Negativ 🡇","Negativ utveckling 🡇", "Neutral 🡆",  "Neutral utveckling 🡆", "Oklar utveckling", "Ingen regional bedömning")
      )
    )%>%
    tab_style(
      style = cell_text(weight = "bold"),           # Bold för målen
      locations = cells_body(columns = Miljömål)
      ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_title()
    ) %>% 
    tab_source_note(
      source_note = "Källa: Länsstyrelsen"
    ) %>%
    tab_options(
      table.font.names = "sourcesanspro",
      table.font.size = px(14),
      table.align = "center",
      heading.align = "center"
    ) 

}


riskomraden <- function(){
  
  df <- read.csv2('Data/data-och-statistik-fororenade-omraden-fororenade-omraden-.csv')
  names(df)[1] <- 'Län'
  
  # Hitta index för Uppsala
  upsala_idx <- which(df$Län == "Uppsala län")
  
  # Flytta Uppsala först, behåll resten i originalordning
  df_ordered <- df[c(upsala_idx, setdiff(1:nrow(df), upsala_idx)), ]
  
  
  # Skapar tabell
  df_ordered %>% gt() %>%
    tab_header(
      title = paste("Riskklassade förorenade områden"),
      subtitle = paste("År", 2015)
    ) %>%
    cols_label(
      Riskklass.1 = "Riskklass 1",
      Riskklass.2 = "Riskklass 2",
      Riskklass.3 = "Riskklass 3",
      Riskklass.4 = "Riskklass 4"
    )  %>%
    data_color(
      columns = c(Riskklass.1,Riskklass.2,Riskklass.3,Riskklass.4),
      colors = col_numeric(
        palette = c("#6F787E", "#D0342C"),  # färggradient
        domain = NULL  # NULL = tar min/max från kolumnen automatiskt
      )
    )  %>% 
    tab_style(
      style = cell_text(weight = "bold"),           # Bold för målen
      locations = cells_body(columns = Län)
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_title()
    ) %>% 
    tab_source_note(
      source_note = "Källa: Naturvårdsverket"
    ) %>%
    tab_options(
      table.width  = pct(100),
      table.font.names = "sourcesanspro",
      table.font.size = px(16),
      table.align = "center",
      heading.align = "center"
    ) 
  
}



grundvatten_sort <- function(){
  
  gml_file <- "Data/grundvatten/grundvattenkvalitet_analysresultat_provplatser.gpkg" 
  suppressMessages( suppressWarnings( 
    df <- st_read(gml_file,quiet = TRUE) )) %>% st_drop_geometry()
  
  # Tar endast ut punkter i länet
  df_platser <- df %>% filter(lanskod == !!lanskod) %>% 
    select(provplatsnamn, nationellt_provplatsid)
  
  # Läser in data
  df <- read.csv('Data/analys_grundvatten.csv') %>% 
    rename('nationellt_provplatsid' = properties.nationellt_provplatsid) %>% 
    filter(properties.provtyp=='grundvatten',nationellt_provplatsid %in%
             df_platser$nationellt_provplatsid)
  
  df <- df %>% arrange(desc(properties.inlamningsdat))
  
  # Formaterar data, väljer ut variabler och formaterar om
  df <- df %>% mutate(properties.provtagningsdat = as.Date(properties.provtagningsdat),
                      properties.inlamningsdat = as.Date(properties.inlamningsdat))
  
  df <- df %>% left_join(df_platser , by='nationellt_provplatsid')
  
  df <- df %>%  filter(!is.na(provplatsnamn))
  
  
  
  # Väljer ut och byter namn på kolumner
  df_table <- df %>%
    select(
      Provplats = provplatsnamn,
      Parameter = properties.param,
      Datum = properties.inlamningsdat,
      Värde = properties.matvardetal,
      Enhet = properties.enhet
    ) %>%
    group_by(Provplats, Parameter) %>% # Senaste året som finns för parametern på platsen
    filter(Datum == max(Datum, na.rm = TRUE)) %>%
    ungroup()
  
  # Färgparameter
  df_table <- df_table %>%
    group_by(Parameter) %>%
    mutate(Värde_norm = rescale(Värde, to = c(0, 1))) %>%
    ungroup()
  
  #  reactable
  table <- reactable(
    df_table,
    elementId = "table",
    searchable = FALSE,
    filterable = FALSE,
    columns = list(
      Provplats = colDef(filterable = TRUE, searchable = TRUE),
      Parameter = colDef(filterable = FALSE),
      Datum = colDef(filterable = FALSE),
      Värde = colDef(filterable = FALSE),
      Enhet = colDef(filterable = FALSE)
    ),
    rowStyle = function(index) {
      row <- df_table[index, ]
      # Color from light yellow (low) to red (high)
      color <- scales::col_numeric(
        palette = c("#4AA271", "#D57667"),
        domain = c(0, 1)
      )(row$Värde_norm)
      list(background = color)
    },
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50)
  )
  
  # Add source
  table <- table %>%
    add_source("Källa: SGU")
  
  table
  
}
  
  
