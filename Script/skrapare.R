############################
#### Skrapar data #########
############################

############### Laddar paket och ställer in settings
{
  source("Script/install_load_packages.R")
  source("Script/settings.R")
  source("Script/search_kolada.R")
  
  install_and_load()
  settings <- get_settings()
  
  kommunkod <- settings$kommunkod
  kommuner <- settings$kommuner
  kommun_colors <- settings$kommun_colors
  riket_narliggande <- settings$riket_narliggande
  lanskod <- settings$lanskod
  lan <- settings$lan
  
}



# https://www.rus.se/regional-arlig-uppfoljning/uppsala-lan-2024/

rus_reader <- function(lan='uppsala-lan', year='2024'){
  
  # URL till sidan
  url <- paste0("https://www.rus.se/regional-arlig-uppfoljning/",lan,"-",year,"/")
  
  # Ladda sidan
  page <- read_html(url)
  
  # Hämta huvudtexten 
  h2_nodes <- page %>%
    html_elements("h2.wp-block-heading, h3.wp-block-heading")
  
  # Vi börjar efter "Generationsmålet" för att hoppa över inledning
  start_index <- which(html_text2(h2_nodes) == "Generationsmålet") + 1
  h2_nodes <- h2_nodes[start_index:length(h2_nodes)]
  
  # Funktion som hämtar text fram till nästa h2
  extract_section <- function(i) {
    node <- h2_nodes[i]
    if (html_name(node) != "h2") return(NULL)  # hoppa över h3
    title <- html_text2(node)
    
    # Hämta allt innehåll fram till nästa h2
    siblings <- node %>% html_nodes(xpath = "following-sibling::*")
    next_h2 <- which(html_name(siblings) == "h2")[1]
    if (!is.na(next_h2)) siblings <- siblings[1:(next_h2 - 1)]
    
    # Extrahera textblock
    text_blocks <- siblings %>% html_text2()
    text_blocks <- str_squish(text_blocks)
    
    # Försök hitta Målbedömning och Miljötillstånd (trendpil)
    målbedömning <- NA
    trend <- NA
    
    if (any(str_detect(text_blocks, regex("Målbedömning", ignore_case = TRUE)))) {
      idx <- which(str_detect(text_blocks, regex("Målbedömning", ignore_case = TRUE)))
      målbedömning <- text_blocks[idx + 1]  # texten kommer efter rubriken
    }
    if (any(str_detect(text_blocks, regex("Miljötillstånd", ignore_case = TRUE)))) {
      idx <- which(str_detect(text_blocks, regex("Miljötillstånd", ignore_case = TRUE)))
      trend <- text_blocks[idx + 1]
    }
    
    # Beskrivning = text före "Målbedömning"
    desc_end <- ifelse(any(str_detect(text_blocks, regex("Målbedömning", ignore_case = TRUE))),
                       which(str_detect(text_blocks, regex("Målbedömning", ignore_case = TRUE)))[1] - 1,
                       length(text_blocks))
    description <- paste(text_blocks[1:desc_end], collapse = " ")
    
    tibble(
      Miljömål = title,
      Målbedömning = målbedömning,
      Trend = trend,
      Beskrivning = description
    )
  }
  
  # Applicera på alla h2
  result <- map_dfr(seq_along(h2_nodes), extract_section)
  
  # Spara tabell
  file_name <- paste0('Data/miljomal_',lan,"_",year,'.csv' )
  write.csv(result,file_name,row.names = F)

}

