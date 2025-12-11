########## Funktion för att söka och ladda data från Kolada ###########
{
  source("Script/install_load_packages.R")
  source("Script/settings.R")
  
  install_and_load()
  settings <- get_settings()
  
  kommunkod <- settings$kommunkod
  kommuner <- settings$kommuner
  kommun_colors <- settings$kommun_colors
  riket_narliggande <- settings$riket_narliggande
  lan <- settings$lan
  lanskod <- settings$lanskod
  
}

# Funktion för sökning 
search_kolada <- function(sok_ord = NULL){
  # Stopfunktioner för felinmatning
  
  # sok_ord
  if(is.null(sok_ord) || !is.character(sok_ord) || nchar(trimws(sok_ord)) == 0){
    stop("Argumentet 'sok_ord' måste vara ett icke-tomt teckensträng.")
  }
  
  # Tar hem information om alla variabler på kolada
  kpi_lista <- get_kpi()
  
  # Tar ut raderna med ordet i titeln i sig eller nära 
  resultat <- kpi_lista[agrep(sok_ord, kpi_lista$title, ignore.case = TRUE, max.distance = 0.1), ]
  
  if(nrow(resultat)==0){ # om det inte finns data med sökordet så retuneras detta
    stop('Tyvärr hittas inget för det sökordet')
  }
  if(nrow(resultat) > 100){
    warning("Sökordet matchar fler än 100 KPI:er, överväg att precisera sökningen.")
  }
  return(resultat) # Returnerar ids och förklaringar
}




# Funktion för sökning och hämtning av data direkt, match avgör hur nära sökningen måste vara korrekt
search_and_fetch_kolada <- function(sok_ord = NULL, kommunniva = 'municipality', kommunkod = NULL, match= 0.1){
  # Stopfunktioner för felinmatning
  
  # sok_ord
  if(is.null(sok_ord) || !is.character(sok_ord) || nchar(trimws(sok_ord)) == 0){
    stop("Argumentet 'sok_ord' måste vara ett icke-tomt teckensträng.")
  }
  
  # kommunniva
  if(!kommunniva %in% c("municipality", "ou")) {
    stop("Argumentet 'kommunniva' måste vara 'municipality' eller 'ou'")
  }
  
  # kommunkod
  if(is.null(kommunkod)){ # kollar om kommunkod ligger som global(vilket det bör göra i LiL(Läget i Länet))
    kommunkod <- get0("kommunkod", envir = .GlobalEnv)
    if(is.null(kommunkod)){
      stop("Variabeln 'kommunkod' finns ej. Ange kommunkod som argument eller definiera den globalt.")
    }
  }
  

  
  # Tar hem information om alla variabler på kolada
 
  kpi_lista <- get_kpi()
  # Tar ut raderna med ordet i titeln i sig / nära rätt ord
  resultat <- kpi_lista[agrep(sok_ord, kpi_lista$title, ignore.case = TRUE, max.distance = match), ]
 
  if(nrow(resultat) > 50){
    print("Sökordet matchar fler än 50 KPI:er, precisera sökningen. Retunerar listan med titlar")
    return(resultat)
    stop()
    
  }
  
  if(nrow(resultat)==0){
    stop('Tyvärr hittas inget för det sökordet')
  }

  kpi_ids <- resultat$id
  
  # Byternamn på id till kpi
  resultat <- resultat %>% rename(kpi = id)

  # Tar hem data för variablerna och kommunerna, använder felhantering om det inte går
  if(length(kommunkod) == 1 && kommunkod == "Alla"){
    # Om data för alla kommuner ska laddas ned
    
    alla_kommuner <- unlist(get_municipality(max_results =500)[1])
    data <- tryCatch(
      get_values(kpi = kpi_ids, municipality = alla_kommuner, unit_type = kommunniva),
      error = function(e) stop("Fel vid hämtning av data: testa funktionen search_kolada(). Detaljer: ", e$message)
    )
    
    }else{ 
  data <- tryCatch(
    get_values(kpi = kpi_ids, municipality = kommunkod, unit_type = kommunniva),
    error = function(e) stop("Fel vid hämtning av data: testa funktionen search_kolada(). Detaljer: ", e$message)
  )
    }
  
  # Slår ihop data med titeln
  df <- resultat %>% select(title, kpi) %>% left_join(data, by ='kpi')
  
  return(df)
  
}

