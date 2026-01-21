############################
# Laddar och sparar data####
############################

############### Laddar paket och ställer in settings
{
  source("Script/install_load_packages.R")
  source("Script/settings.R")
  source("Script/search_kolada.R")
  source("Script/load_geodata.R")
  
  install_and_load()
  settings <- get_settings()
  
  kommunkod <- settings$kommunkod
  kommuner <- settings$kommuner
  kommun_colors <- settings$kommun_colors
  riket_narliggande <- settings$riket_narliggande
  lanskod <- settings$lanskod
  lan <- settings$lan
  
}


# Skapar folder för data om den inte existerar
if (!file.exists('Data')){
  dir.create('Data')
}

##### Deso
{
# 2025

url <- "https://geodata.scb.se/geoserver/stat/wfs?service=WFS&REQUEST=GetFeature&version=1.1.0&TYPENAMES=stat:DeSO_2025&outputFormat=geopackage"
output_file <- "Data/DeSO_2025.gpkg"

# Kollar om den redan finns
if (file.exists(output_file)) {
  
} else {
  
  response <- GET(url, write_disk(output_file, overwrite = TRUE))
  
}

# 2018

url <- "https://geodata.scb.se/geoserver/stat/wfs?service=WFS&REQUEST=GetFeature&version=1.1.0&TYPENAMES=stat:DeSO_2018&outputFormat=geopackage"
output_file <- "Data/DeSO_2018.gpkg"

# Kollar om den redan finns
if (file.exists(output_file)) {
  
} else {
  
  response <- GET(url, write_disk(output_file, overwrite = TRUE))
  
}

# Kopplingar 
# 2025
url <- 'https://www.scb.se/contentassets/e3b2f06da62046ba93ff58af1b845c7e/koppling-deso2025-regso2025.xlsx'
output_file <- "Data/koppling-deso2025-regso2025.xlsx"

# Kollar om den redan finns
if (file.exists(output_file)) {
  
} else {
  
  response <- GET(url, write_disk(output_file, overwrite = TRUE))
  
}
# 2018
url <- 'https://www.scb.se/contentassets/e3b2f06da62046ba93ff58af1b845c7e/koppling-deso2018-regso2020.xlsx'
output_file <- "Data/koppling-deso2018-regso2020.xlsx"

# Kollar om den redan finns
if (file.exists(output_file)) {
  
} else {
  
  response <- GET(url, write_disk(output_file, overwrite = TRUE))
  
}

print('Nedladdning av "DeSO" har genomförts')


############# Shape-fil    #########
url <- 'https://www.scb.se/contentassets/3443fea3fa6640f7a57ea15d9a372d33/shape_svenska_250121.zip'
output_file <- "Data/shape.zip"

# Kollar om den redan finns
if (file.exists(output_file)) {
  
} else {
  
  response <- GET(url, write_disk(output_file, overwrite = TRUE))
  
}

# Extrahera ZIP-filen
unzip("Data/shape.zip", files = "Kommun_Sweref99TM.zip", exdir = "Data")
unzip("Data/shape.zip", files = "LanSweref99TM.zip", exdir = "Data")
unzip("Data/Kommun_Sweref99TM.zip", exdir = "Data/Kommun_Sweref99TM")
unzip("Data/LanSweref99TM.zip", exdir = "Data/Lan_Sweref99TM")

print('Nedladdning av "Kommun_Sweref99TM" har genomförts')
}
########### Deso land/vatten areal ##############
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__MI__MI0802/Areal2025/
url <- 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/MI/MI0802/Areal2025'

meta <- pxweb_get(url)

# Visa tillgängliga regionkoder
regioner <- meta$variables[[1]]$values

# Välj endast regioner som börjar med "03"
uppsala_koder <- regioner[startsWith(regioner, lanskod)]

pxweb_query_list <- list(
  "Region" =uppsala_koder , # Uppsala läns kommuner
  "ArealTyp" = '*', 
  'ContentsCode' = '*',
  "Tid" = c("*")    # Årtal att hämta data för
)

px_data <- pxweb_get(url,pxweb_query_list)
px_deso <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

write.csv(px_deso, "Data/df_deso_land_vatten.csv", row.names = F)

print('Nedladdning av "df_deso_land_vatten.csv" har genomförts')

############## SCB ############
# Markanvändning 
{
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__MI__MI0803__MI0803A/MarkanvN/
pxweb_query_list <- list(
  "Region" = kommunkod, # Uppsala läns kommuner
  "Markanvandningsklass" = c("16", "213", "3", "421", "811", "911"), 
  'ContentsCode' = '*',
  "Tid" = c("*")    # Årtal att hämta data för
)

px_data <- pxweb_get(
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/MI/MI0803/MI0803A/MarkanvN",
  query = pxweb_query_list
)

# Steg 4: Omvandla data till ett data.frame för enklare hantering i R
px_markanvandning <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")


# Omstrukturera data så att markanvändningsklasser blir kolumner
px_markanvandning <- px_markanvandning %>%
  pivot_wider(names_from = "markanvändningsklass", values_from = "Markanvändningen, hektar")


# Skapa nya andelsvariabler för varje markanvändningsklass baserat på 'total jordbruksmark'
px_markanvandning <- px_markanvandning %>%
  mutate(
    `andel total jordbruksmark` = `total jordbruksmark` / `total landareal`,
    `andel total skogsmark` = `total skogsmark` / `total landareal`,
    `andel bebyggd och anlagd mark` = `bebyggd och anlagd mark ` / `total landareal`,
    `andel öppen myrmark` = `öppen myrmark` / `total landareal`,
    `andel övrig mark` = `övrig mark` / `total landareal`
  )

write.csv(px_markanvandning, "Data/df_markanvandning.csv", row.names = F)

print('Nedladdning av "df_markanvandning.csv" har genomförts')
}


############# Kolada 

############## AVFALL ##################
# Kommunalt avfall som samlats in för materialåtervinning, inkl. biologisk behandling, andel (%)
{
  df_avfall <- search_and_fetch_kolada("Kommunalt avfall")
  df_avfall <- df_avfall %>% filter(year >= 2010)
  write.csv(df_avfall, "Data/df_avfall.csv", row.names = F)
  print('Nedladdning av "df_avfall.csv" har genomförts')
}

# Insamlat mat- och restavfall, kg/invånare (justerat)
{
  df_matavf <- search_and_fetch_kolada("Insamlat mat- och restavfall,")
  titles <- unique(df_matavf$title)[1]
  write.csv(df_matavf, "Data/df_matavf.csv", row.names = F)
  print('Nedladdning av "df_matavf.csv" har genomförts')
}

# Insamlat förpackningar och returpapper, kg/invånare (justerat)
{
  df_returpapp <- search_and_fetch_kolada("Insamlat förpackningar och returpapper")
  #unique(df_returpapp$title)
  write.csv(df_returpapp, "Data/df_returpapp.csv", row.names = F)
  print('Nedladdning av "df_returpapp.csv" har genomförts')
}

# Insamlat grovavfall, kg/invånare (justerat)
{
  df_grovt <- search_and_fetch_kolada("Insamlat grovavfall")
  #unique(df_grovt$title)
  write.csv(df_grovt, "Data/df_grovt.csv", row.names = F)
  print('Nedladdning av "df_grovt.csv" har genomförts')
}

# Insamlat farligt avfall (inkl. elavfall och batterier), kg/invånare (justerat)
{
  df_farligt <- search_and_fetch_kolada("Insamlat farligt avfall")
  #unique(df_farligt$title)
  write.csv(df_farligt, "Data/df_farligt.csv", row.names = F)
  print('Nedladdning av "df_farligt.csv" har genomförts')
}

# Avgift för avfallshämtning 
{
  df_avfall_avgift <- search_and_fetch_kolada("Avgift för avfallshämtning")
  unique(df_avfall_avgift$title)
  write.csv(df_avfall_avgift, "Data/df_avfall_avgift.csv", row.names = F)
  print('Nedladdning av "df_avfall_avgift.csv" har genomförts')
}

# Avgift för avfallshämtning 
{
  df_avfall_avgift <- search_and_fetch_kolada("Avgift för avfallshämtning (ny definition)")
  unique(df_avfall_avgift$title)
  write.csv(df_avfall_avgift, "Data/df_avfall_avgift.csv", row.names = F)
  print('Nedladdning av "df_avfall_avgift.csv" har genomförts')
}


# kostnad för avfallshämtning 
{
  df_avfall_kost <- search_and_fetch_kolada("Kostnad avfallshantering")
  unique(df_avfall_kost$title)
  write.csv(df_avfall_kost, "Data/df_avfall_kost.csv", row.names = F)
  print('Nedladdning av "df_avfall_kost.csv" har genomförts')
}




############## Natur ############
# Skyddad natur, andel (%)
{
  df_skyddad_natur <- search_and_fetch_kolada("Skyddad natur ")
  write.csv(df_skyddad_natur, "Data/df_skyddad_natur.csv", row.names = F)
  print('Nedladdning av "df_skyddad_natur.csv" har genomförts')
}

# Medelavstånd skyddad natur
{
  df_avstand_natur <- search_and_fetch_kolada("Medelavstånd till skyddad")
  #unique(df_avstand_natur$title)
  write.csv(df_avstand_natur, "Data/df_avstand_natur.csv", row.names = F)
  print('Nedladdning av "df_avstand_natur.csv" har genomförts')
}


############# Miljökvalitetsindex ##############
{
  df_miljokval <- search_and_fetch_kolada("Miljökvalitet", kommunkod='Alla')
  df_miljokval <- df_miljokval %>% filter(title == "Miljökvalitet - Kommunindex")
  write.csv(df_miljokval, "Data/df_miljokval.csv", row.names = F)
  print('Nedladdning av "df_miljokval.csv" har genomförts')
}

############# Hållbarhetsindex ###########
# Miljömässig hållbarhet - Kommunindex
{
  df_hallbarhet <- search_and_fetch_kolada("Miljömässig hållbarhet", kommunkod='Alla')
  write.csv(df_hallbarhet, "Data/df_hallbarhet.csv", row.names = F)
  print('Nedladdning av "df_hallbarhet.csv" har genomförts')
}



############ Ekologisk mark #############
{
  df_eko <- search_and_fetch_kolada("Ekologisk brukad")
  #unique(df_eko$title)
  write.csv(df_eko, "Data/df_eko.csv", row.names = F)
  print('Nedladdning av "df_eko.csv" har genomförts')
}



############ Slåtteräng #############
{
  df_slatt <- search_and_fetch_kolada("Slåtteräng")
  unique(df_slatt$title)
  write.csv(df_slatt, "Data/df_slatt.csv", row.names = F)
  print('Nedladdning av "df_slatt.csv" har genomförts')
}



############ Betesmark #############
{
  df_betesmark <- search_and_fetch_kolada("Betesmark", match = 0)
  unique(df_betesmark$title)
  write.csv(df_betesmark, "Data/df_betesmark.csv", row.names = F)
  print('Nedladdning av "df_betesmark.csv" har genomförts')
}
############ Ekologisk sjö #############
{
  df_ekovatten <- search_and_fetch_kolada(" god ekologisk", match = 0)
  unique(df_ekovatten$title)
  write.csv(df_ekovatten, "Data/df_ekovatten.csv", row.names = F)
  print('Nedladdning av "df_ekovatten.csv" har genomförts')
}




################ kalmarksareal ##########
{
  # https://pxweb.skogsstyrelsen.se/pxweb/sv/Skogsstyrelsens%20statistikdatabas/Skogsstyrelsens%20statistikdatabas__Miljohansyn/JO1403_8.a.px/
url <- 'https://pxweb.skogsstyrelsen.se:443/api/v1/sv/Skogsstyrelsens statistikdatabas/Miljohansyn/JO1403_8.a.px'

pxweb_query_list <- list(
  "Region" = '*', # Uppsala läns kommuner
  'Variabel' = '*',
  'År' = '*'
)

px_data <- pxweb_get(url,
  query = pxweb_query_list
)

# Steg 4: Omvandla data till ett data.frame för enklare hantering i R
kalmarksareal <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Filterar ut län
kalmarksareal <- kalmarksareal %>% filter(!Region %in% c("Götaland",
                                                         "Svealand",
                                                         "Norra Norrland",
                                                         "Södra Norrland"))

write.csv(kalmarksareal, "Data/df_kalmarksareal.csv", row.names = F)

print('Nedladdning av "df_kalmarksareal.csv" har genomförts')

}


########### Produktiv skogsareal ############
{
  # https://pxweb.skogsstyrelsen.se/pxweb/sv/Skogsstyrelsens%20statistikdatabas/Skogsstyrelsens%20statistikdatabas__Fastighets-%20och%20agarstruktur/PX12.px/
  url <- 'https://pxweb.skogsstyrelsen.se:443/api/v1/sv/Skogsstyrelsens statistikdatabas/Fastighets- och agarstruktur/PX12.px'
  
  
  meta <- pxweb_get(url)
  
  pxweb_query_list <- list(
    "Kommun" = as.character(26:(26+7)), # Uppsala läns kommuner
    'Variabel' = '*',
    'År' = '*'
  )
  
  px_data <- pxweb_get(url,
                       query = pxweb_query_list
  )
  
  # Steg 4: Omvandla data till ett data.frame för enklare hantering i R
  prod_skog <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  
  prod_skog <- prod_skog %>% filter(År > 2004)
  
  write.csv(prod_skog, "Data/df_prod_skog.csv", row.names = F)
  
  print('Nedladdning av "df_prod_skog.csv" har genomförts')
  
}



########### Vatten kolada ###########
{ # Avgift för vatten och avlopp inkl. moms för typfastighet enligt Nils Holgersson-modellen, kr/kvm
  df_avgift <- search_and_fetch_kolada("Avgift för vatten och avlopp inkl. moms för typfastighet")
  df_avgift <- df_avgift %>% filter(year >= 2010)
  write.csv(df_avgift, "Data/avgift_vatten_NHM.csv", row.names = F)
  print('Nedladdning av "avgift_vatten_NHM.csv" har genomförts')
}

{ #   Investeringsutgifter vattenförsörjning och avloppshantering, kr/inv
  df_avgift <- search_and_fetch_kolada("Investeringsutgifter vattenförsörjning och avloppshantering, kr/inv")
  df_avgift <- df_avgift %>% filter(year >= 2010)
  write.csv(df_avgift, "Data/investering_vatten.csv", row.names = F)
  print('Nedladdning av "investering_vatten.csv" har genomförts')
}

{ #   Vattenanvändning totalt, senaste mätning, kbm/inv
  df_anvandning <- search_and_fetch_kolada("Vattenanvändning", kommunkod='Alla')
  #unique(df_anvandning$title)
  df_anvandning <- df_anvandning %>% filter(year >= 2010)
  write.csv(df_anvandning, "Data/vattenanvandning.csv", row.names = F)
  print('Nedladdning av "vattenanvandning.csv" har genomförts')
}

{ #    Nettokostnad vattenförsörjning och avloppshantering, kr/inv
  df_anvandning <- search_and_fetch_kolada("Nettokostnad vattenförsörjning och avloppshantering, kr/inv", match = 0)
  #unique(df_anvandning$title)
  df_anvandning <- df_anvandning %>% filter(year >= 2010)
  write.csv(df_anvandning, "Data/nettokostnad_vatten.csv", row.names = F)
  print('Nedladdning av "nettokostnad_vatten.csv" har genomförts')
}

{ # Grundvattenförekomster med god kemisk och kvantitativ status, andel (%)
  df_grund <- search_and_fetch_kolada("Grundvattenförekomster med god kemisk och kvantitativ status, andel (%)", match = 0)
  #unique(df_anvandning$title)
  df_grund <- df_grund %>% filter(year >= 2004)
  write.csv(df_grund, "Data/df_grund.csv", row.names = F)
  print('Nedladdning av "df_grund.csv" har genomförts')
}





################# GEOdata atom  #############
# Denna funktion är endast skapad för att inte köra koden nedan, kör varje del manuellt om ny data finns tillgänglig.
#geo_data <- function(){
############# Skyddade områden: naturvårdsområden ##########
# https://www.geodata.se/geodataportalen/srv/swe/catalog.search#/metadata/dd8371a0-f692-44e3-bd0b-25de8dee8906
#load_geodata_atom(url='https://geodata.naturvardsverket.se/atom/inspire/ps/SE_ProtectedSites_serviceFeed.xml',file_path ="Data/ProtectedSites")

########### Produktionsanläggningar (Används ej för tillfället, bristfällig kvalitet)########## 
# https://www.geodata.se/geodataportalen/srv/swe/catalog.search#/metadata/354cb6ff-b2e5-4945-8b4e-9df1c8c401c8
#Produktionsanläggningar, rapportering 2021 (EU Registry on industrial sites - 2010/75/EU samt 166/2006). Gäller data från år 2020.
#load_geodata_atom(url='https://geodata.naturvardsverket.se/atom/inspire/pf/SE_PF_EURegistry_serviceFeed.xml',file_path ="Data/prodanlagg")


########### Biogeografiska regioner:(Används ej för tillfället, bristfällig kvalitet) ########## 
# https://www.geodata.se/geodataportalen/srv/swe/catalog.search#/metadata/b166b68e-1305-4acc-b3de-a4f9cfa22faf
#load_geodata_atom(url='https://geodata.naturvardsverket.se/atom/inspire/br/SE_BR_Biogeoregion_serviceFeed.xml',file_path ="Data/Biogeografiska")
#}


# Denna funktion är endast skapad för att inte köra koden nedan, kör varje del manuellt om ny data finns tillgänglig.

############# grundvattenkvalitet(Används ej för tillfället, bristfällig kvalitet) ###########
{
# Länk tagen från hemsidan 
# https://www.geodata.se/geodataportalen/srv/swe/catalog.search#/metadata/20b36a27-3a24-4a61-a3e0-4f533e76a60b
#url<- 'https://resource.sgu.se/data/oppnadata/grundvattenkvalitet-analysresultat-provplatser/grundvattenkvalitet-analysresultat-provplatser.zip'     

# Var filen ska sparas lokalt
#dest <- "Data/grundvatten.zip"

# Skapa mapp om den inte finns
#dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

# Ladda ned filen
#download.file(url, dest, mode = "wb")

# länk från : https://www.sgu.se/produkter-och-tjanster/geologiska-data/grundvatten--geologiska-data/grundvattenkvalitet/
#url1 <- 'https://api.sgu.se/oppnadata/grundvattenkvalitet-analysresultat-provplatser/ogc/features/v1/collections/analysresultat/items?f=application/json&limit=10000&filter=lan=%2703%27'

#data <- GET(url1)
# Läs in JSON-innehållet
#content_json <- rawToChar(data$content)
#data <- fromJSON(content_json, flatten = TRUE)

# Platta ut automatiskt
#df <- data$features %>% as_tibble()

#write.csv(df, 'Data/analys_grundvatten.csv', row.names = F)
}


############## GRundvattennivå(Används ej för tillfället, bristfällig kvalitet) ##########
# https://www.sgu.se/produkter-och-tjanster/geologiska-data/grundvatten--geologiska-data/grundvattennivaer/
# https://api.sgu.se/oppnadata/grundvattennivaer-observerade/ogc/features/v1/openapi?f=text%2Fhtml
{
#items_url <- "https://api.sgu.se/oppnadata/grundvattennivaer-observerade/ogc/features/v1/collections/nivaer/items"

# Testa att hämta första 1000 med GEOJSON
#res <- GET(items_url, query = list(
#  f = "application/geo+json",
#  limit = 1000
#))

#url_stations <- "https://api.sgu.se/oppnadata/grundvattennivaer-observerade/ogc/features/v1/collections/stationer/items"

#res <- GET(url_stations, query = list(
 # f = "application/geo+json",
#  limit = 10000
#))


#get_stations <- function() {
 # url <- "https://api.sgu.se/oppnadata/grundvattennivaer-observerade/ogc/features/v1/collections/stationer/items"
  
  
#  res <- GET(url_stations, query = list(
 #   f = "application/geo+json",
  #  limit = 10000
  #))
  
  #stations
}

#stations <- get_stations()

#cat("Antal stationer:", nrow(stations), "\n")


### Filtrera stationer till Uppsala län (lanskod = 03)

#stations_uppsala <- stations %>% 
 # filter(lanskod == "03")

#cat("Antal stationer i Uppsala län:", nrow(stations_uppsala), "\n")


###  Hämta observationer i block


#get_observations <- function(limit = 2000) {
#  base_url <- "https://api.sgu.se/oppnadata/grundvattennivaer-observerade/ogc/features/v1/collections/nivaer/items"
  
#  all <- list()
 # start <- 0
#  page <- 1
  
 # repeat {
  #  cat("Hämtar sida", page, "(startIndex =", start, ")\n")
    
  #  res <- GET(base_url, query = list(
   #   f = "application/json",
    #  limit = limit,
    #  startIndex = start
  #  ))
    
   # if (status_code(res) != 200) {
  #    cat("\nAvbröt: API svarade:", status_code(res), "\n")
   #   break
  #  }
    
   # txt <- content(res, "text", encoding = "UTF-8")
  #  parsed <- fromJSON(txt)
    
  #  items <- parsed$features$properties
    
  #  if (length(items) == 0) {
  #    cat("Inga fler rader.\n")
  #    break
  #  }
    
   # cat("  → Hämtade", nrow(items), "rader\n")
    
   # all[[page]] <- items
    
    # pagination
  #  start <- start + limit
  #  page <- page + 1
  #}
  
#  bind_rows(all)
#}

#obs_all <- get_observations()

#cat("Totalt antal observationer hämtade:", nrow(obs_all), "\n")



###  Filtrera observationer till stationer som ligger i Uppsala län


#obs_uppsala <- obs_all %>%
#  filter(platsbeteckning %in% stations_uppsala$platsbeteckning)

#cat("Observationer i Uppsala län:", nrow(obs_uppsala), "\n")



###  Slå samman koordinater + observationer


# plocka ut koordinatkolumner från stationslagret
#stations_clean <- stations_uppsala %>%
#  select(platsbeteckning ) %>%
#  mutate(
 #   lon = st_coordinates(stations_uppsala)[,1],
#    lat = st_coordinates(stations_uppsala)[,2]
#  ) %>%
 # st_drop_geometry()

# slå ihop via platsbeteckning
#merged <- obs_uppsala %>%
 # left_join(stations_clean, by = "platsbeteckning")

#cat("Efter join:", nrow(merged), "rader\n")


###  Gör om till sf-objekt med riktiga punkter


#grundvatten_uppsala_sf <- st_as_sf(
#  merged,
#  coords = c("lon", "lat"),
#  crs = 4326,
#  remove = FALSE
#)

#cat("Klart! sf-objekt med geometri skapat.\n")

#sf::st_write(
#  grundvatten_uppsala_sf,
#  "Data/grundvatten_uppsala_sf.gpkg",
#  delete_dsn = TRUE
#)

#cat("Fil sparad: Data/grundvatten_uppsala_sf.gpkg\n")
#}
