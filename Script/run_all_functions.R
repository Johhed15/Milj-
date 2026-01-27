####### Kör alla andra scripts 
####### Laddar ner data
####### Sparar plots 

{
  source("Script/install_load_packages.R")
  source("Script/settings.R")
  install_and_load()
  settings <- get_settings()
  
  kommunkod <- settings$kommunkod
  kommuner <- settings$kommuner
  kommun_colors <- settings$kommun_colors
  upplat_colors <- settings$upplat_colors
  source("Script/load_save_data.R")
  source("Script/create_save_plots.R")
  source("Script/skrapare.R")
  rus_reader(year='2025') # Ställ in rätt län manuellt så den passar den riktiga URL koden
  # Samma inställning måste användas i create_tables -> rus_lansstyrelse()
}


# Tabell som ska laddas ner för senaste datan måste göras manuellt här
# https://www.naturvardsverket.se/data-och-statistik/fororenade-omraden/fororenade-omraden/
# Sparas som "data-och-statistik-fororenade-omraden-fororenade-omraden-.csv"

# Datan som laddas ned med load_geodata_atom() körs inte automatiskt då jag inte har koll på hur de uppdateras!
# Kolla igenom detta manuellt först på hemsidan innan kod körs, ligger i funktionen geo_data() i scriptet load_save_data()
# Detta gäller vattendata som ligger i funktionen vatten_geo().



######### Funktioner som sparar bilder till folder: "Figurer", Många är interaktiva i denna rapport och sparas inte

# Avfall
avfall_avgift()
avfall_kostnad()


# Index
miljo_index()

hallbarhetsindex()


# Avstånd till skyddad natur
avstand_skyddadnatur()

# Ekologisk mark
ekomark()

# betesmark
betesmark()

#Slåtteräng
slatt_mark()

# vatten
eko_vatten()

avgift_vatten()

inv_vatten()

netto_vatten()

grundvatten_kolada()

