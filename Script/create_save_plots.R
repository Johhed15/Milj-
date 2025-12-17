
#########################
#### Sparar diagram #####
#########################

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


########### Avfall ###########

avfall <- function(){
  df <- read.csv('Data/df_avfall.csv')
  
  
  df$title <- str_wrap(df$title, width= 50)
  titles <- unique(df$title)
  # Tar ut varaiabler till dropdownen
  regioner <- unique(df$municipality)
  n_region <- length(regioner)
  
  # Namn till dropdown
  y_titles <- c(
    "Materialåtervinning (%)",
    "Avfall till deponi (kg/invånare)",
    "Totalt insamlat avfall (kg/invånare)"
  )
  
  df <- df %>% mutate(municipality = factor(municipality, levels = sort(kommuner)))
  
  # Sätter names så det matchar titlarna
  names(y_titles) <- titles
  
  # Bygg plotly-objekt
  fig <- plot_ly( height = 700)
  
  # loop över alla variabler och kommuner
  for (title in titles) {
    df_title <- df %>% filter(title == !!title)
    
    for (region in levels(df_title$municipality)) {
      # Filtrerar ut data och läger in trace
      df_region <- df_title %>% filter(municipality == region)
      
      df_region <- df_region %>% mutate(year = factor(year, levels = unique(year)))
      
      fig <- fig %>%
        add_trace(
          x = df_region$year,
          y = df_region$value,
          type = "scatter",
          mode = "lines+markers",
          name = region,
          line = list(color = kommun_colors[region],width = 5),
          marker = list(color = kommun_colors[region],size = 8),
          visible = ifelse(title == titles[1], TRUE, FALSE)
        )
    }
  }
  
  # Skapa dropdown-knappar
  buttons <- lapply(seq_along(titles), function(i) {
    visible_vec <- rep(FALSE, length(titles)*n_region)
    visible_vec[((i-1)*n_region + 1):(i*n_region)] <- TRUE
    
    list(
      method = "update",
      args = list(
        list(visible = visible_vec),
        list(
          title = paste("<b>",titles[i],"<b>"),
          yaxis = list(
            title = paste("<b>",y_titles[[titles[i]]],"<b>"),
            rangemode =  "tozero"
          )
        )
      ),
      label = y_titles[i]
    )
  })
  
  #  Layout
  fig <- fig %>%
    layout(font = list(family = "sourcesanspro", size=18),
           margin = list(t = 120),
           title = list(text = paste("<b>",titles[1],"<b>"), y = 0.95, x = 0.55),
           xaxis = list(title = "", tickangle = -45),
           yaxis = list(title = "<b>Andel (%)<b>", 
                        rangemode = "tozero"),
           hovermode = 'x unified',
           updatemenus = list(
             list(
               y = -0.1,
               x=1.1,
               buttons = buttons,
               direction = "up"
             )),
           annotations = list(
             text ='Källa: Kolada och Avfall Sverige',
             x = 0,            
             y = -0.15,        
             xref = "paper",
             yref = "paper",
             xanchor = "left",
             yanchor = "bottom",
             showarrow = FALSE,
             font = list(size = 12)
           )
    )
  
  
  # tar bort plotly-funktioner
  fig <- plotly::config(
    fig,
    modeBarButtonsToRemove = c(
      'zoom2d',     # zoom button
      'pan2d',      # pan button
      'select2d',   # box select
      'lasso2d',    # lasso select
      'zoomIn2d',   # zoom in
      'zoomOut2d'   # zoom out
    ),
    displaylogo = FALSE)   # remove plotly logo/link
  
  fig
  
}


Avfall_kategoori <- function(){
  # Läser in data
  df <- read.csv('Data/df_matavf.csv') %>% bind_rows(
  read.csv('Data/df_returpapp.csv'))%>% bind_rows(
  read.csv('Data/df_grovt.csv'))%>% bind_rows(
  read.csv('Data/df_farligt.csv'))
  

  titles <- unique(df$title)
  # Tar ut varaiabler till dropdownen
  regioner <- unique(df$municipality)
  n_region <- length(regioner)
  
  # Kortar ned titlar
  df <- df %>%
    mutate(
      title_short = case_when(
        title == "Insamlat mat- och restavfall, kg/invånare (justerat)" ~ "Mat- och restavfall",
        title == "Insamlat förpackningar och returpapper, kg/invånare (justerat)" ~ "Förpackningar och returpapper",
        title == "Insamlat grovavfall, kg/invånare (justerat)" ~ "Grovavfall",
        title == "Insamlat farligt avfall (inkl. elavfall och batterier), kg/invånare (justerat)" ~ "Farligt avfall",
        TRUE ~ title
      )
    )
  df$title <- str_wrap(df$title, width= 50)
  
  # Färgschema
  kategori_col <- c("#4AA271","#F9B000","#8B4A9C", "#6F787E")
  
  names(kategori_col) <- unique(df$title_short)
  

  # Tar bort NA
  df <- df %>% filter(!is.na(value))
  
  # Ordnar efter kommunerna
  df <- df %>% mutate(municipality = factor(municipality, levels =sort(kommuner)))
  
  df <- df %>% arrange(municipality, title_short, year)
  
  fig <- plot_ly(height = 600)
  
  region <-  sort(unique(df$municipality))
  
  # Loopar över kommuner och kategorier för traces
  for (kommun in region) {
    for (kategori in unique(df$title_short)) {
      
      df_k <- df %>% filter(municipality == kommun, title_short == kategori)
      
      fig <- fig %>%
        add_trace(
          data = df_k,
          x = ~year,
          y = ~value,
          type = "scatter",
          mode = "lines+markers",
          name = kategori,
          line = list(color = kategori_col[kategori], width = 5),
          marker = list(color = kategori_col[kategori],size = 8),
          visible = ifelse(kommun == unique(df$municipality)[1], TRUE, FALSE),
          hovertemplate = paste0(
            " %{y:.1f} kg/inv"
          )
        )
    }
  }
  
  
  # Dropdown för kommuner
  buttons <- lapply(seq_along(region), function(i) {
    vis_vec <- rep(FALSE, length(region) * length(unique(df$title_short)))
    vis_vec[((i - 1) * length(unique(df$title_short)) + 1):(i * length(unique(df$title_short)))] <- TRUE
    list(
      method = "update",
      args = list(list(visible = vis_vec),
                  list(title = paste("<b>Insamlat avfall per kategori –", region[i],"<b>"))),
      label = region[i]
    )
  })

  #  Lägg till dropdownmenyn 
  fig <- fig %>%
    layout(hovermode = 'x unified',
      title = paste("<b>Insamlat avfall per kategori –", region[1],"<b>"),
      font = list(family = "sourcesanspro",size=16),
      xaxis = list(title = "", tickmode = "linear", tickangle = -45),
      yaxis = list(title = "<b>Kg/invånare<b>"),
      showlegend = TRUE,
      legend = list(
        orientation = "h",   # horisontell legend
        x = 0.5,            # centrerad
        y = -0.2,           # under grafen
        xanchor = "center",
        yanchor = "bottom",
        font = list(size = 14)
      ),
      updatemenus = list(list(
        active = 0,
        buttons = buttons,
        direction = "down",
        x = 0,
        xanchor = "center",
        y = 1.1,
        yanchor = "top"
      )),
      annotations = list(
        text = "Källa: Kolada och Avfall Sverige",
        x = 0,
        y = -0.13,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 12)
      )
    ) %>%
    plotly::config(
      modeBarButtonsToRemove = c('zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d'),
      displaylogo = FALSE
    )
  
  fig
  
}
  

avfall_avgift <- function(){
  # Läser in data
  df <- read.csv('Data/df_avfall_avgift.csv') %>% filter(!is.na(value)) %>% 
    filter(year == max(year))

  
  # Skapar plot
  p <- ggplot(df, aes(x = municipality, y=value))+ 
    geom_col(position="dodge", fill="#B81867")+ 
    labs(title= paste("Avgift för avfallshämtning",max(df$year)),
         x = "",y='kr/kvm',caption = 'Källa: Nils Holgersson gruppen')+ 
    theme(axis.text.x = element_text(angle = 90),
          legend.position="none",
          text = element_text(family = "sourcesanspro", size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          axis.text.x.bottom  = element_text(angle = 45, hjust=1),
          plot.caption = element_text(hjust = 0, vjust=4),
          plot.margin = grid::unit(c(t=40, 30, 15, 15), "pt"))
  p
  # sparar som svg
  ggsave('Figurer/avgift_avfall.svg',plot = p,device = "svg", width = 7, height = 5)
  ggsave('Figurer/avgift_avfall.png',plot = p,device = "png", width = 7, height = 5, dpi=96) # png
}

avfall_kostnad <- function(){
  # Läser in data
  df <- read.csv('Data/df_avfall_kost.csv') %>% filter(!is.na(value)) %>% 
    filter(year == max(year))
  
  df$title <- ifelse(df$title=="Kostnad avfallshantering, kr/inv", 'Kostnad', "Nettokostnad")
  
  
  # Skapar plot
  p <- ggplot(df, aes(x = municipality, y=value))+ 
    geom_col(position="dodge",fill="#B81867")+ facet_wrap(~title, ncol=1, scales ='free')+
    labs(title= paste("Kostnad avfallshantering",max(df$year)),
         x = "",y='kr/inv', ,caption = 'Källa: SCB')+ 
    theme(axis.text.x = element_text(angle = 90),
          legend.position="none",
          text = element_text(family = "sourcesanspro", size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          axis.text.x.bottom  = element_text(angle = 45, hjust=1),
          plot.caption = element_text(hjust = 0, vjust=4))
  p
  # sparar som svg
  ggsave('Figurer/kost_avfall.svg',plot = p,device = "svg", width = 7, height = 8)
  
  ggsave('Figurer/kost_avfall.png',plot = p,device = "png", width = 7, height = 8, dpi =96)
}




###### Miljö/Hållbarhetsindex ########


miljo_index <- function(){
  
  # Läser in data 
  df <- read.csv("Data/df_miljokval.csv") %>% filter(year==max(year),
                                                      municipality_type =='K')
  
  # Tar ut kvartiler, max, min och filtrerar kommun
  quantiles <- quantile(df$value,probs = c(0.25, 0.75))
  max_p <- df[which.max(df$value),]
  min_p <- df[which.min(df$value),]
  kommun_df <- df %>% filter(municipality %in% kommuner)
  
  # Skapar barplot med flera linjer 
  p<- ggplot(kommun_df, aes(x=municipality, y=value))+ 
    geom_col(fill="#B81867")+ 
    geom_hline(yintercept = quantiles[1], color = "black", linetype = "dashed", linewidth = 1) +  # 25th 
    geom_hline(yintercept = quantiles[2], color = "black", linetype = "dashed", linewidth = 1) +   # 75th 
    geom_hline(yintercept = max_p$value, color = "black", linetype = "solid", linewidth = 1) +  
    geom_hline(yintercept = min_p$value, color = "black", linetype = "solid", linewidth = 1) +   
    annotate("text", 
             x = Inf, 
             y = quantiles[1], 
             label = "25:e percentilen", 
             hjust = -0.05, vjust = 0.2, 
             color = "black", size = 4) +
    annotate("text", 
             x = Inf, 
             y = quantiles[2], 
             label = "75:e percentilen", 
             hjust = -0.05, vjust = 0.2, 
             color = "black", size = 4) +
    annotate("text", 
             x = Inf, 
             y = max_p$value, 
             label = paste(max_p$municipality, '(max)'), 
             hjust = -0.05, vjust = 0.2, 
             color = "black", size = 4) +
    annotate("text", 
             x = Inf, 
             y = min_p$value, 
             label = paste(min_p$municipality, '(min)'), 
             hjust = -0.05, vjust = 0.2, 
             color = "black", size = 4) +
    ylim(0,100)+
    
    labs(x = "", y="Index", 
         title= paste("Miljöindex per kommun år", unique(df$year)),
         caption = 'Källa: SCB')+ 
    theme(axis.text.x = element_text(angle = 90),
          legend.position="none",
          text = element_text(family = "sourcesanspro", size = 14),
          axis.title.y = element_text(angle = 0, vjust = 1, hjust = 0.5),
          axis.text.x.bottom  = element_text(angle = 45, hjust=1),
          plot.margin = grid::unit(c(15, r=90, 15, 15), "pt"),
          plot.caption = element_text(hjust = 0))+ coord_cartesian(clip = "off")
  p
  # sparar som svg
  ggsave('Figurer/miljoindex.svg',plot = p,device = "svg", width = 7, height = 7)
  
  ggsave('Figurer/miljoindex.png',plot = p,device = "png", width = 7, height = 8, dpi =96)
}

hallbarhetsindex <- function(){
  
  # Läser in data 
  df <- read.csv("Data/df_hallbarhet.csv") %>% filter(year==max(year),
                        municipality_type =='K')

  # Tar ut kvartiler, max, min och filtrerar kommun
  quantiles <- quantile(df$value,probs = c(0.25, 0.75))
  max_p <- df[which.max(df$value),]
  min_p <- df[which.min(df$value),]
  kommun_df <- df %>% filter(municipality %in% kommuner)
  
  # Skapar barplot med flera linjer 
  p<- ggplot(kommun_df, aes(x=municipality, y=value))+ 
    geom_col(fill="#B81867")+ 
    geom_hline(yintercept = quantiles[1], color = "black", linetype = "dashed", linewidth = 1) +  # 25th 
    geom_hline(yintercept = quantiles[2], color = "black", linetype = "dashed", linewidth = 1) +   # 75th 
    geom_hline(yintercept = max_p$value, color = "black", linetype = "solid", linewidth = 1) +  
    geom_hline(yintercept = min_p$value, color = "black", linetype = "solid", linewidth = 1) +   
    annotate("text", 
             x = Inf, 
             y = quantiles[1], 
             label = "25:e percentilen", 
             hjust = -0.05, vjust = 0.2, 
             color = "black", size = 4) +
    annotate("text", 
             x = Inf, 
             y = quantiles[2], 
             label = "75:e percentilen", 
             hjust = -0.05, vjust = 0.2, 
             color = "black", size = 4) +
    annotate("text", 
             x = Inf, 
             y = max_p$value, 
             label = paste(max_p$municipality, '(max)'), 
             hjust = -0.05, vjust = 0.2, 
             color = "black", size = 4) +
    annotate("text", 
             x = Inf, 
             y = min_p$value, 
             label = paste(min_p$municipality, '(min)'), 
             hjust = -0.05, vjust = 0.2, 
             color = "black", size = 4) +
    ylim(0,100)+
    
    labs(x = "", y="Index", 
         title= paste("Hållbarhetsindex per kommun år", unique(df$year)),
         caption = 'Källa: SCB')+ 
    theme(axis.text.x = element_text(angle = 90),
          legend.position="none",
          text = element_text(family = "sourcesanspro", size = 14),
          axis.title.y = element_text(angle = 0, vjust = 1, hjust = 0.5),
          axis.text.x.bottom  = element_text(angle = 45, hjust=1),
          plot.caption = element_text(hjust = 0),
          plot.margin = grid::unit(c(l=15,r=90,t=40,b=15),"pt"))+ coord_cartesian(clip = "off")
  p
  # sparar som svg
  ggsave('Figurer/hallbarhetsindex.svg',plot = p,device = "svg", width = 7, height = 7)
  
  
  ggsave('Figurer/hallbarhetsindex.png',plot = p,device = "png", width = 7, height = 7, dpi =96)
}



########################## Markanvändning #####################

karta_skog <- function(){
  # Läser in data
  px_markanvandning <- read.csv('Data/df_markanvandning.csv')  
  shapefile_path <- "Data/Kommun_Sweref99TM/Kommun_Sweref99TM.shp"
  
  suppressMessages(
    suppressWarnings(
      kommun_shape <- st_read(shapefile_path, quiet = TRUE)
    ))
  
  # Tar ut uppsala län
  kommun_shape <- kommun_shape[grep(paste0("^",lanskod), kommun_shape$KnKod), ]
  
  # kommun_shape och px_markanvandning kopplas ihop baserat på KnNamn och region
  kommun_shape_merged <- kommun_shape %>%
    left_join(px_markanvandning, by = c("KnNamn" = "region"))
  
  #Gör karta i Leaflet
  # Omprojicera till WGS84 (EPSG:4326) för att använda i Leaflet
  kommun_shape_leaflet <- st_transform(kommun_shape_merged, crs = 4326)
  
  # Popup text med data för alla variabler
  popup <-  kommun_shape_leaflet %>%
    group_by(KnKod,vart.5.e.år) %>%
    summarise(
      popup = paste0(
        unique(KnNamn),' år ',unique(vart.5.e.år),"<br>",
        paste0(
          "Andel Skogsmark: ", round(andel.total.skogsmark,3)*100,' %',"<br>",
          "Andel jordbruksmark: ", round(andel.total.jordbruksmark,3)*100,' %',"<br>",
          "Andel bebyggd och anlagd mark: ", round(andel.bebyggd.och.anlagd.mark,3)*100,' %',"<br>",
          "Andel öppen myrmark: ", round(andel.öppen.myrmark,3)*100,' %',"<br>",
          "Andel övrig mark: ", round(andel.övrig.mark,3)*100,' %',"<br>",
          collapse = "<br>"
        )
      ),
      .groups = "drop"
    ) %>%
    st_drop_geometry()
  
  
  # Lägger in popup text
  kommun_shape_leaflet <- kommun_shape_leaflet %>%
    left_join(popup, by = c("KnKod", 'vart.5.e.år'))
  
  # Gör 2 av 3 popups tomma per (varje kommun har 3, räcker att 1 visas)
  for (i in 1:nrow(kommun_shape_leaflet)) {
    if (i %% 3 != 0) {
      kommun_shape_leaflet$popup[i] <- " "
    }
  }
  
  
  # Layer för namnen på kommunerna
  kommun_centroids <- st_centroid(kommun_shape_leaflet)
  
  # Skapa den interaktiva Leaflet-kartan
  fig <- leaflet(data = kommun_shape_leaflet) %>%
    addTiles() %>%
    # Lägger in data
    addPolygons(
      fillColor = ~colorBin('viridis', andel.total.skogsmark)(andel.total.skogsmark),
      weight = 2,
      opacity = 1,
      color = "black",
      fillOpacity = 1,
      popup = ~popup,
      label = ~paste0(KnNamn, ": Andel skogsmark: ", scales::percent(andel.total.skogsmark, accuracy = 0.1)),
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE
      )
    ) %>%
    # Lägger till legend
    addLegend(
      pal = colorBin('viridis', kommun_shape_merged$andel.total.skogsmark, reverse = FALSE),
      values = kommun_shape_merged$andel.total.skogsmark,
      title = paste("Andel Skogsmark",max(kommun_shape_leaflet$vart.5.e.år)),
      position = "bottomright",
      labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x)
    ) %>% 
    # Kommunlabels
    addLabelOnlyMarkers(
      data = kommun_centroids,
      label = ~KnNamn ,
      labelOptions = labelOptions(
        noHide = TRUE,
        direction = 'center',
        textOnly = TRUE,
        style = list(
          "color" = "black",
          "font-size" = "12px",
          "font-weight" = "bold",
          "text-shadow" = "1px 1px 2px white, -1px 1px 2px white, 1px -1px 2px white, -1px -1px 2px white"
        )
      ))
  fig  
} 

################### Land/vatten areal ##############

###### EJ klar 
karta_landareal <- function(){
  # Läser in data
  df <- read.csv('Data/df_deso_land_vatten.csv')  
  
  suppressMessages({
    suppressWarnings({
      st_layers("Data/DeSO_2025.gpkg")
      deso_sf <- st_read("Data/DeSO_2025.gpkg", layer = "DeSO_2025", quiet = TRUE) %>%
        filter(lanskod == !!lanskod) # we keep only Uppsala län
    })
  })
  
  df <- df %>% rename(desokod =region)
  
  df <- df %>%
    select(desokod, år, arealtyp, Hektar) %>%
    pivot_wider(
      names_from = arealtyp,
      values_from = Hektar
    ) %>% group_by(desokod) %>% 
    mutate(andel_sjo = (`inlandsvatten exkl de fyra stora sjöarna`) / totalt * 100) %>% ungroup()
  
  
  # Slår ihop datan
  df_deso <- deso_sf %>% left_join(df, by = "desokod")
  
  # Filtrera bort 'totalt'
  df_popup <- df %>%
    group_by(desokod) %>%
    summarise(
      popup_text = paste0(
        desokod, " år ", år, "<br>",
        paste0(
          "Landareal", ": ", round(landareal,2), " ha (", round(landareal/totalt*100,1), "%)","<br>",
          "Inlandsvatten exkl de fyra stora sjöarna", ": ", round(`inlandsvatten exkl de fyra stora sjöarna`,2), " ha (", round(`inlandsvatten exkl de fyra stora sjöarna`/totalt*100,1), "%)","<br>",
          "De fyra stora sjöarna", ": ", round(`de fyra stora sjöarna`,2), " ha (", round(`de fyra stora sjöarna`/totalt*100,1), "%)","<br>",
          "Havsvatten", ": ", round(havsvatten,2), " ha (", round(havsvatten/totalt*100,1), "%)","<br>",
          collapse = "<br>"
        )
      ),
      .groups = "drop"
    )
  
  
  # Slå ihop med sf-objektet
  deso_sf_pop <- df_deso %>%
    left_join(df_popup, by = c("desokod"))
  

  colscale <- viridis::viridis(20)
  
  # Karta
  map <- mapview(
    deso_sf_pop,
    zcol = "andel_sjo",
    legend = TRUE,
    color = colscale,
    layer.name = paste("Andel sjöareal", max(df$år, na.rm = TRUE)),
    popup = deso_sf_pop$popup_text,
    label = paste("DeSO:", deso_sf_pop$desokod)
  )
  
  map
} 


############ NATUR  ##############


andel_skyddadnatur <- function(){
  # Läser in data
  df <- read.csv('Data/df_skyddad_natur.csv') %>% 
    filter(title != "Medelavstånd till skyddad natur, km",
           title != "Skyddad natur totalt, andel (%)")
  
  # Tar ut senaste året
  latest_year <- max(df$year)
  
  df <- df %>% 
    filter(year == latest_year, !is.na(value))
  
  # Tar bort en del av texten
  df$title <- str_remove_all(df$title, "Skyddad natur |,\\s*andel\\s*\\(%\\)" )
  
  df$title <- tools::toTitleCase(df$title)
  
  # Colors
  kategori_col <- c("#4AA271", "#019CD7", "#F9B000")
  names(kategori_col) <- unique(df$title)
  
  # Ordnar efter kommunerna
  
  df <- df %>% mutate(municipality = factor(municipality, levels =sort(kommuner)))
  
  # Fixar ordning
  region <-  sort(unique(df$municipality))
  
  df <- df %>% arrange(municipality)
  
  # Tar ut antal per kategori
  trace_per_kategori <- table(df$title)/length(unique(df$year))
  ord <- c("Land","Inlandsvatten","Hav")
  
  trace_per_kategori <- trace_per_kategori[match(ord, names(trace_per_kategori))]
  
  # Skapar plot
  fig <- plot_ly()
  trace_region <- c()
  
  # Loopar över kommunerna
  for (kommun in region) {
    df_k <- df %>% filter(municipality == kommun)
    
    fig <- fig %>%
      add_trace(
        data = df_k,
        x = ~title,
        y = ~value,
        type = "bar",
        marker = list(color = kategori_col[df_k$title]),
        name = kommun,
        visible = ifelse(kommun == region[1], TRUE, FALSE),
        hovertemplate = paste0(
          "%{y:.1f} %"
        )
      )
    
    trace_region <- c(trace_region, kommun)
  }
  
  # Dropdown
  buttons <- lapply(region, function(reg) {
    vis_vec <- trace_region == reg
    
    list(
      method = "update",
      args = list(
        list(visible = vis_vec),
        list(title = paste0("<b>Andel skyddad natur – ", reg, " (", latest_year, ")</b>"))
      ),
      label = reg
    )
  })
  
  # Layout 
  fig <- fig %>%
    layout(
      barmode = "group",
      hovermode = "x unified",
      title = paste0("<b>Andel skyddad natur – ", region[1], " (", latest_year, ")</b>"),
      font = list(family = "sourcesanspro", size=18),
      xaxis = list(title = "",tickfont = list(size = 18)),
      yaxis = list(title = "<b>Andel (%)</b>", range = c(0, 100)),
      showlegend = FALSE,
      updatemenus = list(list(
        active = 0,
        buttons = buttons,
        direction = "down",
        x = 0,
        xanchor = "center",
        y = 1.15,
        yanchor = "top"
      )),
      annotations = list(
        text = "Källa: SCB",
        x = 0,
        y = -0.08,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 12)
      )
    ) %>%
    plotly::config(
      modeBarButtonsToRemove = c('zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d'),
      displaylogo = FALSE
    )
  
  fig
  
}

# Avstånd till skyddad natur
avstand_skyddadnatur <- function(){
  # Läser in data
  df <- read.csv('Data/df_avstand_natur.csv') %>% 
    filter(year == max(year))

  # Barplot
  p <- ggplot(df, aes(x = municipality, y=value))+ 
          geom_col(position="dodge",fill="#B81867")+ 
    labs(title= paste("Medelavstånd till skyddad natur år",max(df$year)),
         x = "",y='Kilometer', caption = 'Källa: SCB')+ 
    theme(axis.text.x = element_text(angle = 90),
          legend.position="none",
          text = element_text(family = "sourcesanspro", size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          axis.text.x.bottom  = element_text(angle = 45, hjust=1),
          plot.caption = element_text(hjust = 0, vjust=4))
  p
  # sparar som svg
  ggsave('Figurer/avstand_skyddadnatur.svg',plot = p,device = "svg", width = 7, height = 6)
  
  
  ggsave('Figurer/avstand_skyddadnatur.png',plot = p,device = "png", width = 7, height = 6, dpi =96)
  
}

# Ekologisk mark
ekomark <- function(){
  # Läser in data
  df <- read.csv('Data/df_eko.csv')
  
  # filtrerar data och skapar split på andel
  df <- df %>% filter(!is.na(value)) %>% filter(year == max(year))
  
  # Skapar barplot
  p <- ggplot(df, aes(x = municipality, y=value))+ 
    geom_col(position="dodge",fill="#B81867")+ 
    labs(title= paste("Ekologiskt brukad åkermark år", max(df$year)),
         x = "",y='Andel (%)', caption = 'Källa: Jordbruksverket')+ 
    ylim(0,50)+
    theme(axis.text.x = element_text(angle = 90),
          legend.position="none",
          text = element_text(family = "sourcesanspro", size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          axis.text.x.bottom  = element_text(angle = 45, hjust=1),
          plot.caption = element_text(hjust = 0, vjust=4))
  p
  # sparar som svg
  ggsave('Figurer/eko_mark.svg',plot = p,device = "svg", width = 7, height = 6)
  
  
  ggsave('Figurer/eko_mark.png',plot = p,device = "png", width = 7, height = 6, dpi =96)
}

# betesmark
betesmark <- function(){
  # Läser in data
  df <- read.csv('Data/df_betesmark.csv') %>% filter(!is.na(year)) %>% 
    filter(year == max(year))
  
  # Ändrar titlarna till det kortare
  df$title <- ifelse(grepl('andel',df$title )==T, 'Andel (%)', 'Hektar')
  
  # Skapar plot
  p <- ggplot(df, aes(x = municipality, y=value))+ 
    geom_col(position="dodge",fill="#B81867")+ facet_wrap(~title, ncol=1, scales = 'free')+
    labs(title= paste("Total betesmark år",max(df$year)),
         x = "",y='', caption = 'Källa: Jordbruksverket')+ 
    theme(axis.text.x = element_text(angle = 90),
          legend.position="none",
          text = element_text(family = "sourcesanspro", size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          axis.text.x.bottom  = element_text(angle = 45, hjust=1),
          plot.caption = element_text(hjust = 0, vjust=4))
  p
  # sparar som svg
  ggsave('Figurer/betesmark.svg',plot = p,device = "svg", width = 7, height = 8)
  
  
  
  ggsave('Figurer/betesmark.png',plot = p,device = "png", width = 7, height = 8, dpi =96)
  
}


#Slåtteräng
slatt_mark <- function(){
  # Läser in data
  df <- read.csv('Data/df_slatt.csv') %>% filter(!is.na(value)) %>% 
    filter(year == max(year))
  
  # Ändrar titlarna till det kortare
  df$title <- ifelse(grepl('andel',df$title )==T, 'Andel (%)', 'Hektar')
  
  # Skapar plot
  p <- ggplot(df, aes(x = municipality, y=value))+ 
    geom_col(position="dodge", fill="#B81867")+ facet_wrap(~title, ncol=1,scales = "free" )+
    scale_fill_manual(values = kommun_colors)+
    labs(title= paste("Slåtteräng år",max(df$year)),
         x = "",y='', caption = 'Källa: Jordbruksverket')+ 
    theme(axis.text.x = element_text(angle = 90),
          legend.position="none",
          text = element_text(family = "sourcesanspro", size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          axis.text.x.bottom  = element_text(angle = 45, hjust=1),
          plot.caption = element_text(hjust = 0, vjust=4))
  p
  # sparar som svg
  ggsave('Figurer/slatterang.svg',plot = p,device = "svg", width = 7, height = 8)
  
  ggsave('Figurer/slatterang.png',plot = p,device = "png", width = 7, height = 8, dpi =96)
}


########### Skyddade områden gis #########

# visar var alla skyddade områden är
skydd_karta <- function(){
  
  # Läser in data
  
  gml_file <- "Data/ProtectedSites/PS.protectedSites.NR.gml" 
  suppressMessages( suppressWarnings( 
    protected_sites <- st_read(gml_file, layer = "ProtectedSite",quiet = TRUE) ))
  gml_file <- "Data/ProtectedSites/PS.protectedSites.DVO.gml" 
  suppressMessages( suppressWarnings( 
    dvo_sites <- st_read(gml_file, layer = "ProtectedSite",quiet = TRUE) ))
  gml_file <- "Data/ProtectedSites/PS.protectedSites.KR.gml" 
  suppressMessages( suppressWarnings( 
    KR_sites <- st_read(gml_file, layer = "ProtectedSite",quiet = TRUE) ))
  
  # Shapefil för länet 
  shapefile_path <- "Data/Lan_Sweref99TM/Lan_Sweref99TM_region.shp" 
  suppressMessages( suppressWarnings( 
    lan_shape <- st_read(shapefile_path, quiet = TRUE) )) 
  
  # Tar ut länet 
  lan_shape <- lan_shape %>% filter(LnKod == lanskod) # Ta endast ut det som ligger i länet
  protected_sites <- st_transform(protected_sites, st_crs(lan_shape)) 
  dvo_sites <- st_transform(dvo_sites, st_crs(lan_shape)) 
  KR_sites <- st_transform(KR_sites, st_crs(lan_shape)) 
  
  # Ta endast ut det som ligger i länet
  protected_sites_lan <- st_intersection(protected_sites, lan_shape) 
  dvo_sites <- st_intersection(dvo_sites, lan_shape) 
  KR_sites <- st_intersection(KR_sites, lan_shape) 
  
  
  # Rätt format 
  
  protected_sites_lan <- st_transform(protected_sites_lan, 4326) 
  dvo_sites <- st_transform(dvo_sites, 4326) 
  KR_sites <- st_transform(KR_sites, 4326)
  lan_shape <- st_transform(lan_shape, 4326)
  
  # Färgschema
  site_colors <- list("Naturreservat" = "#4AA271",
                      "Djur- och växtskyddsområde" = "#D57667",
                      "Kulturreservat" = "#F9B000")
  
  # Label options
  my_label_options <- labelOptions(
    direction = "auto",
    style = list(
      "font-size" = "14px",
      "color" = "black",
      "background-color" = "white",
      "padding" = "2px 4px"
    )
  )
  # Skapar karta
  map <-leaflet() %>% 
    addTiles() %>%
    
    # Add polygons
    addPolygons(data = lan_shape, color = "#B81867", fill = FALSE, weight = 2) %>% 
    
    addPolygons(data = protected_sites_lan,
                color = site_colors$Naturreservat,    
                fillOpacity = 0.5, weight = 3, label = ~text, group="Naturreservat",
                labelOptions = my_label_options) %>% 
    
    addPolygons(data = dvo_sites,
                color = site_colors$`Djur- och växtskyddsområde`,
                fillOpacity = 0.5, weight = 4, label = ~text, group="Djur- och växtskyddsområde",
                labelOptions = my_label_options) %>% 
    
    addPolygons(data = KR_sites,
                color = site_colors$Kulturreservat,
                fillOpacity = 0.5, weight = 4, label = ~text, group="Kulturreservat",
                labelOptions = my_label_options) %>% 
    
    # Add legend
    addLegend(position = "topright",
              colors = site_colors,
              labels = names(site_colors),
              title = "Skyddade områden")
  map <- map %>%
    htmlwidgets::prependContent(
      htmltools::tags$style(
        ".info.legend { text-align: left !important; }"
      )
    )
  map
  
  
}
########### Vatten ########

eko_vatten <- function(){
  # Läser in data
  df <- read.csv('Data/df_ekovatten.csv') %>% filter(!is.na(value)) %>% 
    filter(year == max(year), title != "Kustvatten med god ekologisk status, andel (%)",
           value > 0)

  # Tar bort andel från titel
  df$title <- str_remove(df$title,', \\s*andel\\s*\\(%\\)')
  
  for(t in unique(df$title)){
    # Filtrerar ut titeln
    temp <- df %>% filter(title==t)
    
  # Skapar plot
  p <- ggplot(temp, aes(x = municipality, y=value))+ 
    geom_col(position="dodge", fill="#B81867")+
    labs(title= paste(t, 'år',max(df$year)),
         x = "",y='Andel (%)', caption = 'Källa: Jordbruksverket')+ 
    theme(axis.text.x = element_text(angle = 90),
          legend.position="none",
          text = element_text(family = "sourcesanspro", size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          axis.text.x.bottom  = element_text(angle = 45, hjust=1),
          plot.caption = element_text(hjust = 0, vjust=4))
  print(p)
  # sparar som svg
  text <- str_split(t, " ")[[1]][1]
  ggsave(paste0('Figurer/eko_vatten_',text
                ,'.svg'),plot = p,device = "svg", width = 7, height = 6)
  
  
  ggsave(paste0('Figurer/eko_vatten_',text
                ,'.png'),plot = p,device = "png", width = 7, height = 6, dpi=96)
  
  }
  
  
}
  

############ Föroreningar ##########
############# Kalmarksareal #########

kalmark <- function(){
  
  # Läser in data och ta bort siffror från länsnamn
  df <- read.csv("Data/df_kalmarksareal.csv") %>% 
    mutate(value = X8a..Medel..median.och.95.e.percentilen.i.hektar.för.sammanhängande.kalmarksareal)
  df$Region <- str_remove_all(df$Region, "[0-9]+ ")

  # Spara bara percentil på median för uppsala, plotta alla linjer som grå förutom uppsala i ploly så att man kan markera och se vilken 
  # linje som är vilken
  
  # Tar ut uppsala
  df_uppsala <- df %>% filter(Region == "Uppsala län",Variabel  == 'Median')
  df_median <-   df %>% filter(Region != "Uppsala län", Variabel  == 'Median')
  
  
  
  # Skapar plot
  fig <- plot_ly()
  
  #  Lägg till alla län (grå linjer)
  for (region in unique(df_median$Region)) {
    
    temp <- df_median %>% filter(Region == region)
    
    fig <- fig %>%
      add_trace(
        data = temp,
        x = ~År,
        y = ~value,
        type = "scatter",
        mode = "lines",
        line = list(color = "#6F787E", width = 1.2),
        name = region,
        hoverinfo = "text",
        text = I(paste(temp$Region, "<br>År:",temp$År, "<br>Median:", round(temp$value, 1))),
        showlegend = FALSE
      )
  }
  
  # Lägg till Uppsala median
  fig <- fig %>%
    add_trace(
      data = df_uppsala ,
      x = ~År,
      y = ~value,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#B81867", width = 5),
      marker = list(color = "#B81867", size = 8),
      hoverinfo = "text",
      text = I(paste(df_uppsala$Region,"<br>År:",temp$År, "<br>Median:", round(df_uppsala$value, 1)))
    )

  #  Layout
  fig <- fig %>%
    layout(font = list(family = "sourcesanspro", size=18),
           margin = list(t = 50),
           title = list(text = paste("<b>Medianen för sammanhängande kalmarksareal<b>"), y = 0.95, x = 0.55),
           xaxis = list(title = "", tickangle = -45,
                        tickmode = "linear",         
                        dtick = 2),
           yaxis = list(title = "<b>Hektar<b>", 
                        rangemode = "tozero"),
           annotations = list(
             text ='Källa: Skogsstyrelsen',
             x = 0,            
             y = -0.2,        
             xref = "paper",
             yref = "paper",
             xanchor = "left",
             yanchor = "bottom",
             showarrow = FALSE,
             font = list(size = 12)
           )
    )   
  
  # tar bort plotly-funktioner
  fig <- plotly::config(
    fig,
    modeBarButtonsToRemove = c(
      'zoom2d',     # zoom button
      'pan2d',      # pan button
      'select2d',   # box select
      'lasso2d',    # lasso select
      'zoomIn2d',   # zoom in
      'zoomOut2d'   # zoom out
    ),
    displaylogo = FALSE)   # remove plotly logo/link
  
  
  fig
      
}

############# Produktiv skogsmarksareal #########

prod_skog <- function(){
  
  # Läser in data och tar bort siffrar från kommun-namn
  df <- read.csv('Data/df_prod_skog.csv') %>% rename('Value' = X12..Deklarerad.produktiv.skogsmarksareal..1.000.ha...medelinnehav.och.medianinnehav.i.hektar.per.brukningsenhet..antal.brukningsenheter.och.ägare)
  
  df <- df %>%
    mutate(
      Kommun = factor(str_to_sentence(str_remove_all(Kommun, "[0-9]+ ")),
                      levels = sort(unique(str_to_sentence(str_remove_all(Kommun, "[0-9]+ "))))
      )
    )
  
  # Ordnar data
  df <- df %>% mutate(År = factor(År, levels = unique(År)))
  
  # variabler till plott  
  titles <- unique(df$Variabel)
  
  n_region <- length(unique(df$Kommun))
  
  # Bygg plotly-objekt
  fig <- plot_ly( height = 700)
  
  # loop över alla variabler och kommuner
  for (title in titles) {
    df_title <- df %>% filter(Variabel == !!title)
    
    for (region in levels(df_title$Kommun)) {
      # Filtrerar ut data och lägger in trace
      df_region <- df_title %>% filter(Kommun == region)
      
      fig <- fig %>%
        add_trace(
          x = df_region$År,
          y = df_region$Value,
          type = "scatter",
          mode = "lines+markers",
          name = region,
          line = list(color = kommun_colors[region],width = 5),
          marker = list(color = kommun_colors[region],size = 8),
          visible = ifelse(title == titles[1], TRUE, FALSE)
        )
    }
  }
  
  # Skapa dropdown-knappar
  buttons <- lapply(seq_along(titles), function(i) {
    visible_vec <- rep(FALSE, length(titles)*n_region)
    visible_vec[((i-1)*n_region + 1):(i*n_region)] <- TRUE
    
    list(
      method = "update",
      args = list(
        list(visible = visible_vec),
        list(
          title = paste("<b>",titles[i],"<b>"),
          yaxis = list(
            title = paste("<b>",ifelse(titles[i] %in% c("Medelbrukningsenhet", "Medianbrukningsenhet"),
                       "Hektar per brukningsenhet",titles[i] ),"<b>"),
            rangemode =  "tozero"
          )
        )
      ),
      label = titles[i]
    )
  })
  
  #  Layout
  fig <- fig %>%
    layout(font = list(family = "sourcesanspro", size=18),
           margin = list(t = 50),
           title = list(text = paste("<b>",titles[1],"<b>"), y = 0.95, x = 0.55),
           xaxis = list(title = "", tickangle = -45,
                        dtick = 2),
           yaxis = list(title = paste("<b>",titles[1],"<b>"), 
                        rangemode = "tozero"),
           hovermode = 'x unified',
           updatemenus = list(
             list(
               y = -0.1,
               x=1.1,
               buttons = buttons,
               direction = "up"
             )),
           annotations = list(
             text ='Källa: Skogsstyrelsen',
             x = 0,            
             y = -0.15,        
             xref = "paper",
             yref = "paper",
             xanchor = "left",
             yanchor = "bottom",
             showarrow = FALSE,
             font = list(size = 12)
           )
    )
  
  
  # tar bort plotly-funktioner
  fig <- plotly::config(
    fig,
    modeBarButtonsToRemove = c(
      'zoom2d',     # zoom button
      'pan2d',      # pan button
      'select2d',   # box select
      'lasso2d',    # lasso select
      'zoomIn2d',   # zoom in
      'zoomOut2d'   # zoom out
    ),
    displaylogo = FALSE)   # remove plotly logo/link
  
  fig
  
}




########### Produktionsanläggningar ###########
prod_karta <- function(){
  
  # Läser in data
  
  gml_file <- "Data/prodanlagg/pf.ProductionFacility.SWE.EPSG4258.gml" 
  suppressMessages( suppressWarnings( 
    df <- st_read(gml_file,quiet = TRUE) ))
  
  # Shapefil för länet 
  shapefile_path <- "Data/Lan_Sweref99TM/Lan_Sweref99TM_region.shp" 
  suppressMessages( suppressWarnings( 
    lan_shape <- st_read(shapefile_path, quiet = TRUE) )) 
  
  # Tar ut länet 
  lan_shape <- lan_shape %>% filter(LnKod == lanskod) # Ta endast ut det som ligger i länet
  df <- st_transform(df, st_crs(lan_shape)) 

  
  
  # Ta endast ut det som ligger i länet
  df <- st_intersection(df, lan_shape) 
  
  doc <- read_xml("Data/prodanlagg/LCPEPRTR_2020_20210924_version_1.gml")
  
  # Anläggningar
  facilities <- xml_find_all(doc, ".//ProductionFacilityReport")
  
  # Funktion som plockar ut emissioner till dataframe
  emissions_list <- lapply(facilities, function(fac) {
    facility_id <- xml_text(xml_find_first(fac, ".//InspireId/localId"))
    
    releases <- xml_find_all(fac, ".//pollutantRelease")
    if(length(releases) == 0) return(NULL)
    
    data.frame(
      localId = facility_id,
      medium = xml_text(xml_find_all(releases, ".//mediumCode")),
      pollutant = xml_text(xml_find_all(releases, ".//pollutant")),
      value = as.numeric(xml_text(xml_find_all(releases, ".//totalPollutantQuantityKg"))),
      stringsAsFactors = FALSE
    )
  })
  
  # Slår ihop rader från listan
  emissions_df <- bind_rows(emissions_list)
  
  # summerar alla varje sorts utsläpp
  emissions_df <- emissions_df %>%
    group_by(localId) %>%
    summarise(value = sum(value), .groups = "drop") 

  
  # Join to spatial df
  df <- df %>%
    left_join(emissions_df, by = "localId")
  
  # Transform CRS for Leaflet
  df <- st_transform(df, 4326)
  lan_shape <- st_transform(lan_shape, 4326)

  
  # Färgschema och hover text options
  pal <- colorNumeric(
    palette = c("white", "#D0342C"),  
    domain = df$value,
    na.color = "#6F787E"         
  )
  
  my_label_options <- labelOptions(
    direction = "auto",
    style = list(
      "font-size" = "12px",
      "color" = "black",
      "background-color" = "white",
      "padding" = "2px 4px"
    )
  )
  
  leaflet() %>% 
    addTiles() %>%
    addPolygons(data = lan_shape, color = "#B81867", fill = FALSE, weight = 3) %>% 
    addPolygons(data = df, opacity = 1,  weight = 10,color = ~pal(value),
                label = ~paste0(name, " Totalt utsläpp: ", value),
  labelOptions = my_label_options) %>%
    addLegend(
      pal = pal,
      values = df$value,
      title = "Totalt utsläpp (kg)",
      position = "bottomright"
    )
  
}

########## Naturgeografiska regioner ###########

naturgeo_karta <- function(){
  
  # Läser in data
  
  gml_file <- "Data/Biogeografiska/SE_BR_NaturgeografiskaRegioner.gml" 
  suppressMessages( suppressWarnings( 
    df <- st_read(gml_file, quiet = TRUE) ))
 
  
  # Shapefil för länet 
  shapefile_path <- "Data/Lan_Sweref99TM/Lan_Sweref99TM_region.shp" 
  suppressMessages( suppressWarnings( 
    lan_shape <- st_read(shapefile_path, quiet = TRUE) )) 
  
  # Tar ut länet 
  lan_shape <- lan_shape %>% filter(LnKod == lanskod) # Ta endast ut det som ligger i länet
  df <- st_transform(df, st_crs(lan_shape)) 
  
  # Ta endast ut det som ligger i länet
  df <- st_intersection(df, lan_shape) 
 
  
  
  # Rätt format 
  
  df <- st_transform(df, 4326) 
  lan_shape <- st_transform(lan_shape, 4326)
  
  # Färgschema
  area_names <- sort(unique(df$name))  
  site_colors <- c("#019CD7","#4AA271","#F9B000")[1:length(area_names)]
  names(site_colors) <- area_names
  
  pal <- colorFactor(palette = site_colors, domain = area_names)
  
  # Label options
  my_label_options <- labelOptions(
    direction = "auto",
    style = list(
      "font-size" = "14px",
      "color" = "black",
      "background-color" = "white",
      "padding" = "2px 4px"
    )
  )
  # Skapar karta
  leaflet() %>% 
    addTiles() %>%
    
    # Add polygons
    addPolygons(data = lan_shape, color = "#B81867", fill = FALSE, weight = 2) %>% 
    
    addPolygons(data = df,   
                fillOpacity = 0.5, weight = 3, label = ~name,color=~pal(name),
                labelOptions = my_label_options)%>% 
    
    # Add legend
    addLegend(position = "topright",
              colors = site_colors,
              labels = names(site_colors),
              title = "Biogeografiska regioner")
  
  
  
}






############ Grundvatten ########


grundvatten <- function(){
  
  gml_file <- "Data/grundvatten/grundvattenkvalitet_analysresultat_provplatser.gpkg" 
  suppressMessages( suppressWarnings( 
    df <- st_read(gml_file,quiet = TRUE) ))
  
  # Shapefil för länet 
  shapefile_path <- "Data/Lan_Sweref99TM/Lan_Sweref99TM_region.shp" 
  suppressMessages( suppressWarnings( 
    lan_shape <- st_read(shapefile_path, quiet = TRUE)  %>% filter(LnNamn== 'Uppsala')))
  
  
  
  
  # Tar endast ut punkter i länet
  df <- df %>% filter(lanskod == !!lanskod)
  
  # Fixar format
  df <- st_transform(df, 4326)
  lan_shape <- st_transform(lan_shape, 4326)
  df <- st_cast(df, "POINT")
  
  # Filtrera bort rader utan värden
  df <- df[!is.na(df$positionskvalitet_tx), ]
  
  # Datumgrupper
  df$tdat <- as.Date(df$tdat)
  df$tdat_group <- ifelse(df$tdat > ymd("2019-12-31"), '2020 eller senare', 'Innan 2020')
  
  # Ordning och färger
  df$positionskvalitet_tx <- factor(df$positionskvalitet_tx,
                                    levels = c("<20 m","<50 m","<100 m", "<250 m", "osäkert"))
  colors <- c("<20 m" = "#4AA271",
              "<50 m" ="#F9B000",
              "<100 m" = "#E67E22",
              "<250 m" = "#D57667",
              "osäkert" =  "#6F787E")
  
  pal <- colorFactor(palette =  unname(colors),
                     domain = names(colors),
                     ordered = TRUE )
  
  
  # Popup
  df$popup_text <- paste0(
    "<b>", df$provplatsnamn, "</b><br>",
    "Geografisk region enligt SGU:s bedöminsgrunder", df$region_bdgr, "<br>",
    "Typ: ", df$provplatstyp_tx, "<br>",
    "Positionskvalitet: ", df$positionskvalitet_tx, "<br>",
    "Antal prover: ", df$antal_prov, "<br>",
    "Från datum: ", df$fdat, "<br>",
    "Till datum: ", df$tdat,"<br>",
    "Jordart:" ,df$jordart_tx,"<br>",
    "Bergart:" ,df$bergart_tx,"<br>",
    "Grundvattenmiljo enligt SGUs bedömingsgrunder:", df$gvmiljo_bedgr_tx,"<br>",
    "Geohydraliskt läge:",df$geohylag_tx
    
  )
  
  # Leaflet-kartan
  map <-leaflet(df) %>%
    addTiles() %>%
    addPolygons(data = lan_shape, color = "#B81867", fill = FALSE, weight = 2) %>% 
    addCircleMarkers(
      data = df %>% filter(tdat_group == 'Innan 2020'),
      popup = ~popup_text,
      group = 'Innan 2020',
      color = ~pal(positionskvalitet_tx),
      fillOpacity = 0.7, radius = 6, weight = 1
    ) %>%
    addCircleMarkers(
      data =  df %>% filter(tdat_group == '2020 eller senare'),
      popup = ~popup_text,
      group = '2020 eller senare',
      color = ~pal(positionskvalitet_tx),
      fillOpacity = 0.7, radius = 6, weight = 1
    ) %>%
    addLayersControl(
      overlayGroups = c('Innan 2020', '2020 eller senare'),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend(
      position = "bottomright",
      colors = colors,
      labels = names(colors),
      title = "Positionskvalitet",
      opacity = 1
    )
  
  # Fixar texten i legenden så den inte blir centrerad
  map <- map %>%
    htmlwidgets::prependContent(
      htmltools::tags$style(
        ".info.legend { text-align: left !important; }"
      )
    )
  
  map
  
}

grundvatten_test <- function(){
  
  # Läser in data
  df_res <- read.csv('Data/analys_grundvatten.csv') %>% 
    rename('nationellt_provplatsid' = properties.nationellt_provplatsid) %>% 
    filter(properties.provtyp=='grundvatten')
  
  df <- df %>% arrange(properties.inlamningsdat)
  
  df <- df %>% mutate(properties.provtagningsdat = as.Date(properties.provtagningsdat),
                      properties.inlamningsdat = as.Date(properties.inlamningsdat))
  
  # Skapar plot
  fig <- plot_ly()
  
  # Antal tester
  params <- sort(unique(sd_ts$data()$properties.param))
  
  # Enhet per test
  param_units <- sd_ts$data() %>%
    group_by(properties.param) %>%
    summarise(unit = first(na.omit(properties.enhet))) %>%
    ungroup()
  
  # lista för platser
  visible_states <- vector("list", length(params))
  
  # trace per test
  for (i in seq_along(params)) {
    
    p <- params[i]
    
    # filtrera gemensam data och lägg till trace
    fig <- fig %>%
      add_trace(
        data = sd_ts$data() %>% filter(properties.param == p),
        x = ~properties.inlamningsdat,
        y = ~properties.matvardetal,
        type = "scatter",
        mode = "lines+markers",
        name = p,
        visible = ifelse(i == 1, TRUE, FALSE),   
        connectgaps = FALSE 
      )
    
    # mask
    visible_vec <- rep(FALSE, length(params))
    visible_vec[i] <- TRUE
    visible_states[[i]] <- visible_vec
  }
  
  #  dropdown 
  fig <- fig %>%
    layout(
      title = "Tidsserie för vald parameter",
      xaxis = list(title = "Datum"),
      yaxis = list(title = param_units$unit[1]),   # default unit = first parameter
      updatemenus = list(
        list(
          type = "dropdown",
          x = 1.25,
          y = 1,
          buttons = lapply(seq_along(params), function(i) {
            list(
              method = "update",
              args = list(
                # Update visible traces
                list(visible = visible_states[[i]]),
                # Update axis label
                list(yaxis = list(title = param_units$unit[i]))
              ),
              label = params[i]
            )
          })
        )
      )
    )
  
  # tar bort plotly-funktioner
  fig <- plotly::config(
    fig,
    modeBarButtonsToRemove = c(
      'zoom2d',     # zoom button
      'pan2d',      # pan button
      'select2d',   # box select
      'lasso2d',    # lasso select
      'zoomIn2d',   # zoom in
      'zoomOut2d'   # zoom out
    ),
    displaylogo = FALSE)   # remove plotly logo/link
  
  
  fig
}


grundvattenniva <- function(){
  
  gml_file <- "Data/grundvatten_uppsala_sf.gpkg" 
  suppressMessages( suppressWarnings( 
    df <- st_read(gml_file,quiet = TRUE) ))
  
  # Shapefil för länet 
  shapefile_path <- "Data/Lan_Sweref99TM/Lan_Sweref99TM_region.shp" 
  suppressMessages( suppressWarnings( 
    lan_shape <- st_read(shapefile_path, quiet = TRUE)  %>% filter(LnNamn== 'Uppsala')))
  
  # Fixar format
  df <- st_transform(df, 4326)
  lan_shape <- st_transform(lan_shape, 4326)
  
  # Datumgrupper
  df$obsdatum <- as.Date(df$obsdatum)
  df$lastupdate <- as.Date(df$lastupdate)
  
  # Beräkna skillnad och medelvärde per station
  df_stats <- df %>%
    group_by(platsbeteckning) %>%
    arrange(obsdatum) %>%
    mutate(days = as.numeric(obsdatum - min(obsdatum))) %>%
    summarise(
      n_obs = n(),
      slope_m_per_year =
        if (sum(!is.na(grundvattenniva_m_o_h)) >= 10 &&
            (max(obsdatum) - min(obsdatum)) >= (365)*5)
          coef(lm(grundvattenniva_m_o_h ~ days))[2] * 365
      else
        NA,
      medelv_m_o_h = mean(grundvattenniva_m_o_h, na.rm = TRUE),
      first_date=first(obsdatum),
      last_date = last(obsdatum),
      last_niva_m_o_h = last(grundvattenniva_m_o_h),
      last_niva_m_urok = last(grundvattenniva_m_urok),
      last_niva_m_u_markyta = last(grundvattenniva_m_u_markyta),
      metod = last(metod_for_matning),
      anmarkning = last(nivaanmarkning),
      .groups="drop"
    )
  
  
  df_newest <- df_stats
  df_newest$tdat_group <- ifelse(df_newest$last_date > ymd("2009-12-31"), 
                                 '2010 eller senare', 'Innan 2010')
  
  df_newest$popup_text <- paste0(
    "<h4 style='margin:0; padding:0;'>Station ", df_newest$platsbeteckning, "</h4>",
    "Senaste mätning: ", df_newest$last_date, "<br><br>",
    
    "<b>Mätvärden:</b><br>",
    "M under rörets överkant: <b>", round(df_newest$last_niva_m_urok, 2), "</b><br>",
    "M över havet: <b>", round(df_newest$last_niva_m_o_h, 2), "</b><br>",
    "M under markytan: <b>", round(df_newest$last_niva_m_u_markyta, 2), "</b><br>",
    "<br>",
    
    "<b>Sammanfattning:</b><br>",
    "Medelvärde (m ö.h.): <b>", round(df_newest$medelv_m_o_h, 2), "</b><br>",
    "Första mätning: ", df_newest$first_date, "<br>",
    "Antal mätningar: ", df_newest$n_obs, "<br>",
    "<br>",
    
    "<b>Trend:</b><br>",
    "Lutning per år: ",
    ifelse(df_newest$slope_m_per_year > 0,
           paste0("<span style='color:green;'><b>+", 
                  round(df_newest$slope_m_per_year, 2), 
                  " m/år</b></span>"),
           paste0("<span style='color:red;'><b>",
                  round(df_newest$slope_m_per_year, 2), 
                  " m/år</b></span>")
    ),
    "<br><br>",
    
    "<b>Metadata:</b><br>",
    "Mätmetod: ", df_newest$metod, "<br>",
    "Anmärkning: ",
    ifelse(is.na(df_newest$anmarkning), "Saknas", df_newest$anmarkning), "<br>",
    
    "</div>"
  )
  
  # Färgpallett för skillnad (färgläggs efter förändring)
  min_val <- floor(min(df_newest$slope_m_per_year, na.rm = TRUE))
  max_val <- ceiling(max(df_newest$slope_m_per_year, na.rm = TRUE))
  
  # Skapa breaks som inkluderar 0
  if (min_val < 0 && max_val > 0) {
    # Skapa breaks för negativa värden (röd gradient)
    breaks_neg <- seq(min_val, 0, length.out = 4)
    # Skapa breaks för positiva värden (grön gradient)
    breaks_pos <- seq(0, max_val, length.out = 4)[-1]  # Ta bort 0 för att undvika duplikat
    breaks <- round(c(breaks_neg, breaks_pos),1)
    
    # Färgpaletter: mörkröd → ljusröd för negativa, ljusgrön → mörkgrön för positiva
    colors_neg <- colorRampPalette(c("#D57667","#F7E4E1"))(length(breaks_neg) - 1)
    colors_pos <- colorRampPalette(c("#DBECE3", "#4AA271"))(length(breaks_pos))
    my_colors <- c(colors_neg, colors_pos)
  } else if (max_val <= 0) {
    # Endast negativa värden
    breaks <- seq(min_val, max_val, length.out = 5)
    my_colors <- colorRampPalette(c("#D57667","#F7E4E1"))(length(breaks) - 1)
  } else {
    # Endast positiva värden
    breaks <- seq(min_val, max_val, length.out = 5)
    my_colors <- colorRampPalette(c("#DBECE3", "#4AA271"))(length(breaks) - 1)
  }
  
  pal <- colorBin(palette = my_colors, bins = breaks, 
                  domain = df_newest$slope_m_per_year, na.color = "black")
  
  df_newest$color <- pal(df_newest$slope_m_per_year)
  
  # Leaflet-karta
  map <- leaflet(df_newest) %>%
    addTiles() %>%
    addPolygons(data = lan_shape, color = "#B81867", fill = FALSE, weight = 2) %>% 
    addCircleMarkers(
      data = df_newest %>% filter(tdat_group == 'Innan 2010'),
      popup = ~popup_text,
      group = 'Innan 2010',
      fillColor = ~color,
      color = 'black',
      fillOpacity = 0.7, radius = 6, weight = 1
    ) %>%
    addCircleMarkers(
      data = df_newest %>% filter(tdat_group == '2010 eller senare'),
      popup = ~popup_text,
      group = '2010 eller senare',
      fillColor = ~color,
      color = 'black',
      fillOpacity = 0.7, radius = 6, weight = 1
    ) %>%
    addLayersControl(
      overlayGroups = c('Innan 2010', '2010 eller senare'),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = df_newest$slope_m_per_year,
      title = "Trend grundvattennivå<br><small>meter/år (m ö.h.)</small>",
      opacity = 1
    )
  
  # Fixar texten i legenden
  map <- map %>%
    htmlwidgets::prependContent(
      htmltools::tags$style(
        ".info.legend { text-align: left !important; }"
      )
    )
  
  map
  
}

