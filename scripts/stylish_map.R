## Stylish Map
# DAO
# 31/10/2026

# Prepare Space -----------------------------------------------------------

dev.off()
rm(list = ls())
pacman::p_load(tidyverse, scales, janitor, readxl, writexl, leaflet, sf)

# Cargar Datos ------------------------------------------------------------

# afluencia_desglose_stc <- read_csv('input/afluenciastc_desglosado_10_2025.csv', locale = locale(encoding = "utf-8"))
# afluencia_desglose_stc
# Encoding(afluencia_desglose_stc$linea) <- 'UTF-8'
# Encoding(afluencia_desglose_stc$estacion) <- 'UTF-8'
# afluencia_desglose_stc <- afluencia_desglose_stc %>% mutate(linea = str_replace(linea, 'LÃ­nea', 'Line' ))

afluencia_fix <- read_csv('output/clave_afluencia.csv')

# Cargar Mapa -------------------------------------------------------------

mapa_metro <- read_sf('~/Desktop/Mapas/stcmetro_shp/STC_Metro_estaciones_utm14n.shp') %>% clean_names()
lineas_metro <- read_sf('~/Desktop/Mapas/stcmetro_shp/STC_Metro_lineas_utm14n.shp') %>% clean_names()
mapa_cdmx <- read_sf('~/Desktop/Mapas/alcaldías_cdmx/alcaldias_cdmx.shp') %>% clean_names()

# Analysis ----------------------------------------------------------------


## Base Map
mapa_metro %>% as.data.frame() %>% count(nombre, linea, est)

metro_bb <- mapa_metro %>% st_bbox()

st_crs(mapa_cdmx) == st_crs(mapa_metro)

ggplot()+
  geom_sf(data = mapa_cdmx)+
  geom_sf(data = mapa_metro, aes(color = linea))+
  coord_sf(
    xlim = c(metro_bb["xmin"], metro_bb["xmax"]),
    ylim = c(metro_bb["ymin"], metro_bb["ymax"])
  )
  
## Estaciones con mayor afluencia promedio

mapa_metro <- mapa_metro %>% left_join(afluencia_fix %>% select(-n, -n_mojibake) %>% rename(pd = n_plain))

# afluencia_desglose_stc %>% as.data.frame() %>% count(linea, estacion) 


mapa_metro %>% arrange(-pd) %>% mutate(per = pd/sum(pd) * 100) %>% as.data.frame() %>% 
  mutate(cumsum(per))


summary(mapa_metro$pd)
mapa_metro %>% as.data.frame() %>% 
  ggplot()+
  geom_histogram(aes(x = pd))

ggplot()+
  geom_sf(data = mapa_cdmx)+
  geom_sf(data = mapa_metro, aes(color = linea, size = pd), alpha = 0.4)+
  geom_sf(data = mapa_metro , aes(color = linea))+
  labs(title = 'Subway lines are unequal in their usage.',
       subtitle = 'The most used stations lie in the outer ring of the city.', 
       caption = 'Source: Mexico City Open Data Portal', 
       color ='Line', size = 'Average Daily Tix')+
  # geom_sf(data = mapa_metro %>% slice_max(order_by = pd, n = 10), aes(color = linea, size = pd))+
  coord_sf(
    xlim = c(metro_bb["xmin"], metro_bb["xmax"]),
    ylim = c(metro_bb["ymin"], metro_bb["ymax"])
  )+
  theme_minimal()+
  scale_color_manual( values = c( "01" = "#F04E98",  # pink
                       "02" = "#005CAB",  # blue
                       "03" = "#B0A32A",  # olive green
                       "04" = "#6FBAA5",  # teal
                       "05" = "#FFD100",  # yellow
                       "06" = "#DA291C",  # red
                       "07" = "#E87722",  # orange
                       "08" = "#009A44",  # green
                       "09" = "#5C3317",  # brown
                       "12" = "#C09A2A",  # gold
                       "A"  = "#981D97",  # purple
                       "B"  = "#B2B4B2"   # gray-green
  ))+
  theme(
    # Background
    plot.background = element_rect(fill = "#f5f5f5", color = NA),
    panel.background = element_rect(fill = "#f5f5f5", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.title = element_text(size = 24, family = 'Helvetica', face = 'bold',margin = margin(b = 8),color = "#1a1a1a"
    ),
    
    plot.subtitle = element_text(size = 14, family = 'Helvetica',margin = margin(b = 20),color = "#4a4a4a"
    ),
    
    plot.caption = element_text(size = 9,family = 'Helvetica',color = "#666666",hjust = 1,margin = margin(t = 15)
    ),
    
    axis.text.x = element_blank(),
    
    axis.text.y = element_blank()
    )



# Leaflet -----------------------------------------------------------------


metro_colors <- c(
  "01" = "#F04E98",  # pink
  "02" = "#005CAB",  # blue
  "03" = "#B0A32A",  # olive green
  "04" = "#6FBAA5",  # teal
  "05" = "#FFD100",  # yellow
  "06" = "#DA291C",  # red
  "07" = "#E87722",  # orange
  "08" = "#009A44",  # green
  "09" = "#5C3317",  # brown
  "12" = "#C09A2A",  # gold
  "A"  = "#981D97",  # purple
  "B"  = "#B2B4B2"   # gray-green
)

pal <- colorFactor(
  palette = unname(metro_colors),
  domain = names(metro_colors)
)

# Ensure the data use WGS84 coordinates
mapa_cdmx_leaflet <- st_transform(mapa_cdmx, 4326)
mapa_metro_leaflet <- st_transform(mapa_metro, 4326)
lineas_metro_leaflet <- st_transform(lineas_metro, 4326)
lineas_metro_leaflet <- st_make_valid(lineas_metro_leaflet)
# Fixing stuff
lineas_metro_leaflet <- lineas_metro_leaflet %>% mutate(lt = as.numeric(linea)) %>% 
  mutate(linea = ifelse(is.na(lt), linea, str_pad(lt, 2,'0', side = 'left')))

# Scale point sizes for visualization
pd_range <- range(mapa_metro_leaflet$pd, na.rm = TRUE)

# Comma for the numbering
mapa_metro_leaflet <- mapa_metro_leaflet %>% mutate(pd_t = comma(pd, accuracy = 0.1))

pacman::p_load(maplibre)

leaflet() %>%
  addTiles() %>%
  # CDMX map
  addPolygons(
    data = mapa_cdmx_leaflet,
    fill = T,
    color = "black", 
    fillColor = 'lightgrey',
    fillOpacity = 0.5, 
    weight = 1
  ) %>%
  # Lines
  addPolylines(data = lineas_metro_leaflet,
               color = ~pal(linea), 
               weight = 3, 
               fillOpacity = 1) %>% 
  # All metro stations / observations
  addCircleMarkers(
    data = mapa_metro_leaflet,
    radius = ~scales::rescale(pd, to = c(2, 8), from = pd_range),
    color = ~pal(linea),
    fillColor = ~pal(linea),
    fillOpacity = 0.7,
    stroke = FALSE,
    group = "All observations",
    popup = ~paste0(
      "Line: ", linea,
      "<br>Daily Average: ", pd_t
    ), 
    label = ~paste0(
      "Line: ", linea,
      " | Daily Average: ", scales::comma(pd)
    ), 
    labelOptions = labelOptions(
      direction = "auto",
      opacity = 0.95,
      textsize = "14px",
      style = list(
        "font-family" = "Arial",
        "font-weight" = "bold",
        "padding" = "8px 12px"
      ))
  ) %>%
  
  # Top 10 observations by pd
  addCircleMarkers(
    data = mapa_metro_leaflet %>%
      slice_max(order_by = pd, n = 10, with_ties = FALSE),
    radius = ~scales::rescale(pd, to = c(5, 14), from = pd_range),
    color = ~pal(linea),
    fillColor = ~pal(linea),
    fillOpacity = 0.9,
    weight = 10,
    stroke = TRUE,
    group = "Top 10",
    popup = ~paste0(
      "Line: ", linea,
      "<br>Daily Average: ", pd_t
    ), 
    label = ~paste0(
      "Line: ", linea,
      " | Daily Average: ", scales::comma(pd)
    ), 
    labelOptions = labelOptions(
      direction = "auto",
      opacity = 0.95,
      textsize = "14px",
      style = list(
        "font-family" = "Arial",
        "font-weight" = "bold",
        "padding" = "8px 12px"
      ))
  ) %>%
  
  # Layer controls
  addLayersControl(
    overlayGroups = c(
      "All observations",
      "Top 10"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) 




