## Afluencia STC
# DAOA
# 01/12/2025

# Preparar Espacio --------------------------------------------------------

pacman::p_load(tidyverse, scales, janitor, readxl)
rm(list = ls())

# Cargar Datos ------------------------------------------------------------

afluencia_simple_stc <- read_csv('input/afluenciastc_simple_10_2025.csv')
afluencia_desglose_stc <- read_csv('input/afluenciastc_desglosado_10_2025.csv')
afluencia_mb <- read_csv('input/afluenciamb_desglosado_10_2025.csv')
afluencia_rtp <- read_csv('input/afluenciartp_desglosado_10_2025.csv')
afluencia_cb <- read_csv('input/afluencia_desglosada_cb_10_2025.csv')
afluencia_tr <- read_csv('input/afluencia_desglosada_trolebus_10_2025.csv')
afluencia_tl <- read_csv('input/afluencia_desglosada_tl_10_2025.csv')


Encoding(afluencia_desglose_stc$linea) <- 'UTF-8'
afluencia_simple_stc <- afluencia_simple_stc %>% mutate(linea = str_replace(linea, 'LÃ­nea', 'Line' ))

# Plot --------------------------------------------------------------------

plt_data <- afluencia_simple_stc %>% mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
  group_by(fecha) %>% 
  summarise(afluencia = sum(afluencia))


plt_data %>% filter(fecha>='2021-01-01') %>% ggplot(aes(fecha, afluencia))+
  geom_line(linewidth = 2)+
  geom_point(data = . %>% filter(month(fecha)==10))+
  geom_text(data = . %>% filter(month(fecha)==10), aes(label = comma(afluencia/100000)), vjust = -1, 
            size = 8, family = 'Helvetica')+
  geom_vline(xintercept = seq.Date(as.Date('2021-01-01'), as.Date('2025-02-01'), by = 'year'))+
  theme_minimal()+
  labs(x = '', y = '', title = 'Subway Ticket Sales have increased almost twofold since 2021', 
       subtitle = 'CDMX Subway Ridership (Ticket Sales in Millions)',
       caption = 'Source: CDMX Open Data Portal')+
  theme(
    
    
    plot.background = element_rect(fill = "#f5f5f5", color = NA),
    panel.background = element_rect(fill = "#f5f5f5", color = NA),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Axis lines - only x and y axis
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    
    plot.title =  element_text(size = 24, family = 'Helvetica'),
    plot.subtitle =  element_text(size = 14, family = 'Helvetica'),
    axis.text.x =  element_text(size = 14, family = 'Helvetica'),
    axis.text.y =  element_blank()
  )+
  ylim(50000000,130000000)


plt_data %>% 
  filter(fecha >= '2021-01-01') %>% 
  ggplot(aes(fecha, afluencia)) +
  
  # Main line with color
  geom_line(linewidth = 2, color = "#D96533") +  # CDMX Metro pink
  
  # Points at October (year markers)
  geom_point(data = . %>% filter(month(fecha) == 10), 
             color = "#D96533", size = 3) +
  
  # Labels
  geom_text(data = . %>% filter(month(fecha) == 10), 
            aes(label = comma(afluencia/1000000, accuracy = 1)),  # Fixed: divide by 1M for millions
            vjust = -1, 
            size = 5, 
            family = 'Helvetica',
            fontface = 'bold',
            color = "#2b2b2b") +
  
  # Year dividers
  geom_vline(xintercept = seq.Date(as.Date('2021-01-01'), 
                                   as.Date('2025-02-01'), 
                                   by = 'year'),
             color = "#333333",
             linewidth = 0.8, linetype = 'dashed') +
  
  # Scales
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y",
               expand = expansion(mult = c(0.02, 0.02))) +
  
  scale_y_continuous(limits = c(50000000, 130000000),
                     expand = expansion(mult = c(0, 0.05))) +
  
  # Labels
  labs(x = '', 
       y = '', 
       title = 'Subway Ticket Sales have increased almost twofold since 2021', 
       subtitle = 'Monthly CDMX Subway Ridership (Ticket Sales in Millions)',
       caption = 'Source: CDMX Open Data Portal') +
  
  # Theme
  theme_minimal() +
  theme(
    # Backgrounds
    plot.background = element_rect(fill = "#f5f5f5", color = NA),
    panel.background = element_rect(fill = "#f5f5f5", color = NA),
    
    # Remove grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Axis lines
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    
    # Text
    plot.title = element_text(size = 24, 
                              family = 'Helvetica', 
                              face = 'bold',
                              margin = margin(b = 8),
                              color = "#1a1a1a"),
    
    plot.subtitle = element_text(size = 14, 
                                 family = 'Helvetica',
                                 margin = margin(b = 20),
                                 color = "#4a4a4a"),
    
    plot.caption = element_text(size = 9,
                                family = 'Helvetica',
                                color = "#666666",
                                hjust = 1,
                                margin = margin(t = 15)),
    
    axis.text.x = element_text(size = 14, 
                               family = 'Helvetica',
                               color = "#2b2b2b"),
    
    axis.text.y = element_blank(),
    
    axis.ticks = element_blank(),
    
    # Margins
    plot.margin = margin(20, 30, 20, 20)
  )


# Por sistema -------------------------------------------------------------


totales_sistema <- left_join(afluencia_mb %>% mutate(fecha = floor_date(fecha, unit = 'year'))%>% group_by(fecha) %>% 
  summarise(mb = sum(afluencia)),

afluencia_rtp %>% mutate(fecha = floor_date(fecha, unit = 'year'))%>% group_by(fecha) %>% 
  summarise(rtp = sum(afluencia, na.rm = T))) %>% left_join(

afluencia_tl %>% mutate(fecha = floor_date(fecha, unit = 'year'))%>% group_by(fecha) %>% 
  summarise(tren = sum(afluencia, na.rm = T))) %>% left_join(

afluencia_tr %>% mutate(fecha = floor_date(fecha, unit = 'year'))%>% group_by(fecha) %>% 
  summarise(trole = sum(afluencia, na.rm = T))) %>% left_join(

afluencia_simple_stc %>% mutate(fecha = floor_date(fecha, unit = 'year'))%>% group_by(fecha) %>% 
    summarise(stc = sum(afluencia, na.rm = T))
)




totales_sistema %>% mutate(electricos = tren+trole) %>% 
  select(-tren,-trole) %>% 
  pivot_longer(cols = !fecha) %>% filter(fecha=='2024-01-01') %>% 
  mutate(name = case_when(name == 'mb' ~ 'Metrobus (BRT)', 
                          name == 'stc' ~ 'Subway', 
                          name == 'electricos' ~ 'Trolley Service + Light Rail', 
                          name == 'rtp' ~ 'Organized Bus System'
                          )) %>% 
  ggplot(aes(reorder(name, -value), value, fill = name))+
  geom_col(color ='black')+
  geom_text(aes(label = comma(value/100000)), color = 'black', size = 12, family = 'Helvetica', vjust = -0.5)+
  labs(x = '', y = '', caption = 'Source: CDMX Open Data Portal', 
       title = 'The subway constitutes over 50% of the City\'s public transit ticket sales', 
       subtitle = 'Ticket Sales in 2024 (in millions)')+
  scale_fill_manual(values = c(
                               '#B72B34', 
                               '#85BA41',
                               '#EC5F2A',
                               '#69C0DE' 
                               ))+
  # Theme
  theme_minimal() +
  theme(
    # Backgrounds
    plot.background = element_rect(fill = "#f5f5f5", color = NA),
    panel.background = element_rect(fill = "#f5f5f5", color = NA),
    # Remove grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Axis lines
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    # Text
    plot.title = element_text(size = 20, 
                              family = 'Helvetica', 
                              face = 'bold',
                              margin = margin(b = 8),
                              color = "#1a1a1a"),
    plot.subtitle = element_text(size = 14, 
                                 family = 'Helvetica',
                                 margin = margin(b = 20),
                                 color = "#4a4a4a"),
    plot.caption = element_text(size = 9,
                                family = 'Helvetica',
                                color = "#666666",
                                hjust = 1,
                                margin = margin(t = 15)),
    axis.text.x = element_text(size = 14, 
                               family = 'Helvetica',
                               color = "#2b2b2b"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    # Margins
    plot.margin = margin(20, 30, 20, 20), 
    legend.position = 'none'
  )+
  ylim(0,1300000000)



# New systems adoption ----------------------------------------------------



chart_data <- afluencia_cb %>% 
  mutate(fecha = floor_date(fecha, unit = 'month')
         ) %>% 
  drop_na(linea) %>% 
  group_by(fecha, linea) %>% 
  summarise(afluencia =sum(afluencia)) %>% 
  rbind(afluencia_tr %>% filter(linea == 'Línea 10') %>% 
          mutate(fecha = floor_date(fecha, unit = 'month')
          ) %>% 
          drop_na(linea) %>% 
          group_by(fecha, linea) %>% 
          summarise(afluencia =sum(afluencia)) 
          )

chart_data %>% 
  ggplot(aes(fecha, afluencia, color = linea)) +
  
  # Use actual lines instead of smooth
  geom_line(linewidth = 2) +
  
  # Points only at most recent data
  geom_point(data = . %>% 
               group_by(linea) %>% 
               filter(fecha == max(fecha)),
             size = 4) +
  
  # Labels only at end of each line
  geom_text(data = . %>% 
              group_by(linea) %>% 
              filter(fecha == max(fecha)),
            aes(label = paste0(comma(afluencia/1000000, accuracy = 0.1), "M")),
            hjust = -0.2,
            size = 6,
            family = 'Helvetica',
            fontface = 'bold',
            show.legend = FALSE) +
  
  # Add subtle horizontal reference lines
  geom_hline(yintercept = c(1000000, 2000000, 3000000), 
             color = "#d0d0d0", 
             linewidth = 0.3,
             linetype = "dotted") +
  
  # Year dividers
  geom_vline(xintercept = seq.Date(as.Date('2022-01-01'), 
                                   as.Date('2025-02-01'), 
                                   by = 'year'),
             color = "#333333",
             linewidth = 0.8, linetype = 'dashed') +
  
  # Better color palette - distinct but harmonious
  scale_color_manual(
    values = c(
      'Línea 1' = '#17becf',  # Deep blue
      'Línea 2' = '#17becf',  # Cyan
      'Línea 10' = '#2C55B4'   # Orange for newest line
    ),
    labels = c('Cable Car Line 1 (2022)', 'Raised Trolley Bus (2024)', 'Cable Car Line 2 (2022)')
  ) +
  
  # Scales
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.08))  # Extra space on right for labels
  ) +
  
  scale_y_continuous(
    labels = function(x) paste0(x/1000000, "M"),
    breaks = seq(0, 3000000, 1000000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # Labels
  labs(
    x = '', 
    y = '', 
    title = 'Instant adoption of the new lines ~8 million riders a month now', 
    subtitle = 'Monthly CDMX Cablebús Ridership (Ticket Sales in Millions)',
    caption = 'Source: CDMX Open Data Portal',
    color = ''
  ) +
  
  # Theme
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f5f5f5", color = NA),
    panel.background = element_rect(fill = "#f5f5f5", color = NA),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    
    plot.title = element_text(
      size = 24, 
      family = 'Helvetica', 
      face = 'bold',
      margin = margin(b = 8),
      color = "#1a1a1a"
    ),
    
    plot.subtitle = element_text(
      size = 14, 
      family = 'Helvetica',
      margin = margin(b = 20),
      color = "#4a4a4a"
    ),
    
    plot.caption = element_text(
      size = 9,
      family = 'Helvetica',
      color = "#666666",
      hjust = 1,
      margin = margin(t = 15)
    ),
    
    axis.text.x = element_text(
      size = 14, 
      family = 'Helvetica',
      color = "#2b2b2b"
    ),
    
    axis.text.y = element_text(
      size = 12,
      family = 'Helvetica',
      color = "#666666"
    ),
    
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(fill = "#f5f5f5", color = NA),
    legend.text = element_text(size = 11, family = 'Helvetica'),
    legend.key.width = unit(1.5, "cm"),
    
    axis.ticks = element_blank(),
    plot.margin = margin(20, 40, 20, 20)  # Extra right margin for end labels
  )





