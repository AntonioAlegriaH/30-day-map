

pacman::p_load(dplyr,
               tidyr,
               stringr,
               sf,
               ggspatial,
               ggplot2,
               scales,
               ggtext)


# Dataset: https://opendata.dc.gov/datasets/DCGIS::metro-lines/explore?location=38.892509%2C-77.020630%2C13.00

  theme_set(theme_minimal(base_size = 15))
  
  data_lines_dc <- read_sf("resources/Metro_Lines/Metro_Lines.shp")
  data_stations_in_dc <- read_sf("resources/Metro_Stations_in_DC/Metro_Stations_in_DC.shp")
  data_lines_regional <- read_sf("resources/Metro_Lines_Regional/Metro_Lines_Regional.shp")
  data_stations_regional <- read_sf("resources/Metro_Stations_Regional/Metro_Stations_Regional.shp")
  

# Lines Colors HEX codes:
# blue:    #006aac : 0, 106, 172      
# green:   #00a14a : 0, 161, 74  
# orange:  #f88400 : 248, 132, 0 
# red:     #e00022 : 224, 0, 34 
# silver:  #979d95 : 151, 157, 149 
# yellow:  #ffd000 : 255, 209, 0   

# grayer than your soul
 
ggplot()+
  geom_sf(data = data_stations_in_dc,
          colour = "#444444",
          size = 5)+
  geom_sf(data = data_lines_dc,
          colour = "grey30",
          size = .1)+
  theme_void()+
  labs(title = "DC Metro Lines",
       subtitle = "30 Day Map Challenge - Day 16 Minimal",
       caption = "Source: opendata.dc.gov | #30DayMapChallenge
       Dataviz: antonioalegria.io | tw: @elmedicobrujo")+
  theme(plot.title.position = "plot"
        )


# Color implementation: pending. The backgroungd only applies inside the ploting region, not the whole panel.
  
# Separate station lines  
#  data_stations_regional[ , 16:19] <- str_split_fixed(data_stations_regional$LINE, 
#                                                      pattern = ",",
#                                                      n = 4)  
## Recode an error from grn to green
#    data_stations_regional <-  
#    data_stations_regional %>% 
#    mutate(V1=recode(V1, 
#                     `grn`="green",
#    ))
#  
#
## Labels stations
#text_stations <- data_stations_regional %>% 
#  mutate(lon = sf::st_coordinates(.)[,1],
#    lat = sf::st_coordinates(.)[,2])      
#  
#  
#
## bg
#
#background <- png::readPNG("bg.png")




# Separate station lines  
  data_stations_in_dc[ , 15:18] <- str_split_fixed(data_stations_in_dc$LINE, 
                                                      pattern = ",",
                                                      n = 4)  
# Recode an error from grn to green
  data_stations_in_dc <-  
    data_stations_in_dc %>% 
    mutate(V1=recode(V1, 
                     `grn`="green",
    ))
  

# Labels stations
text_stations_in_dc <- data_stations_in_dc %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2])      
  


ggplot()+
  geom_sf(data = data_lines_dc,
          aes(colour = NAME),
          size = 1)+
  geom_sf(data = data_stations_in_dc,
          aes(color = V1),
          size = 12)+
 # geom_text(data = text_stations_in_dc,
 #           aes(label = NAME,
 #               x = lon, y = lat),
 #           colour = "grey30",
 #           size = 1)+
  theme_void()+
  scale_colour_manual("",
                      values = c("#006aac",
                                 "#00a14a",
                                 "#f88400",
                                 "#e00022",
                                 "#979d95",
                                 "#ffd000"
                      ), 
                      labels = c("Blue Line", 
                                 "Green Line", 
                                 "Orange Line",
                                 "Red Line",
                                 "Silver Line",
                                 "Yellow Line")
  )+
  labs(title = "M",
       subtitle = "DC Metro Lines: 30 Day Map Challenge - Day 16 Minimal \n\nInspired by the unrealized DC Metro map \ndesigns by Massimo Vignelli,for the \nWashington DC Metro subway in 1968",
       caption = "Source: opendata.dc.gov | #30DayMapChallenge
       Dataviz: antonioalegria.io | tw: @elmedicobrujo")+
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 200,
                                  hjust = 1.1,
                                  family = "Roboto",
                                  face = "bold"
        ),
        plot.subtitle = element_text(size = 15,
                                     margin = margin(-6.5,0,0,0,"cm"),
                                     family = "Copperplate"
        ),
        plot.caption = element_text(size = 10, hjust =1.099),
        legend.position = c(0.08,0.1),
        legend.text = element_text(size = 15),
        plot.margin = margin(1,0,1,0,"cm"),
        text = element_text(colour = "grey30")
  )+
  coord_sf(xlim =c(-77.08577, -76.94328),
           ylim =c(38.83827, 38.97979),
           clip = "off", 
           expand = FALSE) 




ggsave("dc_metro_5.png",
       plot = last_plot(),
       scale = 4,
       width = 1200,
       height = 1200,
       bg = "#edf6f9",
       units = "px")
