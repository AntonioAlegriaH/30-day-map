pacman::p_load(dplyr,
               tidyr,
               pryr,
               sf,
               ggspatial,
               ggplot2,
               scales)


# https://opendata.scot/datasets/city+of+edinburgh+council-quiet+routes/
# https://opendata.scot/datasets/city+of+edinburgh+council-national+cycle+network/


# National Cycling Network
# National Cycle Network routes within the Edinburgh Council boundary

# Edinburgh Boundaries
  data_bounbdaries <- read_sf("resources/Community_Council_Boundaries/Community_Council_Boundaries.shp")
  object_size(data_bounbdaries)

# Edinburgh council's ward boundaries 
  data_wards <- read_sf("resources/Edinburgh_Ward_Boundaries/Edinburgh_Ward_Boundaries.shp")
  object_size(data_bounbdaries)
  
  data_Cycling <- read_sf("resources/National_Cycle_Network/National_Cycle_Network.shp")
  object_size(data_Cycling)

  
  data_quiet <- read_sf("resources/Quiet_Routes/Quiet_Routes.shp")

### Test -----    

p1 <-   ggplot()+
    geom_sf(data = data_bounbdaries,
            fill = "#e5e5e5",
            color = "#14213d")+
    theme_void()+
    labs(title = "Edinburgh Community Council Boundaries")+
    theme(plot.margin = margin(1,1,1,1, "cm"),
          text = element_text(colour = "#14213d",
                              family = "Futura",
                              face = "bold"),
          plot.title = element_text(size = 15))

p2 <-   ggplot()+
    geom_sf(data = data_wards,
            fill = "#e5e5e5",
            color = "#14213d")+
    theme_void()+
    labs(title = "Edinburgh Ward Boundaries")+
    theme(plot.margin = margin(1,1,1,1, "cm"),
          text = element_text(colour = "#14213d",
                              family = "Futura",
                              face = "bold"),
          plot.title = element_text(size = 15))
    



ggsave("wards.png",
       plot = p2,
       width = 1200,
       height = 1200,
       units = "px",
       dpi = 300, 
       scale = 2,
       bg = "#edede9"
       )

### final plot -----

ggplot()+
  geom_sf(data = data_wards,
          fill = "grey95",
          color = "grey80"
          )+
  geom_sf(data = data_Cycling,
          color = "grey20")+
  geom_sf(data = data_quiet,
          color = "grey20")
  annotate(geom = "text", label = "Edinburgh has many safe \nand interesting cycling \nroutes which can be \nenjoyed by all ages. \nFrom off-street paths, \nformer railway lines \nand sea view esplanades, \nthere are cycling routes \nto suit most abilities.",
           hjust = 1, 
           family = "Futura", 
           color = "grey30",
           x = 334100, # 328000 hjust = 0 
           y = 683000)+
  annotate(geom = "text", label = "Day 24: 2 Colours",
           hjust = 1, 
           family = "Futura", 
           fontface = "bold",
           color = "grey30",
           x = 333990, # 328000 hjust = 0 
           y = 687000)+
  labs(title = "Edinburgh \nCycle \nNetwork",
       subtitle = "",
       caption = "Data:Open Data Scotland | #30DayMapChallenge,
       Dataviz: antonioalegria.io | tw: @elmedicobrujo")+
  theme_void()+
  theme(plot.margin = margin(1,1,1,1, "cm"),
        text = element_text(color = "grey30"),
        plot.title = element_text(size = 80, family = "Futura",
                                  face = "bold",
                                  ),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 9))+
  coord_sf(clip = "off", 
           xlim = c(309580, 332968) , 
           ylim = c(659269, 680699) )


ggsave("Edinburgh-cycle_testing.png",
       plot = last_plot(),
       width = 1200,
       height = 1200,
       units = "px",
       scale = 3,
       dpi = 300,
       bg = "white"
       )
  
   
# Color testing
# fill = "grey95"
# color = "grey80"
# cyclo
# color = "grey20"
# text 
# color = "grey30",
# https://coolors.co/palette/f15025-ffffff-e6e8e6-ced0ce-191919

 
color_text = "#191919"
color_cyclo = "#f15025"


p <- ggplot()+
  geom_sf(data = data_wards,
          fill = "#fafaff",
          color = "#e4d9ff")+
  geom_sf(data = data_quiet,
          color = color_cyclo, 
          size = 1.2)+ #1.2
  geom_sf(data = data_Cycling,
          color = color_text, 
          size = 1.3)+ #1.3
  annotate(geom = "text", label = "Edinburgh has many safe \nand interesting cycling \nroutes which can be \nenjoyed by all ages. \nFrom off-street paths, \nformer railway lines \nand sea view esplanades, \nthere are cycling routes \nto suit most abilities.",
           hjust = 1, 
           family = "Futura", 
           color = color_text,
           x = 334100, # 328000 hjust = 0 
           y = 683000)+
  annotate(geom = "text", label = "Day 24: 2 Colours",
           hjust = 1, 
           family = "Futura", 
           fontface = "bold",
           color = color_text,
           x = 333990, # 328000 hjust = 0 
           y = 687000)+
  annotate(geom = "text", label = "National Cycle Network",
           hjust = 1, 
           family = "Futura", 
           fontface = "bold",
           color = color_text,
           size = 5,
           x = 330000, # 328000 hjust = 0 
           y = 662500)+
  annotate(geom = "text", label = "Quiet Routes",
           hjust = 1, 
           family = "Futura", 
           fontface = "bold",
           color = color_cyclo,
           size = 5,
           x = 329000, # 328000 hjust = 0 
           y = 664900)+
  labs(title = "Edinburgh \nCycle \nNetwork",
       subtitle = "",
       caption = "Data:Open Data Scotland | #30DayMapChallenge,
       Dataviz: antonioalegria.io | tw: @elmedicobrujo")+
  theme_void()+
  theme(plot.margin = margin(1,1,1,1, "cm"),
        text = element_text(color = color_text),
        plot.title = element_text(size = 80, family = "Futura",
                                  face = "bold",
        ),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 9))+
  coord_sf(clip = "off", 
           xlim = c(309580, 332968) , 
           ylim = c(659269, 680699) )


ggsave("Edinburgh-cycle_testing.png",
       plot = p,
       width = 1200,
       height = 1200,
       units = "px",
       scale = 3,
       dpi = 300,
       bg = "white"
)
