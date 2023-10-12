
pacman::p_load(dplyr,
               tidyr,
               raster,
               ggplot2,
               tiff,
               scales,
               viridis,
               magick,
               rasterVis,
               showtext)

# Data: https://hub.worldpop.org/geodata/summary?id=41389
# Remix from: https://twitter.com/JessWhoMaps/status/1589564901412196354?s=20&t=3ziqQQpq3UCwwVh_WUeyaw


  fn <- "resources/deu_pd_2020_1km.tif"
  r1 <- raster(fn)
  pryr::object_size(r1)

  
# As a dataframe

  data_raster_df <- raster::as.data.frame(r1, xy=TRUE)
  data_df <- data_raster_df %>% 
    drop_na()
  pryr::object_size(data_raster_df)
  
  
  # ggplot:
  ggplot() +
    geom_raster(data = data_df , 
                aes(x = x, 
                    y = y, 
                    fill =  deu_pd_2020_1km), show.legend = FALSE)+ 
    coord_quickmap(clip = "off", 
                   xlim = c(5.877917, 15.000000) ,
                   ylim = c(47.40000, 55.000000))+
    theme_void()+
    labs(title = "GERMANY POPULATION DENSITY:",
         caption = "Data: @WorldPopProject | #30DayMapChallenge \nDay30: Remix from: Jess Hepburn | tw: @JessWhoMaps \nDataviz: antonioalegria.io | tw: @elmedicobrujo")+
    scale_fill_viridis(option = "G", direction = -1,
                       labels = comma_format(big.mark = ","),
                       name = "People per square km:"
                       )+
    guides(fill = guide_legend(title.position = "top",
                               label.position = "left" ,
                               reverse = FALSE))+
    theme(plot.margin = margin(t=.75,
                               r = 5,
                               b = 1,
                               l = 5, "cm"),
          plot.title.position = "panel",
          plot.title = element_text(size = 39,
                                    family = "Futura",
                                    face = "bold", 
                                    hjust = .5, 
                                    colour = "#000000"
                                    ),
          plot.caption = element_text(size = 8,
                                      hjust = 1),
          legend.position = c(1,.4),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8),
          legend.margin = margin(.5,0,-1,0, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.key.width = unit(1,"cm"),
          text = element_text(colour = "#333333"))
    
    
  
    ggsave("remix_5.png",
         plot = last_plot(),
         scale = 3,
         width = 1200,
         height = 1200,
         units = "px",
         bg = "white",
         dpi = 350
          
          )
  
  
# Full disclousure: legends were added in keynote xd
  
  
  # TEST including country sf -----  
  
  germany_sf <- giscoR::gisco_get_countries(resolution = "20",
                                            country = "Germany")
  

    
  germany_sf %>% 
    ggplot()+
    geom_sf()+
    geom_raster(data = data_df , 
                aes(x = x, 
                    y = y, 
                    fill =  deu_pd_2020_1km))+
    coord_sf()+
    theme_void()+
    labs(title = "Germany population density",
         caption = "Data: @@WorldPopProject | #30DayMapChallenge
         Day30: Remix from: Jess Hepburn | tw: @JessWhoMaps 
         Dataviz: antonioalegria.io | tw: @elmedicobrujo")+
    scale_fill_viridis_c()
  
  
# end test-------  