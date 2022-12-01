
# 1-exploring -------------------------------------------------------------
  
  pacman::p_load(readr,
                 dplyr,
                 tidyr,
                 sf,
                 pryr,
                 lubridate,
                 rmapshaper,
                 ggspatial,
                 scales,
                 ggplot2,
                 gganimate
  )

theme_set(theme_void(base_size = 15))  
  
# Dataset: https://datos.cdmx.gob.mx/dataset/afluencia-diaria-del-metro-cdmx
# 


# Estaciones
  data_estaciones <- read_sf("resources/stcmetro_shp/STC_Metro_estaciones_utm14n.shp")
  object_size(data_estaciones)
  
# Lineas
  data_lineas <- read_sf("resources/stcmetro_shp/STC_Metro_lineas_utm14n.shp")
  object_size(data_lineas)
  
# Afluencia diaria
  
  data_afluencia <- read_csv("resources/afluenciastc_desglosado_08_2022.csv")
  object_size(data_afluencia)
  spec(data_afluencia)

  
# Plot testing
    
  ggplot()+
    geom_sf(data = data_lineas)+
    geom_sf(data = data_estaciones)

  
# Wrangling Afluencia por estación
  
  
  ## check for same metro stations code [recursive search of names]
  stations_1 <- unique(data_estaciones$NOMBRE)
  stations_2 <- unique(afluencia_diaria$estacion_recode)
  
  setdiff(stations_2,  stations_1)


# assembling all plots

  data_lineas_order <- 
    data_lineas %>% 
    mutate(linea_recode = recode(LINEA,
                                 "01" = "1",
                                 "02" = "2", 
                                 "03" = "3", 
                                 "04" = "4",
                                 "05" = "5",
                                 "06" = "6",
                                 "07" = "7",
                                 "08" = "8", 
                                 "09" = "9",
                                 "A" = "A",
                                 "B" = "B" ,
                                 "12" = "12"
                                 ))
  
  data_lineas_order$linea_recode <- 
    factor(data_lineas_order$linea_recode, 
           levels=c("1", "2", "3", "4", "5", 
                    "6", "7", "8", "9", "A", "B" , "12")
           )
  
  afluencia_parsed <-
    data_afluencia %>%
    drop_na() %>% 
    group_by(fecha, estacion) %>% 
    summarise(sum_total = sum(afluencia)) %>% 
    mutate(NOMBRE = recode(estacion,  #NOMBRE to join with the sf data
                           "20 de Noviembre" = "Hospital 20 de Noviembre",
                           "Azcapotzalco"     = "UAM Azcapotzalco",
                           "Blvd. Puerto Aéreo" = "Boulevard Puerto Aéreo",
                           "Deptvo. 18 de Marzo" = "Deportivo 18 de Marzo",
                           "Etiopía" = "Etiopía/Plaza de la Transparencia",
                           "Ferrería" = "Ferrería/Arena Ciudad de México",       
                           "Garibaldi" = "Garibaldi/Lagunilla",
                           "Gómez Farías"  = "Gomez Farías", 
                           "Inst. del Petróleo" = "Instituto del Petróleo",
                           "Isabel la Católica" = "Isabel La Católica",
                           "La Villa-Basilica" = "La Villa/Basílica",
                           "Miguel A. de Q." = "Miguel Ángel de Quevedo",    
                           "Mixiuhca" = "Mixhiuca",
                           "Niños Héroes" = "Niños Héroes/Poder Judicial CDMX",
                           "Periférico Oriente" = "Periferico Oriente",
                           "San Andrés Tomatlán" = "San Andres Tomatlán",
                           "San Juan Letrán" = "San Juan de Letrán",
                           "San Pedro los Pinos" = "San Pedro de Los Pinos",
                           "Tezozomoc" = "Tezozómoc",
                           "U A M  I" = "UAM-I",     
                           "Viveros" = "Viveros/Derechos Humanos",
                           "Zócalo" = "Zócalo/Tenochtitlan")
    ) %>% 
    ungroup()
  
   
  
  
  data_estaciones_order <- 
    data_estaciones %>% 
    mutate(linea_recode = recode(LINEA,
                                 "01" = "1",
                                 "02" = "2", 
                                 "03" = "3", 
                                 "04" = "4",
                                 "05" = "5",
                                 "06" = "6",
                                 "07" = "7",
                                 "08" = "8", 
                                 "09" = "9",
                                 "A" = "A",
                                 "B" = "B" ,
                                 "12" = "12"
    ))
  
  
  
  
  data_estaciones_order$linea_recode <- 
    factor(data_estaciones_order$linea_recode, 
           levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B" , "12")
    )
  
  
  estaciones_joined_parsed <- 
    left_join(data_estaciones_order,
              afluencia_parsed,
              by = "NOMBRE") 

  
  
  
  
  # alcaldias
  data_alcaldias <- read_sf("resources/alcaldías_cdmx/alcaldias_cdmx.shp")
  ggplot()+
    geom_sf(data = data_alcaldias,
            alpha =.5, 
            fill = "#edf6f9",
            color = "grey90")+
    geom_sf(data = data_lineas_order, 
            aes(color = linea_recode),
            size = .5)
  
  

  
  p <- ggplot()+
    geom_sf(data = data_alcaldias,
            alpha =.5, 
            fill = "#edf6f9",
            color = "grey90")+
   geom_sf(data = data_lineas_order, 
           aes(color = linea_recode),
           size = .5)+
   geom_sf(data = estaciones_joined_parsed,
           aes(size = sum_total,
               color = linea_recode))+
    scale_color_manual(values  = c("#e74a8e",#Linea1
                                  "#0b5b9d", #Linea2
                                  "#b59b28", #Linea3
                                  "#6fbab1", #Linea4
                                  "#fcd030", #Linea5
                                  "#d81c22", #Linea6
                                  "#e77022", #Linea7
                                  "#128c41", #Linea8
                                  "#5a352f", #Linea9
                                  "#9d2180", #LineaA
                                  "#bbb9b8", #LineaB,
                                  "#c3995a" #Linea12
                                  ),
                       name = "Metro Lines")+
    scale_size(guide="none") + 
    labs(
      caption = "Data: datos.cdmx.gob.mx | #30DayMapChallenge
         Dataviz: antonioalegria.io | tw: @elmedicobrujo")+
    theme(plot.title = element_text(size = 18,
                                    family = "MetroDF",
                                   hjust = 1),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 14,
                                       hjust = -.4,
                                       margin = margin(.5,0,0,0,"cm")),
          plot.caption = element_text(size = 7,
                                      hjust = 1.2),
          text = element_text(colour = "#444444"),
          legend.title = element_text(size = 9, face = "bold"),
          legend.text = element_text(size = 8),
          plot.margin = margin(1,1,1,1, "cm"))+
    coord_sf(clip = "off")
    

  

  ggsave("plot_static.png",
         plot = p,
         scale = 2,
         dpi = 300,
         width = 1200,
         height = 1200,
         units = "px",
         bg = "white"
          )     

  
# Final animation    

  my_animation <- 
    p + transition_time(fecha)+
    labs(title = "Day 23: Movement | Mexico City Metro",
         subtitle = "Maping One Year of Daily Ridership: {frame_time}")
  
  
  animate(my_animation, 
          height = 1400,
          width = 1400, 
          units = "px", 
          res = 150,
          fps = 20,
          detail = 2)
  
  
  anim_save("metro_afluence.gif", 
            animation = last_animation(), 
  )
  