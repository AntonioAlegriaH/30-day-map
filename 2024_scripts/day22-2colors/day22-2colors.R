

# Day 22: 2 Colors --------------------------------------------------------

pacman::p_load(
  tidyr, 
  dplyr,
  sf,
  ggplot2,
  scales,
  osmdata,
  install = FALSE
)




# 1-import data -----------------------------------------------------------
zip_file_1 <- "day-22/data/09_ciudaddemexico.zip"
zip_file_2 <- "day-22/data/09_Manzanas_INV2020_shp.zip"

extraction_dir <- "day-22/data"

unzip(zip_file_1, exdir = extraction_dir)
unzip(zip_file_2, exdir = extraction_dir)

rm(zip_file_1, zip_file_2)

# blocks

manznas_path <- "day-22/data/09_Manzanas_INV2020_shp/INV2020_IND_PVEU_MZA_09.shp"

manzanas_cdmx_raw <- read_sf(manznas_path) %>% 
  st_transform(crs = 4326) %>% 
  select(CVEGEO,
         CVE_MUN,
         NOM_MUN,
         CVE_AGEB,
         CVE_MZA,
         TIPOMZA,
         POBTOT,
         VIVTOT,
         PROM_OCUP, #Promedio de ocupantes por vivienda.
         PARATRAN_C,# Parada de transporte colectivo. 
         TRANSCOL_C # Transporte colectivo
         ) %>% 
  mutate(
    POBTOT = as.numeric(POBTOT),
    VIVTOT = as.numeric(VIVTOT),
    PROM_OCUP = as.numeric(gsub("[^0-9\\.]", "", PROM_OCUP)), # Removes all non-numeric characters except ".",
    PARATRAN_C = as.numeric(PARATRAN_C),
    TRANSCOL_C = as.numeric(TRANSCOL_C)
  )


# rtp routes

rtp_path <- "day-22/data//rtp_shp/RTP_lineas.shp"
rtp_path_stops <- "day-22/data//rtp_shp/RTP_paradas.shp" 
rtp_rutas <- read_sf(rtp_path) %>% 
  filter(ESTATUS != "SUSPENDIDA")
rtp_stops <- read_sf(rtp_path_stops)

# OSM LIMITS

osm_query <- opq(bbox = st_bbox(rtp_rutas)) %>%
  add_osm_feature(key = "boundary",
                  value = "administrative") %>%
  osmdata_sf()

admin_boundaries <- osm_query$osm_multipolygons[-1, ]


# 2- clean and prepare data -----------------------------------------------


buffer_distance <- 20  


rtp_buffer <- st_buffer(rtp_rutas, dist = buffer_distance)


manzanas_cdmx_df <- manzanas_cdmx_raw %>%
  mutate(
    intersects = if_else(
      lengths(st_intersects(geometry, rtp_buffer$geometry)) > 0, 
      "yes", 
      "no"
    ),
    num_rutas = lengths(st_intersects(geometry, rtp_buffer$geometry))
  ) %>% 
  select(CVEGEO,
         CVE_MUN,
         NOM_MUN,
         intersects,
         num_rutas)

rm(manzanas_cdmx_raw)

# 3-visualize (ggplot) ----------------------------------------------------

# Unique Alcaldías just for fun
alcaldias <- unique(manzanas_cdmx_df$NOM_MUN)

plots_list <- lapply(alcaldias, function(alcaldia) {
  alcaldia_data <- manzanas_cdmx_df[manzanas_cdmx_df$NOM_MUN == alcaldia, ]
  bbox <- st_bbox(alcaldia_data)
  
  ggplot() +
    geom_sf(data = alcaldia_data, aes(fill = intersects), color = NA) +
    geom_sf(data = rtp_rutas,
            color = "green4",
            linewidth = 1)+
    scale_fill_manual(values = c("yes" = "orange", "no" = "gray")) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
             ylim = c(bbox["ymin"], bbox["ymax"]), 
             expand = FALSE) +
    labs(
      title = paste("RTP Route Availability in", alcaldia),
      fill = "RTP Route"
    ) +
    theme_minimal()
})

# View the first plot
print(plots_list[[16]])

bbox <- st_bbox(rtp_rutas)
subtitle_text <- "City Blocks with One or More Bus Routes Available"
title_text <- "RTP Bus Route Availability in CDMX"
caption_text <- "Source: INEGI-INVEA, CDMX Datos Abiertos, OSM | #30DAYMAPCHALLENGE: Day 22: 2 Colors | Created by: antonioalegria.io @elmedicobrujo"

p <- ggplot() +
  geom_sf(
    data = admin_boundaries,
    linewidth = .1,
    color = scales::alpha("green4", 1),
    fill= "#f1f1f1"
  )+
  geom_sf(data = manzanas_cdmx_df , 
          aes(fill = intersects), 
          linewidth = .01,
          color = scales::alpha("green4", 1))+
  geom_sf(data = rtp_rutas,
          color = "orange",
          linewidth = .5, 
          show.legend = TRUE)+
  geom_sf(
    data = rtp_stops,
    color = "green4",
    shape = 21,
    size = .3,
    stroke = .1
  )+
  scale_fill_manual(values = c("yes" = "green4",
                               "no" = "#f1f1f1"
                               ),
                    labels = c("no" = "Doesn't Have a Bus Route",
                               "yes" = "Has a Bus Route")) +
  coord_sf(expand = FALSE,
           xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"]),) +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       fill = "") +
  theme_void()+
  theme(plot.title.position = "plot",
        plot.title = element_text(family = "Anton",
                                  size = 30,
                                  color = "green4",
                                  hjust = 0.5,
                                  vjust = -146),
        plot.subtitle = element_text(family = "Barlow Condensed",
                                     face = "bold",
                                     size = 15,
                                     color = "green4",
                                     hjust = 0.5,
                                     vjust = -180),
        plot.caption = element_text(
          family = "Barlow Condensed",
          size = 9,
          color = "orange",
          hjust = .5,
          vjust = -10
        ),
        legend.text = element_text(family = "Barlow Condensed",
                                   color = "orange",
                                   size = 12,
                                   hjust = .5),
        legend.key.width = unit(50, "pt"),
        legend.position = "bottom",
        legend.margin = margin(90,0,0,0, "pt"),
        plot.margin = margin(20,20,50,20, "pt")    )




ggsave(
  filename = "rutas-rtp.png",
  plot = p,
  width = 800,
  height = 800,
  units = "px",
  scale = 5,
  dpi = 400,
  bg = "#f1f1f1"
)



# 4-barchart --------------------------------------------------------------

alcaldias_df <- manzanas_cdmx_df %>% 
  as_tibble() %>%
  group_by(NOM_MUN) %>%
  summarise(
    blocks_with_routes = sum(intersects == "yes"),
    blocks_total = n(),                           
    .groups = "drop"                              
  )



alcaldias_df %>% 
  ggplot(aes(reorder(NOM_MUN,blocks_with_routes), blocks_with_routes))+
  geom_col(aes(NOM_MUN, blocks_total),
           color = "green4",
           fill = NA)+
  geom_col(fill = "orange",
           color = NA, width = .7)+
  geom_text(aes(label =  comma(blocks_with_routes)),
            color = "orange",
            family = "Barlow Condensed",
            fontface = "bold",
            hjust = -.5,
            show.legend = FALSE
            )+
  #geom_text(aes(label =  comma(blocks_total)),
  #          color = "green4",
  #          family = "Barlow Condensed",
  #          fontface = "bold",
  #          hjust = -5,
  #          vjust = -.1,
  #          show.legend = FALSE
  #)+
  labs(title = "Blocks with RTP Bus Routes in each borough of CDMX")+
  coord_flip(clip = "off",
             expand = FALSE)+
  theme_minimal()+
  theme(plot.title.position = "plot",
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = margin(20,80,20,20,"pt"),
        plot.title = element_text(
          family = "Anton",
          size = 20,
          color = "green4",
          margin = margin(0,0,20,0, "pt")
        ),
        axis.text = element_text(
          family = "Barlow Condensed",
          face = "bold",
          color = "green4",
          size = 13
        )
        
        )

  


ggsave(
  filename = "rtp_rutas-chart.png",
  plot = last_plot(),
  width = 400,
  height = 400,
  units = "px",
  scale = 5,
  bg = NA
  
)
