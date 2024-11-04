
# Day 4 Hexagons ----------------------------------------------------------

pacman::p_load(
  tidyr,
  dplyr,
  sf,
  ggplot2,
  geodata,
  install = FALSE
)


# 1-Importa datasets and sf objects ---------------------------------------

washington_sf <- gadm(
  "US",level = 2,resolution = 2,
  path = tempdir()
) %>% 
  st_as_sf() %>% 
  filter(NAME_1 == "District of Columbia")

glimpse(washington_sf)
unique(washington_sf$NAME_1)


washington_sf %>% 
  ggplot()+
  geom_sf()

path <- "day-4/Sex_Offender_Registry.zip"
unzip(path)

ws_dc <- read_sf("day-4/State_of_Washington_DC_in_2020.shp") %>%
  select(
    OBJECTID,
    CITY_NAME, 
    STATE_NAME,
    geometry
  )

data <-read_sf("day-4/Sex_Offender_Registry.shp") %>% 
  st_transform(crs = st_crs(ws_dc)) %>% 
  select(
    OBJECTID,
    MAXCLASSIF,
    TYPE,
    DISTRICT,
    geometry
  )



# 2-hexbins ---------------------------------------------------------------

# MAKEGRID
dc_grid <- st_make_grid(ws_dc,
                        square = FALSE,
                        cellsize = 350)  

pryr::object_size(dc_grid)
 
 dc_hex_350 <- dc_grid[ws_dc]

 
 dc_hex_350 %>% 
   ggplot()+
   geom_sf()
 
 
 dc_hex_350 <- st_sf(dc_hex_350) %>% 
   mutate(index = row_number())
 
 
 hex_joined_df <- st_join(data, 
                             dc_hex_350, 
                             left = FALSE)  

 
 sum_joined_hex <- hex_joined_df %>% 
   group_by(index) %>% 
   count()
 
 
 hex_joined_df %>% 
   ggplot()+
   geom_sf()
 
 
 counts_no_geo_hex_1k <- sum_joined_hex %>% 
   as_tibble() %>% # converting as a tibble 
   select(-geometry)

 
 dc_counts_hex <- left_join(dc_hex_350, 
                                counts_no_geo_hex_1k)  

 
p <-  dc_counts_hex %>% 
   ggplot()+
   geom_sf(aes(fill = n),
           color = "#8D818C", 
           size = .1,
           show.legend = FALSE)+
   annotate("text", Inf, 4721171, 
            label = "Data provided by the Court Services 
            and Offender Supervision Agency 
            identified sex offender registry 
            providing location at the 
            block level", 
            hjust = 1,
            vjust = 1,
            family = "Barlow Condensed",
            color = "#444444",
            size = 2)+
   scale_fill_continuous(high = "#132B43", 
                         low = "#56B1F7", 
                         na.value = "#f1f1f1")+
   labs(title = "Sex Offender Registry",
        subtitle = "City of Washington, DC",
        caption = "Source: Open Data DC\n#30DAYMAPCHALLENGE: Day 4 Hexagons\nMade by: antonioalegria.io @elmedicobrujo")+
   theme_void()+
   theme(plot.margin = margin(10,10,10,10,"pt"),
         plot.title = element_text(size = 20,
                                   family = "Poppins",
                                   face = "bold",
                                   color = "#333333"),
         
         plot.subtitle = element_text(family = "Barlow Condensed",
                                      color = "#444444"),
         plot.caption = element_text(family = "Barlow Condensed",
                                     color = "#555555",
                                     hjust = 0,
                                     size = 6)
         )
 
 
 
 
 
 
 
 # For Windows only
 install.packages("extrafont")
 library(extrafont)
 font_import()
 loadfonts(device = "win"  )

 

# 3-render ----------------------------------------------------------------

library(rayshader) 

 
 rayshader::plot_gg(
   p,
   width=5,
   height=5,
   scale=200,offset_edges = T,
   solid = FALSE,
   raytrace = FALSE,
   shadowdepth = -.01,
   background = "white",
   max_error = 0.01,
   windowsize=c(800,800),
   zoom = 1, 
   phi = 90,
   theta = 0)
 
 render_camera(zoom = .59,
               phi = 65,
               theta = 0)
 
 
 render_highquality(
   filename = "day4-hex_2.png",
   width =500*6, height = 500*6,
   samples = 640, 
   sample_method = "sobol",
   interactive = FALSE,
   lightdirection = c(290, 300), 
   lightaltitude=c(80,45), 
   lightintensity = 550,
   lightcolor=c("#132B43", "#ffffff"),
 )
