# 1-libraries -------------------------------------------------------------

pacman::p_load(dplyr,
               tidyr,
               sf,
               ggplot2,
               scales,
               install = FALSE)

theme_set(theme_minimal(base_size = 15))
theme_update(
  plot.title.position = "plot",
  axis.line.x = element_line(linewidth = .2, colour = "white"),
  axis.line.y = element_line(linewidth = .2, colour = "white"),
  panel.grid = element_blank()
)


# 2-import data  ---------------------------------------------


data_access_point <- readr::read_csv("2023-10-20-puntos_de_acceso_wifi.csv") 
readr::problems(data_access_point)

# there's 3 obs with no longitude data (NA's)

#12127     5 a double -99.186 
#12192     5 a double -99.186 
#12257     5 a double -99.186 

skimr::skim(data_access_point)

# Mexico City map polygons

data_map <- read_sf("poligonos_alcaldias_cdmx/poligonos_alcaldias_cdmx.shp") 

st_bbox(data_map)

# Transform into an sf object, project, and drop NA's
data_sf <- data_access_point %>%
  mutate(alcaldia = case_when(alcaldia == "Gustavo A Madero" ~ "Gustavo A. Madero",
                              .default = alcaldia
  )
  ) %>% 
  drop_na(longitud) %>% 
  st_as_sf(.,coords = c("longitud","latitud"),
           crs = st_crs(data_map)) 

# Double ckeck
#There's some points with a long data that doesn't match 
# Mexico City area/bbox
st_bbox(data_sf)
st_bbox(data_map)



data_sf %>% 
  ggplot()+
  geom_sf()+
  coord_sf(xlim = c(-99.36492,-98.94030),
           ylim = c(  19.04824,  19.59276))



# 3- How many? ------------------------------------------------------------


count_data <- data_sf %>% 
  count(alcaldia, sort = T) %>% 
  as_tibble() %>% 
  dplyr::select(1:2)


count_data %>% 
  ggplot(aes(forcats::fct_reorder(alcaldia, n),n,
             fill = alcaldia))+
  geom_col(lineend = "round")+
  geom_text(aes(label = comma(n, big.mark = ",")),
            nudge_y = -190,
            color = "#1d2e3d",
            size = 4,
            family = "Barlow Condensed"
  )+
  scale_fill_manual(values = palettePersonal)+
  coord_flip(clip = "off",
             expand = FALSE)+
  labs(title = "",
       x = "",
       y = "How Many?")+
  theme(panel.grid.minor.x = element_line(color = "white"),
        plot.margin = margin(2,2,2,0,"cm"),
        axis.text.y = element_text(size = 14),
        text = element_text(family = "Barlow Condensed",
                            color = "white"),
        axis.text = element_text(color = "white"),
        axis.line.x = element_blank(),
        legend.position = "none")


ggsave(
  plot = last_plot(),
  filename = "howmany.png",
  width = 800,
  height = 1200,
  unit = "px",
  bg = NA,
  scale = 2,
  dpi = 300
)


# 3.1- final map -----------------------------------------------------------


palettePersonal <- c(
  "#0D6E6E", #(Primary 100) Deep Sea
  "#4a9d9c", #(Primary 200) Teal Blue
  "#afffff", #(Primary 300) Light Cyan
  "#FF3D3D", #(Accent 100) Coral Red
  "#ffe0c8", #(Accent 200) Apricot
  "#FFFFFF", #(Text 100) White
  "#e0e0e0", #(Text 200) Light Gray
  "#FFD700", #(Gold)
  "#FFA500", #(Orange)
  "#00FF7F", #(Spring Green
  "#8ECC7A", #(Custom Color 1)  Fresh Green
  "#FF6347", #(Custom Color 2)  Bright Orang
  "#5744FF", #(Custom Color 3)  Rich Blue
  "#EB65A0", #(Custom Color 4)  Orchid Pink
  "#15D0D0", #(Custom Color 5)  Turquoise
  "#A1A1A1" #(Custom Color 6)  Gray Ash
)

p <- ggplot() +
  #geom_raster(data = elevation_data, aes(x = x, 
  #                                       y = y, 
  #                                       fill = elevation))+
  geom_sf(data = data_map,
          alpha = .1,
          color = "#f1f1f1",
          linewidth = 1.5
          
  )+
  geom_sf_text(data = data_map,
               aes(label = NOMGEO
               ),
               color = "#f1f1f1",
               size = 2.5)+
  geom_sf(data = data_sf,
          aes(color = alcaldia),
          size = .0001,
          alpha = 1,
          show.legend = FALSE)+
  coord_sf(xlim = c(-99.36492,-98.94030),
           ylim = c(  19.04824,  19.59276),
           clip = "off")+
  scale_color_manual(values = palettePersonal)+
  theme_void()+
  theme(plot.margin = margin(4,1,4,3, "cm"))
p


ggsave(
  plot = p,
  filename = "access_points_3.png",
  width = 1200,
  height = 1200,
  unit = "px",
  scale = 5,
  dpi = 400,
  bg = "#1d2e3d"
)





