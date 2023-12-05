
# 1-packages and data -----------------------------------------------------

pacman::p_load(dplyr,
               tidyr,
               sf,
               raster,
               terra,
               ggplot2,
               scales, 
               install = FALSE)


#### Get data: Madagascar
madagascar_sf <- geodata::gadm(country = "Madagascar",
                               level = 0,
                               path = tempdir())%>% 
  sf::st_as_sf()


## OR manuall download
madagascar_sf <- readRDS("gadm41_MDG_0_pk.rds") %>% 
  terra::unwrap() %>% 
  sf::st_as_sf()

# 2- creating the walls ---------------------------------------------------
# Info: there are 3 walls with aprox 50 meters of height  
# Maria: The distance from Wall Maria to Wall Rose was said to be about 100 km,
# Rose: The distance from Wall Rose to Wall Sina about 130 km
# Sheena: The radius of Wall Sina about 250 km.
# -Source:  Attack on Titan anime: Episode 1 — Currently Publicly Available Information.
# 
# Assuming the Walls to be perfect circles, 
# this would have made the total perimeter 
# of Wall Maria to be around 3,016 km with 
# the total area inside the Walls being 723,822 km².



# 2.1-calculating areas ---------------------------------------------------

area_madagascar <- 5.87041e+11 %>% 
  comma(area_madagascar,big.mark = ",")
area_sheena <- 1.9635e+11 %>% 
  comma(big.mark = ",")
area_rose <-  %>% 
  comma(big.mark = ",")
area_maria <-  %>% 
  comma(big.mark = ",")


st_area(madagascar_sf_proj)
st_area(centroid_buffer_sheena)
st_area(centroid_buffer_rose)
st_area(centroid_buffer_maria)


# 2.2 projection ----------------------------------------------------------

madagascar_sf_proj <- madagascar_sf %>% 
  st_transform(crs = 29701)


madagascar_sf_proj_t <- madagascar_sf %>% 
  st_transform(crs = 4385 )

# check
madagascar_sf_proj %>% 
  ggplot()+
  geom_sf()

rm(madagascar_sf)

# 2.3-centroids and buffer ------------------------------------------------

# Proposal 
centroid <- madagascar_sf_proj %>% 
  sf::st_centroid() 

centroid_buffer_sheena_r <- centroid %>% 
  sf::st_buffer(dist = 65000) %>% 
  mutate(wall_height = 50)

centroid_buffer_rose_r <- centroid_buffer_sheena_r %>% 
  sf::st_buffer(dist = 53000)%>% 
  mutate(wall_height = 50)

centroid_buffer_maria_r <- centroid_buffer_rose_r %>% 
  sf::st_buffer(dist = 50000)%>% 
  mutate(wall_height = 50)


# Buffers made from with Public Available Information
centroid_buffer_sheena <- centroid %>% 
  sf::st_buffer(dist = 250000)%>% 
  mutate(wall_height = 50)

centroid_buffer_rose <- centroid_buffer_sheena %>% 
  sf::st_buffer(dist = 130000)%>% 
  mutate(wall_height = 50)

centroid_buffer_maria <- centroid_buffer_rose %>% 
  sf::st_buffer(dist = 100000)%>% 
  mutate(wall_height = 50)



# 3-map ------------------------------------------------------------------

# this version is not the one used in the raytracing process
madagascar_sf_proj %>% 
  ggplot()+
  geom_sf(color = "#283618", fill = "#dad7cd")+
  geom_sf(data = centroid,
          size = .1, 
          color = "#a3b18a")+
  geom_sf(data = centroid_buffer_sheena, 
          color = "#588157", linewidth = 3,
          alpha = 0.0)+
  geom_sf(data = centroid_buffer_rose, 
          color = "#3a5a40", linewidth = 3,
          alpha = 0.0)+
  geom_sf(data = centroid_buffer_maria, 
          color = "#344e41", linewidth = 3,
          alpha = 0.0)+
  labs(title = "SNK in Real Life:",
       subtitle = "*with all the anime & manga information available",
       caption = "Data: Attack on Titan anime: Episode 1 | Currently Publicly Available Information 
       Dataviz: antonioalegria.io | @elmedicobrujo")+
  theme_minimal()+
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 70, 
                                  family = "Holy Union", 
                                  color = "#283618"),
        plot.subtitle = element_text(family = "Highway Gothic"),
        plot.caption =  element_text(family = "Highway Gothic"),
        text = element_text(colour = "#444444"),
        plot.margin = margin(2,2,2,2,"cm")
  )


ggsave(plot= last_plot(),
       filename = "snk_map_2.png",
       height = 1200,
       width = 1200,
       units = "px",
       scale = 4,
       dpi = 350,
       bg = "#f1f1f1",
)


# 4-raytracing ------------------------------------------------------------


pacman::p_load(rayshader,
               glue,
               rgl,
               install = FALSE)


p <- 
  ggplot()+
  geom_sf(data= madagascar_sf_proj,
          color = "#283618", fill = "#dad7cd",
          size = 1
  )+
  geom_sf(data = centroid_buffer_sheena, 
          aes(fill = wall_height),
          color = "#588157",
          alpha = 0.0,
          linewidth = 1)+
  geom_sf(data = centroid_buffer_rose, 
          aes(fill = wall_height),
          color = "#3a5a40",
          alpha = 0.0,
          linewidth = 1)+
  geom_sf(data = centroid_buffer_maria, 
          aes(fill = wall_height),
          color = "#344e41",
          alpha = 0.0,
          linewidth = 1)+
  theme_void()+
  theme(legend.position = "none"
  )

p
p_2 <- 
  ggplot()+
  geom_sf(data= madagascar_sf_proj,
          color = "#283618", fill = "#dad7cd",
          size = 1
  )+
  geom_sf(data = centroid_buffer_sheena_r, 
          aes(fill = wall_height),
          color = "#588157",
          alpha = 0.0,
          linewidth = 1)+
  geom_sf(data = centroid_buffer_rose_r, 
          aes(fill = wall_height),
          color = "#3a5a40",
          alpha = 0.0,
          linewidth = 1)+
  geom_sf(data = centroid_buffer_maria_r, 
          aes(fill = wall_height),
          color = "#344e41",
          alpha = 0.0,
          linewidth = 1)+
  theme_void()+
  theme(legend.position = "none"
  )



rgl::close3d()

rayshader::plot_gg(
  p,
  width=5,
  height=5,
  scale=100,
  solid = FALSE,
  raytrace = FALSE,
  shadowdepth = -.01,
  background = "#dad7cd",
  max_error = 0.01,
  windowsize=c(800,800),
  zoom = 0.55, 
  phi = 90,
  theta = 0)


rgl::close3d()

render_camera(zoom=1,theta=0,phi=75)

render_highquality(
  filename = "snk_f.png",
  width =3000, height = 3000,
  samples = 500, 
  sample_method = "sobol",
  interactive = FALSE,
  lightdirection = c(290, 90), 
  lightaltitude=c(70,90), 
  lightintensity = 500,
  lightcolor=c("#344e41", "#dad7cd"),
)

