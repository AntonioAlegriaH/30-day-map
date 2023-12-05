
# 1- Mexico Dental clinics --------------------------------------------------

pacman::p_load(raster,
               geodata, 
               sf,
               tidyverse,
               osmdata,
               ggfx,
               install = FALSE)



# 2- get data -------------------------------------------------------------

mex_lv_1 <-  raster::getData('GADM', country='MX', level=1) %>% 
  st_as_sf()

frontier <- c("Baja California")

baja_1<- st_as_sf(mex_lv_1) %>% 
  filter(NAME_1 %in% frontier)

# dental clinics data
# Source: INEGI DENUE
inegi_denue <- sf::read_sf("INEGI_DENUE_09112023/INEGI_DENUE_09112023.shp") %>% 
  st_transform(crs = st_crs(baja_1))

# is it working?

baja_1 %>% 
  ggplot()+
  geom_sf()+
  geom_sf(data = inegi_denue
  )# yes


# join points inside the baja_1 geography 


# join healthcare locations with states
# st_join by default is a left join. 
# keep all x values. join all the y values intersecting x
# left: healthcare points.    



joined_df_cat <- st_join(inegi_denue, 
                         baja_1, 
                         left = FALSE) 

joined_df_cat_simp <-  joined_df_cat %>% 
  select(id,
         nombre_act,
         per_ocu,
         nomb_asent,
         entidad,
         municipio,(GID_0:HASC_1)
  )


rm(joined_df_cat, frontier)

# baja have only 3231 institutions



# 3. Hexagon map  ---------------------------------------------------------

# How to create my own polygons.
# grouping by anything inside the limits
# Make a grid of hexagons
# I get the inspiration from Spencer Schien tutorials:
# https://www.youtube.com/@MrPecners

# the benefits of hexagon grids creates an even spacing in all directions  


baja_mex_grid <- st_make_grid(baja_1, 
                              square = FALSE,
                              cellsize = .9)  

baja_mex_grid %>% 
  ggplot()+
  geom_sf()

# Working with crs: 4326 make the hex look squishy  
# Google the place + epsgto get the crs
# In this case is EPSG: 2154

mex_new_crs <- st_transform(baja_1, crs = 4485)


# Make the hex grid 
# lower(0) the number of cellsize there are more hexagons  


hex_grid_1K <- mex_new_crs %>% 
  st_make_grid(square = FALSE,cellsize = 1000
  ) 


# This is a selection of hexagons that are included in the baja
# area

mex_hex_1K <- hex_grid_1K[mex_new_crs]

# its easier if you do it directly inside the plot function  

mex_new_crs %>% 
  ggplot()+
  geom_sf(size = 2)+
  geom_sf(data = mex_hex, 
          alpha = 0.5)
coord_sf(crs = 2154)


# join dentist with the hex bins
# summarise winery locations within the hex bins


# 1K hexs

mex_hex_sf_1k <- st_sf(mex_hex_1K) %>% 
  mutate(index = row_number())


# 1K  
health_points_hex_1k <- st_transform(sum_joined, 
                                     crs = st_crs(mex_hex_sf_1k))


# Summarise wine locations within departpents
# 1K 

hex_joined_df_1k <- st_join(health_points_hex_1k, 
                            mex_hex_sf_1k, 
                            left = FALSE)   

sum_joined_hex_1k <- hex_joined_df_1k %>% 
  group_by(index) %>% 
  count()




# join counts with hexes  
# build a df with no geometry with only counts

# 1k

counts_no_geo_hex_1k <- sum_joined_hex_1k %>% 
  as_tibble() %>% # converting as a tibble 
  select(-geometry)


# join with the france sf objects by the GID_2 
# That gives us a numeric variable to plot in each department


# 1K
mex_counts_hex_1k <- left_join(mex_hex_sf_1k, counts_no_geo_hex_1k)  



# Plot hexes joined with the hexes


p <- mex_counts_hex %>% 
  ggplot(aes(fill = n),
  )+
  ggfx::with_shadow(
    geom_sf( color = "grey95", size = .1, 
             show.legend = FALSE),
    x_offset = 30,
    y_offset = 30,
    colour = "#444444"
    
  ) +
  theme_void()+
  scale_fill_continuous(high = "#132B43", 
                        low = "#56B1F7", na.value = "white")+
  labs(title = "Mexico Healthcare",
       caption = "dataviz: antonioalegria.io | @elmedicobrujo | data: OSM")+
  theme(title = element_text(size = 10,
                             family = "Barlow Condensed",
                             color = "#F1f1f1",
                             hjust = .1,
                             margin = margin(0,2,10,0,"cm")),
        plot.caption = element_text(size = 12,
                                    family = "Barlow Condensed",
                                    face = "bold",
                                    hjust = .5,
                                    margin = margin(5,0,0,0, "cm")),
        plot.margin = margin(3,3,3,3, "cm"))


p

ggsave("Mexico-health-hexs_1k_fx.png",
       plot = p,
       width = 1200,
       height = 1200,
       units = "px",
       scale = 4,
       dpi = 400,
       bg = "#468faf"
)  



# 5. rayshader with ggplot -----------------------------------------

pacman::p_load(rayshader,
               glue,
               install = FALSE)


# Option A
p <- mex_counts_hex %>% 
  ggplot(aes(fill = n))+
  geom_sf( color = "grey95", size = .1, 
           show.legend = FALSE)+
  theme_void()+
  scale_fill_continuous(high = "#132B43", 
                        low = "#56B1F7", na.value = "white")+
  labs(title = "México Healthcare",
       caption = "dataviz: antonioalegria.io | @elmedicobrujo | data: OSM | {rayshader}")+
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 24,
                                  family = "Broadway",
                                  color = "#716A5C",
                                  hjust = .5,
                                  margin = margin(0,0,60,0)
        ),
        plot.caption = element_text(size = 7,
                                    family = "Century Gothic",
                                    face = "bold",
                                    hjust = .5,
                                    color = "#716A5C",
                                    margin = margin(2.5,0,0,1, "cm")),
        plot.margin = margin(1,1,1,1, "cm"),
        plot.background = element_blank(),
        panel.background = element_blank())


p



# Option B

p <- mex_counts_hex %>% 
  ggplot(aes(fill = n))+
  geom_sf(color = "#8D818C",
          size = .1,
          show.legend = FALSE)+
  scale_fill_continuous(high = "#132B43", 
                        low = "#56B1F7", 
                        na.value = "#f1f1f1")+
  theme_void()

p

# Use either option, depending what you want. 

# 3. raytracing -----------------------------------------------------------


rgl::close3d()

rayshader::plot_gg(
  p,
  width=5,
  height=5,
  scale=100,
  solid = FALSE,
  raytrace = FALSE,
  shadowdepth = -.01,
  background = "white",
  max_error = 0.01,
  windowsize=c(800,800),
  zoom = 0.55, 
  phi = 90,
  theta = 0)


render_camera(zoom=1,theta=0,phi=75)


render_highquality(
  filename = "baja_dental_taupe.png",
  width =3000, height = 3000,
  samples = 500, 
  sample_method = "sobol",
  interactive = FALSE,
  lightdirection = c(290, 90), 
  lightaltitude=c(60,90), 
  lightintensity = 550,
  lightcolor=c("#132B43", "#ffffff"),
)

