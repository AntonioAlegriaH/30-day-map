
# 1- libraries ------------------------------------------------------------


pacman::p_load(dplyr,
               tidyr,
               sf,
               ggplot2,
               scales,
               ggfx,
               install = FALSE
)

theme_set(theme_minimal(base_size = 15))
theme_update(
  plot.title.position = "plot",
  axis.line.x = element_line(linewidth = .2, colour = "grey50"),
  axis.line.y = element_line(linewidth = .2, colour = "grey50"),
  panel.grid = element_blank()
)

# 2-data import -----------------------------------------------------------

# https://data.buenosaires.gob.ar/dataset/arbolado-publico-lineal/resource/51668e94-12af-401f-aba5-84476f78bb9d

barrios <- read_sf("barrios/barrios_wgs84.shp") 
comunas <- read_sf("comunas/comunas_wgs84.shp") 
arbolado <- read_sf("arbolado-publico-lineal-2017-2018/arbolado-publico-lineal-2017-2018.shp")

# Tree data was to big: `arbolado`. 
# After processing the dataset. I saved the results in `sum_joined_arboledas.rds`

sum_joined_barrios <- readRDS("sum_joined_barrios.rds")


# Use this if you want to map comunas
sum_joined_comunas <- st_join(comunas, arbolado) %>% 
  select(ID,COMUNAS,BARRIOS,PERIMETRO,AREA) %>% 
  group_by(ID,BARRIOS,PERIMETRO,AREA) %>% 
  count(COMUNAS, sort = T)

# Use this if you want to map barrios
sum_joined_barrios <- st_join(barrios, arbolado) %>% 
  select(BARRIO,COMUNA,PERIMETRO,AREA) %>% 
  group_by(COMUNA,PERIMETRO,AREA) %>% 
  count(BARRIO, sort = T)

saveRDS(sum_joined_comunas,
        file = "sum_joined_comunas.rds")

saveRDS(sum_joined_barrios,
        file = "sum_joined_barrios.rds")


sum_joined_barrios <- readRDS("sum_joined_barrios.rds")
glimpse(sum_joined_barrios)

#How many tress
sum(sum_joined_barrios$n)



high_color <- "#345e37"
low_color <- "#e0e0e0"


# 3- mapping --------------------------------------------------------------



p <- ggplot()+
  geom_sf(data= sum_joined_barrios,
          aes(fill = n),
          show.legend = FALSE)+
  scale_fill_gradient(low = low_color,
                      high = high_color)+
  geom_sf(data = comunas,
          linewidth = 1,
          color = "#333333",
          fill = NA)+
  geom_sf_text(data = barrios,
               aes(label = BARRIO),
               color = "#666666",
               fontface = "bold",
               size = 1)+
  geom_sf_text(data = sum_joined_barrios,
               aes(label = comma(n, big.mark = ",")),
               color = "#FFFFFF",
               fontface = "bold",
               nudge_y = -.0028,
               size = 1.2)+
  theme_void()
theme(
  plot.background = element_rect(
    fill = "white",
    colour = "white"))

p


ggsave(
  plot = last_plot(),
  filename = "arbolado_caba_2.png",
  width = 1200,
  height = 1200,
  units = "px",
  scale = 4,
  dpi = 400,
  bg = "#f1f1f1"
)


# 4-raytracing ------------------------------------------------------------

# I highly recommend you to make a second script for this section. 
# Run it only when you're happy with the plot. 

# 1-load packages ---------------------------------------------------------

pacman::p_load(rayshader,
               glue,
               rgl,
               install = FALSE)



rgl::close3d()


rayshader::plot_gg(
  p,
  width=6,
  height=5,
  scale=100,
  solid = FALSE,
  raytrace = FALSE,
  offset_edges = TRUE,
  #shadowdepth = .1,  
  background = "white",
  max_error = 0.01,
  windowsize=c(800,800),
  zoom = 0.7, 
  phi = 70,
  theta = 0)

rgl::close3d()

render_camera(zoom=.7,theta=0,phi = 70)

render_snapshot()

render_highquality(
  filename = "caba_3.png",
  width =1500, height = 1500,
  samples = 3000, 
  interactive = FALSE,
  lightdirection = c(290), 
  lightaltitude=70, 
  lightintensity = 600,
  lightcolor=c("#c6ffe6"),
)