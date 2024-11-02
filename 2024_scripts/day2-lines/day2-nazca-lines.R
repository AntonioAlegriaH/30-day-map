
# Day 2: Lines ------------------------------------------------------------
pacman::p_load(dplyr,
               tidyr,
               sf,
               geodata,
               ggplot2,
               scales,
               rayshader,
               glue,
               install = FALSE)



# 2-clean data ------------------------------------------------------------


path_data <- "day-2/data/geoglifos.zip"
unzip(path_data,exdir = "day-2/data/")


data_raw <-  sf::st_read("day-2/data/geoglifos.shp")

glimpse(data_named)


data_named <- data_raw %>% 
  mutate(name_geoglyph = case_when(OBJECTID %in% c(1,2) ~ "El Colibrí",
                                   OBJECTID %in% c(3,4,5,6) ~ "BUSCARNOMBRE",
                                   OBJECTID == 7 ~ "El Mono",
                                   OBJECTID == 8 ~ "La Araña",
                                   OBJECTID %in% c(9,10,11) ~ "La Espiral",
                                   OBJECTID %in% c(12,13,14,15,16) ~ "La Flor",
                                   OBJECTID %in% c(17,18,19) ~ "El Árbol",
                                   OBJECTID %in% c(22,23) ~ "Las Manos",
                                   OBJECTID %in% c(20,21,55,56,57,58,59,60,61,62) ~ "El Lagarto/Not complete",
                                   OBJECTID %in% c(24,25,26,27) ~ "El Flamingo /Not Complete",
                                   OBJECTID %in% c(28,29,30,31,32,33,34,35,37,38) ~ "El Pelícano",
                                   OBJECTID %in% c(39,40,41,42) ~ "El Loro",
                                   OBJECTID == 63 ~ "El Pulpo",
                                   OBJECTID == 65 ~ "El Condor",
                                   OBJECTID == 75 ~ "El Pulpo Duplicate",
                                   OBJECTID == 77 ~ "El Condor Duplicate",
                                   OBJECTID %in% c(78,79,80,81) ~ "El Perro",
                                   OBJECTID %in% c(66,67,68,69) ~ "El Perro Duplicate",
                                   TRUE ~ 'NOTNAMED')) %>% 
  select(OBJECTID, name_geoglyph, SHAPE_Leng, geometry)

select_geoglyphs <- c("El Colibrí",
                      "BUSCARNOMBRE"
                      )
select_geoglyphs <- c("El Pelícano",
                      "El Loro",
                      "El Flamingo /Not Complete"
                      )

data_named %>% 
 filter(name_geoglyph %in% select_geoglyphs) %>% 
  ggplot()+
  geom_sf()+
  geom_sf_text(aes(label = name_geoglyph),check_overlap = T)


# 3-get raster -------------------------------------------------------------

glyph_elcondor <- data_named %>% 
  filter(name_geoglyph %in% select_geoglyphs)

st_crs(glyph_elcondor)

bbox_values <- st_bbox(glyph_elcondor)

elev <- elevatr::get_elev_raster(locations = glyph_elcondor, 
                                 prj = st_crs(glyph_elcondor),
                                 z = 14,clip = "bbox", 
                                 expand = .002
                                 ) 

# note¿?:Is it possible to get the extent of a terra raster?

pryr::object_size(elev)
raster::plot(elev)

raster::extent(glyph_elcondor)

elmat <- raster_to_matrix(elev) 

# 4- RENDER -----------------------------------------------------
zscale_value <- 1
rgl::par3d("windowRect")

elmat %>% 
  height_shade(
    colorRampPalette(
      c('#7b82c9','#6464a9','#0a143d',
        '#c9b0de', '#e0d1ec','#ffffff'),
    )(256)
  ) %>% 
  add_overlay(generate_line_overlay(
    glyph_elcondor,
    extent = raster::extent(elev),
    heightmap = elmat, 
    color = "#0a143d",resolution_multiply = 3,
    linewidth = 1),rescale_original = TRUE) %>% 
  plot_3d(
    heightmap = elmat,

    zscale = zscale_value,
    solid = T,
    shadow = T,
    windowsize = c(500,500), 
    zoom = 2,
    phi = 85,
    theta = 0,
    baseshape = "rectangle",
    background = "#ffffff",
    shadowcolor = "#7209b7",
    shadow_darkness = .9,
    shadowwidth = 135.7*3,#min(dim(elmat))/10
    #shadowdepth = -300, # soliddepth: min(elmat)-1*zscale = -53,
    solidcolor = "#7209b7"
  )

#lineas 
for (i in 1:nrow(glyph_elcondor)){
  render_path(glyph_elcondor$geometry[i], 
              extent = raster::extent(elev), 
              heightmap = elmat, 
              color= "#e8a200",
              linewidth = 2, zscale=zscale_value, offset = 20,
              antialias = TRUE, # lines lookbad if this is T
              clear_previous = FALSE)
}


render_camera(
  zoom = .19,theta = 0,phi = 89,fov = 10
)

rgl::par3d("windowRect")

imgname <- "nazca-lines-5.png"

render_highquality(
  filename = imgname,
  width = 500*4, 
  height = 500*4,
  sample_method = "sobol",
  interactive = FALSE,
  samples = 512,
  lightdirection = c(240), 
  lightaltitude=60,
  lightcolor=c("#5555ff"),
  lightintensity = 30,
  path_material = rayrender::light,
  path_material_args = list(
    intensity = .6,
    importance_sample = FALSE),
  line_radius = .09,
  smooth_line = TRUE
      
    
)


