

pacman::p_load(dplyr,
               tidyr,
               ggplot2,
               scales,
               sf,
               rayshader,
               install = FALSE)


# I use http://bboxfinder.com/ several times until i found the angle 
# that I was looking for

#RECTANGLE BEST VIEW

xmin <- -98.988372 #long
xmax <- -98.506529
ymin <- 18.951504 #lat
ymax <- 19.248798

volcanes <- data.frame(x = c(xmin, xmax), 
                       y = c(ymin, ymax))

elev <- elevatr::get_elev_raster(volcanes, 
                                 z = 12, 
                                 prj = "+proj=longlat +datum=WGS84 +no_defs",
                                 clip = 'bbox',
                                 src = "aws")

elmat <- raster_to_matrix(elev) 


test_texture <- 
  create_texture(
    lightcolor = "white",
    shadowcolor = "white",
    leftcolor = "grey0",
    rightcolor = "white",
    centercolor = "grey")


rgl::close3d()

elmat %>% 
  sphere_shade(texture = test_texture,
               sunangle = 180) %>% 
  plot_3d(elmat, 
          zscale = 15, 
          theta = 0, 
          phi = 30, 
          zoom = .6, 
          windowsize=c(1200,1200),
          background = '#ffffff', 
          solid = T, 
          shadow = T,
          solidcolor = 'grey30', 
          alpha = .2
  )

rgl::close3d()

# This is not the final view, but close enough
render_camera(zoom=.2,theta=180,phi = 0,fov = 100)

# Note: This render took way too long in my pc.
#       macOs seems to handle better the cloud rendering
render_clouds(elmat, zscale = 15, start_altitude = 3000, 
              end_altitude = 4000, attenuation_coef = 2, 
              cloud_cover = 0.6,
              fractal_levels = 20,
              layers = 15,
              baseshape = "rectangle",
              clear_clouds = T)


render_highquality(
  filename = "volcanes_2000.png",
  width =3000, height = 3000,
  samples = 2000, 
  interactive = FALSE,
  lightdirection = c(180), 
  lightaltitude=70, 
  lightintensity = 650,
  lightcolor=c("white"),
)
