
pacman::p_load(elevatr,
               sf,
               dplyr,
               raster,
               rayshader, 
               giscoR,
               MetBrewer
               )

 
  # Full tutorial: 
  # https://spencerschien.info/post/data_viz_how_to/high_quality_rayshader_visuals/
  
  cities <- gisco_get_urban_audit(year = "2020", 
                                  level = "CITIES")
  
  barcelona <- cities[cities$URAU_NAME == "Barcelona",]
  
  pryr::object_size(barcelona)  
  
  barc_raster <- get_elev_raster(barcelona, 
                                 z = 13, 
                                 clip = "locations") 
  
  pryr::object_size(barc_raster)  
  
  barc_elev_mat <- raster_to_matrix(barc_raster)  
  
  pryr::object_size(barc_elev_mat)  
  
  
# Size plot 
  # Dynaimcally set window height and width based on object size
  w <- nrow(barc_elev_mat)
  h <- ncol(barc_elev_mat)
  
  # Scale the dimensions so we can use them as multipliers
  wr <- w / max(c(w,h))
  hr <- h / max(c(w,h))
  
  # Limit ratio so that the shorter side is at least .75 of longer side
  if (min(c(wr, hr)) < .75) {
    if (wr < .75) {
      wr <- .75
    } else {
      hr <- .75
    }
  }
  
  
  
# Pallete
  pal <- "Java"
  colors <- met.brewer(pal,direction = -1)
  
  rgl::rgl.close()
  
  # Plot 2D    
  barc_elev_mat %>% 
    height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>% 
    add_shadow(ray_shade(barc_elev_mat,
                         zscale = 10,
                         sunaltitude = 60,
                         sunangle = 90),
               max_darken = 0.1) %>%
    plot_map()
  
  
  # Plot 3d
  barc_elev_mat %>% 
    height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
   # add_shadow(ray_shade(barc_elev_mat,
   #                      sunaltitude = 60,
   #                      sunangle = 90,
   #                      zscale=1),
   #            max_darken=0.1) %>%
    plot_3d(barc_elev_mat,  
            zscale = 1, 
            fov = 0, 
            theta = 135, 
            zoom = 0.75, 
            phi = 45, 
            windowsize = c(2000, 2000))
  
  
  # It takes one hour to render. Be patient.
  
  render_highquality(
    "barc-java_highres.png", 
    parallel = TRUE, 
    samples = 300,
    light = FALSE, 
    interactive = FALSE,
    environment_light = "enviroment/brown_photostudio_02_4k.hdr", # <- choose your .hdr
    intensity_env = 1.5,
    rotate_env = 180,
    width = round(3000), 
    height = round(3000)
  )
  
  
  
  render_snapshot("barcelona-ray_Java_5.png",
                 
                  vignette = FALSE)
