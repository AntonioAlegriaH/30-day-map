# NYC sea rise 100 years
#https://data.cityofnewyork.us/Environment/Sea-Level-Rise-Maps-2020s-100-year-Floodplain-/ezfn-5dsb
# NYC sea rise 500 years
#https://data.cityofnewyork.us/Environment/Sea-Level-Rise-Maps-2050s-500-year-Floodplain-/qwca-zqw3

#https://data.cityofnewyork.us/api/assets/1F7A128D-4F0D-430C-A98E-53E44CFFCAD7?download=true
  
  pacman::p_load(dplyr,
                 tidyr,
                 sf,
                 rmapshaper,
                 ggplot2,
                 scales,
                 ggspatial
  )
  
  theme_set(theme_minimal(base_size = 15))

# Sea level 2050  
  nyc_sea_level_rise <- read_sf("resources/Sea Level Rise Maps (2050s 500-year Floodplain)/geo_export_2b6d5401-f7ec-420d-9327-94d4dc8d4128.shp")
  pryr::object_size(nyc_sea_level_rise)
  nyc_sea_simp <- rmapshaper::ms_simplify(nyc_sea_level_rise,keep = 0.05)  
  pryr::object_size(nyc_sea_simp)

# nyc boro division
  nyc_boro_shp <- sf::read_sf("resources/Borough Boundaries/geo_export_29807758-f1bd-4e37-b856-cb4e361914a9.shp")
  pryr::object_size(nyc_boro_shp)
  data_nyc_sf_simp <- rmapshaper::ms_simplify(nyc_boro_shp,keep = 0.05)  
  pryr::object_size(data_nyc_sf_simp)
  
# nyc street data (!!!Highly recomend run the next lines on jobs!!!!)
  
  nyc_street_sf <- read_sf("resources/NYC Street Centerline (CSCL)/geo_export_f6b5391a-2181-494b-93de-964e7f6c49bf.shp")
  pryr::object_size(nyc_street_sf)
  nyc_street_simp <- rmapshaper::ms_simplify(nyc_street_sf)
  pryr::object_size(nyc_street_simp)
  
  
# plot test  
  ggplot()+
    geom_sf(data= data_nyc_sf_simp)+
    geom_sf(data = nyc_street_simp)
  geom_sf(data= nyc_sea_simp,
          aes(fill = abfe_0_2pc))+
    theme_void()
  


# caluculate centroids for name ploting

nyc_borough_centroid <- cbind(data_nyc_sf_simp, 
                              st_coordinates(st_centroid(data_nyc_sf_simp)))



# plot final
ggplot()+
  geom_sf(data= data_nyc_sf_simp,
          fill = "grey97", 
          color = "grey80", 
          size = .3
  )+
  geom_sf(data = nyc_street_simp,
          size = .3,
          colour = "#EFE2BE")+
  geom_sf(data= nyc_sea_simp,
          aes(fill = abfe_0_2pc),
          alpha = 0.8,
          colour = NA)+
  geom_text(data = nyc_borough_centroid, 
            aes(X, Y, label = boro_name),
            check_overlap = TRUE,
            colour = "#0a2a34",
            vjust = -1,
            size = 6,
            family= "Roboto", fontface = "bold")+
  labs(title = "Day 18: Blue | NYC 2050's Floodplain level rise projections",
       subtitle = "Higher sea levels are extremely likely by mid-century. Projections for \nsea level rise in New York City are: By the 2050s, the middle range of \nprojections is 11 to 24 inches, and the high estimate is 31 inches.",
       caption = "Data: data.cityofnewyork.us | #30DayMapChallenge
       Dataviz: antonioalegria.io | tw: @elmedicobrujo")+
  theme_void()+
  #coord_sf(xlim =c(-8277901,-8199999) ,ylim =c(4922454,5000454) ,clip = "off", expand = FALSE)+
  annotation_scale(location = "tr",height = unit(0.2, "cm"),
                   bar_cols = c("grey", "#FBF8EF"), 
                   line_col = "#444444",
                   text_col = "#FBF8EF",text_cex = 1.5) +
  annotation_north_arrow(location = "tl",
                         # Use true north
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style =  north_arrow_minimal(line_width = .5,
                                                      line_col = "#FBF8EF",
                                                      fill = "#FBF8EF",
                                                      text_col = "#FBF8EF",
                                                      text_family = "",
                                                      text_face = "plain",
                                                      text_size = 15)
  )+
  scale_fill_gradient2("Advisory Base Flood Elevations",
                       low = "#001219",
                       mid = "#005F73",
                       high = "#94D2BD",
                       midpoint = 10
                       
  )+
  theme(plot.margin = margin(1,1,1,1,"cm"),
        plot.title = element_text(size = 52,
                                  family = "Avenir Next Condensed",
                                  face = "bold",
                                  color = "#E9D8A6"
        ),
        plot.subtitle = element_text(size = 22,
                                     margin = margin(t = .5,
                                                     r = 0,
                                                     b = 1,
                                                     l = 0,
                                                     "cm"),
                                     color = "#FBF8EF",
                                     family = "Roboto Light"
                                     ),
        text = element_text(colour = "#FFFFFF",
        ),
        plot.caption = element_text(color = "#FBF8EF",
                                    size = 13,
                                    family = "Roboto Light"),
        legend.position = c(.11,.6),
        legend.key.width = unit(.5,"cm"),
        legend.key.height = unit(5,"cm"),
        legend.text = element_text(color = "#FBF8EF",
                                   size = 13,
                                   family = "Roboto Light"),
        legend.title = element_text(color = "#FBF8EF",
                                    family = "Roboto Light",
                                    margin = margin(0,0,1,0, "cm"),
                                    size = 18)
        )


ggsave("blue-sea-level_final.png",
       plot = last_plot(),
       width = 1200,
       height = 1200,
       units = "px",
       bg = "#0a2a34",
       scale = 5,
       dpi = 300
       
       
)


  #203c46
#14333d