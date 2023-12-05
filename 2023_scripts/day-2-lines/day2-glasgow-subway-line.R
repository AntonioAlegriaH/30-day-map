
# 1-import libraries ------------------------------------------------------

pacman::p_load(raster,
               geodata, 
               sf,
               dplyr,
               tidyr,
               ggplot2,
               osmdata,
               ggfx,
               scales,
               install = FALSE)


theme_set(theme_minimal(base_size = 15))
theme_update(
  plot.title.position = "plot",
  axis.line.x = element_line(linewidth = .2, colour = "grey50"),
  axis.line.y = element_line(linewidth = .2, colour = "grey50"),
  panel.grid = element_blank()
)
# 2-get spatial data ------------------------------------------------------


gbr_lv_2 <-  raster::getData('GADM', 
                             country='GBR', 
                             level=2) %>% 
  st_as_sf() %>%  # transform into an sf
  filter(NAME_1 == "Scotland")


edi_lv_2 <- gbr_lv_2 %>% 
  filter(NAME_2 == "Glasgow")



# is it working?

scotland %>% 
  ggplot()+
  geom_sf()


# 2.1 Obtain data from OSM:------------------------------------


# railway values

railway_value <- c("light_rail",
                   "narrow_gauge",
                   "rail",
                   "tram")


# subway values
railway_value_subway <-  "subway"


# highway values
highway_value <- c("motorway",
                   "trunk",
                   "primary",
                   "secondary",
                   "tertiary",
                   "unclassified",
                   "residential")


# Extract Data
# This is used to describe the type of railway 

# Railway
railway_value_city <- opq(st_bbox(edi_lv_2),
                          timeout = 400) %>% # Spatial boundaries 
  add_osm_feature(key = "railway",
                  value = railway_value ) %>% 
  osmdata_sf()

# Subway
railway_value_subway_city <- opq(st_bbox(edi_lv_2),
                                 timeout = 400) %>% # Spatial boundaries 
  add_osm_feature(key = "railway",
                  value = railway_value_subway) %>% 
  osmdata_sf()

# Subway Stations  
railway_value_subway_stations_city <- opq(st_bbox(edi_lv_2),
                                          timeout = 400) %>% # Spatial boundaries 
  add_osm_feature(key = "railway",
                  value = "station") %>% 
  osmdata_sf()

# Highways Streets
highway_values_city <- opq(st_bbox(edi_lv_2),
                           timeout = 400) %>% # Spatial boundaries 
  add_osm_feature(key = "highway",
                  value = highway_value) %>% 
  osmdata_sf()



# Subset lines and points  
lines_railway_city <- railway_value_city$osm_lines 
lines_subway_city <- railway_value_subway_city$osm_lines
points_subway_stations_city <- railway_value_subway_stations_city$osm_points
lines_highway_city <- highway_values_city$osm_lines


# Include ridership data
# IMPORTANT: Skip to the last lines before this  
points_subway_stations_city_ridership <- 
  points_subway_stations_city %>% 
  filter(name %in% stations_name$name,
         network != "National Rail" ) %>% 
  left_join(., stations_name, by = "name") %>% 
  st_as_sf()

# check    
points_subway_stations_city_ridership %>% 
  ggplot()+
  geom_sf(aes(size = entries))+
  geom_sf_text(aes(label = name))



# Highways lines and points select/cut
# careful it takes some time

# Railway Select
joined_df_lines_city <- st_join(lines_railway_city,
                                edi_lv_2, 
                                left = FALSE) %>% 
  select((GID_0:geometry), 
         osm_id,electrified,
         usage,
         name,
         railway,                        
  )

# Subway Select only public railways    
joined_df_subway_city <- st_join(lines_subway_city,
                                 edi_lv_2, 
                                 left = FALSE) %>% 
  select((GID_0:geometry), 
         osm_id,electrified,
         operator,
         layer,
         name,
         railway,                        
  ) %>% 
  filter(layer == "-2" )

# Check where are not public lines.
# When pulling data from OSM, there was railways that are not open to the public.
# Those rails are for maintenance, so we need to remove them.

lines_subway_city %>% 
  ggplot()+
  geom_sf()+
  geom_sf_text(aes(label = layer))+
  facet_wrap(vars(layer))

# Subway stations Select  
joined_df_subway_stations_city <- st_join(points_subway_stations_city_ridership,
                                          edi_lv_2, 
                                          left = FALSE) %>% 
  select((GID_0:geometry), 
         osm_id,
         station,
         name,
         entries,
         exits,
         railway,                        
  ) %>% 
  filter(station == "subway")

# Highways Select  
joined_df_lines_highways_city <- st_join(lines_highway_city,
                                         edi_lv_2, 
                                         left = FALSE) %>% 
  select((GID_0:geometry), 
         osm_id,name,highway,
         surface,
         width) 


st_bbox(joined_df_subway_city)

# 3- mapping --------------------------------------------------------------

# colors
backgroud_color <- "#14213d"
fill_color <- "#14213d"
lines_color <- "#e5e5e5"
railways_color <- "#ffffff"
subway_lines_color <- "#fca311"
stations_color <- "#333333"



p <- edi_lv_2 %>% 
  ggplot()+
  geom_sf(data = joined_df_lines_highways_city,
          linewidth = .2,
          color = lines_color,
          alpha = .4) +
  # ggfx::with_outer_glow(
  geom_sf(data= joined_df_lines_city,
          alpha= .4, linewidth= 1.2,
          color = railways_color)+
  #colour = "#FCF158",
  #sigma = 20)+
  ggfx::with_outer_glow(
    geom_sf(data= joined_df_subway_city,
            alpha= 1, linewidth= 2.5,
            color = subway_lines_color),
    colour = subway_lines_color,
    sigma = 20)+
  geom_sf(data= joined_df_subway_stations_city,
          aes(size = entries),
          color = subway_lines_color)+
  geom_sf_text(
    data = joined_df_subway_stations_city,
    aes(label = comma(entries, 
                      big.mark = ",",
                      suffix = "\nEntries")
    ), 
    color = lines_color, 
    size = 1.5,
    family = "Barlow",
    fontface = "bold"
  )+
  theme_void()+
  scale_size(range = c(4,10))+
  theme(plot.background = element_rect(fill = backgroud_color,
                                       color = backgroud_color),
        plot.margin = margin(2,2,2,2,"cm"),
        legend.position = "none")

p


ggsave(plot = p,
       filename = "day2-lines.png",
       width = 1200,
       height = 1200,
       units = "px",
       scale = 5,
       dpi = 400,
       bg = NA
       
)



# 4-ridership data --------------------------------------------------------

# This is necesary to ridership data
# obtained from:
#https://www.glasgowlive.co.uk/news/subway-usage-west-end-south-15759229   
stations_name <-tibble(
  name = c("Bridge Street",
           "Buchanan Street",
           "Cessnock",
           "Cowcaddens",
           "Govan",
           "Hillhead",
           "Ibrox",
           "Kelvinbridge",
           "Kelvinhall",
           "Kinning Park",
           "Partick",
           "Shields Road",
           "St Enoch",
           "St George's Cross",
           "West Street"),
  entries = c(378253,
              2494562,
              481760,
              450412, 
              800770, 
              1826070,
              557739,
              1037404,
              684767,
              324587,
              938452,
              494968,
              1986100,
              540988,
              96036),
  exits = c(380293,
            2738750,
            452510,
            523299,
            861043,
            1885919,
            540564,
            1017013,
            659222,
            330117,
            949992,
            499101,
            1789195, 
            516700,
            99223)
)

# how many entries?
sum(stations_name$entries) %>% 
  print()



# Entries by Stations: Barplot

stations_name %>% 
  ggplot(aes(fct_reorder(name, entries), entries))+
  geom_col(width = .5,
           fill = subway_lines_color)+
  geom_text(aes(label = scales::comma(entries, big.mark = ",") ),
            family = "Barlow Condensed", 
            color = subway_lines_color,nudge_y = 200000
            
  )+
  coord_flip(clip = "off",
             expand = FALSE)+
  labs(title = "",
       x = "",
       y = "")+
  theme(plot.margin = margin(1,1,1,1,"cm"),
        axis.text.y = element_text(color = lines_color,
                                   family = "Barlow Condensed"),
        axis.line.x = element_blank(),
        axis.text.x = element_blank())



ggsave(
  plot = last_plot(),
  filename = "entries.png",
  width = 1000,
  height = 1200,
  units = "px",
  bg = NA,
  scale = 2,
  dpi = 300
)




