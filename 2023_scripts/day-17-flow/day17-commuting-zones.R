
# 1-libraries -------------------------------------------------------------

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
  axis.line.x = element_line(linewidth = .2, colour = "grey70"),
  axis.line.y = element_line(linewidth = .2, colour = "grey70"),
  axis.text = element_text(family = "Barlow Condensed"),
  axis.title = element_text(family = "Barlow Condensed",
                            color = labels_color),
  panel.grid = element_blank()
)

# 2-load data -------------------------------------------------------------

#https://dataforgood.facebook.com/dfg/tools/commuting-zones


data_commuting <- readr::read_csv("data-for-good-at-meta-commuting-zones-march-2023.csv") %>% 
  st_as_sf(wkt = "geography", crs = 4326)

pryr::object_size(data_commuting)

glimpse(data_commuting)


mex_map <- geodata::gadm("Mexico",
                         level = 1,
                         path = tempdir(),
                         resolution = 1) %>% 
  terra::unwrap() %>% 
  st_as_sf()


# 3- explore --------------------------------------------------------------

unique(data_commuting$region)
[1] "North"   "Europe"  "Africa"  "Asia"    "Oceania" "South" 
unique(data_commuting$cz_gen_ds)


data_commuting_north <- data_commuting %>% 
  filter(region == "North",
         country == "Mexico")


data_commuting_north$name <- gsub("\\b([a-z])", "\\U\\1", 
                                  data_commuting_north$name, perl=TRUE)


skimr::skim(data_commuting_north)

unique(data_commuting_north$name)


high_color <- "#03045e"
low_color <- "#caf0f8"
labels_color <- "#444444"
bg_color <- "#f1f1f1"
shadow_color <- "#444444"

sf_object <- geom_sf(data = data_commuting_north,
                     aes(fill = area),
                     alpha =1)
ggplot()+
  #geom_sf(data = mex_map,
  #        alpha = .1,
  #        color = "#2b2d42")+
  with_shadow(sf_object,
              colour = shadow_color,
              x_offset = 20,
              y_offset = 20) +
  geom_sf_text(data = data_commuting_north,
               aes(label = name),
               size = 3,
               color = labels_color,
               check_overlap = TRUE,
               family = "Barlow Condensed")+
  scale_fill_gradient(low = low_color,
                      high = high_color, name = "Area of commuting zone",
                      labels = comma_format(big.mark = ",",
                                            suffix = " km²"))+
  guides(fill = guide_legend(title.position = "top"))+
  theme_void()+
  theme(plot.margin = margin(2,2,2,2,"cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 18,
                                   family = "Barlow Condensed"),
        legend.title = element_text(size = 20,
                                    family = "Barlow Condensed"),
        legend.key.height = unit(.5, "lines"),
        legend.key.width = unit(3, "lines"))



ggsave(
  plot = last_plot(),
  filename = "commuting_zones_2.png",
  width = 1200,
  height = 1200,
  units = "px",
  scale = 6,
  dpi = 400,
  bg = bg_color,
)



data_commuting_north %>% 
  ggplot(aes(area,win_population, size = area,
             color = area))+
  geom_point(alpha = .8)+
  ggrepel::geom_text_repel(aes(label = name),
                           family = "Barlow Condensed",
                           color = labels_color,
                           size = 3,
                           nudge_x = .15,
                           box.padding = 1,
                           nudge_y = 1,
                           segment.curvature = -0.1,
                           segment.ncp = 3,
                           segment.size = 0.2,
                           segment.angle = 20)+
  #ggrepegeom_text(aes(label = name),
  #          size = 2,
  #          family = "Barlow Condensed",
  #          color = labels_color)+
  scale_color_gradient(low = low_color,
                       high = high_color, name = "Area of commuting zone",
                       labels = comma_format(big.mark = ",",
                                             suffix = " km²"))+
  scale_y_continuous(labels = comma_format(big.mark = ",",
                                           suffix = " MLL",
                                           scale = .000001
  ))+
  scale_x_continuous(labels = comma_format(big.mark = ",",
                                           suffix = " km²"))+
  scale_size(range = c(3,12))+
  labs(x = "Area of commuting zone",
       y = "Population Density")+
  theme(plot.margin = margin(2,2,2,2,"cm"),
        legend.position = "none")

ggsave(
  plot = last_plot(),
  filename = "commuting_zones_chart_1.png",
  width = 1200,
  height = 1200,
  units = "px",
  scale = 3,
  dpi = 400,
  bg = NULL,
)


