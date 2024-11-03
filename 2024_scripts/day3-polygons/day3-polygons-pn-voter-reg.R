
# Day 3: Polygons ---------------------------------------------------------

pacman::p_load(
  dplyr,
  tidyr,
  scales,
  sf,
  readr,
  ggplot2,
  stringr,
  install = FALSE
)


#https://www.pa.gov/en/agencies/dos/resources/voting-and-elections-resources/voting-and-election-statistics.html

path_data <- "day-3/data/currentvotestats.xls"

data_registration <- readxl::read_xls(path = path_data,sheet = 1,range = "A2:L69") %>% 
  select(!c(`...4`,`...6`,`...8`,`...10`,`...12`)) %>% 
  mutate(CountyName = str_to_title(CountyName),
         CountyName = case_match(CountyName, "Mckean" ~ "McKean",
                                 .default = CountyName)
         )
### get map data
library(urbnmapr)

state_sf <-  get_urbn_map(map = "counties", sf = TRUE) %>% 
  filter(state_abbv == "PA") %>% 
  mutate(county_name = str_replace(.$county_name, " County", ""))



setdiff(unique(data_registration$CountyName), unique(state_sf$county_name))


pn_voter_registration <- data_registration %>% 
  left_join(state_sf, by = c("CountyName" = "county_name")) %>% 
  janitor::clean_names() %>% 
  mutate(other_no_aff = other+no_aff) %>% 
  st_as_sf()


pn_voter_registration %>% 
  summarise(total_register_d = sum(dem),
            total_register_r = sum(rep),
            total_register_noAff = sum(other_no_aff)
            )



pn_voter_registration_long <- pn_voter_registration %>% 
  select(county_name,dem, rep, other_no_aff, total_count_of_all_voters,geometry) %>% 
  pivot_longer(cols = c(dem, rep, other_no_aff, total_count_of_all_voters),
               names_to ="registration_class",values_to = "n_register") %>% 
  select(county_name, registration_class,n_register, geometry) %>% 
  mutate(centroid = st_centroid(geometry))

  
rm(data_registration, path_data)



# 2-mapping ---------------------------------------------------------------


#Not run#
pn_voter_registration_long %>%
  filter(registration_class == "total_count_of_all_voters") %>% 
ggplot()+
geom_sf(aes(fill = n_register))+
theme_minimal()+
  scale_fill_gradient(low = "#56B1F7",
                      high = "#132B43")

  
pn_voter_registration_long %>%
  filter(registration_class != "total_count_of_all_voters") %>%
  ggplot()+
  geom_sf(aes(fill = n_register),
        show.legend = FALSE)+
  geom_sf(aes(geometry = centroid), color = "#444444", 
          size = .5)+
  ggrepel::geom_text_repel(
    aes(label = glue::glue("{county_name}\n","{comma(n_register)}"), geometry = geometry),
    stat = "sf_coordinates",min.segment.length = .2,
    force_pull = 0,force = .01, 
    box.padding = 0, 
    size = 2, family = "Barlow Condensed",
    color = "white",     # text color
    bg.color = "grey30", # shadow color
    bg.r = 0.15          # shadow radius
  )+
theme_minimal()+
  scale_fill_gradient2(
    low = "#80ffdb",
    mid = "#4ea8de",
    high = "#7400b8",
  )+
  facet_wrap(vars(registration_class))+
  theme(axis.title = element_blank(),
        axis.text = element_blank())



# Calculate the global mean across dem, rep, and other_no_aff classes
global_mean <- pn_voter_registration_long %>%
  filter(registration_class %in% c("dem", "rep", "other_no_aff")) %>%
  summarize(median_n_register = median(n_register, na.rm = TRUE),
            range_max = max(n_register),
            range_min = min(n_register)) 
  



global_mean

plot_registered_voters <- function(affiliation, low_color, mid_color, high_color){
  
  
  pn_voter_registration_long %>%
    filter(registration_class == affiliation) %>%
    ggplot()+
    geom_sf(aes(fill = n_register),
            show.legend = FALSE)+
    geom_sf(aes(geometry = centroid), color = "#444444", 
            size = .5)+
    ggrepel::geom_text_repel(
      aes(label = glue::glue("{county_name}\n","{comma(n_register)}"), geometry = geometry),
      stat = "sf_coordinates",min.segment.length = .2,
      force_pull = 0,force = .01, 
      box.padding = 0, 
      size = 3, family = "Barlow Condensed",
      color = "white",     # text color
      bg.color = "grey50", # shadow color
      bg.r = 0.12          # shadow radius
    )+
    theme_minimal()+
    scale_fill_gradient2(
      midpoint = 	19054,
      low = low_color,
      mid = mid_color,
      high = high_color,
      limits = c(379, 801991)
    )+
    labs(title = affiliation)+
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          plot.title = element_text()
         )
  
  
}



dem <- plot_registered_voters(affiliation = "dem", 
                       low_color = "#caf0f8",
                       mid_color = "#00b4d8",
                       high_color = "#03045e"
                       )

rep <- plot_registered_voters(affiliation = "rep", 
                       low_color = "#caf0f8",
                       mid_color = "#00b4d8",
                       high_color = "#03045e"
                       )

other <- plot_registered_voters(affiliation = "other_no_aff", 
                       low_color = "#caf0f8",
                       mid_color = "#00b4d8",
                       high_color = "#03045e"
                       )

rm(dem, rep, other)




pn_voter_registration_long %>% 
  as_tibble() %>% 
  filter(registration_class != "total_count_of_all_voters") %>% 
  ggplot(aes(reorder(county_name, n_register), n_register))+
  geom_col()+
  scale_y_continuous(labels = comma)+
  coord_flip()+
  facet_wrap(vars(registration_class))




ggsave(
plot = rep,
filename = "test_poly_other_rep.png",
width = 1200,
height = 800,
units = "px",
scale = 3,
bg = "#f1f1f1"
)

#End Not run#

# 3- biscale --------------------------------------------------------------

library(biscale)
library(cowplot)
library(rayshader)

dim_size <- 3
pal_biscale <- "DkViolet2"
pal_biscale <- "GrPink2"


# create classes
data <- bi_class(pn_voter_registration, 
                  x = dem,
                 y = rep, 
                 style = "quantile",
                 dim = dim_size)


map <- ggplot() +
  geom_sf(data = data,
          mapping = aes(fill = bi_class), 
          color = "white", size = 0.1, 
          show.legend = FALSE) +
  geom_sf_text(data = data,
    aes(label = county_name),
    size = .65, family = "Barlow Condensed",
    color = "white"     # text color
  )+
  bi_scale_fill(pal = pal_biscale, 
                dim = dim_size,
                flip_axes = T) +
  labs(
    title = "Pennsylvania",
    subtitle = "Voter registration statistics by county\nInformation as of 28/Oct/2024",
    caption = "Source: pa.gov\n#30DAYMAPCHALLENGE: Day 3 Polygons\nMade by: antonioalegria.io @elmedicobrujo"
  )+
  theme_void()+
  theme(plot.background = element_rect(fill = "white",
                                        color ="white"),
        plot.margin = margin(20,10,20,20,"pt"),
        plot.title = element_text(size = 20,
                                  family = "Barlow Condensed",
                                  face = "bold",
                                  color = "#333333"),
        plot.subtitle = element_text(size = 7,
                                     family = "Barlow Condensed",
                                     margin = margin(0,0,20,0,"pt"),
                                     color = "#555555"),
        plot.caption = element_text(family = "Barlow Condensed",
                                    hjust = 0,
                                    size = 3,
                                    margin = margin(30,0,0,0,"pt"))
        )
  
map
  
  
  
  
legend <- bi_legend(pal = pal_biscale,
                    dim = dim_size,
                    flip_axes = TRUE,
                    xlab = "Higher Dem Registration",
                    ylab = "Higher Rep Registration ",
                    size = 3.5)+
  theme(panel.background = element_rect(fill = NA,
                                        color = NA),
        text = element_text(family = "Barlow Condensed",
                            size = 2.8)
  )



finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.5, 0.07, 0.22, 0.22)


finalPlot

plot_gg(finalPlot, width=3.5, multicore = TRUE, 
        scale=150,pointcontract = 1,background = "white",
        solid = FALSE,offset_edges = TRUE,
        windowsize = c(500,500), sunangle=310,
        zoom = 0.60, phi = 89, theta = 0)

render_camera(zoom = .6,
              phi = 80,
              theta = 0)

file_img_name <- "pn-voter-registration-2.png"

render_highquality(
  filename = file_img_name,
  preview = TRUE,
  interactive = FALSE,
  width = 500*6,
  height = 500*6,
  samples = 512,
  sample_method = "sobol",
  lightdirection = 270,
  lightaltitude = 90,
  parallel = T
)

# For Windows only
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device = "win"  )
