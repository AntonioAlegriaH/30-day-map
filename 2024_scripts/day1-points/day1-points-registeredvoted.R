# Day 1: Points -----------------------------------------------------------

pacman::p_load(dplyr,
               tidyr,
               sf,
               ggplot2,
               scales,
               stringr,
               osmdata,
               urbnmapr,
               install = FALSE)



# 1- import and clean data ----------------------------------------------------------


path <- "day-1/data/vote04b_2022.csv"
#source https://www.census.gov/data/tables/time-series/demo/voting-and-registration/p20-586.html

#numer in thousands

data <- readr::read_csv(path) %>% 
  janitor::clean_names() %>% 
  mutate(state= str_to_title(state))

glimpse(data)


states_sf <- get_urbn_map("states", sf = TRUE) %>% 
  st_as_sf() %>% 
  st_transform(crs = 9311) %>% 
  rename(state = state_name) %>% 
  mutate(state = str_to_title(state))


plot(states_sf)

unique(data$state)
setdiff(unique(data$state), unique(states_sf$state))


data_join <- data %>% 
  select(state, characteristics,total_registered, total_voted) %>% 
  right_join(.,states_sf, by = "state") %>% 
  mutate(voting_rate = (total_voted / total_registered) *100) %>% 
  st_as_sf() 
data_join_centroid <- data_join %>% 
  st_centroid()


data_join %>%
  filter(characteristics == "Total") %>% 
  ggplot()+
  geom_sf(aes(fill = voting_rate),
          show.legend = FALSE)+
  geom_sf_text(aes(label =glue::glue("{state}\n","{round(voting_rate)}%")),
               color = "white")+
  scale_fill_gradient(low = "#56B1F7",
                      high = "#132B43") +
    theme(panel.background = element_blank(),
          panel.grid = element_line(color = "#f1f1f1"))
    


# Plot legend combined Light
states_sf %>% 
  ggplot()+
  ggfx::with_shadow(
    geom_sf(fill = "white",
            color = "grey60",
            alpha = .9),
    colour = "#132B43",
    sigma = 0,
    x_offset = 7, 
    y_offset = 7
  )+
  geom_sf(data = data_join_centroid %>% 
            filter(characteristics == "Total"),
          aes(fill = voting_rate,
              size = voting_rate,),
          color = "#444444",
          linewidth = 2,
          alpha = .7, shape = 21,
          show.legend = TRUE)+
  ggrepel::geom_text_repel(
    data = data_join_centroid %>% 
      filter(characteristics == "Total"),
    aes(label =glue::glue("{state}\n","{round(voting_rate)}%"),
        geometry = geometry
        ),
    color = "white",     
    bg.color = "grey30", 
    bg.r = 0.15,         
    size = 3.5,
    family = "Barlow Condensed",
    stat = "sf_coordinates",
    min.segment.length = 0,
    force_pull = 1,
    segment.color	= "#f1f1f1"
  )+
  labs(title = "VOTING RATE 2022",
       subtitle = "How many people registered to vote vs How many actually voted"
       )+
  scale_size(range = c(1,23),
             name = "Voting Rate Percentage Scale",
             labels = label_percent(scale = 1) # Convert to percentage
             )+ 
  scale_fill_gradient(low = "#56B1F7",
                     high = "#132B43",
                     name = "Voting Rate Percentage Scale",
                     guide = "legend",
                     labels = label_percent(scale = 1)  # Convert to percentage
                     ) +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 70,
                                  family = "Barlow",
                                  face = "bold",
                                  color = "#333333",
                                  hjust = .5),
        plot.subtitle = element_text(family = "Barlow Condensed",
                                     face = "italic",
                                     color = "#555555",
                                     size = 20,
                                     hjust = .5),
        panel.grid = element_line(color = "#E0E0E0"),
        plot.background = element_blank(),
        axis.title = element_blank(),
        text = element_text(family = "Barlow_Condensed"),
        axis.text = element_text(size = 8),
        legend.background = element_blank(),
        legend.position = "bottom", 
        legend.text.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = .5,
                                    size = 20,
                                    family = "Barlow Condensed",
                                    margin = margin(0,0,-2,0,"pt")),
        legend.text = element_text(family = "Barlow Condensed",
                                   color = "#ffffff",
                                   face = "bold",
                                   margin = margin(-35,0,0,0,"pt"),),
        plot.margin = margin(0,50,0,50, "pt"))



ggsave(
plot = last_plot(),
filename = "voting_rate_1.png",
width = 1200,
height = 1200,
units = "px",
scale = 4,
bg = "#F5F5F5"
)



# Blue version

states_sf %>% 
  ggplot()+
  ggfx::with_shadow(
    geom_sf(fill = "white",
            color = "grey60",
            alpha = .9),
    colour = "#56B1F7",
    sigma = 0,
    x_offset = 9, 
    y_offset = 9
  )+
  
  geom_sf(data = data_join_centroid %>% 
            filter(characteristics == "Total"),
          aes(fill = voting_rate,
              size = voting_rate,),
          color = "#444444",
          linewidth = 1,
          alpha = .7, shape = 21,
          show.legend = TRUE)+
  ggrepel::geom_text_repel(
    data = data_join_centroid %>% 
      filter(characteristics == "Total"),
    aes(label =glue::glue("{state}\n","{round(voting_rate)}%"),
        geometry = geometry
        ),
    color = "white",     # text color
    bg.color = "grey30", # shadow color
    bg.r = 0.15,          # shadow radius
    size = 3.5,
    family = "Barlow Condensed",
    stat = "sf_coordinates",
    min.segment.length = 0,
    force_pull = 1,
    segment.color	= "#f1f1f1"
  )+
  labs(title = "VOTING RATE, 2022",
       subtitle = "Number of Registered Voters vs. Number of Actual Voters"
       )+
  scale_size(range = c(1,23),
             name = "Voting Rate (% Scale)",
             labels = label_percent(scale = 1) # Convert to percentage
             )+ 
  scale_fill_gradient(low = "#56B1F7",
                     high = "#132B43",
                     name = "Voting Rate (% Scale)",
                     guide = "legend",
                     labels = label_percent(scale = 1)  # Convert to percentage
                     ) +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 70,
                                  family = "Barlow",
                                  face = "bold",
                                  color = "#F5F5F5",
                                  hjust = .5),
        plot.subtitle = element_text(family = "Barlow Condensed",
                                     face = "italic",
                                     color = "#F1F1F1",
                                     size = 20,
                                     hjust = .5),
        panel.grid = element_line(color = "#3D3D3D"),
        plot.background = element_blank(),
        axis.title = element_blank(),
        text = element_text(family = "Barlow_Condensed"),
        axis.text = element_text(size = 8,
                                 color = "#F1F1F1"),
        legend.background = element_blank(),
        legend.position = "bottom", 
        legend.text.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = .5,
                                    size = 20,
                                    family = "Barlow Condensed",
                                    margin = margin(0,0,0,0,"pt"),
                                    color = "#F5F5F5"),
        legend.text = element_text(family = "Barlow Condensed",
                                   color = "#ffffff",
                                   face = "bold",
                                   size = 10,
                                   margin = margin(-35,0,0,0,"pt")),
        plot.margin = margin(0,50,0,50, "pt"))



ggsave(
plot = last_plot(),
filename = "voting_rate_dark.png",
width = 1200,
height = 1200,
units = "px",
scale = 3.5,
bg = "#132B43"
)
