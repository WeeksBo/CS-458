# Run when opening session
library(sf)
library(dplyr)
library(ggplot2)
# Load cleaned data
slido_time <- readRDS("C:/OSU/CS_458/Data/Clean_Data/slido_time.rds")
slido_map <- readRDS("C:/OSU/CS_458/Data/Clean_Data/slido_map.rds")

yearly_counts <- slido_time %>%
  st_drop_geometry() %>%
  group_by(YEAR) %>%
  summarise(events = n())

yearly_no1996 <- yearly_counts %>%
  filter(YEAR != 1996)

# Plots line graph with 1996
ggplot(yearly_counts , aes(x = YEAR, y = events)) +
  geom_line(aes(color = "Actual Events"), linewidth = 1) +
  geom_smooth(aes(color = "Overall Trend"), method = "loess", se = FALSE, 
              linetype = "dashed") +
  scale_color_manual(
    name = "Legend",
    values = c("Actual Events" = "steelblue", 
               "Overall Trend" = "darkred")
  ) +
  labs(
    title = "Oregon Landslide Events Over Time (1950-2023)",
    x = "Year",
    y = "Number of Landslide Events"
  ) +
  theme_minimal() + 
  theme(
    legend.position = c(.2,.8))

 # Plots line graph with no 1996
ggplot(yearly_no1996, aes(x = YEAR, y = events)) +
  geom_line(aes(color = "Actual Events"), linewidth = 1) +
  geom_smooth(aes(color = "Overall Trend"), method = "loess", se = FALSE, 
              linetype = "dashed") +
  scale_color_manual(
    name = "Legend",
    values = c("Actual Events" = "steelblue", 
               "Overall Trend" = "darkred")
  ) +
  labs(
    title = "Oregon Landslide Events Over Time (1950-2023)",
    subtitle = "1996 excluded",
    x = "Year",
    y = "Number of Landslide Events"
  ) +
  theme_minimal() + 
  theme(
    legend.position = c(.2,.8))

# Get coordinates from the spatial data
slido_coords <- slido_map %>%
  st_transform(crs = 4326) %>%
  st_coordinates() %>%
  as.data.frame()

# Plotting Map of Oregon
oregon <- map_data("state", region="oregon")
or_base <- ggplot(data=oregon,
                  mapping = aes(x=long, y=lat, group=group)) +
  coord_fixed(1.3) +
  geom_polygon(color="black", fill="gray")
or_base
or_county <- map_data("county", region="oregon")
or_base_c <- or_base +
  geom_polygon(data=or_county, fill=NA, color="white") +
  geom_polygon(color="black", fill=NA)
or_base_c
# Add landslide points on top of Oregon Map
or_base_c +
  geom_point(data = slido_coords, 
             aes(x = X, y = Y, group = NULL),
             color = "red", size = 0.5, alpha = 0.3) +
  labs(
    title = "Oregon Landslide Locations (1928-2023)",
    subtitle = "Red Dots Represent Recorded Landslides",
    x="Longitude",
    y="Latitude"
  )

# Create cause_clean object
cause_clean <- slido %>%
  st_drop_geometry() %>%
  mutate(CAUSE_CLEAN = case_when(
    str_detect(tolower(CONTR_FACT), "road|cut slope|fill slope|road fill|steep road") ~ "Road Related",
    str_detect(tolower(CONTR_FACT), "clear cut|clearcut|reforested clearcut") ~ "Clear Cut",
    str_detect(tolower(CONTR_FACT), "natural") ~ "Natural",
    str_detect(tolower(CONTR_FACT), "human") ~ "Human",
    str_detect(tolower(CONTR_FACT), "pre-existing|existing|exisitng") ~ "Pre-existing Slide",
    TRUE ~ "Other/Unknown"
  )) %>%
  filter(CAUSE_CLEAN != "Other/Unknown")

# Then plot from it
cause_clean %>%
  group_by(CAUSE_CLEAN) %>%
  summarise(events = n()) %>%
  arrange(desc(events)) %>%
  ggplot(aes(x = reorder(CAUSE_CLEAN, events), y = events, fill = CAUSE_CLEAN)) +
  geom_col(fill = "darkorange4") +
  coord_flip() +
  labs(
    title = "Top 5 Contributing Factors to Oregon Landslides",
    x = "Contributing Factor",
    y = "Number of Events"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
 


library(viridis)

slido_time %>%
  st_drop_geometry() %>%
#  filter(YEAR != 1996) %>%
  filter(!is.na(TYPE_MOVE)) %>%
  filter(TYPE_MOVE != "") %>%
  mutate(decade = as.factor(floor(YEAR / 10) * 10)) %>%
  mutate(TYPE_CLEAN = case_when(
    str_detect(tolower(TYPE_MOVE), "debris|flow") ~ "Debris Flow",
    str_detect(tolower(TYPE_MOVE), "slide|earthslide") ~ "Slide",
    str_detect(tolower(TYPE_MOVE), "fall|rock") ~ "Rock Fall",
    str_detect(tolower(TYPE_MOVE), "earthflow|efl") ~ "Earthflow",
    TRUE ~ "Other"
  )) %>%
  group_by(TYPE_CLEAN, decade) %>%
  summarise(events = n(), .groups = "drop") %>%
  ggplot(aes(x = decade, y = TYPE_CLEAN, fill = events)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Events") +
  labs(
    title = "Oregon Landslide Types by Decade",
    x = "Decade",
    y = "Landslide Type",
    caption = "Source: DOGAMI SLIDO Database"
  ) +
  theme_minimal()

ggsave("Visualizations/heatmap.png", width = 10, height = 8, dpi = 300)