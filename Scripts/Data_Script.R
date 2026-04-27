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
    y = "Number of Landslide Events",
    caption = "Source: DOGAMI SLIDO Database"
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
    y = "Number of Landslide Events",
    caption = "Source: DOGAMI SLIDO Database"
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
    caption = "Source: DOGAMI SLIDO Database",
    x="Longitude",
    y="Latitude"
  )