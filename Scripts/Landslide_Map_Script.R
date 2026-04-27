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


