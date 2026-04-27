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