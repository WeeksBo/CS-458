# Run when opening session
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
# Load cleaned data
slido_time <- readRDS("C:/OSU/CS_458/Data/Clean_Data/slido_time.rds")
slido_map <- readRDS("C:/OSU/CS_458/Data/Clean_Data/slido_map.rds")
slido <- st_read(
  "C:/OSU/CS_458/Data/Raw_Data/SLIDO_Release_4p5_wMetadata.gdb",
  layer = "Historic_Landslide_Points"
)

yearly_counts <- slido_time %>%
  st_drop_geometry() %>%
  group_by(YEAR) %>%
  summarise(events = n())

yearly_no1996 <- yearly_counts %>%
  filter(YEAR != 1996)
