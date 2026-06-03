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

precip <- read.csv("C:/OSU/CS_458/Data/Raw_Data/percipitation.csv")

precip_clean <- precip %>%
  group_by(DATE) %>%
  summarise(
    PRCP = mean(PRCP, na.rm = TRUE),
    TAVG = mean(TAVG, na.rm = TRUE)
  ) %>%
  rename(YEAR = DATE)

#roads <- st_read(
#  "Data/Raw_Data/Transportation_Road.gdb",
#  layer = "Transportation_Statewide_Road"
#)

precip_2025 <- read.csv("C:/OSU/CS_458/Data/Raw_Data/Oregon_percipitation_data.csv")
precip_2025_clean <- precip_2025 %>%
  filter(!is.na(PRCP))

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

slope_data <- slido %>%
  st_drop_geometry() %>%
  filter(!is.na(SLOPE))


fires <- read.csv("C:/OSU/CS_458/Data/Raw_Data/ODF_Fire.csv")

# Clean and filter to Oregon fires with coordinates
fires_clean <- fires %>%
  filter(!is.na(Latitude), !is.na(Longitude), 
         !is.na(FinalFireSizeAcres), FinalFireSizeAcres > 0) %>%
  select(FireYear, Latitude, Longitude, FinalFireSizeAcres, County)

# Filter to larger fires only (over 100 acres)
fires_large <- fires_clean %>%
  filter(FinalFireSizeAcres >= 100)

# Remove outlier fires and bad coordinates
fires_large <- fires_clean %>%
  filter(FinalFireSizeAcres >= 100,
         FinalFireSizeAcres < 100000,
         Latitude > 41, Latitude < 47,
         Longitude > -125, Longitude < -116)


