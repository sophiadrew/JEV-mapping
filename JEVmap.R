
# JEV Animated Map
# Sophia Drewry

# load packages
library(readr) #for loading Excel files & txt files
library(readxl)
library(tidyr)
library(dplyr) #for data processing
library(stringr)
library(lubridate) #to arrange date form of data
library(raster)
library(ggplot2)
library(viridis)
library(ggthemes)
library(gganimate)
library(av)
library(maps)
library(janitor)
library(zoo)
library(scales)
library(ozmaps)


# load dta
jev <- read_csv("jev-empres.csv")

# data can be found at https://empres-i.apps.fao.org/epidemiology

# cleaning data ----------------------------------------------------------------
# fixing date variable

str(jev$Observation.date..dd.mm.yyyy.)
jev <- jev %>% mutate(date = as.Date(dmy(Observation.date..dd.mm.yyyy.)))
str(jev$date)




# static #######################################################################
# source: https://stackoverflow.com/questions/33227182/how-to-set-use-ggplot2-to-map-a-raster

# shape data
oz_states <- ozmaps::ozmap_states
oz_states <- ozmaps::ozmap_states %>% filter(NAME != "Other Territories")

# city data
oz_capitals <- tibble::tribble( 
  ~city,           ~lat,     ~lon,
  "Sydney",    -33.8688, 151.2093,  
  "Melbourne", -37.8136, 144.9631, 
  "Brisbane",  -27.4698, 153.0251, 
  "Adelaide",  -34.9285, 138.6007, 
  "Perth",     -31.9505, 115.8605, 
  "Hobart",    -42.8821, 147.3272, 
  "Canberra",  -35.2809, 149.1300, 
  "Darwin",    -12.4634, 130.8456, 
)

# Altitude data
x <- getData('alt', country = "AUS")

# changing resolution
res(x)
xup <- aggregate(x, fact = 0.1/res(x)) # aggregate output
res(xup)
test_spdf <- as(xup, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

## high res only
## original map is at this resolution, take 2+ hours to render
#test_spdf <- as(x, "SpatialPixelsDataFrame")
#test_df <- as.data.frame(test_spdf)
#colnames(test_df) <- c("value", "x", "y")


# static map
map <- ggplot() +  
  geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=.05) + 
  geom_sf(data = oz_states, color="white", fill= NA, size=0.25) +
  geom_point(data = jev2, aes(x=Longitude, y=Latitude), color = "#FFE33B", size = 1) +
  geom_point(data = oz_capitals, mapping = aes(x = lon, y = lat), pch = 17, color = "black", size = 1, fill = "black") + 
  geom_text(data = oz_capitals, aes(x = lon, y = lat, label=city),hjust = -.1, vjust = 1) +
  scale_fill_viridis() +
  coord_sf() +
  labs(title = "Japanese Encephilitis Swine Cases in Australia in 2022") + 
  labs(fill = "Meters above\nSea Level") +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  theme(plot.title = element_text(face="bold", size=12, hjust = 0.5))

map

################################################################################
# Animation  ###################################################################
  

jev2 <- jev %>% mutate(date_format = format(date, '%m-%d-%Y')) %>% 
  arrange(date) %>%
  mutate(date_format = factor(date_format, unique(date_format)))

# for dates by week
jevx<- jev %>% 
  group_by(week = format(date, '%Y-%U')) %>%
  arrange(week) %>%
  mutate(week = factor(week, unique(week)))



animated <- ggplot() +  
  #borders(regions ="Australia", fill = "grey50")+
  geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_sf(data = oz_states, color="white", fill= NA, size=0.25) +
  geom_point(data = jev2, aes(x=Longitude, y=Latitude, group = Event.ID), color = "#FFE33B", size = 1) +
  geom_point(data = oz_capitals, mapping = aes(x = lon, y = lat), pch = 17, color = "black", size = 1, fill = "black") + 
  geom_text(data = oz_capitals, aes(x = lon, y = lat, label=city),hjust = -.1, vjust = 1)+
  scale_fill_gradient2( low = "#DCDEB5", mid = "#74C8BD", high = "#39598C", midpoint = 1000) +
  coord_sf() +
  #coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  labs(title = "Japanese Encephilitis Swine Cases in Australia\nDate: {frame_time}") + 
  labs(fill = "Meters above\nSea Level") +
  #transition_states(states = date, transition_length = 2, state_length = 2, wrap = FALSE) +
  transition_time(date) +
  enter_fade() +
  exit_fade() +
  shadow_mark(color = "#b58c19") +
  ease_aes("linear") + 
  theme(plot.title = element_text(face="bold", size=25, hjust = 0.5))

animated

# gif save
gganimate::animate(
    animated,
      width = 1000, height = 800, fps = 8, 
    renderer=gifski_renderer("map.gif"))

# mp4 saving
b <- gganimate::animate(animated,  width = 1000, height = 800, 
                        duration = 15, fps = 8, res=300, renderer = av_renderer())
anim_save("output.mp4", b)










