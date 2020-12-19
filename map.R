library(sf)
library(tidyverse)

rus_shp <- read_sf("https://code.highcharts.com/mapdata/countries/ru/custom/ru-all-disputed.geo.json")


rus_shp %>% 
  ggplot(aes()) +
  geom_sf() +
  coord_sf(datum = NA)
