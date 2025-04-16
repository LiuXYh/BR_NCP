#### Global light-dark bottle dataset utilized in this study

library(reshape2)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)
library(terra)


# Distribution of global light-dark bottle incubation sampling stations (Fig. S6)
LD_dataset <- read.csv('../data/light-dark_bottle_dataset.csv')
LD_dataset$station <- paste(LD_dataset$Lon, LD_dataset$Lat, LD_dataset$Date, sep = '-')
LD_dataset <- LD_dataset[!duplicated(LD_dataset$station), ]

LD_dataset_sf <- st_as_sf(LD_dataset[c('Lon', 'Lat', 'NCP')], coords = c('Lon', 'Lat'), crs = 4326)
world_map <- ne_countries(scale = 'medium', returnclass = 'sf')
graticules1 <- st_transform(st_as_sf(vect(st_graticule(lon = seq(-180,180, 1), lat = seq(-90,90, 1)))), '+proj=robin')
graticules2 <- st_transform(st_as_sf(vect(st_graticule(lon = seq(-180,180, 60), lat = seq(-90,90, 30)))), '+proj=robin')

p_station <- ggplot() +
geom_sf(data = graticules1, color = 'transparent') +
geom_sf(data = graticules2, color = '#cbcbcb', linewidth = 0.1, linetype = 2) +
geom_sf(data = world_map, fill = '#a8a8a8', color = NA, size = 0.1) +
coord_sf(crs = st_crs('+proj=robin')) +
geom_sf(data = LD_dataset_sf, size = 1, color = '#eeb421', inherit.aes = FALSE) +
theme_minimal()

p_station

