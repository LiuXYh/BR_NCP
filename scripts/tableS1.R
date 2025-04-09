#### Comparison of heterotrophic areas in subtropical oligotrophic gyres before and after bias correction in light-dark bottle measurements

library(doBy)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)
library(terra)


# Subtropical oligotrophic gyres are defined by climatological Chla < 0.1 mg m−3 (Dai et al. 2023; Dave et al. 2015)
dat <- read.csv('../data/WOA_RS_grid.csv')
Chla_year <- summaryBy(Chla~Lon+Lat, dat, FUN = function(x) mean(x, na.rm = TRUE))
names(Chla_year) <- c('Lon', 'Lat', 'Chla')
Chla_year <- subset(Chla_year, Chla < 0.1)


# Examine the global distribution of Chla < 0.1 mg m-3
Chla_sf <- st_as_sf(Chla_year[c('Lon', 'Lat', 'Chla')], coords = c('Lon', 'Lat'), crs = 4326)
world_map <- ne_countries(scale = 'medium', returnclass = 'sf')
graticules1 <- st_transform(st_as_sf(vect(st_graticule(Lon = seq(-180,180, 1), Lat = seq(-90,90, 1)))), '+proj=robin')
graticules2 <- st_transform(st_as_sf(vect(st_graticule(Lon = seq(-180,180, 60), Lat = seq(-90,90, 30)))), '+proj=robin')

p_Chla <- ggplot() +  
geom_sf(data = Chla_sf, aes(color = log10(Chla)), size = 0.5, shape = 15, inherit.aes = FALSE) +
scale_color_gradientn(colors = c('#000084', '#004FFF', '#0EFFF0', '#8DFF70', '#F8FF05', '#FF6900', '#8D0000'), 
	limits = c(-2, 1), 
	breaks = c(-2, -1, 0, 1), 
	labels = c('0.01', '0.1', '1', '10')) +
geom_sf(data = graticules1, color = 'transparent') +
geom_sf(data = graticules2, color = '#cbcbcb', linewidth = 0.1, linetype = 2) +
geom_sf(data = world_map, fill = '#a8a8a8', color = NA, size = 0.1) +
coord_sf(crs = st_crs('+proj=robin')) +
theme_minimal() +
labs(color = 'Chla mg m-3')

p_Chla


# Based on the map above, further select the appropriate range for oligotrophic gyres
NPSG1 <- subset(Chla_year, Lat > 5 & Lat < 40 & Lon > 125 & Lon < 180)
SPSG1 <- subset(Chla_year, Lat > -40 & Lat < -5 & Lon > 165 & Lon < 180)
NPSG2 <- subset(Chla_year, Lat > 0 & Lat < 40 & Lon > -180 & Lon < -110)
SPSG2 <- subset(Chla_year, Lat > -40 & Lat < 0 & Lon > -180 & Lon < -80)
NPSG <- rbind(NPSG1, NPSG2)
SPSG <- rbind(SPSG1, SPSG2)
NASG <- subset(Chla_year, Lat > 5 & Lat < 40 & Lon > -75 & Lon < -10)
SASG <- subset(Chla_year, Lat > -40 & Lat < -5 & Lon > -60 & Lon < 10)
IOSG <- subset(Chla_year, Lat > -40 & Lat < -10 & Lon > 50 & Lon < 110)
NPSG$gyres <- 'NPSG'
SPSG$gyres <- 'SPSG'
NASG$gyres <- 'NASG'
SASG$gyres <- 'SASG'
IOSG$gyres <- 'IOSG'
ocean_gyre <- rbind(NPSG, SPSG, NASG, SASG, IOSG)

ocean_gyre_sf <- st_as_sf(ocean_gyre[c('Lon', 'Lat', 'Chla')], coords = c('Lon', 'Lat'), crs = 4326)

p_ocean_gyre <- ggplot() +  
geom_sf(data = ocean_gyre_sf, aes(color = log10(Chla)), size = 0.5, shape = 15, inherit.aes = FALSE) +
scale_color_gradientn(colors = c('#000084', '#004FFF', '#0EFFF0', '#8DFF70', '#F8FF05', '#FF6900', '#8D0000'), 
	limits = c(-2, 1), 
	breaks = c(-2, -1, 0, 1), 
	labels = c('0.01', '0.1', '1', '10')) +
geom_sf(data = graticules1, color = 'transparent') +
geom_sf(data = graticules2, color = '#cbcbcb', linewidth = 0.1, linetype = 2) +
geom_sf(data = world_map, fill = '#a8a8a8', color = NA, size = 0.1) +
coord_sf(crs = st_crs('+proj=robin')) +
theme_minimal() +
labs(color = 'Chla mg m-3')

p_ocean_gyre


# Calculate the area of oligotrophic gyres and the heterotrophic areas before and after respiration correction
# The resulting values are recorded in Table S1
dat <- read.csv('../output/global_NCP_grid_EOF.csv')
dat_year <- subset(dat, Month == 13)

dat_year <- na.omit(merge(dat_year, ocean_gyre, by = c('Lon', 'Lat'), all.y = TRUE))
dat_year[which(dat_year$NCP >= 0),'Before_correction'] <- 'Autotrophy'
dat_year[which(dat_year$NCP < 0),'Before_correction'] <- 'Heterotrophy'
dat_year[which(dat_year$NCP_corr >= 0),'After_correction'] <- 'Autotrophy'
dat_year[which(dat_year$NCP_corr < 0),'After_correction'] <- 'Heterotrophy'
dat_year$area <- 110000*cos(abs(dat_year$Lat)*pi/180)*110000/1000000/1000000

summaryBy(area~gyres, dat_year, FUN = sum)  # unit: 10^6 km2
summaryBy(area~gyres+Before_correction, dat_year, FUN = sum)
summaryBy(area~gyres+After_correction, dat_year, FUN = sum)

