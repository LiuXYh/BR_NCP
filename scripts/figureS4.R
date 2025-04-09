#### Several non-bottle-based estimates of net community production and carbon export from previous studies

library(ggplot2)
library(doBy)
library(sf)
library(rnaturalearth)
library(dplyr)
library(terra)


RS_model <- read.csv('../data/WOA_RS_grid.csv')
BG_model <- read.csv('../data/inverse_biogeochemical_model.csv')


# Satellite-derived estimates of NCP from O2/Ar observations, calculated as Li and Cassar (2016) (Fig. S4a)
RS_model$Li_Cassar <- (8.57*RS_model$CbPM)/(17.9+RS_model$SST)
model <- summaryBy(Li_Cassar~Lon+Lat, RS_model, FUN = function(x) mean(x, na.rm = TRUE))  # annual mean
names(model)[3] <- 'NCP'

model[which(model$NCP >= 300),'NCP'] <- 350
model[which(model$NCP >= 250 & model$NCP < 300),'NCP'] <- 300
model[which(model$NCP >= 200 & model$NCP < 250),'NCP'] <- 250
model[which(model$NCP >= 150 & model$NCP < 200),'NCP'] <- 200
model[which(model$NCP >= 100 & model$NCP < 150),'NCP'] <- 150
model[which(model$NCP >= 50 & model$NCP < 100),'NCP'] <- 100
model[which(model$NCP > 0 & model$NCP < 50),'NCP'] <- 50
model[which(model$NCP %in% c(NA, NaN)),'NCP'] <- 0

model_sf <- st_as_sf(model[c('Lon', 'Lat', 'NCP')], coords = c('Lon', 'Lat'), crs = 4326)  # robinson map
world_map <- ne_countries(scale = 'medium', returnclass = 'sf')
graticules1 <- st_transform(st_as_sf(vect(st_graticule(lon = seq(-180,180, 1), lat = seq(-90,90, 1)))), '+proj=robin')
graticules2 <- st_transform(st_as_sf(vect(st_graticule(lon = seq(-180,180, 60), lat = seq(-90,90, 30)))), '+proj=robin')

p_Li_Cassar <- ggplot() +
geom_sf(data = model_sf, aes(color = NCP), size = 0.5, shape = 15, inherit.aes = FALSE) +
scale_color_gradientn(colors = c('white', '#fce0d2', '#fbbd9e', '#fc694a', '#c8181b', '#a80029'), 
	limits = c(0, 350), 
	breaks = c(0, 50, 100, 150, 200, 250, 300), 
	labels = c('0', '50', '100', '150', '200', '250', '>300')) +
geom_sf(data = graticules1, color = 'transparent') +
geom_sf(data = graticules2, color = '#cbcbcb', linewidth = 0.1, linetype = 2) +
geom_sf(data = world_map, fill = '#a8a8a8', color = NA, size = 0.1) +
coord_sf(crs = st_crs('+proj=robin')) +
theme_minimal() +
labs(color = 'NCP\n(mg C m-2 day-1)')

p_Li_Cassar


# Global carbon export estimates obtained from an inverse biogeochemical model, calculated as Wang et al (2023) (Fig. S4b)
for (i in 1:nrow(model)) {
    BG_model1 <- BG_model[BG_model$Lon <= (model[i,'Lon']+1) & BG_model$Lat <= (model[i,'Lat']+1), ]
    BG_model1 <- BG_model1[BG_model1$Lon >= (model[i,'Lon']-1) & BG_model1$Lat >= (model[i,'Lat']-1), ]
    model[i,'NCP'] <- mean(BG_model1$TOCflux, na.rm = TRUE)
}

model[which(model$NCP >= 300),'NCP'] <- 350
model[which(model$NCP >= 250 & model$NCP < 300),'NCP'] <- 300
model[which(model$NCP >= 200 & model$NCP < 250),'NCP'] <- 250
model[which(model$NCP >= 150 & model$NCP < 200),'NCP'] <- 200
model[which(model$NCP >= 100 & model$NCP < 150),'NCP'] <- 150
model[which(model$NCP >= 50 & model$NCP < 100),'NCP'] <- 100
model[which(model$NCP > 0 & model$NCP < 50),'NCP'] <- 50
model[which(model$NCP %in% c(NA, NaN)),'NCP'] <- 0

model_sf <- st_as_sf(model[c('Lon', 'Lat', 'NCP')], coords = c('Lon', 'Lat'), crs = 4326)  # robin map

p_Wang <- ggplot() +
geom_sf(data = model_sf, aes(color = NCP), size = 0.5, shape = 15, inherit.aes = FALSE) +
scale_color_gradientn(colors = c('white', '#fce0d2', '#fbbd9e', '#fc694a', '#c8181b', '#a80029'), 
	limits = c(0, 350), 
	breaks = c(0, 50, 100, 150, 200, 250, 300), 
	labels = c('0', '50', '100', '150', '200', '250', '>300')) +
geom_sf(data = graticules1, color = 'transparent') +
geom_sf(data = graticules2, color = '#cbcbcb', linewidth = 0.1, linetype = 2) +
geom_sf(data = world_map, fill = '#a8a8a8', color = NA, size = 0.1) +
coord_sf(crs = st_crs('+proj=robin')) +
theme_minimal() +
labs(color = 'TOC flux\n(mg C m-2 day-1)')

p_Wang


# Global carbon export estimates based on the 234Th-derived empirical carbon export ratio (f-ratio), calculated as Henson et al (2011) (Fig. S4c)
RS_model$Henson <- RS_model$CbPM*0.23*exp(-0.08*RS_model$SST)
model <- summaryBy(Henson~Lon+Lat, RS_model, FUN = function(x) mean(x, na.rm = TRUE))  # annual mean
names(model)[3] <- 'NCP'

model[which(model$NCP >= 300),'NCP'] <- 350
model[which(model$NCP >= 250 & model$NCP < 300),'NCP'] <- 300
model[which(model$NCP >= 200 & model$NCP < 250),'NCP'] <- 250
model[which(model$NCP >= 150 & model$NCP < 200),'NCP'] <- 200
model[which(model$NCP >= 100 & model$NCP < 150),'NCP'] <- 150
model[which(model$NCP >= 50 & model$NCP < 100),'NCP'] <- 100
model[which(model$NCP > 0 & model$NCP < 50),'NCP'] <- 50
model[which(model$NCP %in% c(NA, NaN)),'NCP'] <- 0

model_sf <- st_as_sf(model[c('Lon', 'Lat', 'NCP')], coords = c('Lon', 'Lat'), crs = 4326)  # robin map

p_Henson <- ggplot() +
geom_sf(data = model_sf, aes(color = NCP), size = 0.5, shape = 15, inherit.aes = FALSE) +
scale_color_gradientn(colors = c('white', '#fce0d2', '#fbbd9e', '#fc694a', '#c8181b', '#a80029'), 
	limits = c(0, 350), 
	breaks = c(0, 50, 100, 150, 200, 250, 300), 
	labels = c('0', '50', '100', '150', '200', '250', '>300')) +
geom_sf(data = graticules1, color = 'transparent') +
geom_sf(data = graticules2, color = '#cbcbcb', linewidth = 0.1, linetype = 2) +
geom_sf(data = world_map, fill = '#a8a8a8', color = NA, size = 0.1) +
coord_sf(crs = st_crs('+proj=robin')) +
theme_minimal() +
labs(color = 'POC flux\n(mg C m-2 day-1)')

p_Henson

