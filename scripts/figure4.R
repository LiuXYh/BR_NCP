#### First-order reassessment of the global ocean’s metabolic balance in the euphotic zone by correcting bacterial respiration biases

library(reshape2)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)
library(terra)
library(MBA)


# Read data, for calcuLation details, see "scripts/figureS6.R"
dat <- read.csv('../output/global_NCP_grid_EOF.csv')
dat <- subset(dat, Lat > -65 & Lat < 65)


## Annual NCP derived from uncorrected light-dark bottle measurements (Fig. 4a)
dat_year <- subset(dat, Month == 13)

NCP_smooth <- mba.surf(dat_year[c('Lon', 'Lat', 'NCP')], no.X = 200, no.Y = 200, extend = FALSE)
dimnames(NCP_smooth$xyz.est$z) <- list(NCP_smooth$xyz.est$x, NCP_smooth$xyz.est$y)
NCP_smooth <- reshape2::melt(NCP_smooth$xyz.est$z, varnames = c('Lon', 'Lat'), value.name = 'NCP')
NCP_smooth <- na.omit(NCP_smooth)
for (i in 1:nrow(dat_year)) {
	dat_year_i <- NCP_smooth[NCP_smooth$Lon <= (dat_year[i,'Lon']+1) & NCP_smooth$Lat <= (dat_year[i,'Lat']+1), ]
	dat_year_i <- dat_year_i[dat_year_i$Lon >= (dat_year[i,'Lon']-1) & dat_year_i$Lat >= (dat_year[i,'Lat']-1), ]
	dat_year[i,'NCP'] <- mean(dat_year_i$NCP, na.rm = TRUE)
}

dat_year[which(dat_year$NCP %in% c(NA, NaN)),'NCP'] <- 0
dat_year[which(dat_year$NCP <= -300),'NCP'] <- -350
dat_year[which(dat_year$NCP > -300 & dat_year$NCP <= -250),'NCP'] <- -300
dat_year[which(dat_year$NCP > -250 & dat_year$NCP <= -200),'NCP'] <- -250
dat_year[which(dat_year$NCP > -200 & dat_year$NCP <= -150),'NCP'] <- -200
dat_year[which(dat_year$NCP > -150 & dat_year$NCP <= -100),'NCP'] <- -150
dat_year[which(dat_year$NCP > -100 & dat_year$NCP <= -50),'NCP'] <- -100
dat_year[which(dat_year$NCP > -50 & dat_year$NCP < 0),'NCP'] <- -50
dat_year[which(dat_year$NCP >= 300),'NCP'] <- 350
dat_year[which(dat_year$NCP >= 250 & dat_year$NCP < 300),'NCP'] <- 300
dat_year[which(dat_year$NCP >= 200 & dat_year$NCP < 250),'NCP'] <- 250
dat_year[which(dat_year$NCP >= 150 & dat_year$NCP < 200),'NCP'] <- 200
dat_year[which(dat_year$NCP >= 100 & dat_year$NCP < 150),'NCP'] <- 150
dat_year[which(dat_year$NCP >= 50 & dat_year$NCP < 100),'NCP'] <- 100
dat_year[which(dat_year$NCP > 0 & dat_year$NCP < 50),'NCP'] <- 50

NCP_sf <- st_as_sf(dat_year[c('Lon', 'Lat', 'NCP')], coords = c('Lon', 'Lat'), crs = 4326)
world_map <- ne_countries(scale = 'medium', returnclass = 'sf')
graticules1 <- st_transform(st_as_sf(vect(st_graticule(Lon = seq(-180,180, 1), Lat = seq(-90,90, 1)))), '+proj=robin')
graticules2 <- st_transform(st_as_sf(vect(st_graticule(Lon = seq(-180,180, 60), Lat = seq(-90,90, 30)))), '+proj=robin')

p_NCP <- ggplot() +  
geom_sf(data = NCP_sf, aes(color = NCP), size = 0.4, shape = 15, inherit.aes = FALSE) +
scale_color_gradientn(colors = c('#323596', '#4475b4', '#5b9cc3', '#abd9e7', '#e2f1fa', 'white', '#fce0d2', '#fbbd9e', '#fc694a', '#c8181b', '#a80029'), 
	limits = c(-350, 350),  
	breaks = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), 
	labels = c('<-300', '-250', '-200', '-150', '-100', '-50', '0', '50', '100', '150', '200', '250', '>300')) +
geom_sf(data = graticules1, color = 'transparent') +
geom_sf(data = graticules2, color = '#cbcbcb', linewidth = 0.1, linetype = 2) +
geom_sf(data = world_map, fill = '#a8a8a8', color = NA, size = 0.1) +
coord_sf(crs = st_crs('+proj=robin')) +
theme_minimal() +
labs(color = 'NCP\n(mg C m-2 day-1)')

p_NCP


## Annual mean NCP_corr after correcting for bacterial respiration overestimation caused by bottle confinemen (Fig. 4b)
NCP_corr_smooth <- mba.surf(dat_year[c('Lon', 'Lat', 'NCP_corr')], no.X = 200, no.Y = 200, extend = FALSE)
dimnames(NCP_corr_smooth$xyz.est$z) <- list(NCP_corr_smooth$xyz.est$x, NCP_corr_smooth$xyz.est$y)
NCP_corr_smooth <- reshape2::melt(NCP_corr_smooth$xyz.est$z, varnames = c('Lon', 'Lat'), value.name = 'NCP_corr')
NCP_corr_smooth <- na.omit(NCP_corr_smooth)
for (i in 1:nrow(dat_year)) {
	dat_year_i <- NCP_corr_smooth[NCP_corr_smooth$Lon <= (dat_year[i,'Lon']+1) & NCP_corr_smooth$Lat <= (dat_year[i,'Lat']+1), ]
	dat_year_i <- dat_year_i[dat_year_i$Lon >= (dat_year[i,'Lon']-1) & dat_year_i$Lat >= (dat_year[i,'Lat']-1), ]
	dat_year[i,'NCP_corr'] <- mean(dat_year_i$NCP_corr, na.rm = TRUE)
}

dat_year[which(dat_year$NCP_corr %in% c(NA, NaN)),'NCP_corr'] <- 0
dat_year[which(dat_year$NCP_corr <= -300),'NCP_corr'] <- -350
dat_year[which(dat_year$NCP_corr > -300 & dat_year$NCP_corr <= -250),'NCP_corr'] <- -300
dat_year[which(dat_year$NCP_corr > -250 & dat_year$NCP_corr <= -200),'NCP_corr'] <- -250
dat_year[which(dat_year$NCP_corr > -200 & dat_year$NCP_corr <= -150),'NCP_corr'] <- -200
dat_year[which(dat_year$NCP_corr > -150 & dat_year$NCP_corr <= -100),'NCP_corr'] <- -150
dat_year[which(dat_year$NCP_corr > -100 & dat_year$NCP_corr <= -50),'NCP_corr'] <- -100
dat_year[which(dat_year$NCP_corr > -50 & dat_year$NCP_corr < 0),'NCP_corr'] <- -50
dat_year[which(dat_year$NCP_corr >= 300),'NCP_corr'] <- 350
dat_year[which(dat_year$NCP_corr >= 250 & dat_year$NCP_corr < 300),'NCP_corr'] <- 300
dat_year[which(dat_year$NCP_corr >= 200 & dat_year$NCP_corr < 250),'NCP_corr'] <- 250
dat_year[which(dat_year$NCP_corr >= 150 & dat_year$NCP_corr < 200),'NCP_corr'] <- 200
dat_year[which(dat_year$NCP_corr >= 100 & dat_year$NCP_corr < 150),'NCP_corr'] <- 150
dat_year[which(dat_year$NCP_corr >= 50 & dat_year$NCP_corr < 100),'NCP_corr'] <- 100
dat_year[which(dat_year$NCP_corr > 0 & dat_year$NCP_corr < 50),'NCP_corr'] <- 50

NCP_corr_sf <- st_as_sf(dat_year[c('Lon', 'Lat', 'NCP_corr')], coords = c('Lon', 'Lat'), crs = 4326)

p_NCP_corr <- ggplot() +  
geom_sf(data = NCP_corr_sf, aes(color = NCP_corr), size = 0.4, shape = 15, inherit.aes = FALSE) +
scale_color_gradientn(colors = c('#323596', '#4475b4', '#5b9cc3', '#abd9e7', '#e2f1fa', 'white', '#fce0d2', '#fbbd9e', '#fc694a', '#c8181b', '#a80029'), 
	limits = c(-350, 350),  
	breaks = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), 
	labels = c('<-300', '-250', '-200', '-150', '-100', '-50', '0', '50', '100', '150', '200', '250', '>300')) +
geom_sf(data = graticules1, color = 'transparent') +
geom_sf(data = graticules2, color = '#cbcbcb', linewidth = 0.1, linetype = 2) +
geom_sf(data = world_map, fill = '#a8a8a8', color = NA, size = 0.1) +
coord_sf(crs = st_crs('+proj=robin')) +
theme_minimal() +
labs(color = 'NCP\n(mg C m-2 day-1)')

p_NCP_corr


## Changes in net autotrophic and heterotrophic regions before and after correction (Fig. 4c)
dat_year[which(dat_year$NCP>0 & dat_year$NCP_corr>0),'type'] <- 1
dat_year[which(dat_year$NCP<=0 & dat_year$NCP_corr>0),'type'] <- 0
dat_year[which(dat_year$NCP<=0 & dat_year$NCP_corr<=0),'type'] <- -1
table(dat_year$type)/sum(table(dat_year$type))

# Note:
# -1: (13.4 %), before correction < 0, & after correction < 0
# 0: (52.4 %) before correction < 0 & after correction ≥ 0
# 1:(34.2 %) before correction ≥ 0 & after correction ≥ 0

type_sf <- st_as_sf(dat_year[c('Lon', 'Lat', 'type')], coords = c('Lon', 'Lat'), crs = 4326)

p_compare <- ggplot() +  
geom_sf(data = type_sf, aes(color = type), size = 0.4, shape = 15, inherit.aes = FALSE) +
scale_color_gradientn(colors = c('#4475b4', '#fce0d2', '#c8181b'),   
	breaks = c(-1, 0, 1), 
	labels = c('(13.4 %), before correction < 0, & after correction < 0', '(52.4 %) before correction < 0 & after correction ≥ 0', '(34.2 %) before correction ≥ 0 & after correction ≥ 0')) +
geom_sf(data = graticules1, color = 'transparent') +
geom_sf(data = graticules2, color = '#cbcbcb', linewidth = 0.1, linetype = 2) +
geom_sf(data = world_map, fill = '#a8a8a8', color = NA, size = 0.1) +
coord_sf(crs = st_crs('+proj=robin')) +
theme_minimal() +
labs(color = 'Type')

p_compare

