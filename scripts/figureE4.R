#### Monthly climatology in depth-integrated net community production above the euphotic zone

library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)
library(terra)
library(MBA)


# Read data, for calcuLation details, see "scripts/figureS7.R"
dat <- read.csv('../output/global_NCP_grid_EOF.csv')


# Plot robinson map showing monthly climatological NCP
for (n in 1:12) {
	
	dat_month <- subset(dat, Month == n)
	
	month_smooth <- mba.surf(dat_month[c('Lon', 'Lat', 'NCP_corr')], no.X = 200, no.Y = 200, extend = FALSE)
	dimnames(month_smooth$xyz.est$z) <- list(month_smooth$xyz.est$x, month_smooth$xyz.est$y)
	month_smooth <- reshape2::melt(month_smooth$xyz.est$z, varnames = c('Lon', 'Lat'), value.name = 'NCP_corr')
	month_smooth <- na.omit(month_smooth)
	for (i in 1:nrow(dat_month)) {
		dat_month_i <- month_smooth[month_smooth$Lon <= (dat_month[i,'Lon']+1) & month_smooth$Lat <= (dat_month[i,'Lat']+1), ]
		dat_month_i <- dat_month_i[dat_month_i$Lon >= (dat_month[i,'Lon']-1) & dat_month_i$Lat >= (dat_month[i,'Lat']-1), ]
		dat_month[i,'NCP_corr'] <- mean(dat_month_i$NCP_corr, na.rm = TRUE)
	}
	
	dat_month[which(dat_month$NCP_corr %in% c(NA, NaN)),'NCP_corr'] <- 0
	dat_month[which(dat_month$NCP_corr <= -300),'NCP_corr'] <- -350
	dat_month[which(dat_month$NCP_corr > -300 & dat_month$NCP_corr <= -250),'NCP_corr'] <- -300
	dat_month[which(dat_month$NCP_corr > -250 & dat_month$NCP_corr <= -200),'NCP_corr'] <- -250
	dat_month[which(dat_month$NCP_corr > -200 & dat_month$NCP_corr <= -150),'NCP_corr'] <- -200
	dat_month[which(dat_month$NCP_corr > -150 & dat_month$NCP_corr <= -100),'NCP_corr'] <- -150
	dat_month[which(dat_month$NCP_corr > -100 & dat_month$NCP_corr <= -50),'NCP_corr'] <- -100
	dat_month[which(dat_month$NCP_corr > -50 & dat_month$NCP_corr < 0),'NCP_corr'] <- -50
	dat_month[which(dat_month$NCP_corr >= 300),'NCP_corr'] <- 350
	dat_month[which(dat_month$NCP_corr >= 250 & dat_month$NCP_corr < 300),'NCP_corr'] <- 300
	dat_month[which(dat_month$NCP_corr >= 200 & dat_month$NCP_corr < 250),'NCP_corr'] <- 250
	dat_month[which(dat_month$NCP_corr >= 150 & dat_month$NCP_corr < 200),'NCP_corr'] <- 200
	dat_month[which(dat_month$NCP_corr >= 100 & dat_month$NCP_corr < 150),'NCP_corr'] <- 150
	dat_month[which(dat_month$NCP_corr >= 50 & dat_month$NCP_corr < 100),'NCP_corr'] <- 100
	dat_month[which(dat_month$NCP_corr > 0 & dat_month$NCP_corr < 50),'NCP_corr'] <- 50
	
	NCP_corr_sf <- st_as_sf(dat_month[c('Lon', 'Lat', 'NCP_corr')], coords = c('Lon', 'Lat'), crs = 4326)
	world_map <- ne_countries(scale = 'medium', returnclass = 'sf')
	graticules1 <- st_transform(st_as_sf(vect(st_graticule(Lon = seq(-180,180, 1), Lat = seq(-90,90, 1)))), '+proj=robin')
	graticules2 <- st_transform(st_as_sf(vect(st_graticule(Lon = seq(-180,180, 60), Lat = seq(-90,90, 30)))), '+proj=robin')
	
	p <- ggplot() +  
		geom_sf(data = NCP_corr_sf, aes(color = NCP_corr), size = 0.4, shape = 15, inherit.aes = FALSE) +
		scale_color_gradientn(colors = c('#323596', '#4475b4', '#5b9cc3', '#abd9e7', '#e2f1fa', 'white', '#fce0d2', '#fbbd9e', '#fc694a', '#c8181b', '#a80029'), 
			limits = c(-350, 350),  
			breaks = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), 
			labels = c('<-300', '-250', '-200', '-150', '-100', '-50', '0', '50', '100', '150', '200', '250', '>300')) +
		geom_sf(data = graticules1, color = 'transparent') +
		geom_sf(data = graticules2, color = '#cbcbcb', linewidth = 0.1, linetype = 2) +
		geom_sf(data = world_map, fill = '#a8a8a8', color = NA, size = 0.1) +
		coord_sf(crs = st_crs('+proj=robin')) +
		theme_minimal()+
		labs(color = 'NCP\n(mg C m-2 day-1)')
	
	ggsave(paste0('../output/Month-', n, '-NCP_corr.png'), p, width = 6, height = 3)
}

