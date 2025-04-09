#### First-order reassessment of the global ocean’s metabolic balance in the euphotic zone by correcting bacterial respiration biases

library(reshape2)
library(doBy)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)
library(terra)
library(MBA)


# Read data, for calcuLation details, see "scripts/figureS7.R"
dat <- read.csv('../output/global_NCP_grid_EOF.csv')


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


# Choose the subtropical oligotrophic gyres for the calculation. For detailed information, see "scripts/tableS1.R"
dat <- read.csv('../data/WOA_RS_grid.csv')
Chla_year <- summaryBy(Chla~Lon+Lat, dat, FUN = function(x) mean(x, na.rm = TRUE))
names(Chla_year) <- c('Lon', 'Lat', 'Chla')
Chla_year <- subset(Chla_year, Chla < 0.1)

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


## Comparison of annually integrated regional NCP before and after correction in five subtropical oligotrophic gyres (Fig. 4c)

dat <- read.csv('../output/global_NCP_grid_EOF.csv')
dat_year <- subset(dat, Month == 13)
dat_year <- na.omit(merge(dat_year, ocean_gyre, by = c('Lon', 'Lat'), all.y = TRUE))

dat_year$Lat_sum <- 110000*cos(abs(dat_year$Lat)*pi/180)*110000*(dat_year$NCP*365/1000)/10^15
dat_year_sum1 <- summaryBy(Lat_sum~gyres, dat_year, FUN = sum)
dat_year_sum1$NCP <- 'Before_correction'
dat_year$Lat_sum <- 110000*cos(abs(dat_year$Lat)*pi/180)*110000*(dat_year$NCP_corr*365/1000)/10^15
dat_year_sum2 <- summaryBy(Lat_sum~gyres, dat_year, FUN = sum)
dat_year_sum2$NCP <- 'After_correction'
dat_year_sum <- rbind(dat_year_sum1, dat_year_sum2)

dat_year_sum$gyres <- factor(dat_year_sum$gyres, levels = c('NASG', 'SASG', 'NPSG', 'SPSG', 'IOSG'))
dat_year_sum$NCP <- factor(dat_year_sum$NCP, levels = c('Before_correction', 'After_correction'))

p_NCP_gyre <- ggplot(dat_year_sum, aes(gyres, Lat_sum.sum, fill = NCP)) +
geom_col(position = 'dodge', color = 'black') +
scale_fill_manual(values = c('#4475b4', '#fde9df')) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
scale_y_continuous(limits = c(-3, 1), expand = c(0, 0)) +
geom_hline(yintercept = 0) +
labs(x = '', y = 'NCP (Pg C yr−1)', fill = '')

p_NCP_gyre

## Temporal variation of monthly averaged daily rates of corrected NCP in the five subtropical oligotrophic gyres (Fig. 4e)

dat <- read.csv('../output/global_NCP_grid_EOF.csv')
dat_month <- subset(dat, Month != 13)
dat_month <- na.omit(merge(dat_month, ocean_gyre, by = c('Lon', 'Lat'), all.y = TRUE))
dat_month <- summaryBy(NCP_corr~gyres+Month, dat_month, FUN = c(mean, sd))

dat_month$gyres <- factor(dat_month$gyres, levels = c('NASG', 'SASG', 'NPSG', 'SPSG', 'IOSG'))

p_NCP_gyre_month <- ggplot(dat_month, aes(Month, NCP_corr.mean)) +
geom_errorbar(aes(ymin = NCP_corr.mean-NCP_corr.sd, ymax = NCP_corr.mean+NCP_corr.sd, color = gyres), position = position_dodge(width = 0.5), width = 0.35) +
geom_line(aes(color = gyres), position = position_dodge(width = 0.5)) +
geom_point(aes(color = gyres), position = position_dodge(width = 0.5), size = 2) +
scale_color_manual(values = c('#0d3dc8', '#009c00', '#c52125', '#de8c26', '#a800f4')) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
scale_x_continuous(breaks = c(1:12), expand = c(0, 0)) +
scale_y_continuous(limits = c(-500, 500), expand = c(0, 0)) +
geom_hline(yintercept = 0) +
labs(y = 'NCP (mg C m−2 day−1)', color = '')

p_NCP_gyre_month

