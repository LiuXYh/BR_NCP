#### Meridional distribution of net community production (NCP) and spatiotemporal patterns within subtropical oligotrophic gyres.

library(reshape2)
library(doBy)
library(ggplot2)


## Meridional distribution of annually averaged NCP or carbon export compared with independent model projections and BGC-Argo field observations (Fig. 5a)
NCP <- read.csv('../output/global_NCP_grid_EOF.csv')
NPP <- read.csv('../data/WOA_RS_grid.csv')
Wang <- read.csv('../data/inverse_biogeochemical_model.csv')
Argo <- read.csv('../data/BGC-Argo.csv')

Wang <- summaryBy(TOCflux~Lat, Wang, FUN = function(x) mean(x, na.rm = TRUE))
names(Wang)[2] <- 'NCP'
Wang$type <- 'Inverse biogeochemical model'  # Wang et al (2023)
Wang <- Wang[c('Lat', 'NCP', 'type')]

NPP$O2_Ar <- (8.57*NPP$CbPM)/(17.9+NPP$SST)
NPP[which(is.na(NPP$O2_Ar)),'O2_Ar'] <- 0
Li <- summaryBy(O2_Ar~Lat, NPP, FUN = mean)
names(Li)[2] <- 'NCP'
Li$type <- 'O2/Ar-based model'  # Li and Cassar (2016)
Li <- Li[c('Lat', 'NCP', 'type')]

NPP$f_ratio <- NPP$CbPM*0.23*exp(-0.08*NPP$SST)
NPP[which(is.na(NPP$f_ratio)),'f_ratio'] <- 0
Henson <- summaryBy(f_ratio~Lat, NPP, FUN = mean)
names(Henson)[2] <- 'NCP'
Henson$type <- '234Th-based model'  # Henson et al (2011)
Henson <- Henson[c('Lat', 'NCP', 'type')]

NCP_year <- subset(NCP, Month == 13)
NCP_uncorr <- summaryBy(NCP~Lat, NCP_year, FUN = c(mean, sd))
names(NCP_uncorr)[2] <- 'NCP'
NCP_uncorr$type <- 'This study (before correction)'
NCP_uncorr <- NCP_uncorr[c('Lat', 'NCP', 'type')]

NCP_corr <- summaryBy(NCP_corr~Lat, NCP_year, FUN = c(mean, sd))
names(NCP_corr)[2] <- 'NCP'
NCP_corr$type <- 'This study (after correction)'
NCP_corr <- NCP_corr[c('Lat', 'NCP', 'type')]

dat <- rbind(Wang, Li, Henson, NCP_uncorr, NCP_corr)

p_NCP_lat_mean <- ggplot() +
geom_line(data = dat, aes(x = Lat, y = NCP, color = type), size = 1.5) +
scale_color_manual(values = c('#323697', '#a8002a', '#388200', '#ff8700', '#6ca9b4'), limits = c('This study (before correction)', 'This study (after correction)', 'O2/Ar-based model', 'Inverse biogeochemical model', '234Th-based model')) +
geom_point(data = Argo, aes(x = Lat, y = NCP), color = '#9cf5ff') +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
labs(x = 'Lat', y = 'mg C m-2 day-1', color = 'NCP or carbon export') +
scale_x_continuous(breaks = c(-80, -60, -40, -20, 0, 20, 40, 60, 80), limits = c(-65, 65), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(-300, 400, 100), limits = c(-300, 400), expand = c(0, 0)) +
geom_hline(yintercept = 0) +
coord_flip()

p_NCP_lat_mean


## Choose the subtropical oligotrophic gyres for the calculation. For detailed information, see "scripts/tableS2.R"
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


## Comparison of annually integrated regional NCP before and after correction in five subtropical oligotrophic gyres (Fig. 5b)

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


## Temporal variation of monthly averaged daily rates of corrected NCP in the five subtropical oligotrophic gyres (Fig. 4c)

dat <- read.csv('../output/global_NCP_grid_EOF.csv')
dat_month <- subset(dat, Month != 13)
dat_month <- na.omit(merge(dat_month, ocean_gyre, by = c('Lon', 'Lat'), all.y = TRUE))
dat_month <- summaryBy(NCP_corr~gyres+Month, dat_month, FUN = c(mean, sd))

dat_month$gyres <- factor(dat_month$gyres, levels = c('NASG', 'SASG', 'NPSG', 'SPSG', 'IOSG'))

p_NCP_gyre_month <- ggplot(dat_month, aes(Month, NCP_corr.mean)) +
#geom_errorbar(aes(ymin = NCP_corr.mean-NCP_corr.sd, ymax = NCP_corr.mean+NCP_corr.sd, color = gyres), position = position_dodge(width = 0.5), width = 0.35) +
#geom_line(aes(color = gyres), position = position_dodge(width = 0.5)) +
#geom_point(aes(color = gyres), position = position_dodge(width = 0.5), size = 2) +
geom_line(aes(color = gyres)) +
geom_point(aes(color = gyres), size = 2) +
scale_color_manual(values = c('#0d3dc8', '#009c00', '#c52125', '#de8c26', '#a800f4')) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
scale_x_continuous(breaks = c(1:12), expand = c(0, 0)) +
scale_y_continuous(limits = c(-200, 200), expand = c(0, 0)) +
geom_hline(yintercept = 0) +
labs(y = 'NCP (mg C m−2 day−1)', color = '')

p_NCP_gyre_month

