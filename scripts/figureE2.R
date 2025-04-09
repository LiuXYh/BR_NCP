#### Time-series observations of bacterial abundance and bacterial production during bottle incubations at three selected stations

library(reshape2)
library(ggplot2)
library(patchwork)


# Locations of the three stations with high-frequency sampling (Fig. E2a)
dat <- read.csv('../data/high-frequency_sampling.csv')

map <- map_data('world')
map <- subset(map, long > 95 & long < 135)
map <- subset(map, lat > 0 & lat < 45)

p <- ggplot() +
geom_point(data = dat[!duplicated(dat$Lon), ], aes(x = Lon, y = Lat, color = Station), size = 2.5) +
scale_color_manual(values = c('#ff9f5c', '#00bfc4', '#619cff')) +
geom_polygon(data = map, aes(x = long, y = lat, group = group), color = '#e6e6e6', fill = '#c8c8c8') +
theme_bw() +
theme(panel.grid = element_blank(), 
	legend.position = c(0.2, 0.8), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(105, 125),  ylim = c(10, 35)) +
scale_x_continuous(breaks = seq(105, 125, 5), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(10, 35, 5), expand = c(0, 0)) +
labs(x = 'Lon (°E)', y = 'Lat (°N)')

p


# Temporal variations in BA and BP during the 24-hour incubation (Fig. E2b,c)
BA <- ggplot(dat, aes(Time, BA_mean, color = Station, group = Station)) +
stat_smooth(method = 'nls', formula = y~a*exp(r*x), method.args = list(start = list(a = 0.5, r = 0.2)), se = FALSE) +
geom_errorbar(aes(ymin = BA_mean-BA_sd, ymax = BA_mean+BA_sd), size = 0.3, width = 0.25) +
geom_point() +
scale_color_manual(values = c('#ff9f5c', '#00bfc4', '#619cff')) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
scale_x_continuous(breaks = unique(dat$Time)) +
scale_y_continuous(breaks = seq(0, 12, 3), expand = expansion(mult = c(0.1, 0.1))) +
labs(x = 'Incubation Time (hour)', y = 'BA (×10^5 cells mL−1)')

BP <- ggplot(dat, aes(Time, BP_mean, color = Station, group = Station)) +
stat_smooth(method = 'nls', formula = y~a*exp(r*x), method.args = list(start = list(a = 0.5, r = 0.2)), se = FALSE) +
geom_errorbar(aes(ymin = BP_mean-BP_sd, ymax = BP_mean+BP_sd), size = 0.3, width = 0.25) +
geom_point() +
scale_color_manual(values = c('#ff9f5c', '#00bfc4', '#619cff')) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
scale_x_continuous(breaks = unique(dat$Time)) +
scale_y_continuous(limits = c(0, 0.16), breaks = seq(0, 0.16, 0.04), expand = expansion(mult = c(0, 0.1))) +
labs(x = 'Incubation Time (hour)', y = 'BP (mg C m−3 day−1)')

BA + BP


# Obtain the fitting equation, R2, and p-value for the exponential regression
R2 <- function(mod, y) {
	fit <- fitted(mod)
	SSre <- sum((y-fit)^2)
	SStot <- sum((y-mean(y))^2)
	R2 <- round(1 - SSre/SStot, 3)
	R2
}

P <- function(dat, y, R2, mod) {
	set.seed(123)
	p_num <- 1
	dat_rand <- dat
	for (i in 1:999) {
		dat_rand[[y]] <- sample(dat_rand[[y]])
		SSre_rand <- sum((dat_rand[[y]]-fitted(mod))^2)
		SStot_rand <- sum((dat_rand[[y]]-mean(dat_rand[[y]]))^2)
		R2_rand <- 1 - SSre_rand/SStot_rand
		if (R2_rand > R2) p_num <- p_num + 1
	}
	p_value <- p_num / (999+1)  # Estimate the p-value using 999 permutations
	p_value
}

dat1 <- subset(dat, Station == '37')
mod <- nls(BA_mean~a*exp(r*Time), data = dat1, start = list(a = 0.5, r = 0.2))
mod
R2(mod, dat1$BA_mean)
P(dat1, 'BA_mean', R2(mod, dat1$BA_mean), mod)
#fit (station: 37; BA): Y = 7.895e^0.015, R2 = 0.736, P = 0.008

dat1 <- subset(dat, Station == 'J13')
mod <- nls(BA_mean~a*exp(r*Time), data = dat1, start = list(a = 0.5, r = 0.2))
mod
R2(mod, dat1$BA_mean)
P(dat1, 'BA_mean', R2(mod, dat1$BA_mean), mod)
#fit (station: J13; BA): Y = 6.165e^0.017, R2 = 0.760, P = 0.007

dat1 <- subset(dat, Station == 'TS44')
mod <- nls(BA_mean~a*exp(r*Time), data = dat1, start = list(a = 0.5, r = 0.2))
mod
R2(mod, dat1$BA_mean)
P(dat1, 'BA_mean', R2(mod, dat1$BA_mean), mod)
#fit (station: TS44; BA): Y = 5.128e^0.026, R2 = 0.855, P = 0.001

dat1 <- subset(dat, Station == '37')
mod <- nls(BP_mean~a*exp(r*Time), data = dat1, start = list(a = 0.5, r = 0.2))
mod
R2(mod, dat1$BP_mean)
P(dat1, 'BP_mean', R2(mod, dat1$BP_mean), mod)
#fit (station: 37; BP): Y = 0.013e^0.043, R2 = 0.762, P = 0.001

dat1 <- subset(dat, Station == 'J13')
mod <- nls(BP_mean~a*exp(r*Time), data = dat1, start = list(a = 0.5, r = 0.2))
mod
R2(mod, dat1$BP_mean)
P(dat1, 'BP_mean', R2(mod, dat1$BP_mean), mod)
#fit (station: J13; BP): Y = 0.012e^0.088, R2 = 0.940, P = 0.001

dat <- dat[-which(dat$Station == 'TS44' & dat$Time == 16), ]
dat1 <- subset(dat, Station == 'TS44')
mod <- nls(BP_mean~a*exp(r*Time), data = dat1, start = list(a = 0.5, r = 0.2))
mod
R2(mod, dat1$BP_mean)
P(dat1, 'BP_mean', R2(mod, dat1$BP_mean), mod)
#fit (station: TS44; BP): Y = 0.026e^0.069, R2 = 0.753, P = 0.003

