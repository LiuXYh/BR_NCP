#### Bacterial activity metrics before and after 24-hour incubation in field studies

library(reshape2)
library(ggplot2)


# Change in BA before and after 24-hour incubation (Fig. S3a)
dat <- read.csv('../data/field_sampling.csv')

map <- map_data('world')
map <- subset(map, long > 95 & long < 135)
map <- subset(map, lat > 0 & lat < 45)

dat$BA_change <- dat$BA_t1 / dat$BA_t0

p_BA <- ggplot() +
geom_polygon(data = map, aes(x = long, y = lat, group = group), color = '#e6e6e6', fill = '#c8c8c8') +
#geom_point(data = dat, aes(x = Lon, y = Lat), color = 'black') +
geom_point(data = dat, aes(x = Lon, y = Lat, size = BA_change), color = '#ff704b', alpha = 0.5) +
scale_size(range = c(0, 7), limits = c(0, 5)) +
theme_bw() +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
coord_cartesian(xlim = c(105, 125),  ylim = c(10, 35)) +
scale_x_continuous(breaks = seq(105, 125, 5), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(10, 35, 5), expand = c(0, 0)) +
labs(x = 'Lon (°E)', y = 'Lat (°N)', size = 'Change in BA')

p_BA


# Change in BP before and after 24-hour incubation (Fig. S3b)
dat$BP_change <- dat$BP_t1 / dat$BP_t0
dat[which(dat$BP_change>10),'BP_change'] <- 10

p_BP <- ggplot() +
geom_polygon(data = map, aes(x = long, y = lat, group = group), color = '#e6e6e6', fill = '#c8c8c8') +
#geom_point(data = dat, aes(x = Lon, y = Lat), color = 'black') +
geom_point(data = dat, aes(x = Lon, y = Lat, size = BP_change), color = '#4d63ad', alpha = 0.5) +
scale_size(range = c(0, 7), limits = c(0, 10)) +
theme_bw() +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
coord_cartesian(xlim = c(105, 125),  ylim = c(10, 35)) +
scale_x_continuous(breaks = seq(105, 125, 5), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(10, 35, 5), expand = c(0, 0)) +
labs(x = 'Lon (°E)', y = 'Lat (°N)', size = 'Change in BP')

p_BP


# Change in sBP before and after 24-hour incubation (Fig. S3c)
dat$sBP_t0 <- dat$BP_t0/dat$BA_t0*10^6
dat$sBP_t1 <- dat$BP_t1/dat$BA_t1*10^6
dat$sBP_change <- dat$sBP_t1 / dat$sBP_t0
dat[which(dat$sBP_change>10),'sBP_change'] <- 10

p_sBP <- ggplot() +
geom_polygon(data = map, aes(x = long, y = lat, group = group), color = '#e6e6e6', fill = '#c8c8c8') +
#geom_point(data = dat, aes(x = Lon, y = Lat), color = 'black') +
geom_point(data = dat, aes(x = Lon, y = Lat, size = sBP_change), color = '#7cc8a9', alpha = 0.5) +
scale_size(range = c(0, 7), limits = c(0, 10)) +
theme_bw() +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
coord_cartesian(xlim = c(105, 125),  ylim = c(10, 35)) +
scale_x_continuous(breaks = seq(105, 125, 5), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(10, 35, 5), expand = c(0, 0)) +
labs(x = 'Lon (°E)', y = 'Lat (°N)', size = 'Change in sBP')

p_sBP

