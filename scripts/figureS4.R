#### Distributions of BGE measured at each station

library(ggplot2)


dat <- read.csv('../output/corrected_BR.csv')

map <- map_data('world')
map <- subset(map, long > 95 & long < 135)
map <- subset(map, lat > 0 & lat < 45)

p <- ggplot() +
geom_polygon(data = map, aes(x = long, y = lat, group = group), color = '#e6e6e6', fill = '#c8c8c8') +
#geom_point(data = dat, aes(x = Lon, y = Lat), color = 'black') +
geom_point(data = dat, aes(x = Lon, y = Lat, size = BGE), color = '#eeb421', alpha = 0.5) +
scale_size(range = c(1, 10), limits = c(0, 0.5)) +
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
labs(x = 'Lon (°E)', y = 'Lat (°N)')

p


