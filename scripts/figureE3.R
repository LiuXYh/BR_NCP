#### Comparison of community respiration before and after correcting for the bacterial respiration biases in field studies

library(reshape2)
library(ggplot2)


# Using stacked bar chart to display the CR composition at each sampling station
dat <- read.csv('../output/corrected_BR.csv')

dat$CR <- dat$CR*12
dat$other_plankton <- dat$CR - dat$BR_total
CR <- na.omit(dat[c('No', 'BR_insitu', 'BRbias', 'other_plankton')])
CR <- melt(CR, id = 'No')

p_CR <- ggplot(CR, aes(No, value, fill = variable)) +
facet_wrap(~No, scale = 'free_x') +
geom_col(width = 0.5) +
scale_fill_manual(limits = c('other_plankton', 'BR_insitu', 'BRbias'), values = c('gray', '#243983', '#fcc081')) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'transparent', fill = 'white'), 
	strip.text = element_text(color = 'black', size = 5),
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text = element_text(color = 'black', size = 9),
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank(),
	axis.line.y = element_line(color = 'black', size = 0.5)) +
geom_hline(yintercept = 0, size = 0.5)

p_CR


# Map the marginal sea of the Northwest Pacific
map <- map_data('world')
map <- subset(map, long > 95 & long < 135)
map <- subset(map, lat > 0 & lat < 45)

p <- ggplot() +
geom_polygon(data = map, aes(x = long, y = lat, group = group), color = '#e6e6e6', fill = '#c8c8c8') +
geom_point(data = dat, aes(x = Lon, y = Lat), size = 2.5, color = '#ffa500') +
geom_text(data = dat, aes(x = Lon, y = Lat, label = No), vjust = -0.5) +
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


# Finally, combine the maps and bar charts using external plotting tools to create Fig. E3

