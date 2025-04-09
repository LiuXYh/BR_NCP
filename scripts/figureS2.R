#### Comparison of net community production before and after correcting for bacterial respiration bias at four representative stations where NCP shifted from negative to positive values

library(reshape2)
library(ggplot2)


# Choose stations with NCP < 0 before correction and NCP > 0 after correction, and indicate them on the map
dat <- read.csv('../output/corrected_BR.csv')
dat <- subset(dat, NCP<0 & NCP_corr>0)

map <- map_data('world')
map <- subset(map, long > 95 & long < 135)
map <- subset(map, lat > 0 & lat < 45)

p <- ggplot() +
geom_point(data = dat, aes(x = Lon, y = Lat), size = 2.5, color = '#ffa500') +
geom_text(data = dat, aes(x = Lon, y = Lat, label = No), vjust = -0.5) +
geom_polygon(data = map, aes(x = long, y = lat, group = group), color = '#e6e6e6', fill = '#c8c8c8') +
theme_bw() +
theme(panel.grid = element_blank(), 
	legend.position = c(0.2, 0.8), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(116, 122),  ylim = c(21, 27)) +
scale_x_continuous(breaks = seq(116, 122, 2), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(21, 27, 2), expand = c(0, 0)) +
labs(x = 'Lon (°E)', y = 'Lat (°N)')

p


# Using bar chart to display the NCP before and after correction
dat <- dat[c('No', 'NCP', 'NCP_corr')]
dat <- melt(dat, id = 'No')
dat$variable <- factor(dat$variable, levels = c('NCP', 'NCP_corr'), labels = c('Before correction (heterotrophy)', 'After correction (autotrophy)'))
dat$value <- dat$value/1.4*12  # C:O ratios were converted using a factor of 1:1.4, following the recommendation of Laws (1991)

p_NCP <- ggplot(dat, aes(variable, value, fill = variable)) +
facet_wrap(~No) +
geom_col(width = 0.7) +
scale_fill_manual(values = c('#548fbe', '#c11b29')) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'transparent', fill = 'white'), 
	strip.text = element_text(color = 'black', size = 9),
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text = element_text(color = 'black', size = 9),
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank(),
	axis.line.y = element_line(color = 'black', size = 0.5)) +
geom_hline(yintercept = 0, size = 0.5) +
labs(x = '', y = 'NCP (mg C m−3 day−1)')

p_NCP


# Finally, combine the maps and bar charts using external plotting tools to create Fig. S2

