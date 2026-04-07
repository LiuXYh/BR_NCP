#### Quantification of bacterial respiration bias and its impact on community respiration measurements

library(reshape2)
library(ggplot2)
library(ggpubr)


# Using stacked bar chart to display the CR composition at each sampling station
dat <- read.csv('../output/corrected_BR.csv')

dat$CR <- dat$CR*12
dat$other_plankton <- dat$CR - dat$BR_total
CR <- na.omit(dat[c('No', 'BR_insitu', 'BRbias', 'other_plankton')])
CR <- melt(CR, id = 'No')

p_CR <- ggplot(CR, aes(No, value, fill = variable)) +
facet_wrap(~No, scale = 'free_x') +
geom_col(width = 0.5, position = 'fill') +
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

# Finally, combine the maps and bar charts using external plotting tools to create Fig. 3a


# Density distribution of BR before and after correction of bottle-based measurements (Fig. 3b)
dat <- read.csv('../output/corrected_BR.csv')

BR <- dat[c('No', 'BR_total', 'BR_insitu')]
BR <- melt(BR, id = 'No')
BR$variable <- factor(BR$variable, levels = c('BR_total', 'BR_insitu'), labels = c('Before correction (BRinsitu+BRbias)', 'After correction (BRinsitu)'))

p_BR <- ggplot(BR, aes(x = value)) +
geom_density(aes(fill = variable), position = position_dodge(width = 0), bins = 10, alpha = 0.5, color = NA) +
geom_rug(aes(color = variable), show.legend = FALSE) +
scale_fill_manual(values = c('#323232', '#443983')) +
scale_color_manual(values = c('#323232', '#443983')) +
theme(panel.grid = element_blank(), 
	panel.background = element_blank(),
	axis.line = element_line(color = 'black', size = 0.5), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text = element_text(color = 'black', size = 9)) +
labs(x = 'BR (mg C m−3 day−1)', y = 'Density', fill = '')

p_BR


# Density distribution of BRbias/CR (Fig. 3c)
p_BRbias_CR <- ggplot(dat, aes(x = BRbias_CR)) +
geom_density(bins = 10, fill = '#DE6826', alpha = 0.5, color = NA) +
geom_rug(color = '#DE6826') +
theme(panel.grid = element_blank(), 
	panel.background = element_blank(),
	axis.line = element_line(color = 'black', size = 0.5), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text = element_text(color = 'black', size = 9)) +
scale_x_continuous(limits = c(0, 1)) +
geom_vline(xintercept = mean(dat$BRbias_CR, na.rm = TRUE), linetype = 2) +
labs(x = 'BRbias/CR', y = 'Density')

p_BRbias_CR

