#### Evaluating the assumption of constant BGE during incubation

library(ggplot2)
library(ggpmisc)
library(ggpubr)


# Locations of the stations with ETS measurements (Fig. S10a)
dat <- read.csv('../data/ETS_measurement.csv')

map <- map_data('world')
map <- subset(map, long > 95 & long < 135)
map <- subset(map, lat > 0 & lat < 45)

p <- ggplot() +
geom_point(data = dat, aes(x = Lon, y = Lat), color = '#eeb421', size = 2.5) +
geom_polygon(data = map, aes(x = long, y = lat, group = group), color = '#e6e6e6', fill = '#c8c8c8') +
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


# The relationship between in situ BR-derived INT-F production rates and the empirically derived initial BR (Fig. S10b)

dat$logINTF_0.2_0.8 <- log10(dat$INTF_0.2_0.8)  # in situ BR-derived INT-F production rate, unit: μmol m-3 d-1
dat$logBR_insitu_filtered <- log10(dat$BR_insitu_filtered)  # empirically derived initial BR, unit: mg C m-3 d-1

ETS_BR <- ggplot(dat, aes(logINTF_0.2_0.8, logBR_insitu_filtered)) +
geom_point(color = '#eeb421', size = 3) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
stat_smooth(method = 'lm', formula = y~poly(x, 1), se = TRUE) +
#stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', label.x.npc = 'left', label.y.npc = 'top', size = 2.7) +
stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')), formula = y~poly(x, 1), parse = TRUE, label.x.npc = 'left', label.y.npc = 'top', size = 2.7) +
labs(x = 'log10 BR_INTF', y = 'log10 BR_insitu')

ETS_BR

summary(lm(logBR_insitu_filtered~logINTF_0.2_0.8, dat))

