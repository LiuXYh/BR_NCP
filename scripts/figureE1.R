#### Bacterial activity metrics before and after 24-hour incubation in field studies

library(reshape2)
library(ggplot2)


# Comparison of BA using bar charts before and after 24-hour incubation (Fig. E1a)
dat <- read.csv('../data/field_sampling.csv')

BA <- dat[c('No', 'BA_t0', 'BA_t1')]
BA <- melt(BA, id = 'No')
BA$variable <- factor(BA$variable, levels = c('BA_t0', 'BA_t1'), labels = c('Before incubation (BAt0)', 'After incubation (BAt1)'))

p_BA <- ggplot(BA, aes(variable, value, fill = variable)) +
facet_wrap(~No) +
geom_col(width = 0.7) +
scale_fill_manual(values = c('#548fbe', '#c11b29')) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'transparent', fill = 'white'), 
	strip.text = element_text(color = 'black', size = 6),
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text = element_text(color = 'black', size = 6),
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank(),
	axis.line.y = element_line(color = 'black', size = 0.5)) +
geom_hline(yintercept = 0, size = 0.5) +
labs(x = '', y = 'BA (cells mL−1)')

p_BA


# Comparison of BP using bar charts before and after 24-hour incubation (Fig. E1b)
BP <- dat[c('No', 'BP_t0', 'BP_t1')]
BP <- melt(BP, id = 'No')
BP$variable <- factor(BP$variable, levels = c('BP_t0', 'BP_t1'), labels = c('Before incubation (BPt0)', 'After incubation (BPt1)'))

p_BP <- ggplot(BP, aes(variable, value, fill = variable)) +
facet_wrap(~No) +
geom_col(width = 0.7) +
scale_fill_manual(values = c('#548fbe', '#c11b29')) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'transparent', fill = 'white'), 
	strip.text = element_text(color = 'black', size = 6),
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text = element_text(color = 'black', size = 6),
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank(),
	axis.line.y = element_line(color = 'black', size = 0.5)) +
geom_hline(yintercept = 0, size = 0.5) +
labs(x = '', y = 'BP (mg C m−3 day−1)')

p_BP


# Comparison of sBP using bar charts before and after 24-hour incubation (Fig. E1a)
dat$sBP_t0 <- dat$BP_t0/dat$BA_t0*10^6
dat$sBP_t1 <- dat$BP_t1/dat$BA_t1*10^6

sBP <- dat[c('No', 'sBP_t0', 'sBP_t1')]
sBP <- melt(sBP, id = 'No')
sBP$variable <- factor(sBP$variable, levels = c('sBP_t0', 'sBP_t1'), labels = c('Before incubation (sBPt0)', 'After incubation (sBPt1)'))

p_sBP <- ggplot(sBP, aes(variable, value, fill = variable)) +
facet_wrap(~No) +
geom_col(width = 0.7) +
scale_fill_manual(values = c('#548fbe', '#c11b29')) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'transparent', fill = 'white'), 
	strip.text = element_text(color = 'black', size = 6),
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text = element_text(color = 'black', size = 6),
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank(),
	axis.line.y = element_line(color = 'black', size = 0.5)) +
geom_hline(yintercept = 0, size = 0.5) +
labs(x = '', y = 'sBP (fg C cell−1 day−1)')

p_sBP


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


# Finally, combine the maps and bar charts using external plotting tools to create Fig. E1a-c

