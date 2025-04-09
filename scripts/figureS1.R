#### Surface distributions of environmental parameters measured at stations during five cruises from 2018 to 2023

library(reshape2)
library(doBy)
library(ggplot2)


# Map the marginal sea of the Northwest Pacific
dat <- read.csv('../data/field_sampling.csv')

map <- map_data('world')
map <- subset(map, long > 95 & long < 135)
map <- subset(map, lat > 0 & lat < 45)

p <- ggplot() +
geom_polygon(data = map, aes(x = long, y = lat, group = group), color = '#e6e6e6', fill = '#c8c8c8') +
theme_bw() +
theme(panel.grid = element_blank(), 
	legend.position = c(0.2, 0.8), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(105, 125),  ylim = c(10, 35)) +
scale_x_continuous(breaks = seq(105, 125, 5), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(10, 35, 5), expand = c(0, 0))


# Cruises and stations (Fig. S1a)
p +
geom_point(data = dat, size = 2.5, aes(x = Lon, y = Lat, color = Cruise)) +
scale_color_manual(values = c('#ffc408', '#43d8ff', '#00b358', '#1d54ff', '#ca0000')) +
labs(x = 'Lon (°E)', y = 'Lat (°N)', color = 'Cruise')


# Temperature (°C) (Fig. S1b)
dat_Temperature <- na.omit(dat[c('Lon', 'Lat', 'Temperature')])
dat_Temperature <- dat_Temperature[order(dat_Temperature$Temperature, decreasing = TRUE), ]

p +
geom_point(data = dat_Temperature, size = 2.5, aes(x = Lon, y = Lat, color = Temperature)) +
scale_color_gradientn(colors = c('#000084', '#004FFF', '#0EFFF0', '#8DFF70', '#F8FF05', '#FF6900', '#8D0000')) +
labs(x = 'Lon (°E)', y = 'Lat (°N)', color = 'Temperature')


# Salinity (Fig. S1c)
dat_Salinity <- na.omit(dat[c('Lon', 'Lat', 'Salinity')])
dat_Salinity <- dat_Salinity[order(dat_Salinity$Salinity, decreasing = TRUE), ]

p +
geom_point(data = dat_Salinity, size = 2.5, aes(x = Lon, y = Lat, color = Salinity)) +
scale_color_gradientn(colors = c('#000084', '#004FFF', '#0EFFF0', '#8DFF70', '#F8FF05', '#FF6900', '#8D0000')) +
labs(x = 'Lon (°E)', y = 'Lat (°N)', color = 'Salinity')


# Nitrate (NO3−, μmol L−1) (Fig. S1d)
dat_NO3 <- na.omit(dat[c('Lon', 'Lat', 'NO3')])
dat_NO3 <- dat_NO3[order(dat_NO3$NO3, decreasing = FALSE), ]
dat_NO3$NO3 <- log10(dat_NO3$NO3)

p +
geom_point(data = dat_NO3, size = 2.5, aes(x = Lon, y = Lat, color = NO3)) +
scale_color_gradientn(colors = c('#000084', '#004FFF', '#0EFFF0', '#8DFF70', '#F8FF05', '#FF6900', '#8D0000')) +
labs(x = 'Lon (°E)', y = 'Lat (°N)', color = 'Log10 NO3')


# Phosphate (PO43−, μmol L−1) (Fig. S1e)
dat_PO4 <- na.omit(dat[c('Lon', 'Lat', 'PO4')])
dat_PO4 <- dat_PO4[order(dat_PO4$PO4, decreasing = FALSE), ]
dat_PO4$PO4 <- log10(dat_PO4$PO4)

p +
geom_point(data = dat_PO4, size = 2.5, aes(x = Lon, y = Lat, color = PO4)) +
scale_color_gradientn(colors = c('#000084', '#004FFF', '#0EFFF0', '#8DFF70', '#F8FF05', '#FF6900', '#8D0000'), limits = c(-1.5, 0)) +
labs(x = 'Lon (°E)', y = 'Lat (°N)', color = 'Log10 PO4')


# Silicate (SiO43−, μmol L−1) (Fig. S1f)
dat_Si <- na.omit(dat[c('Lon', 'Lat', 'Si')])
dat_Si <- dat_Si[order(dat_Si$Si, decreasing = TRUE), ]
dat_Si$Si <- log10(dat_Si$Si)

p +
geom_point(data = dat_Si, size = 2.5, aes(x = Lon, y = Lat, color = Si)) +
scale_color_gradientn(colors = c('#000084', '#004FFF', '#0EFFF0', '#8DFF70', '#F8FF05', '#FF6900', '#8D0000')) +
labs(x = 'Lon (°E)', y = 'Lat (°N)', color = 'Log10 Si')


# Chlorophyll-a concentrations (Chl-a, mg m−3) (Fig. S1g)
dat_Chla <- na.omit(dat[c('Lon', 'Lat', 'Chla')])
dat_Chla <- dat_Chla[order(dat_Chla$Chla, decreasing = FALSE), ]
dat_Chla$Chla <- log10(dat_Chla$Chla)

p +
geom_point(data = dat_Chla, size = 2.5, aes(x = Lon, y = Lat, color = Chla)) +
scale_color_gradientn(colors = c('#000084', '#004FFF', '#0EFFF0', '#8DFF70', '#F8FF05', '#FF6900', '#8D0000'), limits = c(-1.4, 1)) +
labs(x = 'Lon (°E)', y = 'Lat (°N)', color = 'Log10 Chla')


# Dissolved oxygen (DO, μmol L−1) (Fig. S1h)
dat_DO <- na.omit(dat[c('Lon', 'Lat', 'DO')])
dat_DO <- dat_DO[order(dat_DO$DO, decreasing = FALSE), ]

p +
geom_point(data = dat_DO, size = 2.5, aes(x = Lon, y = Lat, color = DO)) +
scale_color_gradientn(colors = c('#000084', '#004FFF', '#0EFFF0', '#8DFF70', '#F8FF05', '#FF6900', '#8D0000')) +
labs(x = 'Lon (°E)', y = 'Lat (°N)', color = 'DO')

