#### Environmental coverage of incubation experiments vs global ocean ranges

library(doBy)


# in situ sampling data
dat <- read.csv('../data/field_sampling.csv')

min(dat$Temperature, na.rm = TRUE)
max(dat$Temperature, na.rm = TRUE)

min(dat$Salinity, na.rm = TRUE)
max(dat$Salinity, na.rm = TRUE)

min(dat$NO3, na.rm = TRUE)
max(dat$NO3, na.rm = TRUE)

min(dat$PO4, na.rm = TRUE)
max(dat$PO4, na.rm = TRUE)

min(dat$Si, na.rm = TRUE)
max(dat$Si, na.rm = TRUE)

min(dat$Chla, na.rm = TRUE)
max(dat$Chla, na.rm = TRUE)

min(dat$BA_t0, na.rm = TRUE)
max(dat$BA_t0, na.rm = TRUE)

min(dat$BP_t0, na.rm = TRUE)
max(dat$BP_t0, na.rm = TRUE)


# World Ocean Atlas (WOA) dataset, averaged on a 1° grid
woa <- read.csv('../data/WOA_RS_grid.csv')
woa <- na.omit(woa[c("Lon", "Lat", "Temp_WOA", "Sal_WOA", "Si_WOA", "NO3_WOA", "PO4_WOA", "Chla")])

boxplot(woa$Temp_WOA, plot = FALSE)$stats[c(1, 5)]
boxplot(woa$Sal_WOA, plot = FALSE)$stats[c(1, 5)]
boxplot(woa$NO3_WOA, plot = FALSE)$stats[c(1, 5)]
boxplot(woa$PO4_WOA, plot = FALSE)$stats[c(1, 5)]
boxplot(woa$Si_WOA, plot = FALSE)$stats[c(1, 5)]
boxplot(woa$Chla, plot = FALSE)$stats[c(1, 5)]


# Global open datasets of BA
# data from https://doi.pangaea.de/10.1594/PANGAEA.779142
BA <- read.csv('bacteria120214.csv')
min(BA$Bact.L/1000, na.rm = TRUE)
max(BA$Bact.L/1000, na.rm = TRUE)


# Global open datasets of BP
# data from https://zenodo.org/records/12741063
BP <- read.csv('prokaryote_gge.csv')
min(BP$Bact_prod_filter)
max(BP$Bact_prod_filter)

