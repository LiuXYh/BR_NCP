#### Examine the net community production before and after correction at three typical time-series stations


# Read data, for calcuLation details, see "scripts/figureS7.R"
dat <- read.csv('../output/global_NCP_grid_EOF.csv')

# Check the NCP (units: mg C m–2 day–1) of the grid corresponding to the station and document it in Table S2

# PAP (49.0°N, 16.5°W)
PAP <- subset(dat, Lat == 49 & Lon == -17)
PAP[c('NCP', 'Month')]
PAP[c('NCP_corr', 'Month')]

# ALOHA (22.75°N, 158°W)
ALOHA <- subset(dat, Lat == 23 & Lon == -158)
ALOHA[c('NCP', 'Month')]
ALOHA[c('NCP_corr', 'Month')]

# K2 (47°N, 160°E)
K2 <- subset(dat, Lat == 47 & Lon == 160)
K2[c('NCP', 'Month')]
K2[c('NCP_corr', 'Month')]

