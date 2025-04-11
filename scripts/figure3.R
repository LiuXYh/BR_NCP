#### Density distribution of bacterial respiration biases and its impact on the community respiration measurements

library(reshape2)
library(ggplot2)
library(ggpubr)


# Density distribution of BR before and after correction of bottle-based measurements (Fig. 2a)
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


# Density distribution of BRbias/CR (Fig. 2b)
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

