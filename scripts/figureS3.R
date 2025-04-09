#### Relationship between bacterial abundance and the proportion of bacterial respiration overestimation to community respiration

library(reshape2)
library(ggplot2)
library(linkET)
library(ggpmisc)
library(ggpubr)


# Correlations between BRbias/CR and environmental factors
dat <- read.csv('../output/corrected_BR.csv')
env <- dat[c('Temperature', 'Salinity', 'Chla', 'DO', 'NO3', 'PO4', 'Si', 'BA_t0', 'BP_t0', 'BRbias_CR')]
names(env) <- c('Temperature', 'Salinity', 'Chla', 'DO', 'NO3', 'PO4', 'Si', 'BAt0', 'BPt0', 'BRbias/CR')

qcorrplot(correlate(env, method = 'spearman', engine = 'Hmisc'), type = 'lower', diag = FALSE) +
geom_shaping(color = NA) +
geom_mark(only_mark = TRUE, size = 2.5, sig.thres = 0.05) +
scale_fill_gradientn(colors = c('#2b74b2', '#abd0e5', 'white', '#f9c3ac', '#c33a3c'), limits = c(-1, 1))


# Apply logarithmic transformation to the BRbias/CR and environmental factors, then create a scatter plot to explore their relationships
env <- subset(env, `BRbias/CR` >0)
env$`BRbias/CR` <- -log10(1/env$`BRbias/CR`-1)
env$Temperature <- log10(env$Temperature)
env$Salinity <- log10(env$Salinity)
env$Chla <- log10(env$Chla)
env$DO <- log10(env$DO)
env$NO3 <- log10(env$NO3)
env$PO4 <- log10(env$PO4)
env$Si <- log10(env$Si)
env$BAt0 <- log10(env$BAt0)
env$BPt0 <- log10(env$BPt0)
env1 <- melt(env, id = 'BRbias/CR')

ggplot(env1, aes(value, `BRbias/CR`)) +
facet_wrap(~variable, ncol = 3, scale = 'free_x') +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
geom_point(color = '#eeb421') +
stat_smooth(method = 'lm', formula = y~poly(x,1), se = TRUE, color = 'black', fill = 'gray') +
stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', label.x.npc = 'left', label.y.npc = 'top', size = 2.7) +
labs(x = 'Log10 variable', y = '−Log10(1/(BRbias/CR)−1)')


# As shown above, BRbias/CR is significantly negatively correlated with BA and BP, and after log transformation, a clear linear relationship is observed
# Thus, a linear model is built
summary(lm(`BRbias/CR`~BAt0, env))  #fit: −log10(1/(BRbias/CR)−1)=-1.083log10BA+5.554, R2=0.202, P=0.001
summary(lm(`BRbias/CR`~BPt0, env))  #fit: −log10(1/(BRbias/CR)−1)=-0.666log10BP-1.082, R2=0.139, P=0.007
summary(lm(`BRbias/CR`~BAt0+BPt0, env))  #fit: −log10(1/(BRbias/CR)−1)=-0.849log10BA-0.262log10BP+4.074, R2=0.215, P=0.003


# As shown above, BA and BP are highly collinear, but BA provides a better fit
# Since BA data are more accessible globally, BA was chosen to build the linear model with BRbias/CR


# Linear regression between log10(BA) and −log10(1/(BRbias/CR)−1) (Fig. S3a)
# fit: −log10(1/(BRbias/CR)−1)=-1.083log10BA+5.555, R2=0.202, P=0.001
ggplot(env, aes(BAt0, `BRbias/CR`)) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
geom_point(color = '#eeb421', size = 3) +
stat_smooth(method = 'lm', formula = y~poly(x,1), se = TRUE, color = 'black', fill = 'gray') +
stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')), formula = y~poly(x, 1), parse = TRUE, label.x.npc = 'left', label.y.npc = 'top', size = 2.7) +
scale_y_continuous(limits = c(-3, 1)) +
labs(x = 'Log10BA (cells mL−1)', y = '−Log10(1/(BRbias/CR)−1)')


# Transformed empirical equation based on the regression in Fig. S3a (Fig. S3b)
# fit: BRbias/CR=1/(1+10^(1.083log10BA-5.555))
env$BAt0 <- 10^env$BAt0
env$`BRbias/CR` <- 1/(10^(-1*env$`BRbias/CR`)+1)

ggplot(env, aes(BAt0, `BRbias/CR`)) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
geom_point(color = '#eeb421', size = 3) +
stat_function(fun = function(x) 1/(10^(1.083*log10(x)-5.555)+1), color = 'black', size = 1) +
scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
labs(x = 'BA (cells mL−1)', y = 'BRbias/CR')

