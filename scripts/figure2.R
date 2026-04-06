#### Changes in bacterial activity before and after incubation and their relationships with environmental factors

library(reshape2)
library(ggplot2)
library(ggbreak)
library(linkET)


# Calculate the growth of BA, BP, and sBP before and after 24-hour incubation
dat <- read.csv('../data/field_sampling.csv')

dat$BA_fold <- dat$BA_t1/dat$BA_t0
dat$BP_fold <- dat$BP_t1/dat$BP_t0
dat$sBP_t0 <- dat$BP_t0/dat$BA_t0*10^6  # unit: fg C cell−1 day−1
dat$sBP_t1 <- dat$BP_t1/dat$BA_t1*10^6
dat$sBP_fold <- dat$sBP_t1/dat$sBP_t0


# Get the mean of the fold change
mean(dat$BA_fold, na.rm = TRUE)
mean(dat$BP_fold, na.rm = TRUE)
mean(dat$sBP_fold, na.rm = TRUE)


# Presented as violin plots showing the distribution of growth multiples of BA, BP, and sBP before and after incubation (Fig. 2a)
fold <- dat[c('No', 'BA_fold', 'BP_fold', 'sBP_fold')]
fold <- melt(fold, id = 'No')
fold$variable <- factor(fold$variable, levels = c('BA_fold', 'BP_fold', 'sBP_fold'), labels = c('BAt1/BAt0', 'BPt1/BPt0', 'sBPt1/sBPt0'))

ggplot(fold, aes(variable, value, fill = variable)) +
geom_violin(width = 1) +
geom_boxplot(width = 0.07) +
scale_fill_manual(values = c('#ffdad0', '#a5b4da', '#c7e2b7')) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text = element_text(color = 'black', size = 9),
	legend.position = 'none') +
scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
scale_y_break(breaks = c(10, 10), scales = 0.5) +
geom_hline(yintercept = 1, color = 'red') +
annotate('text', label = '×', x = 1, y = mean(dat$BA_fold, na.rm = TRUE), size = 5, color = 'red') +
annotate('text', label = '×', x = 2, y = mean(dat$BP_fold, na.rm = TRUE), size = 5, color = 'red') +
annotate('text', label = '×', x = 3, y = mean(dat$sBP_fold, na.rm = TRUE), size = 5, color = 'red') +
labs(x = '', y = 'Fold')


# Calculate the extent of BR overestimation and adjust the respiration and NCP values, as detailed in the Methods section
r_filtered <- log(dat$BP_t1_filtered/dat$BP_t0_filtered)
BP_total_filtered <- dat$BP_t0_filtered*(exp(r_filtered)-1)/r_filtered
dat$BGE <- BP_total_filtered/(BP_total_filtered+dat$BR_total_filtered*12)  # Respiratory Quotient (RQ)=1, following the recommendation of Robinson (2008)
r <- log(dat$BP_t1/dat$BP_t0)
BP_total <- dat$BP_t0*(exp(r)-1)/r
dat$BR_total <- BP_total/dat$BGE-BP_total
dat$BR_insitu <- dat$BP_t0/dat$BGE-dat$BP_t0
dat$BRbias <- dat$BR_total-dat$BR_insitu
dat$BRbias_CR <- dat$BRbias/12/dat$CR
dat$NCP_corr <- dat$NCP+dat$BRbias/12

write.csv(dat, '../output/corrected_BR.csv', row.names = FALSE, na = '')  #output


# Correlations between bacterial activity changes and environmental factors (Fig. 2b)
env <- dat[c('Temperature', 'Salinity', 'Chla', 'DO', 'Si', 'PO4', 'NO3', 'BA_t0', 'BP_t0', 'sBP_t0', 'BA_fold', 'BP_fold', 'sBP_fold', 'BRbias_CR')]
names(env) <- c('Temperature', 'Salinity', 'Chla', 'DO', 'Si', 'PO4', 'NO3', 'BAt0', 'BPt0', 'sBPt0', 'BAt1/BAt0', 'BPt1/BPt0', 'sBPt1/sBPt0', 'BRbias/CR')

qcorrplot(correlate(env, method = 'spearman', engine = 'Hmisc'), type = 'lower', diag = FALSE) +
geom_shaping(color = NA) +
geom_mark(only_mark = TRUE, size = 2.5, sig.thres = 0.05) +
scale_fill_gradientn(colors = c('#2b74b2', '#abd0e5', 'white', '#f9c3ac', '#c33a3c'), limits = c(-1, 1))

