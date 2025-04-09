#### Constructed machine-learning models for corrected and uncorrected NCP datasets

library(randomForest)
library(rfPermute)
library(missMDA)
library(reshape2)
library(doBy)
library(ggplot2)
library(ggpmisc)
library(patchwork)


# Read data
dat <- read.csv('../data/euphotic-zone_integrated_NCP_grid.csv')
dat <- na.omit(dat[c('NCP', 'NCP_corr', 'Lon', 'Lat', 'Month', 'MLD', 'Temp_WOA', 'Sal_WOA', 'DO_WOA', 'Si_WOA', 'NO3_WOA', 'Chla', 'GPP')])

dat$NCP <- dat$NCP/1.4*12  # Unit: mmol O2 m-2 day-1 --> mg C m-2 day-1, C:O ratios were converted using a factor of 1:1.4, following the recommendation of Laws (1991)
dat$NCP_corr <- dat$NCP_corr/1.4*12

dat$M_cos <- cos(dat$Month*2*pi/12)  # To ensure numerical continuity in both spatial and temporal dimensions, latitudes, longitudes, and months were converted into periodic functions (Yu et al. 2024)
dat$M_sin <- sin(dat$Month*2*pi/12)
dat$Lat_sin <- sin(dat$Lat*pi/180)
dat$Lon_Lat_sin_cos <- sin(dat$Lon*pi/180)* cos(dat$Lat*pi/180)
dat$Lon_Lat_cos_sin <- -cos(dat$Lon*pi/180)* cos(dat$Lat*pi/180) 


# Divided into 70% training data and 30% validation data
i = 1
set.seed(i)
n <- nrow(dat)
rowlabel <- sample(1:n, 0.70*n)
train_data <- dat[rowlabel,]
test_data <- dat[-rowlabel,]

predictors_train <- train_data[c('M_cos', 'M_sin', 'Lat_sin', 'Lon_Lat_sin_cos', 'Lon_Lat_cos_sin', 'MLD', 'Temp_WOA', 'Sal_WOA', 'DO_WOA', 'Si_WOA', 'NO3_WOA', 'Chla', 'GPP')]
predictors_test <- test_data[c('M_cos', 'M_sin', 'Lat_sin', 'Lon_Lat_sin_cos', 'Lon_Lat_cos_sin', 'MLD', 'Temp_WOA', 'Sal_WOA', 'DO_WOA', 'Si_WOA', 'NO3_WOA', 'Chla', 'GPP')]


# Random forest model (uncorrected NCP)
set.seed(i)
rf_model <- randomForest(predictors_train, train_data$NCP, importance = TRUE)  # train base RF model
rf_predictions <- predict(rf_model, predictors_test)
meta_features <- data.frame(Predictions = rf_predictions)  # create meta-features
meta_features$NCP <- test_data$NCP
meta_model <- randomForest(NCP~., data = meta_features)  # train the meta-model
meta_predictions <- predict(meta_model, newdata = meta_features)  # predict the validation data
results <- data.frame(Actual = test_data$NCP, Predicted = meta_predictions)

# Random forest model (corrected NCP)
set.seed(i)
rf_model_corr <- randomForest(predictors_train, train_data$NCP_corr, importance=TRUE)  # train base RF model
rf_predictions <- predict(rf_model_corr, predictors_test)
meta_features_corr <- data.frame(Predictions = rf_predictions)  # create meta-features
meta_features_corr$NCP_corr <- test_data$NCP_corr
meta_model_corr <- randomForest(NCP_corr~., data = meta_features_corr)  # train the meta-model
meta_predictions <- predict(meta_model_corr, newdata = meta_features_corr)  # predict the validation data
results_corr <- data.frame(Actual = test_data$NCP_corr, Predicted = meta_predictions)


#### Model accuracy and variable importance in predicting net community production using the random forest approach

# Observed versus predicted values for the validation dataset for uncorrected and corrected NCP (Fig. S7a-b)
max1 <- max(c(results$Actual, results$Predicted, results_corr$Actual, results_corr$Predicted), na.rm = TRUE)
min1 <- min(c(results$Actual, results$Predicted, results_corr$Actual, results_corr$Predicted), na.rm = TRUE)

p_NCP <- ggplot(results, aes(Actual, Predicted)) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
geom_point(color = '#ffa500') +
stat_smooth(method = 'lm', formula = y~poly(x,1), color = 'black', se = TRUE) +
stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')), formula = y~poly(x,1), parse = TRUE, label.x.npc = 'left', label.y.npc = 'top', size = 3) +
scale_x_continuous(limits = c(min1, max1), expand = expansion(mult = c(0.1, 0.1))) +
scale_y_continuous(limits = c(min1, max1), expand = expansion(mult = c(0.1, 0.1))) +
labs(x = 'Observed NCP (mg C m−2 day−1 )', y = 'Predicted NCP (mg C m−2 day−1 )') +
geom_abline(color = 'red')

p_NCP_corr <- ggplot(results_corr, aes(Actual, Predicted)) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
geom_point(color = '#ffa500') +
stat_smooth(method = 'lm', formula = y~poly(x,1), color = 'black', se = TRUE) +
stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')), formula = y~poly(x,1), parse = TRUE, label.x.npc = 'left', label.y.npc = 'top', size = 3) +
scale_x_continuous(limits = c(min1, max1), expand = expansion(mult = c(0.1, 0.1))) +
scale_y_continuous(limits = c(min1, max1), expand = expansion(mult = c(0.1, 0.1))) +
labs(x = 'Observed NCP (mg C m−2 day−1 )', y = 'Predicted NCP (mg C m−2 day−1 )') +
geom_abline(color = 'red')

p_NCP + p_NCP_corr

sqrt(mean((results$Predicted - results$Actual)^2))  #RMSE
sqrt(mean((results_corr$Predicted - results_corr$Actual)^2))  #RMSE


# Relative importance of predictor variables in the RF model for the uncorrected and corrected NCP (Fig. S7c-d)
set.seed(i)
rfP_model <- rfPermute(predictors_train, train_data$NCP, importance = TRUE, ntree = 500, nrep = 1000, num.cores = 4)
importance_rfP_model <- data.frame(importance(rfP_model, scale = TRUE), check.names = FALSE)
importance_rfP_model$name <- rownames(importance_rfP_model)
importance_rfP_model$type <- 'NCP'
importance_rfP_model <- importance_rfP_model[order(importance_rfP_model$`%IncMSE`), ]
importance_rfP_model$name <- factor(importance_rfP_model$name, levels = importance_rfP_model$name)

set.seed(i)
rfP_model_corr <- rfPermute(predictors_train, train_data$NCP_corr, importance = TRUE, ntree = 500, nrep = 1000, num.cores = 4)
importance_rfP_model_corr <- data.frame(importance(rfP_model_corr, scale = TRUE), check.names = FALSE)
importance_rfP_model_corr$name <- rownames(importance_rfP_model_corr)
importance_rfP_model_corr$type <- 'NCP_corr'
importance_rfP_model_corr <- importance_rfP_model_corr[order(importance_rfP_model_corr$`%IncMSE`), ]
importance_rfP_model_corr$name <- factor(importance_rfP_model_corr$name, levels = importance_rfP_model_corr$name)

p_NCP <- ggplot() +
geom_col(data = importance_rfP_model, aes(x = name, y = `%IncMSE`), width = 0.5, fill = '#FFC068', color = NA) +
labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
coord_flip()

p_NCP_corr <- ggplot() +
geom_col(data = importance_rfP_model_corr, aes(x = name, y = `%IncMSE`), width = 0.5, fill = '#FFC068', color = NA) +
labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
coord_flip()

p_NCP + p_NCP_corr


#### Extrapolation the NCP to the global ocean by using machine-learning approach

# Read global ocean data (1° gridded monthly climatological data of ocean environmental factors from WOA and remote sensing)
dat <- read.csv('../data/WOA_RS_grid.csv')
dat <- na.omit(dat[c('Lon', 'Lat', 'Month', 'MLD', 'Temp_WOA', 'Sal_WOA', 'DO_WOA', 'Si_WOA', 'NO3_WOA', 'Chla', 'GPP')])

dat$M_cos <- cos(dat$Month*2*pi/12)
dat$M_sin <- sin(dat$Month*2*pi/12)
dat$Lat_sin <- sin(dat$Lat*pi/180)
dat$Lon_Lat_sin_cos <- sin(dat$Lon*pi/180)* cos(dat$Lat*pi/180)
dat$Lon_Lat_cos_sin <- -cos(dat$Lon*pi/180)* cos(dat$Lat*pi/180) 

predictors <- dat[c('M_cos', 'M_sin', 'Lat_sin', 'Lon_Lat_sin_cos', 'Lon_Lat_cos_sin', 'MLD', 'Temp_WOA', 'Sal_WOA', 'DO_WOA', 'Si_WOA', 'NO3_WOA', 'Chla', 'GPP')]


# Global NCP predictions before and after correction (producing 1° gridded monthly climatological NCP, in mg C m-2 day-1).
rf_predictions <- predict(rf_model, predictors)
meta_features <- data.frame(Predictions = rf_predictions)
dat$NCP <- predict(meta_model, newdata = meta_features)

rf_predictions <- predict(rf_model_corr, predictors)
meta_features <- data.frame(Predictions = rf_predictions)
dat$NCP_corr <- predict(meta_model_corr, newdata = meta_features)


# Spatial interpolation of missing seasonal data in high-latitude marine areas
NCP <- dat[c('Lon', 'Lat', 'Month', 'NCP')]
NCP_matrix <- dcast(NCP, Month~Lat+Lon, value.var = 'NCP')
rownames(NCP_matrix) <- NCP_matrix$Month
NCP_matrix <- NCP_matrix[ ,-1]
data_clean <- NCP_matrix[ ,-which(colSums(is.na(NCP_matrix)) >= 9 )]
EOF_NCP <- imputePCA(data_clean)$completeObs
EOF_NCP <- as.data.frame(EOF_NCP)
rownames(EOF_NCP) <- 1:12
EOF_NCP$Month <- rownames(EOF_NCP)
EOF_NCP <- melt(EOF_NCP, id = 'Month')
for (i in 1:nrow(EOF_NCP)) {
	Lat_Lon <- unlist(strsplit(as.character(EOF_NCP[i,'variable']), '_'))
	EOF_NCP[i,'Lon'] <- Lat_Lon[2]
	EOF_NCP[i,'Lat'] <- Lat_Lon[1]
}
EOF_NCP <- EOF_NCP[c('Lon', 'Lat', 'Month', 'value')]
names(EOF_NCP)[4] <- 'NCP'

NCP_corr <- dat[c('Lon', 'Lat', 'Month', 'NCP_corr')]
NCP_corr_matrix <- dcast(NCP_corr, Month~Lat+Lon, value.var = 'NCP_corr')
rownames(NCP_corr_matrix) <- NCP_corr_matrix$Month
NCP_corr_matrix <- NCP_corr_matrix[ ,-1]
data_clean <- NCP_corr_matrix[ ,-which(colSums(is.na(NCP_corr_matrix)) >= 9 )]
EOF_NCP_corr <- imputePCA(data_clean)$completeObs
EOF_NCP_corr <- as.data.frame(EOF_NCP_corr)
rownames(EOF_NCP_corr) <- 1:12
EOF_NCP_corr$Month <- rownames(EOF_NCP_corr)
EOF_NCP_corr <- melt(EOF_NCP_corr, id = 'Month')
for (i in 1:nrow(EOF_NCP_corr)) {
	Lat_Lon <- unlist(strsplit(as.character(EOF_NCP_corr[i,'variable']), '_'))
	EOF_NCP_corr[i,'Lon'] <- Lat_Lon[2]
	EOF_NCP_corr[i,'Lat'] <- Lat_Lon[1]
}
EOF_NCP_corr <- EOF_NCP_corr[c('Lon', 'Lat', 'Month', 'value')]
names(EOF_NCP_corr)[4] <- 'NCP_corr'

EOF <- cbind(EOF_NCP, EOF_NCP_corr[4])
sum(110000*cos(abs(EOF$Lat)*pi/180)*110000*(EOF$NCP*30/1000)/10^15)  # Global estimate of uncorrected NCP: -9 Pg C year-1
sum(110000*cos(abs(EOF$Lat)*pi/180)*110000*(EOF$NCP_corr*30/1000)/10^15)  # Global estimate of corrected NCP: 12 Pg C year-1


# Calculate the annual mean of monthly climatological NCP
EOF_year <- summaryBy(NCP+NCP_corr~Lon+Lat, EOF, FUN = mean)
EOF_year$Month <- 13
EOF_year <- EOF_year[c(1, 2, 5, 3, 4)]
names(EOF_year) <- c('Lon', 'Lat', 'Month', 'NCP', 'NCP_corr')
EOF <- rbind(EOF, EOF_year)
EOF <- EOF[order(EOF$Month, EOF$Lon, EOF$Lat), ]

write.csv(dat, '../output/global_NCP_grid_EOF.csv', row.names = FALSE, quote = FALSE, na = '')  # Output all predicted results

