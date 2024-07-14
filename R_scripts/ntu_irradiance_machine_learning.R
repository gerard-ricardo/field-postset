# # ntu and light data combined. Start of code turned off for speed
# 
# # ########################################################
# #Import Irradiance
# #Data imprt 2018
# setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/6 Post-settlement/2 2018/2018 field/2 water quality")
# irrad1.a <- read.table("2018 ms8 flor.txt", sep="\t", header = TRUE,  comment.char = "",check.names = FALSE, quote="", na.strings=c("NA","NaN", " ") )
# library(dplyr)
# irrad1.a = dplyr::filter(irrad1.a, date.time > 43425)  #note this is different to main analysis because combining by data.time
# irrad1.a = dplyr::filter(irrad1.a, date.time < 43488)
# range(irrad1.a$date)
# ###Data cleaning##############################################
# #Data cleaning
# head(irrad1.a)
# irrad1.a = irrad1.a[complete.cases(irrad1.a), ]
# str(irrad1.a)
# range(irrad1.a$time)
# hist(irrad1.a$time)
# irrad1.a[] <- lapply(irrad1.a, function(x) as.numeric(as.character(x)))   #convert all to numeric
# irrad1.a$date <- as.factor(irrad1.a$date)
# str(irrad1.a)
# irrad1.a = irrad1.a[complete.cases(irrad1.a), ] %>% data.frame()
# range(irrad1.a$X425nm)
# 
# #plot
# head(irrad1.a)
# options(scipen = 999)  #display x no of digits
# library(ggplot2)
# # p1 = ggplot() + geom_point(data = irrad1.a, aes(x = date.time, y = X425nm, color = 'mean'))
# # irrad1.a = filter(irrad1.a, date.time > 43422) #trim away error at start. First real reading but not deployment
# # irrad1.a = filter(irrad1.a, date.time < 43489) #trim away error at end
# # irrad1.a = filter(irrad1.a, date.time > 43661) #trim away error at start. First real reading but not deployment
# # irrad1.a = filter(irrad1.a, date.time < 43673) #trim away error at end
# irrad1.a$date <- factor(irrad1.a$date)
# irrad1.a$blue.green = irrad1.a$X455nm/irrad1.a$X555nm  #add a blue:green row
# 
# ##Clean anomalies (>500 and neg)
# #Observe data
# library(tidyr)
# irrad1.a.long = gather(irrad1.a, class, irrad, X425nm:X695nm)
# head(irrad1.a.long)
# # p0 = ggplot()+geom_point(irrad1.a.long, mapping = aes(x = date.time, y = irrad))+facet_wrap(~class, nrow = 8)#+scale_x_log10(name ="dep sed")
# # p0
# library(dplyr)
# irrad1.a.long = filter(irrad1.a.long, irrad < 500) #trim away error
# irrad1.a.long.neg = irrad1.a.long[which(irrad1.a.long$irrad<0),]  #all neg to 0, 16
# irrad1.a.long = irrad1.a.long %>% anti_join(irrad1.a.long.neg)  #subtracts one datafrma efrom another
# head(irrad1.a.long)
# range(irrad1.a.long$irrad)
# irrad1.a.wide <- irrad1.a.long %>%pivot_wider(names_from = class, values_from = irrad, names_prefix = "")  #year goes to columns, their areas go as the values, area is the prefix
# irrad1.a = irrad1.a.wide
# options(tibble.print_max = 50, tibble.print_min = 50)
# irrad1.a = irrad1.a[complete.cases(irrad1.a), ]
# head(irrad1.a, 100)
# range(irrad1.a$X425nm)
# 
# #3)#Visualize data - plot data split at every factor
# library(ggplot2)
# source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek")
# theme_set(theme_sleek())
# # p0 = ggplot()+geom_point(irrad1.a.long, mapping = aes(x = date.time, y = irrad))+facet_wrap(~class, nrow = 8)#+scale_x_log10(name ="dep sed")
# # p0
# 
# 
# 
# # ##### Convert  irradiance to PAR##############################################
# # #1) Convert  irradiance to PAR/nm with a ~60 bin
# # #  This might be 10x too high if you correct binning to 1 nm.
# # #PAR = Irradiance_simp*nm/con.fac    # Note: This website uses 11961.72 https://www.berthold-bio.com/service-support/support-portal/knowledge-base/how-do-i-convert-irradiance-into-photon-flux.html
# # df.simp = data.frame(irrad1.a$X425nm, irrad1.a$X455nm, irrad1.a$X485nm, irrad1.a$X515nm,irrad1.a$X555nm, irrad1.a$X615nm, irrad1.a$X660nm, irrad1.a$X695nm )
# # head(df.simp)
# # con.fac = 11973.78
# # df.simp2 = (data.frame( irrad1.a$X425nm*425, irrad1.a$X455nm*455, irrad1.a$X485nm*485, irrad1.a$X515nm*515,irrad1.a$X555nm*555, irrad1.a$X615nm*615, irrad1.a$X660nm*660, irrad1.a$X695nm*695))/con.fac
# # #plot(df.simp2$irrad1.a.X555nm...555)
# # head(df.simp2)
# # range(df.simp2)
# # df.simp2 = df.simp2[complete.cases(df.simp2), ]
# # range(df.simp2)
# # 
# # str(df.simp2)
# # nrow(df.simp2)
# # #nrow(irrad1.a)
# 
# ###########
# ##### 1)Convert  irradiance to PAR##############################################
# #1) Convert  irradiance to PAR/nm with a ~60 bin
# #PAR = Irradiance_simp*nm/con.fac    # Note: This website uses 11961.72 https://www.berthold-bio.com/service-support/support-portal/knowledge-base/how-do-i-convert-irradiance-into-photon-flux.html
# df.simp.a = data.frame(irrad1.a$X425nm, irrad1.a$X455nm, irrad1.a$X485nm, irrad1.a$X515nm,irrad1.a$X555nm, irrad1.a$X615nm, irrad1.a$X660nm, irrad1.a$X695nm )
# head(df.simp.a)
# con.fac = 11973.78
# df.par.a = (data.frame( irrad1.a$X425nm*425, irrad1.a$X455nm*455, irrad1.a$X485nm*485, irrad1.a$X515nm*515,irrad1.a$X555nm*555, irrad1.a$X615nm*615, irrad1.a$X660nm*660, irrad1.a$X695nm*695))/con.fac
# colnames(df.par.a) = c('425', '455', '485', '515', '555', '615', '660', '695')
# nrow(df.par.a)
# 
# #plot(df.par.a$irrad1.a.X555nm...555)
# head(df.par.a)
# range(df.par.a)
# df.par.a = df.par.a[complete.cases(df.par.a), ]
# range(df.par.a)
# 
# str(df.par.a)
# nrow(df.par.a)
# nrow(irrad1.a)
# ###########
# #Spline/AUC prep
# # #Cubic interpolation #note only starting from 425 not 400 (may be less than realistic)
# long =   nrow(df.par.a)  #no of rows per channel
# nm = c(425, 455, 485, 515, 555, 615, 660, 695)
# vecc = as.numeric(df.par.a[1,])
# 
# ######################################
# 
# # #Single spline
# # sp.length = 16
# # names = seq(425, 695, length  = sp.length)
# # spl.df = spline(nm, df.simp2[1900,], n = sp.length)
# # 
# # #All splines
# # #tot.par3 = vector("numeric", long)  #placeholder
# # tot.par3  = matrix(nrow = long, ncol = sp.length)
# # for(i in 1:long) {
# #   bob.spline <- spline(nm, df.simp2[i,], n = sp.length)
# #   tot.par3[i,] <- bob.spline$y
# # }
# # tot.par3   #gives a matrix with each row the spline fits
# # spline.df = tot.par3 %>% data.frame()
# # str(spline.df)
# # #plot(spline.df$X7)
# # colnames(spline.df) <- names
# # spline.df$date.time = as.numeric(data1$date.time)
# # spline.df.long = gather(spline.df, class, irrad, "425":"695")
# # spline.df.long$class = as.numeric(spline.df.long$class)
# # str(spline.df.long)
# 
# #####3)AUC for PAR##################
# #single
# library(MESS)
# ff = auc(nm, df.par.a[1,], type = 'spline') 
# auc(nm, vecc, type = 'spline')
# 
# #all
# tot.par.a = vector("numeric", long)  #placeholder
# for(i in 1:long) { 
#   bob <- MESS::auc(nm, df.par.a[i,], type = 'spline', from = 400, to = 700 , rule = 2)   #changed based on workshop 
#   tot.par.a[i] <- bob
# } #THIS ADDS A CUBLINE SPLINE. Because there are only 8 data points, a cubic (join the dot) spline is best. 
# df.par.a$tot.par.a = tot.par.a  #add to main df
# df.par.a$date.time = irrad1$date.time
# df.par.a$time = irrad1$time
# head(df.par.a)  #all good to here
# #plot(df.par.a$date.time, df.par.a$tot.par.a)
# quantile(df.par.a$tot.par.a, probs = c(.01, 0.20, 0.5, 0.75, 0.80, .99)) # quartile of tot par inc night
# cum.par.fun = ecdf(df.par.a$tot.par.a)    # P is a function giving the empirical CDF of X
# cum.par.fun(80)
# 
# min(df.par.a$tot.par.a)
# plot(df.par.a$date.time, df.par.a$tot.par.a)
# length(which(df.par.a$tot.par.a >113))/nrow(df.par.a)
# data3 = df.par.a[which(df.par.a[,9]<0),]  #all neg to 0, 16
# library(dplyr)
# df.par.a = df.par.a %>% anti_join(data3)  #subtracts one datafrma efrom another
# df.par.a = filter(df.par.a, tot.par.a > 1)
# 
# # p0 = ggplot()+geom_point(df.par.a, mapping = aes(x = time, y = tot.par.a, alpha = 0.1 ), col = 'steelblue')+facet_wrap(~date)#+scale_x_log10(name ="dep sed")
# # p0= p0+ scale_y_continuous( limits = c(0, 750))
# # p0
# length(which(df.par.a$tot.par.a >113))/nrow(df.par.a)  #rougly half of day light hrs are over settlement threshold. Note all negative values removed
# nrow(irrad1)
# length(unique(irrad1$date))
# range(df.par.a$`425`)
# data1.spec = df.par.a
# 
# ################################################
# ######NTU##########################################################################
# #2018 ntu flor
# 
# #Data imprt
# setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/6 Post-settlement/2 2018/2018 field/2 water quality")
# data2<- read.table("https://raw.githubusercontent.com/gerard-ricardo/data/master/2018%20ntu%20flor", sep="\t", header = TRUE,  comment.char = "",check.names = FALSE, quote="", na.strings=c("NA","NaN", " ") )
# head(data2)
# data2 = data2[complete.cases(data2), ]
# str(data2)
# data2 = filter(data2, date.time > 43425)
# data2 = filter(data2, date.time < 43488)
# 
# #################################################
# #Data label
# colnames(data2) <- c("date", "time", "date.time", "ntu")   #columns. Can use index to change indic column
# data2$date.time <- as.numeric(as.character(data2$date.time))
# data2$date <- as.numeric(as.character(data2$date))
# data2$time <- as.numeric(as.character(data2$time))
# data2$ntu <- as.numeric(as.character(data2$ntu))
# # data2$X455nm <- as.numeric(as.character(data2$X455nm))
# # data2$X485nm <- as.numeric(as.character(data2$X485nm))
# # data2$X515nm <- as.numeric(as.character(data2$X515nm))
# # data2$X555nm <- as.numeric(as.character(data2$X555nm))
# # data2$X615nm <- as.numeric(as.character(data2$X615nm))
# # data2$X660nm <- as.numeric(as.character(data2$X660nm))
# # data2$X695nm <- as.numeric(as.character(data2$X695nm))
# #data2$date <- as.factor(data2$date)
# str(data2)
# 
# data2 = data2[complete.cases(data2), ]
# head(data2)
# options(scipen = 99)  #display x no of digits
# library(ggplot2)
# 
# #Data cleaning
# data2 <- data2[order(data2$date.time),]
# tail(data2)
# data2 = dplyr::filter(data2, date.time > 43422) #trim away error at start. First real reading but not deployment
# data2 = dplyr::filter(data2, date.time < 43489) #trim away error at end
# data2 = dplyr::filter(data2, ntu < 500) #trim away error at end
# #data2$date
# data2$date <- factor(data2$date)
# unique(data2$date)   #66 days
# 
# #remove negs
# data2.neg = data2[which(data2[,4]<0),]  #all neg to 0, 16
# library(dplyr)
# data2 = data2 %>% anti_join(data2.neg)  #subtracts one datafrma efrom another
# range(data2$ntu)
# 
# #removing 2.5 proceeding value above 3ntu
# head(data2)
# data2$lag = lag(data2$ntu)
# data2 = data2[complete.cases(data2), ]  #remove the na
# data2$ntu.dif=data2$ntu - data2$lag   #diff
# data2$ntu.dif=sqrt(data2$ntu.dif^2)  #overright removing neg numbers
# data2$dif.2.5=data2$ntu.dif/data2$ntu  #overright removing neg numbers
# data2$lead = lead(data2$dif.2.5)
# 
# data4 = data2[which(data2$lead>2.5),]   #a diffrence on great than 10 ntu between readings. 10 radings
# data5 = data4[which(data4$ntu>3),]   #a diffrence on great than 10 ntu between readings. 10 radings
# data6 = data2 %>% anti_join(data5)  #subtracts one datafrma efrom another
# hist(data6$ntu)
# data2 = data6
# data2$SS = data2$ntu*1.1
# quantile(data2$SS, probs = seq(0, 1, 0.20))
# 
# data3 = select (data2,c(date.time,ntu,SS))
# 
# data1.ntu = data3
# str(data1.ntu)
# #data1.ntu$time <- data1.ntu$date.time - floor(data1.ntu$date.time)  #extract decimal time
# 
# ####################################################
# #####Combine and filter to XX NTU########################################
# #combine
# head(data1.ntu, 100)
# head(data1.spec, 100)
# data1.ntu$date.time = round(data1.ntu$date.time, 2)
# data1.spec$date.time = round(data1.spec$date.time, 2)
# str(data1.spec)
# data1.comb  = left_join(data1.ntu, data1.spec, by = 'date.time')  #COMBINING HERE
# data1.comb = data1.comb[complete.cases(data1.comb), ] #remove any ntu without spec
# range(data1.comb$'425')
# # cum.ntu.fun = ecdf(data1.comb$ntu)    # P is a function giving the empirical CDF of X
# # cum.ntu.fun(50)  #inset ntu, return probs
# #data1.comb$ntu.perc = round(cum.ntu.fun(data1.comb$ntu), 2)  #x ntu is percentile
# write.table(data1.comb,'ntu and par.txt')

###################################################

##To do
#add chloropyll
#manage nighttime (remove?)
#cloud cover
#wind speed



#libraries
library(dplyr)
library(tidyr)
library(jpeg)
library(png)
library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek2")  #set theme in code

##1) Import data
#setwd("C:/Users/g_ric/OneDrive/1 Work/4 Writing/1 Ricardo et al  - Modelling multiple ELHS sed scenario/field-work/field.ntu.par")
#data1.comb <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2018%20ntu%20and%20par", header= TRUE,dec=",", na.strings=c("",".","NA"))
#save(data1.comb, file = file.path("./Rdata", "2018_wq_data_comb.RData"))
load('./Rdata/2018_wq_data_comb.RData')  #data1.comb
data1 = data1.comb

#labelling
head(data1)
options(scipen = 999)  # turn off scientific notation
str(data1)
data1[] <- lapply(data1, function(x) as.numeric(as.character(x)))   #convert all to numeric


#data format
range(data1$date.time)
head(data1)
#date.time is 202325 rows, 62.52 days, 0.000011574 d increment, 0.9999936 s increments
data1 %>% group_by(date.time)
max(data1$date.time)  - min(data1$date.time)
(data1$time[2] - data1$time[1] ) * (24 * 60 *60)
#ntu
range(data1$ntu)

range(data1$X425)
data1 = data1[complete.cases(data1), ] #remove any ntu without spec


#add spectral indexes
data1$b_g = data1$X455 / data1$X555   
data1$b_r = data1$X455 / data1$X660 

#add wind
load('./Rdata/2018_19_windspeed_cleveland.RData') #data2
head(data2)
data1$wind_speed <- NA
pb <- txtProgressBar(min = 0, max = nrow(data1), style = 3)
for (i in 1:nrow(data1)) {
  differences <- abs(data1$date.time[i] - data2$datetime)
    closest_index <- which.min(differences)
    data1$wind_speed[i] <- data2$wind_speed[closest_index]
    setTxtProgressBar(pb, i)
}
head(data1)
hist(data1$wind_speed)
plot(data1$date.time, data1$wind_speed)
#lines(data1$std_wind_speed~data1$date.time)




# #categorical filter
# #ntu
# data1$ntu = round(data1$ntu, 1) #round to 1 dec for broader categories
# x.percentile = 0.2  #in probs
# result.perc = round(unname(quantile(data1$ntu, probs = x.percentile)),1) #insert probs, return ntu
# data1 = data1[which(data1$ntu==result.perc),]   #numeric

#filter
ntu_99th_percentile <- quantile(data1$ntu, 0.99)
data1 <- data1[data1$ntu <= ntu_99th_percentile, ]


#daytime, only daylight hrs have results
range(data1$time) *24  #hours of light
#data1$time = round(data1$time, 1) #round to 1 dec for broader categories
#time.cat = 0.4  #values 
#x.percentile = 0.1  #in probs
#result.perc = round(unname(quantile(data1$ntu, probs = x.percentile)),1) #insert probs, return ntu
#data1 = data1[which(data1$time==time.cat),]   #numeric


####plot of all predictors
#data2 = dplyr::select(data1, c(date.time, ntu, tot.par.a, b_g, b_r))  #remove column. Make sure have package on front 
head(data1)
nrow(data1)
data1 = data1[sample(nrow(data1), 1000), ]
data1$std_wind_speed = (data1$wind_speed - min(data1$wind_speed)) / (max(data1$wind_speed) - min(data1$wind_speed)) #standardise to 1
data1$std_ntu = (data1$ntu - min(data1$ntu)) / (max(data1$ntu) - min(data1$ntu)) #standardise to 1
data1$std_tot.par.a =  (data1$tot.par.a - min(data1$tot.par.a)) / (max(data1$tot.par.a) - min(data1$tot.par.a)) #standardise to 1
data1$std_b_g = (data1$b_g - min(data1$b_g)) / (max(data1$b_g) - min(data1$b_g)) #standardise to 1
data1$std_b_r =  (data1$b_r - min(data1$b_r)) / (max(data1$b_r) - min(data1$b_r)) #standardise to 1
data2 = dplyr::select(data1, c(date.time, std_ntu, std_tot.par.a, std_b_g, std_b_r, time, std_wind_speed))  #remove column. Make sure have package on front 
head(data2)
plot(data2$date.time, data2$std_wind_speed)
data1.long = data2 %>% tidyr::pivot_longer(cols = -date.time,  names_to = "predictors" ,values_to = "meas") %>% data.frame()  #keep vec.x, add all other columns to factors , add all their values to meas)
head(data1.long)

p0 = ggplot()
p0 = p0 + geom_point(data = data1.long, aes(x = date.time, y = meas,  group = predictors, color = predictors), alpha = 0.8, size = 1, position=position_jitter(height = 0.01, width = .01))
#p0 = p0 + geom_point(data = data2, aes(x = date.time, y = std_wind_speed), alpha = 0.8, size = 1)
#p0 = p0 + geom_line(data = df1, aes(x =  raw_x, y = pred), color = 'grey30', linewidth=1)
#p0 = p0 +  geom_ribbon(data = df1, aes(x = raw_x, ymin=lower, ymax=upper,fill='grey'),  alpha=0.2)
# p0 <- p0 + geom_smooth(data = data2, aes(x = date.time, y = std_wind_speed, group = 1), 
#                        method = "loess", color = "black", se = TRUE)
p0 = p0 + labs(x=expression(Time),
               y=expression(Normalised~meas~(prop.)))
p0 = p0 + scale_y_continuous( limits = c(0, 1.2)) 
p0 = p0 + theme_sleek2()
p0 = p0 + scale_color_manual( values = c(c('#3B9AB2', '#78B7C5', '#E1AF00','#F21A00', 'grey30', 'pink')), )  #can lookup html  hex colours to get specific eg #FFE6F5
p0 = p0  + theme(legend.position = c(0.8, 0.8))
p0

write.csv(data2, file = file.path("./data", "2018_field_allfactors.csv"))



###Gradient boosted machines####

#this uses env variables to predict the sperctral indexes (but more interesting the other way round i.e can indexes predict env varaibles).
#note that this is the shape of the curve, not the amplitude


# Assuming data2 is your dataframe
library(xgboost)
library(caret) # For confusionMatrix

# distrubtions
# plot(density(data2$std_ntu))
# plot(density(data2$std_tot.par.a))
# plot(density(data2$std_b_g))
# plot(density(data2$std_b_r))
# plot(density(data2$std_wind_speed))
# plot(density(data2$time ))
# 
# 
# plot(std_b_g ~ std_ntu)
library(GGally)
ggpairs(data2)

# Split the data into training and test sets
set.seed(123) # For reproducibility
training_index <- sample(1:nrow(data2), 0.7 * nrow(data2))
train_data <- data2[training_index, ]
test_data <- data2[-training_index, ]

# Exclude 'std_b_g', 'date.time', and 'std_b_r' from predictors
predictor_columns <- setdiff(names(train_data), c("std_b_g", "date.time", "std_b_r"))
dtrain <- xgb.DMatrix(data = as.matrix(train_data[, predictor_columns]), label = train_data$std_b_g)
dtest <- xgb.DMatrix(data = as.matrix(test_data[, predictor_columns]), label = test_data$std_b_g)


# Set parameters for xgboost
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.3,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1
)

# Number of boosting rounds
nrounds <- 100

# Train the model
xgb_model <- xgboost(
  params = params,
  data = dtrain,
  nrounds = nrounds,
  nthread = 2, # Adjust based on your machine
  verbose = 1
)

# Predict on test data
predictions <- predict(xgb_model, dtest)

# Evaluate model performance using RMSE
actuals <- test_data$std_b_g
(rmse <- sqrt(mean((predictions - actuals)^2)))

# MAE
(mae <- mean(abs(predictions - actuals)))

# R-squared
rss <- sum((predictions - actuals) ^ 2)
tss <- sum((actuals - mean(actuals)) ^ 2)
(r_squared <- 1 - rss / tss)

# MAPE - Ensure no division by zero
(mape <- mean(abs((predictions - actuals) / actuals), na.rm = TRUE) * 100)

# Plot feature importance
importance_matrix <- xgb.importance(feature_names = predictor_columns, model = xgb_model)
xgb.plot.importance(importance_matrix)



# Use index to predict factors --------------------------------------------

library(xgboost)
library(caret) # For confusionMatrix

# Prepare the data
# Assuming data2 already has the B/G index calculated and stored in 'std_b_g'
# and the factors you want to predict are 'std_ntu', 'std_wind_speed', 'time'

# Define predictor and target columns
predictor_column <- data2$std_b_g  # Assuming 'std_b_g' is the column name for the B/G index
target_columns <- c("std_ntu", "std_wind_speed", "time")  # Replace with actual factor column names
target_columns <- c("std_ntu")  # Replace with actual factor column names


# Split the data into training and test sets
set.seed(123)  # For reproducibility
training_index <- sample(1:nrow(data2), 0.7 * nrow(data2))
train_data <- data2[training_index, ]
test_data <- data2[-training_index, ]

# Function to train and evaluate model for each factor
train_and_evaluate <- function(target_column) {
  dtrain <- xgb.DMatrix(data = as.matrix(train_data[, predictor_column, drop=FALSE]), label = train_data[[target_column]])
  dtest <- xgb.DMatrix(data = as.matrix(test_data[, predictor_column, drop=FALSE]), label = test_data[[target_column]])
  
  # Set parameters for xgboost
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eta = 0.3,
    max_depth = 6,
    min_child_weight = 1,
    subsample = 1,
    colsample_bytree = 1
  )
  
  # Number of boosting rounds
  nrounds <- 100
  
  # Train the model
  xgb_model <- xgboost(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    nthread = 2,  # Adjust based on your machine
    verbose = 1
  )
  
  # Predict on test data
  predictions <- predict(xgb_model, dtest)
  
  # Evaluate model performance using RMSE
  actuals <- test_data[[target_column]]
  rmse <- sqrt(mean((predictions - actuals)^2))
  
  # MAE
  mae <- mean(abs(predictions - actuals))
  
  # R-squared
  rss <- sum((predictions - actuals) ^ 2)
  tss <- sum((actuals - mean(actuals)) ^ 2)
  r_squared <- 1 - rss / tss
  
  # MAPE - Ensure no division by zero
  mape <- mean(abs((predictions - actuals) / actuals), na.rm = TRUE) * 100
  
  # Print performance metrics
  cat("Performance for", target_column, ":\n")
  cat("RMSE:", rmse, "\n")
  cat("MAE:", mae, "\n")
  cat("R-squared:", r_squared, "\n")
  cat("MAPE:", mape, "%\n\n")
  
  # Plot feature importance
  importance_matrix <- xgb.importance(feature_names = as.character(predictor_column), model = xgb_model)
  xgb.plot.importance(importance_matrix)
  
  return(xgb_model)
}

# Train and evaluate model for each factor
models <- lapply(target_columns, train_and_evaluate)



# clasifier model ---------------------------------------------------------

# Load necessary libraries
library(xgboost)
library(caret)
library(dplyr)

# Load the data
file_path <- "path_to_your_file/2018_field_allfactors.csv"  # Adjust this path
data <- read.csv(file_path)

# Example of defining classes for turbidity and time of day
# Let's assume you have thresholds to define low/high turbidity and early/late time of day

# Define turbidity classes
data <- data2 %>%
  mutate(turbidity_class = ifelse(std_ntu <= 0.1, "low", "high"))

# Define time of day classes (e.g., early: before noon, late: after noon)
data <- data %>%
  mutate(time_of_day_class = ifelse(time <= 0.5, "early", "late"))

# Combine classes into a single column for multi-class classification
data <- data %>%
  mutate(cause_class = paste(turbidity_class, time_of_day_class, sep = "_"))

# Define predictor and target columns
predictor_column <- "std_b_g"  # The B/G index
target_column <- "cause_class"  # The combined cause class

# Prepare the data for classification
X <- as.matrix(data[, predictor_column, drop = FALSE])
y <- as.factor(data[[target_column]])

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(y, p = .7, list = FALSE)
X_train <- X[trainIndex, , drop = FALSE]
X_test <- X[-trainIndex, , drop = FALSE]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Create DMatrix for xgboost
dtrain <- xgb.DMatrix(data = X_train, label = as.numeric(y_train) - 1)
dtest <- xgb.DMatrix(data = X_test, label = as.numeric(y_test) - 1)

# Set parameters for xgboost
params <- list(
  booster = "gbtree",
  objective = "multi:softprob",  # Softmax objective for multi-class classification
  num_class = length(levels(y))  # Number of classes
)

# Number of boosting rounds
nrounds <- 100

# Train the model
xgb_model <- xgboost(
  params = params,
  data = dtrain,
  nrounds = nrounds,
  nthread = 2,  # Adjust based on your machine
  verbose = 1
)

# Predict on test data
predictions <- predict(xgb_model, dtest)
predicted_labels <- max.col(matrix(predictions, ncol = length(levels(y)), byrow = TRUE)) - 1

# Convert numeric predictions back to factor levels
predicted_classes <- levels(y)[predicted_labels + 1]

# Evaluate the model
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), y_test)
print(confusion_matrix)

# Display the predicted probabilities for the first few test samples
predicted_probabilities <- matrix(predictions, ncol = length(levels(y)), byrow = TRUE)
print(predicted_probabilities[1:5, ])

# Convert numeric predictions back to factor levels
predicted_classes <- levels(y)[predicted_labels + 1]

# Display the first few predicted classes and their probabilities
predicted_probabilities <- matrix(predictions, ncol = length(levels(y)), byrow = TRUE)
colnames(predicted_probabilities) <- levels(y)
head(predicted_probabilities)



#########################


# #filter
# data1.filt = filter(data1, ntu > 9)   #change ntu here
# data1.filt = filter(data1.filt, ntu < 10)
# data11 = data1.filt %>% dplyr::select(ntu, X425nm, X455nm, X485nm, X515nm, X555nm ,X615nm ,X660nm, X695nm)
# data11$sum = rowSums(data11[, -1])  #sums all but first col
# data11$std.cf = 1/data11$sum  #standaise to 1
# data2.c = data11$std.cf*(data11[, -c(1, 10, 11)])
# data2.c$ntu = data11$ntu  #add back ntu
#rowSums(data2.c)

#plot
data.long = subset.dc1 %>% pivot_longer(-c(date.time, ntu, SS, time, tot.par.a),  names_to = "bin" ,values_to = "meas")
nm = c(425, 455, 485, 515, 555 ,615 ,660, 695)
data.long$nm = rep(nm, nrow(data.long)/length(nm))
tail(data.long)
med.d3 = data.long %>% group_by(bin) %>% summarise(med = median(meas))
med.d3$nm = nm

#Need to standarise spectra

#background

img <- readPNG("spectrum.PNG")
# Get the plot information so the image will fill the plot box, and draw it
# library(grid)
# gg <- rasterGrob(img)

# library(magick)
# img = image_read("spectrum.PNG")
# image_ggplot(img, interpolate = F)

# #Single spline
names.1 = seq(425, 695, by  = 1)
spl.list = spline(med.d3$nm, med.d3$med, n = length(names.1))
spl.df = data.frame(nm = spl.list$x, meas = spl.list$y)


p0 = ggplot()
#p0 = p0 + annotation_custom(gg, xmin=-1, xmax=1, ymin=-1, ymax=5)
p0 = p0 + ggpubr::background_image(img)
p0 = p0 +geom_point(data.long, mapping = aes(x = nm, y = meas), alpha = 0.05, col = 'grey70')
#p0= p0+ scale_y_continuous( limits = c(0, 750))
p0 = p0 + geom_point(med.d3, mapping = aes(x = nm, y = med, alpha = 0.1 ), size = 3)
#p0 = p0 + geom_smooth(med.d3, mapping = aes(x = nm, y = med, alpha = 0.1), se = F, size = 2, color = "steelblue4", fill = "steelblue1")
p0 = p0 + geom_line(spl.df, mapping = aes( x = nm, y = meas), size = 2, color = "steelblue4")
p0 = p0 + labs(x=expression(Wavelength~(nm)),
              y=expression(Irradiance~(mu~mol~m^{-2}~s^{-1})))
p0 = p0+ scale_x_continuous(breaks=c(425, 455, 485, 515, 555 ,615 ,660, 695))
p0
#grid.raster(my_image)

#ggplot_build(p0)$data[[4]]



##total par############

vec = median(data.long$tot.par.a)
names(vec) = 'Total PAR'

