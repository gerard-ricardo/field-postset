#All ms8 data density

#SEE postset PROJECT FOR ITS WQ ANALYSIS!


#####1 - import data##############
# setwd("C:/Users/gerar/OneDrive/1 Work/3 Results/6 Post-settlement/2 2018/2018 field/2 water quality")
# #Data import 2018 Florence (63)
# irrad1<- read.table("2018 ms8 flor.txt", sep="\t", header = TRUE,  comment.char = "",check.names = FALSE, quote="", na.strings=c("NA","NaN", " ") )
# save(irrad1, file = file.path("./Rdata", "2018_ms8_flor.RData"))
load('./Rdata/2018_ms8_flor.RData')
library(dplyr)
irrad1 = dplyr::filter(irrad1, date > 43424)
irrad1 = dplyr::filter(irrad1, date < 43488)
head(irrad1)
tail(irrad1)
#Data import 2019 Florence (12 d)
# irrad1<- read.table("https://raw.githubusercontent.com/gerard-ricardo/data/master/2019%20ms8%20fl", header = TRUE,  comment.char = "",check.names = FALSE, quote="", na.strings=c("NA","NaN", " ") , fill = T)  #2019 IMO
# irrad1 = irrad1[complete.cases(irrad1), ]
# range(irrad1$date.time)
# irrad1 = filter(irrad1, date > 43661)
# irrad1 = filter(irrad1, date < 43673)

#Data import 2017 Picnic 20 Dec 2017 and retrieval on 14 Feb 2018, 56 days
# irrad1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2017%20imo%20pb", header= TRUE,dec=",", na.strings="")  #2017 PB
# nrow(irrad1)  #12093 obs
# irrad1 = irrad1[complete.cases(irrad1), ]  #12093 obs
# irrad1 = filter(irrad1, date > 43089)
# irrad1 = filter(irrad1, date < 43145)  #11605 obs

#Data import 2017 Florence 20 Dec 2017 and retrieval on 14 Feb 2018, 56 days
# irrad1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2017%20ms8%20florence%20deep", header = TRUE,  comment.char = "",check.names = FALSE, quote="", na.strings=c("NA","NaN", " ") , fill = T)  #2017 PB
# head(irrad1)
# irrad1 = irrad1[complete.cases(irrad1), ]
# irrad1 = filter(irrad1, date > 43089)
# irrad1 = filter(irrad1, date < 43145)

###2 - Data cleaning##############################################
tail(irrad1)
irrad1 = irrad1[complete.cases(irrad1), ]
str(irrad1)
range(irrad1$time)
#hist(irrad1$time)
irrad1[] <- lapply(irrad1, function(x) as.numeric(as.character(x)))   #convert all to numeric
irrad1$date <- as.factor(irrad1$date)
str(irrad1)
irrad1 = irrad1[complete.cases(irrad1), ] %>% data.frame()
range(irrad1$X425nm)

#plot to see outliers
head(irrad1)
options(scipen = 999)  #display x no of digits
library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek1")  #set theme in code
# p1 = ggplot() + geom_point(data = irrad1, aes(x = date.time, y = X425nm, color = 'mean'))
# irrad1 = filter(irrad1, date.time > 43422) #trim away error at start. First real reading but not deployment
# irrad1 = filter(irrad1, date.time < 43489) #trim away error at end
# irrad1 = filter(irrad1, date.time > 43661) #trim away error at start. First real reading but not deployment
# irrad1 = filter(irrad1, date.time < 43673) #trim away error at end
irrad1$date <- factor(irrad1$date)
irrad1$blue.green = irrad1$X455nm/irrad1$X555nm  #add a blue:green row

##Clean anomalies (>500 and neg)
#Observe data
library(tidyr)
irrad1.long = gather(irrad1, class, irrad, X425nm:X695nm)
head(irrad1.long)
# p0 = ggplot()+geom_point(irrad1.long, mapping = aes(x = date.time, y = irrad))+facet_wrap(~class, nrow = 8)#+scale_x_log10(name ="dep sed")
# p0
library(dplyr)
irrad1.long = filter(irrad1.long, irrad < 500) #trim away error
irrad1.long.neg = irrad1.long[which(irrad1.long$irrad<0),]  #all neg to 0, 16
irrad1.long = irrad1.long %>% anti_join(irrad1.long.neg)  #subtracts one dataframe from another
head(irrad1.long)
range(irrad1.long$irrad)
irrad1.wide <- irrad1.long %>%pivot_wider(names_from = class, values_from = irrad, names_prefix = "")  #year goes to columns, their areas go as the values, area is the prefix
irrad1 = irrad1.wide
options(tibble.print_max = 50, tibble.print_min = 50)
irrad1 = irrad1[complete.cases(irrad1), ]
head(irrad1, 100)
range(irrad1$X425nm)

#3)#Visualize data - plot data split at every factor
# p0 = ggplot()+geom_point(irrad1.long, mapping = aes(x = date.time, y = irrad))+facet_wrap(~class, nrow = 8)#+scale_x_log10(name ="dep sed")
# p0   #julien time by each irrad

####
# range(irrad1$X555nm)
# p1
# p2 = ggplot() + geom_point(data = irrad1, aes(x = date.time, y = X555nm, color = 'mean'))
# irrad1 = filter(irrad1, X555nm < 500) #trim away error 
# p2
# p3 = ggplot() + geom_point(data = irrad1, aes(x = date.time, y = X455nm, color = 'mean'))
# p3
# irrad1 = filter(irrad1, X455nm < 2000) #trim away error 
# ###
# p4 = ggplot() + geom_point(data = irrad1, aes(x = date.time, y = X695nm, color = 'mean'))
# p4
# irrad1 = filter(irrad1, X695nm < 750) #trim away error 

# library(cowplot)
# plot_grid(p1, p2, labels = "AUTO", nrow = 2, align = 'v')

##### 3)Convert  irradiance to PAR##############################################
#PAR = Irradiance_simp*nm/con.fac    # Note: This website uses 11961.72 https://www.berthold-bio.com/service-support/support-portal/knowledge-base/how-do-i-convert-irradiance-into-photon-flux.html
df.simp = data.frame(irrad1$X425nm, irrad1$X455nm, irrad1$X485nm, irrad1$X515nm,irrad1$X555nm, irrad1$X615nm, irrad1$X660nm, irrad1$X695nm )
head(df.simp)
con.fac = 11973.78
df.par = (data.frame( irrad1$X425nm*425, irrad1$X455nm*455, irrad1$X485nm*485, irrad1$X515nm*515,irrad1$X555nm*555, irrad1$X615nm*615, irrad1$X660nm*660, irrad1$X695nm*695))/con.fac
colnames(df.par) = c('425', '455', '485', '515', '555', '615', '660', '695')
nrow(df.par)

#plot(df.par$irrad1.X555nm...555)
head(df.par)
range(df.par)
df.par = df.par[complete.cases(df.par), ]
range(df.par)

str(df.par)
nrow(df.par)
nrow(irrad1)

####4 fit spline or auc#############################
#Spline/AUC prep
# #Cubic interpolation #note only starting from 425 not 400 (may be less than realistic)
long =   nrow(df.par)  #no of rows per channel
nm = c(425, 455, 485, 515, 555, 615, 660, 695)
vecc = as.numeric(df.par[1,])

#####4.1 spline###########
#2) Fitting splines (not used for AUC)
 
# #Single spline
# sp.length = 301
# names = seq(400, 700, length  = sp.length)
# spl.df = spline(nm, df.par[1900,], n = sp.length)
# 
# #Single smooth spline
# # sp.length = 301
# # names = seq(400, 700, length  = sp.length)
# # library(splines)
# # tt = as.vector(df.par[1900,]) %>% as.numeric()
# # lda.pred <- lm(tt ~ ns(nm, knots=5))
# # spl.df = smooth.spline(nm, as.vector(df.par[1900,]), df  = 7)
# 
# #All splines
# #tot.par3 = vector("numeric", long)  #placeholder
# tot.par3  = matrix(nrow = long, ncol = sp.length)
# for(i in 1:long) { 
#   bob.spline <- spline(nm, df.par[i,], n = sp.length,  method = "fmm")   
#   tot.par3[i,] <- bob.spline$y
# } 
# tot.par3   #gives a matrix with each row the spline fits
# spline.df = tot.par3 %>% data.frame()
# str(spline.df)
# #plot(spline.df$X7)
# colnames(spline.df) <- names
# spline.df$date.time = as.numeric(irrad1$date.time)  #wide all interpolated splines at 1 nm
# spline.df.long = gather(spline.df, class, irrad, "400":"700")
# spline.df.long$class = as.numeric(spline.df.long$class)
# str(spline.df.long)
# 
# library(ggplot2)
# source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek1")
# test = data.frame(nm, values = t(df.par[2, 1:8]))
# colnames(test) <- c('nm', 'values')
# test2 = data.frame(names = names, values1 = t(spline.df[2, 1:301]))
# colnames(test2) <- c('nm', 'values')
# p0 = ggplot()+geom_point(test, mapping = aes(x = nm, y = values),position = position_jitter(width = .02), alpha = 0.50,size = 3 )+theme_sleek1()
# p0= p0+ geom_line(data = test2, aes(x =  nm, y = values), color = 'grey30', size=1)
# #p0= p0+ scale_y_continuous( limits = c(0, 1)) 
# p0

####animate spectrum##############
#animate spectrum
# str(spline.df.long)
# spline.df.long$dec.d = (floor(spline.df.long$date.time*10))/10
# spline.df.long$dec.d = as.factor(spline.df.long$dec.d)
# spline.df.long$class = as.factor(spline.df.long$class)
# spline.df.gr = spline.df.long %>% group_by(dec.d, class) %>% summarise(estimate = mean(irrad)) %>% data.frame()
# nrow(spline.df.gr)
# spline.df.gr$dec.d <- as.numeric(as.character(spline.df.gr$dec.d))
# spline.df.gr$class <- as.numeric(as.character(spline.df.gr$class))
# str(spline.df.gr)
# library(gganimate)
# library(jpeg)
# img <- readJPEG("spectrum.JPG", native = T)
# g <- rasterGrob(img, interpolate=TRUE)
# g
# p0 = ggplot()+geom_point(spline.df.gr, mapping = aes(x = class, y = estimate))+facet_wrap(~dec.d)#+facet_wrap(~date.time)#+scale_x_log10(name ="dep sed")
# p0 + transition_time(dec.d) #+
  # labs(title = "Day: {frame_time}")#+annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#####4.2 AUC for PAR##################
#single
library(MESS)
ff = auc(nm, df.par[1,], type = 'spline') 
auc(nm, vecc, type = 'spline')

#all
tot.par = vector("numeric", long)  #placeholder
for(i in 1:long) { 
  bob <- MESS::auc(nm, df.par[i,], type = 'spline', from = 400, to = 700 , rule = 2)   #changed based on workshop 
  tot.par[i] <- bob
} #THIS ADDS A CUBLINE SPLINE. Because there are only 8 data points, a cubic (join the dot) spline is best. 
df.par$tot.par = tot.par  #add to main df
df.par$date.time = irrad1$date.time
df.par$time = irrad1$time
head(df.par)  #all good to here
#plot(df.par$date.time, df.par$tot.par)
quantile(df.par$tot.par, probs = c(.01, 0.20, 0.5, 0.75, 0.80, .99)) # quartile of tot par inc night
cum.par.fun = ecdf(df.par$tot.par)    # P is a function giving the empirical CDF of X
cum.par.fun(80)

min(df.par$tot.par)
plot(df.par$date.time, df.par$tot.par)
length(which(df.par$tot.par >113))/nrow(df.par)
data3 = df.par[which(df.par[,9]<0),]  #all neg to 0, 16
library(dplyr)
df.par = df.par %>% anti_join(data3)  #subtracts one datafrma efrom another
df.par = filter(df.par, tot.par > 1)

# p0 = ggplot()+geom_point(df.par, mapping = aes(x = time, y = tot.par, alpha = 0.1 ), col = 'steelblue')+facet_wrap(~date)#+scale_x_log10(name ="dep sed")
# p0= p0+ scale_y_continuous( limits = c(0, 750))
# p0
length(which(df.par$tot.par >113))/nrow(df.par)  #rougly half of day light hrs are over settlement threshold. Note all negative values removed
nrow(irrad1)
length(unique(irrad1$date))

range(df.par$`425`)

###probs at treatment combinations###################################
# data_long2 <- gather(irrad1, factor, measurement, X425nm:X695nm, factor_key=TRUE)  #changes wide to long, with columns to collapse grouped into 'factor', broad:red = left column to right to collapse
# data_long2$obs <- factor(formatC(1:nrow(data_long2), flag="0", width = 3))# unique tank ID for later on
# data_long2 = arrange(data_long2, date.time, factor)  # Allows multiple sorts 
# max.d3 = data_long2 %>% group_by(date.time) %>% summarise(max. = max(measurement))  #basic groups

#max nm without spline
# df.par1 = dplyr::select(df.par,-c(tot.par, date.time, time))
# str(df.par1)
# colnames(df.par1) = c('425', '455', '485', '515', '555', '615', '660', '695')
# df.par$max.par = colnames(df.par1)[apply(df.par1, 1, which.max)] %>% as.numeric()  #this finds the max nm without spline
# df.par_long = df.par %>% pivot_longer(-c(tot.par, max.par),  names_to = "nm.fac" ,values_to = "meas") #keep vec.x, add all other columns to factors , add all their
# df.par_long$nm.num <- as.numeric(as.character(df.par_long$nm.fac))
# 
# by.maxpar = split(df.par_long, df.par_long$max.par)  #split up into different turbidities (by  spectral peak)
# quantile(by.maxpar$'425'$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile 425 (deep blue)  n = 1
# quantile(by.maxpar$'455'$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile  ( blue)  n = 5
# quantile(by.maxpar$'485'$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile  ( turquoise)  n = 161
# quantile(by.maxpar$'515'$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile  ( green)   n = 3408
# quantile(by.maxpar$'555'$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile  ( green-yellow) n = 19751
# quantile(by.maxpar$'615'$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile  ( orange)  n = 1
# quantile(by.maxpar$'660'$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile  ( orange-red)  n = 4
# quantile(by.maxpar$'695'$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile  ( orange-red)  n = 5
# nrow(by.maxpar$'615')
# 
# p0 = ggplot()+geom_point(by.maxpar$'455', mapping = aes(x = nm.num, y = meas, col = tot.par, alpha = 0.3))#+facet_wrap(~class, nrow = 8)#+scale_x_log10(name ="dep sed")
# p0 

#Max PAR i.e what is dominant colour using splines (splines are a bit off)
# spline.df2 = dplyr::select(spline.df,-c(date.time))
# spline.df2$max.par = colnames(spline.df2)[apply(spline.df2, 1, which.max)] %>% as.numeric()
# df.par$max.par = spline.df2$max.par
# quantile(spline.df2$max.par)
# 
# spl.par = vector("numeric", long)  #placeholder
# for(i in 1:long) { 
#   bob <- auc(nm, df.par[i,], type = 'spline')   
#   spl.par[i] <- bob
# } #THIS ADDS A CUBLINE SPLINE. Because there are only 8 data points, a cubic (join the dot) spline is best. 

###blue:green probs########################
df.par$blu.gre = df.par$'455'/df.par$'555'  #blue:green in umols
quantile(df.par$blu.gre, c(0.1, 0.2, 0.5, 0.8, 0.9))
# ecdf1 = ecdf(df.par$tot.par)
# df.par$par.prob = ecdf1(df.par$tot.par)
#bin par
#range(df.par$tot.par)[1]
#bin.par = round(max(df.par$tot.par) - min(df.par$tot.par))
#par.den = density(df.par$tot.par, n= bin.par+150) #bin to ~ integers. Trial and error to get binning right (use table and check for NAs)
#par.den = data.frame(x = par.den$x, y = par.den$y)
#par.den = par.den %>% filter(between(x, range(df.par$tot.par)[1], range(df.par$tot.par)[2]))
#plot(par.den$x, par.den$y)  #this is th counts of PAR throughout the day
#df.par.probs = data.frame(tot.par.r = round(par.den$x), par.probs = par.den$y*1/sum(par.den$y))  #stnadaises counts to 1 i.e probs
df.par$tot.par.r = round(df.par$tot.par)
#table(df.par.probs$tot.par.r) #making sure no duplicates which doubles data in left join. Need to check
#df.par2 = left_join(df.par, df.par.probs, by = 'tot.par.r')  #probs joined onto data frame
#nrow(df.par)

#create bins groups
next_100 <- function(x) { 100*ceiling(x/100) }  #round to 100. works out how many times number occurs in 100. If greater than 1, then it will round up to next group
df.par$tot.par.100 = next_100(df.par$tot.par)
#bin bg
df.par$blu.gre.r = round(df.par$blu.gre, digits = 2)
next_0.2 <- function(x) { 0.2*ceiling(x/0.2) }  #round to 50. works out how many times number occurs in 50. If greater than 1, then it wil round up to next group
df.par$blu.gre.0.2 = next_0.2(df.par$blu.gre)

#df.par2[sapply(df.par2, is.infinite)] <- NA  #replace inf with na
#df.par2 = df.par2[complete.cases(df.par2), ]
#range(df.par2$blu.gre.r)
df.par = filter(df.par, blu.gre.r < 2)  #remove outliers
#bin.par2 = round(max(df.par2$blu.gre.r) - min(df.par2$blu.gre.r), digits = 2)*100  #*100 gets decimals back to whole numbers
#range(df.par2$blu.gre.r)*100
#bg.den = density(df.par2$blu.gre, n=bin.par2+10)
#bg.den = data.frame(x = bg.den$x, y = bg.den$y)
#bg.den = bg.den %>% filter(between(x, range(df.par2$blu.gre.r)[1], range(df.par2$blu.gre.r)[2]))
#plot(bg.den$x, bg.den$y)  #this is th counts of bg throughout the day
#df.bg.probs = data.frame(blu.gre.r = round(bg.den$x, digits  = 2), bg.probs = bg.den$y*1/sum(bg.den$y))
#table(df.bg.probs$blu.gre.r) #making sure no duplicates which doubles data in left join. Need to check
#df.par3 = left_join(df.par2, df.bg.probs, by = 'blu.gre.r')  #probs joined onto data frame

# table(df.par4$tot.par.r) %>% unclass()
# sort(unique(df.par4$tot.par.r))
count.df = table(df.par$blu.gre.0.2, df.par$tot.par.100) %>% unclass() %>% data.frame()
colnames(count.df)  = sort(unique(df.par$tot.par.100))
probs.df = round(count.df/sum(count.df), digits = 3)
probs.df$bg.0.2 = sort(unique(df.par$blu.gre.0.2))
probs.df_long = probs.df %>% pivot_longer(-bg.0.2,  names_to = "tot.par.100" ,values_to = "meas")  #keep vec.x, add all other columns to factors , add all their values to meas)



# p0 <- ggplot(probs.df_long, aes(tot.par.100, bg.0.2))
# p0 = p0 + geom_density_2d_filled(na.rm = T, show.legend = T, contour_var = "density", contour = T, bins = 20)
# p0= p0 + scale_y_continuous( limits = c(0, 1))
# p0= p0 + scale_x_continuous( limits = c(0, 800))
# p0 = p0+ labs(x=expression(PAR),
#               y=expression(Blue:green~ratio))
# p0= p0+ theme_classic()
# p0 = p0 + labs(fill = "Density")
# p0  #this is the probabilities of blue:green ratio for a given PAR


#df.par3$comb.probs = df.par3$par.probs * df.par3$bg.probs   #this is the par probs multiplied by the bg.probs
#sum(df.par3$comb.probs, na.rm = T)  #should this sum to 1? not sure of logic

df.par4 = dplyr::select(df.par,c(tot.par.r, blu.gre.r, date.time, time))
length(which(df.par4$blu.gre.r <0.5))/nrow(df.par4) *100  #38.7% of values are less than 0.5
length(which(df.par4$blu.gre.r >=0.65 & df.par4$tot.par.r >=200))/nrow(df.par4) *100  
length(which(df.par4$blu.gre.r >0.35 & df.par4$blu.gre.r <0.65 & df.par4$tot.par.r <=200))/nrow(df.par4) *100  

#Using outside boxes as greater or equal i.e >=0.65, <=0.35, <=200
length(which(df.par4$blu.gre.r >=0.65 & df.par4$tot.par.r <=200))/nrow(df.par4) *100   #TL = 7.10
length(which(df.par4$blu.gre.r >=0.65 & df.par4$tot.par.r >200))/nrow(df.par4) *100   #TR = 1.40
length(which(df.par4$blu.gre.r >0.35 & df.par4$blu.gre.r <0.65 & df.par4$tot.par.r <=200))/nrow(df.par4) *100   #ML = 43.56
length(which(df.par4$blu.gre.r >0.35 & df.par4$blu.gre.r <0.65 & df.par4$tot.par.r >200))/nrow(df.par4) *100   #MR = 32.65
length(which(df.par4$blu.gre.r <=0.35 & df.par4$tot.par.r <=200))/nrow(df.par4) *100   #BL = 15.64
length(which(df.par4$blu.gre.r <=0.35 & df.par4$tot.par.r >200))/nrow(df.par4) *100   #BR = 0

#7.63+1.4+43.43+31.84+15.69

####
# #filter to midday
# # p0 = ggplot()+geom_point(df.par4, mapping = aes(x = time, y = tot.par.r),position = position_jitter(width = .02), alpha = 0.50,size = 3 )+theme_sleek1()
# # p0
# df.par4 = filter(df.par4, time < 0.55)
# df.par4 = filter(df.par4, time > 0.45)

# ###
# #filter for high ratios
# df.par4 = filter(df.par4, blu.gre.r > 0.65)
# bg.den.pre = density(df.par4$blu.gre.r)  #density extrapolates so need to refilter
# bg.den.pre = data.frame(x = bg.den.pre$x, y = bg.den.pre$y)
# bg.den.pre = bg.den.pre %>% filter(between(x, range(df.par4$blu.gre.r)[1], range(df.par4$blu.gre.r)[2]))
# plot(bg.den.pre$x, bg.den.pre$y)
# plot(df.par4$blu.gre.r, df.par4$comb.probs)
# length(which(df.par4$blu.gre.r <0.7))/nrow(df.par4)
# 
# p0 <- ggplot(df.par4, aes(tot.par.r, blu.gre.r))
# p0 = p0 + geom_density_2d_filled(na.rm = T, show.legend = T, contour_var = "density", bins = 20)
# p0= p0 + scale_y_continuous( limits = c(0.65, 1),  breaks = c(0, 0.3, 0.5, 0.6, 0.7, 0.8))
# p0= p0 + scale_x_continuous( limits = c(0, 50))
# p0 = p0+ labs(x=expression(PAR),
#               y=expression(Blue:green~ratio))
# p0= p0+ theme_classic()
# p0 = p0 + labs(fill = "Density")
# p0  #this is the probabilities of blue:green ratio for a given PAR
# #very narrow peak at 0.7 (not seen if x expanded)
# table(df.par4$tot.par.r) %>% unclass()
# sort(unique(df.par4$tot.par.r))
# count.df2 = table(df.par4$tot.par.r, df.par4$blu.gre.r) %>% unclass() %>% data.frame()
# count.df2$par = sort(unique(df.par4$tot.par.r))
# plot(count.df2$X0.73)
# 
# library(plotly)
# plot_ly(x=df.par4$tot.par.r, y=df.par4$blu.gre.r, z=df.par4$comb.probs, type="contour")
# 
# p0 = ggplot()+geom_point(df.par4, mapping = aes(x = time, y = blu.gre.r),position = position_jitter(width = .02), alpha = 0.10,size = 3 )+theme_sleek1()
# p0
# quantile(df.par4$tot.par.r)
# quantile(df.par4$blu.gre.r, c(0.1, 0.5, 0.9))
# range(df.par4$time)
# #bin time
# time.den = density(df.par4$time, n= 88) #bin 0.01
# plot(time.den$x, time.den$y)  #this is th counts of PAR throughout the day
# df.time.probs = data.frame(time.r = round(time.den$x, digits = 2), time.probs = time.den$y*1/sum(time.den$y))  #counts to probs
# #length(unique(df.time.probs$time.r)) = 86
# table(df.time.probs$time.r) #making sure no duplicates which doubles data in left join
# df.par4$time.r = round(df.par4$time, digits = 2)
# df.par5 = left_join(df.par4, df.time.probs, by = 'time.r')  #probs joined onto data frame
# #bin bg
# #df.par5$blu.gre.r = round(df.par$blu.gre, digits = 2)
# #df.par2[sapply(df.par2, is.infinite)] <- NA  #replace inf with na
# #df.par2 = df.par2[complete.cases(df.par2), ]
# df.par5 = filter(df.par5, blu.gre.r < 2)  #remove outliers
# bin.bg2 = round(max(df.par5$blu.gre.r) - min(df.par5$blu.gre.r), digits = 2)*100  #no. of bins. *100 gets decimals back to whole numbers
# range(df.par5$blu.gre.r)*100
# bg.den2 = density(df.par5$blu.gre.r, n=bin.bg2+9)
# bg.den2 = data.frame(x = bg.den2$x, y = bg.den2$y)
# bg.den2 = bg.den2 %>% filter(between(x, range(df.par5$blu.gre.r)[1], range(df.par5$blu.gre.r)[2]))
# plot(bg.den2$x, bg.den2$y)  #this is the counts of bg above 0.65 (I think it extrapolated some with density fit. 
# df.bg.probs2 = data.frame(blu.gre.r = round(bg.den2$x, digits  = 2), bg.probs = bg.den2$y*1/sum(bg.den2$y))
# table(df.bg.probs2$blu.gre.r) #making sure no duplicates which doubles data in left join
# df.par6 = left_join(df.par5, df.bg.probs2, by = 'blu.gre.r')  #probs joined onto data frame
# 
# max(df.par6$bg.probs)
# df.par6$comb.probs = df.par6$time.probs*df.par6$bg.probs
# df.par6$comb.probs = df.par6$comb.probs*1/sum(df.par6$comb.probs)
# quantile(df.par6$comb.probs)
# 
# p0 <- ggplot(df.par6, aes(time, blu.gre.r))
# p0 = p0 + geom_density_2d_filled(na.rm = T, show.legend = T, bins = 20,  contour_var = "density")
# p0= p0 + scale_y_continuous( limits = c(0.65, 1.2))
# p0= p0 + scale_x_continuous( breaks = c(0.25,0.417, 0.583, 0.75), labels=c('6am', '10am', '2pm', '6pm'))
# p0 = p0+ labs(x=expression(decimal~time),
#               y=expression(Blue:green~ratio))
# p0= p0+ theme_classic()
# p0 = p0 + labs(fill = "Density")
# p0  #this is the probabilities of blue:green ratio for a given PAR
# 
####
t.0.23 = dplyr::filter(df.par4, blu.gre.r == 0.23)
max.0.23 = max(t.0.23$tot.par.r)  #this is the max PAR at 0.23 b:g ratio


####
#Main contour plot  () #note thin peak at 0.7 is lost if x-axis expanded too much
# p0 <- ggplot(df.par4, aes(tot.par.r, blu.gre.r))
# p0 = p0 + geom_density_2d_filled(na.rm = T, show.legend = T, contour_var = "density", contour = T, bins = 15)
# line1 = data.frame(tot.par.r = 0:800, blu.gre.r = rep(0.65, 801))
# line2 = data.frame(tot.par.r = 0:800, blu.gre.r = rep(0.35, 801))
# line3 = data.frame(tot.par.r = rep(200, 2), blu.gre.r = 0:1)
# p0 = p0 +geom_line(line1, mapping = aes(tot.par.r, blu.gre.r), col = 'red')
# p0 = p0 +geom_line(line2, mapping = aes(tot.par.r, blu.gre.r), col = 'red')
# p0 = p0 +geom_line(line3, mapping = aes(tot.par.r, blu.gre.r), col = 'red')
# p0= p0 + scale_y_continuous( limits = c(0, 1))
# p0= p0 + scale_x_continuous( limits = c(0, 800))
# p0 = p0+ labs(x=expression(PAR~(mu~mol~photons~"m"^{2}~"s"^{-1})),
#               y=expression(Blue:green~ratio))
# p0= p0+ theme_classic()
# p0 = p0 + labs(fill = "Density")
# p0  #this is the probabilities of blue:green ratio for a given PAR

#install.packages("akima")
library(akima)
library(ggplot2)

# Interpolation
# interp_data <- with(df.par4, interp(tot.par.r, blu.gre.r, percent_fertilisation, duplicate = "strip"))
# str(interp_data)
# 
# 
# flattened_data <- expand.grid(intercol=interp_data$x, colony=interp_data$y)
# flattened_data$z <- c(interp_data$z)

library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek2")  #set theme in code
library(viridis)
p1.1 <- ggplot(df.par4, aes(tot.par.r, blu.gre.r))
p1.1 <- p1.1 + stat_density2d_filled(aes(fill = ..density..), geom = "tile", contour = FALSE, bins = 100, na.rm = TRUE)
line1 <- data.frame(tot.par.r = 0:800, blu.gre.r = rep(0.65, 801))
line2 <- data.frame(tot.par.r = 0:800, blu.gre.r = rep(0.35, 801))
line3 <- data.frame(tot.par.r = rep(200, 2), blu.gre.r = 0:1)
p1.1 <- p1.1 + geom_line(data = line1, aes(tot.par.r, blu.gre.r), col = 'red')
p1.1 <- p1.1 + geom_line(data = line2, aes(tot.par.r, blu.gre.r), col = 'red')
p1.1 <- p1.1 + geom_line(data = line3, aes(tot.par.r, blu.gre.r), col = 'red')
p1.1 <- p1.1 + scale_fill_viridis_c()
p1.1 <- p1.1 + scale_y_continuous(limits = c(0, 1))
p1.1 <- p1.1 + scale_x_continuous(limits = c(0, 800))
p1.1 <- p1.1 + labs(x = expression(PAR ~ (mu ~ mol ~ photons ~ "m"^{2} ~ "s"^{-1})),
                y = expression(Turbidity~index~(blue:green ~ ratio)), 
                fill = "Prob. density")
p1.1 <- p1.1 + theme_sleek2()
p1.1 = p1.1 + theme(
  legend.position = 'right',
  legend.text = element_text(size = rel(1), colour = "grey20"), # adjust this as per your needs
  legend.background = element_rect(fill = "white", color = NA),
  #legend.key.width = unit(1, "lines"),  # adjust this as per your needs
  #legend.key.height = unit(1, "lines")  # adjust this as per your needs
)
p1.1 <- p1.1 + guides(fill = guide_colourbar(barheight = unit(3, "in")))
p1.1

#save(p1.1, file = file.path("./Rdata", "turbidityindex_PAR_plot.RData"))


####



table(df.par4$tot.par.r) %>% unclass()
sort(unique(df.par4$tot.par.r))
count.df = table(df.par4$tot.par.r, df.par4$blu.gre.r) %>% unclass() %>% data.frame()
count.df$par = sort(unique(df.par4$tot.par.r))
# plot_ly(x=df.par4$tot.par.r, y=df.par4$blu.gre.r, type="surface")
# df.par4.mat = table(df.par4$tot.par.r, df.par4$blu.gre.r) %>% unclass()

# plot_ly(z = ~df.par4.mat)
# fig = plot_ly(z = ~df.par4.mat, type = "surface")
# fig <- fig %>% layout(
#     scene = list(
#       xaxis = list(title = "bg", tickvals = seq(from = 0, to = 1.6, length.out = 16)),
#       yaxis = list(title = "Width", tickvals = seq(from = 0, to = 10, length.out = 11)),
#       zaxis = list(title = "Intensity", range = c(0, 40),  tickvals = seq(from = 0, to = 40, length.out = 8),ticktext = c(0, 10, 40))
#     ))
# 
# fig

#####Spectra at XXX NTU############################################


#####Bluegreen######################
#get blue:green on time-scale
irrad1.long.2 = split(irrad1.long, irrad1.long$class)
irrad1.bg = irrad1.long.2$blue.green
deci.f = 100
irrad1.bg$dec.d = floor((irrad1.bg$date.time*deci.f))/deci.f
irrad1.bg$dec.d <- as.factor(irrad1.bg$dec.d)
irrad1.1 = irrad1.bg %>% group_by(dec.d) %>% summarise(estimate = mean(irrad))

#ratio of blues to green
irrad1.bg = irrad1
irrad1.bg = irrad1.bg %>% data.frame()
irrad1.bg$day = irrad1$date.time-43423
irrad1.bg = filter(irrad1.bg, blue.green < 2) #trim for graph
irrad1.bg = filter(irrad1.bg, time > 0.2) #removing darkness
irrad1.bg = filter(irrad1.bg, time < 0.8) #removing darkness
str(irrad1.bg)
range(irrad1.bg$blue.green)
irrad1.bg$blue.green = irrad1.bg$X455nm/irrad1.bg$X555nm
# plot(irrad1.bg$date.time, irrad1.bg$X455nm/irrad1.bg$X555nm)
p1 = ggplot(irrad1.bg, mapping = aes(x = day, y = blue.green, alpha = 0.1, color ='grey70'))+ geom_line(color = 'grey70')+ theme(legend.position="none")+ scale_y_continuous( limits = c(0, 1.5))
# p1= p1+ scale_colour_gradientn(colours = heat(100))
p1= p1+ scale_color_gradient(low="green", high="blue" ,trans = "sqrt") +theme_sleek1()+ theme(legend.position="none")
p1 = p1+ labs(x=expression('Day'),y=expression('Blue:Green ratio'))
# p1
# p1 = p1 + geom_point(irrad1.bg, mapping = aes(size = 3, color = blue.green))+transition_reveal(day)
# c_gif <- animate(p1, width = 240, height = 240)
#strong blue > 1, med >.75 - 1, low <0.75

#PAR to B:G ratio. What is the max PAR at a given b:g ratio?
head(irrad1.bg)
plot(irrad1.bg$blue.green, irrad1.bg$tot.par)
quantile(irrad1.bg$blue.green)
cum = ecdf(irrad1.bg$blue.green)  #to get percentile at given b:g
cum(0.1663646)
plot(cum)
quantile(irrad1.bg$blue.green, c(0.988))  #usually green light. Only 1.2% has greater 450 bands.
df.bg = data.frame(bg = irrad1.bg$blue.green, PAR = irrad1.bg$tot.par)
df.bg <- df.bg[order(df.bg$bg),]
head(df.bg)
floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)  #I fluked this function. not sure how it works
df.bg$floor = floor_dec(df.bg$bg)
tail(df.bg)
df.bg$floor.bg <- as.factor(as.character(df.bg$floor))
df.bg %>% group_by(floor.bg) %>% summarise(max = quantile(PAR, c(0.99))) %>% data.frame()  #near max for each 0.1 increment to answer the
#question of the max PAR per increment
mm = density(df.bg$bg)
df.mm = data.frame(x = mm$x, y = mm$y )
p1 <- ggplot()
p1 = p1 + geom_line(df.mm, mapping = aes(x = mm$x, y = mm$y))
#p1= p1 + scale_y_continuous( limits = c(0, 1))
#p1= p1 + scale_x_continuous( limits = c(0, 800))
p1 = p1+ labs(x=expression(Blue:green~ratio),
              y=expression(Density))
p1= p1+ theme_sleek1()
p1 
####bg animation###########################
# write.csv(irrad1,'2018_par_ms8.csv')
#animation
# irrad1$date <- as.numeric(as.character(irrad1$date))
# # irrad1$day = irrad1$date-43422
# irrad1$day = irrad1$date-43661
# library(gganimate)
# p0 = ggplot()+geom_point(irrad1, mapping = aes(x = time, y = tot.par,  alpha  = 0.3))+ scale_y_continuous( limits = c(0, 750))+facet_wrap(~day)#+scale_x_log10(name ="dep sed")
# p0 = p0+ labs(x=expression(Time),
#               y=expression(PAR))
# p0= p0+ scale_x_continuous(labels=c("6am", "10am",'2pm','6pm'))+ theme(legend.position="none")
# p0 = p0 + transition_time(day) +
#   labs(title = "Day: {frame_time}")
# b_gif <- animate(p0, width = 240, height = 240)

###panel animation bg############################
#Panel gif
# library(dplyr)
# library(ggplot2)
# library(magick)
# library(gganimate)
# a_mgif <- image_write(a_gif, 'a.gif')
# b_mgif <- image_write(b_gif, 'b.gif')
# c_mgif <- image_write(c_gif, 'c.gif')
# a_mgif <- image_read('a.gif')
# b_mgif <- image_read('b.gif')
# c_mgif <- image_read('c.gif')
# 
# new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
# for(i in 2:100){
#   combined <- image_append(c(a_mgif[i], b_mgif[i]))
#   new_gif <- c(new_gif, combined)
# }
# 
# new_gif
# image_write(new_gif, 'combined.gif')
# image_write(new_gif, 'combined.mp4')

#####daylight hrs#################################
#Fit a spline to all the daylight data
irrad1$tot.par = tot.par
p0 = ggplot()+geom_point(irrad1, mapping = aes(x = time, y = tot.par, alpha = 0.5)) +stat_smooth()
sec.day = 24*60*60  #seconds in a day
sec.daylight = (max(irrad1$time) - min(irrad1$time))*sec.day   #seconds during daylight
ggplot(irrad1, aes(x = time, y = tot.par)) + stat_smooth(aes(outfit = fit<<-..y..), n = sec.daylight) + geom_point()

yy = seq(1:length(fit))
max(fit)  #fit a spline and find maxima. Mean spline max of all data
plot(yy,fit)
sum(fit)/1000000  #same as auc
auc(yy,fit, type = c("linear"))/1000000   #dli of the spline
plot(tot.par ~ time, irrad1)
md.loess = loess(tot.par ~ time, irrad1)
plot(md.loess)
predict(md.loess)
md.pred = predict(md.loess, data.frame(time = seq(0.25, 0.75, length = 80)), se = TRUE)
df1.upper = md.pred$fit + md.pred$se.fit*1.96
df1.upper = md.pred$fit - md.pred$se.fit*1.96

# quantile for daylight hrs PAR
vs = filter(irrad1, time >= 0.25) #trim to the daily hrs
vs1 = filter(vs, time <= 0.75) #trim to the daily hrs
quantile(vs1$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile. median par
mean(vs1$tot.par)  #mean par during daylime

#Max daily PAR
max.d.par = irrad1 %>% group_by(date) %>% summarise(estimate = max(tot.par))
max.d.par$estimate
levels(irrad1$date)
plot(max.d.par$estimate)
quantile(max.d.par$estimate, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile of each max par

quantile(irrad1$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile. median par
gg = ecdf(irrad1$tot.par)    # P is a function giving the empirical CDF of X
1 - gg(113)   #for florence 2018, highlight exceeds settlement threshold 47.6% daylight hrs picnic 2017 50.0% of daylight hrs

p0 = ggplot()+geom_point(irrad1, mapping = aes(x = date.time, y = tot.par),size = 1 )
#p0= p0+ geom_line(data = df.long.s, aes(x =  time, y = pred), color = 'grey30', size=1)
#p0 = p0 + facet_wrap(~date)#+scale_x_log10(name ="XXXX")#+geom_smooth(irrad1, mapping = aes(x = raw.x, y = suc/tot))
p0

#####Clean DLI##########################################
#Clean data to days for DLIS
data2 = data.frame(date = irrad1$date, date.time = irrad1$date.time, irrad1$tot.par)
#data2 = subset(data2, date.time >=43090)  #clipping off zeros
tail(data2, 50)
str(data2)
#data2 = subset(irrad1, nm <750)
data2$dec <- data2$date.time - floor(data2$date.time)  #extracting decimals (removing date compnent)
data2$dec.hr = data2$dec*24  #(convert to hr of day)
data2$hr = floor(data2$dec.hr)
data2$hr <- as.integer(as.character(data2$hr))
data2 = data2[complete.cases(data2), ]
levels(data2$date)

####DLIS########################Note:run from daylight hrs
str(data2)
data2.s = data2[sample(nrow(data2), nrow(data2)*0.1), ] #subsample to 10%
library(dplyr)
sec.day = 24*60*60
models <- data2 %>%
  nest_by(date) %>%
  mutate(model = list(with(data,loess(irrad1.tot.par~ dec, span = 0.5, data = data))))
models$model   #seems to fit and store models with no errors
#now add a vec.x into the tibble
dec.x = seq(0, 1, length = sec.day)
models$predict = list(dec.x)
models2 = models %>% do(dli = predict(.$model, .$predict))  #predict using the two list
models2$dli

ggg = data2 %>%nest_by(date)
plot(ggg$data[[2]]$dec, ggg$data[[2]]$irrad1.tot.par)
lines(dec.x, models2$dli[[2]])

#convert list to df
df <- data.frame(time = dec.x)
df1 <- data.frame(matrix(unlist(models2), nrow=sec.day, byrow=F),stringsAsFactors=FALSE)
colnames(df1) <- levels(data2$date)
df = cbind(df, df1)  #with dec.x
dlis = colSums(df1, na.rm = T)/10^6   #sum seconds and convert from umol to mol (10^6)
quantile(dlis, c(0.01, 0.2, 0.5, 0.8 ,0.95))  #BOOM!
cum.func = ecdf(dlis)
cum.func(9)
length(dlis)
length(which(dlis < 2))

df.long = df %>% pivot_longer(-time,  names_to = "date" ,values_to = "pred")
df.long = df.long[complete.cases(df.long), ]
df.long = arrange(df.long, date, time) 
irrad1 = irrad1 %>%  data.frame()
irrad1.s = irrad1[sample(nrow(irrad1), 10000), ]  #sample 1000 to mprove plot speed
df.long.s = df.long[sample(nrow(df.long), 1000), ]
p0 = ggplot()+geom_point(irrad1.s, mapping = aes(x = time, y = tot.par),size = 1 )
p0= p0+ geom_line(data = df.long.s, aes(x =  time, y = pred), color = 'grey30', size=1)
p0 = p0 + facet_wrap(~date)#+scale_x_log10(name ="XXXX")#+geom_smooth(irrad1, mapping = aes(x = raw.x, y = suc/tot))
p0

####Intepolate manually ######################################################

#Fit spline and interpolate for seconds
library(data.table)
df4 = split(data2,data2$date)
str(df4)
levels(data2$date)
# [1] "43088" "43089" "43090" "43091" "43092" "43093" "43094" "43095" "43096" "43097" "43098" "43099" "43100" "43101" "43102" "43103" "43104"
# [18] "43105" "43106" "43107" "43108" "43109" "43110" "43111" "43112" "43113" "43114" "43115" "43116" "43117" "43118" "43119" "43120" "43121"
# [35] "43122" "43123" "43124" "43125" "43126" "43127" "43128" "43129" "43130" "43131"

# [1] "43423" "43424" "43425" "43426" "43427" "43428" "43429" "43430" "43431" "43432" "43433" "43434" "43435" "43436" "43437" "43438" "43439"
# [18] "43440" "43441" "43442" "43443" "43444" "43445" "43446" "43447" "43448" "43449" "43450" "43451" "43452" "43453" "43454" "43455" "43456"
# [35] "43457" "43458" "43459" "43460" "43461" "43462" "43463" "43464" "43465" "43466" "43467" "43468" "43469" "43470" "43471" "43472" "43473"
# [52] "43474" "43475" "43476" "43477" "43478" "43479" "43480" "43481" "43482" "43483" "43484" "43485" "43486" "43487" "43488"

#single 
sec.day = 24*60*60
sec.daylight = (max(irrad1$time) - min(irrad1$time))*sec.day   #seconds during daylight
df4
#spline method
df5 = with(df4, loess.smooth(`43463`$date.time, `43463`$irrad1.tot.par,evaluation = sec.daylight, family = c("symmetric", "gaussian"),span=0.3))
sum( df5$y)/1000000  #11.87 DLI
plot(df4$`43463`$date.time, df4$`43463`$irrad1.tot.par)
lines(df5$x, df5$y)
#df5 = with(df4, spline(`43102`$date.time, `43102`$tot.par, n = sec.day, method = "periodic"))
#Blocking to hr method
df1 = df4$`43102` %>%group_by(hr) %>%summarise( par.hr = mean(irrad1.tot.par)) %>% as.data.frame()  # average per block
#df1.b = df4 %>%group_by(date) %>%group_by(hr) %>%summarise( par.hr = mean(irrad1.tot.par)) %>% as.data.frame()  # average per block
sum(df1$par.hr*60*60/1000000)


#Fit spline to one day
head(irrad1)
str(irrad1)
library(MESS)
ffgg = auc(nm, df.par[1,], type = 'spline') 
auc(nm, vecc, type = 'spline')





