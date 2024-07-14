#2018 dep sensors

# 1 Import data -----------------------------------------------------------
setwd("C:/Users/g_ric/OneDrive/1 Work/4 Writing/1 Ricardo et al  - Modelling multiple ELHS sed scenario/field-work")
# mid.reef <- read.table(file="MR.txt", header= TRUE,dec=",", na.strings=c("",".","NA"))
# mid.reef$date.time <- as.numeric(as.character(mid.reef$date.time))
# mid.reef$dep1 <- as.numeric(as.character(mid.reef$dep1))
# mid.reef$dep2 <- as.numeric(as.character(mid.reef$dep2))
# mid.reef$dep3 <- as.numeric(as.character(mid.reef$dep3))
# mid.reef$de.hr1 <- as.numeric(as.character(mid.reef$de.hr1))
# mid.reef$de.hr2 <- as.numeric(as.character(mid.reef$de.hr2))
# mid.reef$de.hr3 <- as.numeric(as.character(mid.reef$de.hr3))
# #mid.reef$dep.rate <- as.numeric(as.character(mid.reef$dep.rate))#dep.rate = mid.reef$dep.rate
# #note last line is important

###########################################
#mV to mg cm^2 conversion  (from Blake's excel)
options(scipen = 9999)  # turn off scientific notation
#data1 <- read.table(file="fb dep sensor.txt", header= TRUE,dec=",", na.strings=c("",".","NA")) 
#data1 <- read.table(file="gb dep sensor.txt", header= TRUE,dec=",", na.strings=c("",".","NA")) 
#data1 <- read.table(file="pb dep sensor.txt", header= TRUE,dec=",", na.strings=c("",".","NA")) 
#data1 <- read.table(file="mr dep sensor.txt", header= TRUE,dec=",", na.strings=c("",".","NA")) 
data1 <- read.table(file="vs dep sensor.txt", header= TRUE,dec=",", na.strings=c("",".","NA"))

# 2 Labelling and wrangling -----------------------------------------------
tail(data1)
data1[] <- lapply(data1, function(x) as.numeric(as.character(x)))   #convert all to numeric
str(data1)


# data1 = filter(data1, date.time >= 43433) #trim to the nearest complete day start for VS
# data1 = filter(data1, date.time < 43516) #trimmed to the nearest whote day bottom for VS

#trim to nearest whole day
min.date = ceiling(min(data1$date.time))
max.date = floor(max(data1$date.time))
data1 = dplyr::filter(data1, date.time >= min.date) #trim to the nearest complete day start for MR
data1 = dplyr::filter(data1, date.time < max.date) #trimmed to the nearest whole day bottom for MR
#if NA use a different sensor?
data1$cv.dep1 = ifelse(is.na(data1$dep1), data1$de.hr1*data1$cal1, data1$dep1*data1$cal1)  #choose other if na, and cf
data1$cv.dep2 = ifelse(is.na(data1$dep2), data1$de.hr2*data1$cal2, data1$dep2*data1$cal2)  #
data1$cv.dep3 = ifelse(is.na(data1$dep3), data1$de.hr3*data1$cal3, data1$dep3*data1$cal3)  #
head(data1, 20)

library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek1")  #set theme in code
p0 = ggplot()+geom_point(data1, mapping = aes(x = date.time, y = cv.dep1),position = position_jitter(width = .02, height = .02), alpha = 0.50,size = 3 )+theme_sleek1()
p0= p0+ scale_y_continuous( limits = c(0, 1)) 
p0

#gt first values at t0 of each wipe?
wipe.cv1 <- data1$cv.dep1[seq(1, length(data1$cv.dep1), 6)]  #get values from last wipe
wipe.cv2 <- data1$cv.dep2[seq(1, length(data1$cv.dep2), 6)]  #get values from last wipe
wipe.cv3 <- data1$cv.dep3[seq(1, length(data1$cv.dep3), 6)]  #get values from last wipe
head(wipe.cv1)
data1$wipe.cv.fill1 = rep(wipe.cv1, each=6)  #repeat wipe values for each dep value
data1$wipe.cv.fill2 = rep(wipe.cv2, each=6)
data1$wipe.cv.fill3 = rep(wipe.cv3, each=6)
head(data1$wipe.cv.fill1)
data1$zero.dep1 = data1$cv.dep1 - data1$wipe.cv.fill1
data1$zero.dep2 = data1$cv.dep2 - data1$wipe.cv.fill2
data1$zero.dep3 = data1$cv.dep3 - data1$wipe.cv.fill3
head(data1)
data1$zero.sed1 = ifelse(data1$zero.dep1>0,data1$zero.dep1*200,0)   #removing negative, and adding second conversion factor, what is this 200?
data1$zero.sed2 = ifelse(data1$zero.dep2>0,data1$zero.dep2*200,0)
data1$zero.sed3 = ifelse(data1$zero.dep3>0,data1$zero.dep3*200,0)

p0 = ggplot()+geom_point(data1, mapping = aes(x = date.time, y = zero.sed1),position = position_jitter(width = .02, height = .02), alpha = 0.50,size = 3 )+theme_sleek1()
#p0= p0+ scale_y_continuous( limits = c(0, 1)) 
p0

#correlate pre-wipe vs wipe/cfs
p0 = ggplot()+geom_point(data1, mapping = aes(x = cv.dep1, y = zero.sed1),position = position_jitter(width = .02, height = .02), alpha = 0.50,size = 3 )+theme_sleek1()
p0= p0+ scale_x_continuous( limits = c(0, 1)) 
p0  #

#subset = zero.sed1[43:600]  #checked same as excel
#time = 0:12147
#sub.time = 0:557
#plot(sub.time,subset)
time.d = (seq(0,50, length = 6)/1440)  #in days (60min * 24hr)
data1$group = rep(1:((nrow(data1))/6), each = 6)  #adding grouping column for each hour. Assuming each 6  = 1 hr.
data1$time = rep(time.d, nrow(data1)/6)  #add time (per 10 min) column
head(data1, 7)
#plot(data1$zero.sed1/c.f1/200, data1$zero.sed1)
min(data1$cv.dep1)

#fit regression and get slope. apply regression to each hr group . Slope constrainned to zero! Data = . is the data passed through 'do'
library(dplyr)
slope1 = data1 %>%
  group_by(group) %>%
  do(broom::tidy(lm(zero.sed1 ~ 0 + time.d, data = .)))   # 
slope2 = data1 %>%
  group_by(group) %>%
  do(broom::tidy(lm(zero.sed2 ~ 0 + time.d, data = .)))   # apply regression to each group 
slope3 = data1 %>%
  group_by(group) %>%
  do(broom::tidy(lm(zero.sed3 ~ 0 +time.d, data = .)))   # apply regression to each group 
slope.df1 = as.data.frame(slope1)
slope.df2 = as.data.frame(slope2)
slope.df3 = as.data.frame(slope3)
slope.df1 = filter(slope.df1, term == "time.d")  #slope 
slope.df2 = filter(slope.df2, term == "time.d")  #slope 
slope.df3 = filter(slope.df3, term == "time.d")  #slope 
all.slope = data.frame(slope.df1$estimate, slope.df2$estimate,slope.df3$estimate )
tail(all.slope, 100) #checked against excel. 
all.slope$julien.d = seq(min.date,max.date, length = nrow(all.slope))
head(all.slope, 25)

##approache 1  - get daily dep then average
#get mean slope/day (which is mg/cm^2/day for each sensor)
all.slope$group = rep(1:(nrow(all.slope)/24), each = 24)  #adding grouping column for day
mean.d1 = all.slope %>%
  group_by(group) %>%
  summarise(estimate = mean(slope.df1.estimate))
mean.d2 = all.slope %>%
  group_by(group) %>%
  summarise(estimate = mean(slope.df2.estimate))# apply mean to each hr rate group 
mean.d3 = all.slope %>%
  group_by(group) %>%
  summarise(estimate = mean(slope.df3.estimate))
mg.cm2.d = data.frame(mean.d1$estimate, mean.d2$estimate, mean.d3$estimate)
mg.cm2.d$julien.d = min.date:(max.date-1)  #add date
head(mg.cm2.d)

#get means of all sensors
mg.cm2.d$average = rowMeans(subset(mg.cm2.d, select = c(mean.d1.estimate, mean.d2.estimate, mean.d3.estimate)), na.rm = TRUE)
# plot(mg.cm2.d$julien.d, mg.cm2.d$mean.d1.estimate)
# points(mg.cm2.d$julien.d, mg.cm2.d$mean.d2.estimate)
# points(mg.cm2.d$julien.d, mg.cm2.d$mean.d3.estimate)
# lines(mg.cm2.d$julien.d,mg.cm2.d$average)

##approach 2 - average sensors then get daily dep
all.slope$average = rowMeans(subset(all.slope, select = c(slope.df1.estimate, slope.df2.estimate, slope.df3.estimate)), na.rm = TRUE)
quantile(all.slope$average, c(0.99))
plot(density(all.slope$average))

#all.slope$group = rep(1:(nrow(all.slope)/24), each = 24)  #adding grouping column for day
mean.ave = all.slope %>%
  group_by(group) %>%
  summarise(estimate = mean(average)) %>%  data.frame()
mean.ave$julien.d = min.date:(max.date-1)
# mg.cm2.d = data.frame(mean.d1$estimate, mean.d2$estimate, mean.d3$estimate)
# mg.cm2.d$julien.d = min.date:(max.date-1)  #add date
# head(mg.cm2.d)

# 7 Plotting --------------------------------------------------------------
# library(ggplot2)
# p0 = ggplot(NULL)
# p0 = p0+ scale_x_continuous(name ="Date", limits=c(43424,43522), breaks=c(43425, 43450, 43475, 43500, 43525), labels=c("21/11/2018"	,"16/12/2018",	"10/01/2019",	"4/02/2019",	"1/03/2019"))
# p0 = p0+ geom_point(data=mg.cm2.d, aes(x = julien.d, y = average, color = 'mean'))
# p0 = p0 + scale_y_continuous(name ="Deposition rate (mg/cm^2/d)", limits=c(0,200))
# # p0 = p0 + geom_point(data=mg.cm2.d, aes(x = julien.d, y = mean.d1.estimate, color = 'data'))
# # p0 = p0 + geom_point(data=mg.cm2.d, aes(x = julien.d, y = mean.d1.estimate, color = 'data'))
# # p0 = p0 + geom_point(data=mg.cm2.d, aes(x = julien.d, y = mean.d3.estimate, color = 'data'))
# p0 = p0+ theme(legend.position = c(0.9, 0.85))
# p0   #, breaks=c(43425, 43450, 43475, 43500, 43525), labels=c("21/11/2018	,16/12/2018,	10/01/2019,	4/02/2019,	1/03/2019'), limits=c(43424,43522)

#plot by daily dep sed
library(ggplot2)
p0= ggplot()
p0= p0 + geom_point(data = mg.cm2.d, aes(x = julien.d, y = average, alpha = 0.8), color = 'steelblue', size = 3, position=position_jitter(height = 0.1, width = .01))
p0 = p0+ scale_x_continuous(name ="Date", limits=c(43424,43522), breaks=c(43425, 43450, 43475, 43500, 43525), labels=c("21/11/2018"	,"16/12/2018",	"10/01/2019",	"4/02/2019",	"1/03/2019"))
p0 = p0 + labs(x=expression(Date),
              y=expression(Deposition~rate~(mg~"cm"^{-2}~'d'^{-1})))
#p0 = p0 + scale_y_continuous( limits = c(0, 1)) 
p0 = p0 + theme_sleek1()
p0 = p0 + scale_fill_manual( values = c("grey","khaki2"))
p0 = p0 + theme(legend.position="none")
p0 = p0 +geom_smooth(mg.cm2.d, mapping = aes(x = julien.d, y = average))
p0

#plot by hr dep sed
p0= ggplot()
p0= p0 + geom_point(data = all.slope, aes(x = julien.d, y = average, alpha = 0.8), color = 'steelblue', size = 3, position=position_jitter(height = 0.1, width = .01))
p0 = p0+ scale_x_continuous(name ="Date", limits=c(43424,43522), breaks=c(43425, 43450, 43475, 43500, 43525), labels=c("21/11/2018"	,"16/12/2018",	"10/01/2019",	"4/02/2019",	"1/03/2019"))
p0 = p0 + labs(x=expression(Date),
              y=expression(Deposition~rate~(mg~"cm"^{-2}~'hr'^{-1})))
#p0 = p0 + scale_y_continuous( limits = c(0, 1)) 
p0 = p0 + theme_sleek1()
p0 = p0 + scale_fill_manual( values = c("grey","khaki2"))
p0 = p0 + theme(legend.position="none")
p0 = p0 +geom_smooth(all.slope, mapping = aes(x = julien.d, y = average))
p0

# write.table(data1, file = "data1.csv", sep = ",", col.names = NA,
#             qmethod = "double") #export to csv
#fb.plot = p3
#gb.plot = p3
#pb.plot = p3
#mr.plot = p3
#data1.plot = p3
# 
# require(cowplot)
# plot_grid(fb.plot, gb.plot,pb.plot, mr.plot, data1.plot, p2 , labels = c("fb", "gb", "pb", "mr", "data1","wind"), ncol = 1, align = 'v')

#Plotting a few slopes
# data1.sub = data1[1:100,]
# ggplot(data1.sub, aes(x = time, y = zero.sed1, colour = group)) +
#   geom_point() +
#   facet_wrap( ~ group)

# head(data1)

# library(lattice)
# xyplot(data1$zero.sed1 ~ data1$time | group, data = data1, 
#        auto.key = list(corner = c(0, .98)), cex = 1.5)

# p1 =ggplot()+
#   geom_line(data=mg.cm2.d, aes(x = julien.d, y = average, color = 'mean')) +
#   scale_x_continuous(name ="Date", limits=c(43424,43522), breaks=c(43425, 43450, 43475, 43500, 43525), labels=c("21/11/2018", "16/12/2018", "10/01/2019", "4/02/2019", "1/03/2019") ) +
#   theme_sleek() + 
#   theme(legend.position = c(0.9, 0.85))+
#   scale_color_manual( name = "NTU", labels = "mean", values = 'black')
# 
# p1   #breaks=c(43089, 43099, 43110, 43120, 43130, 43141),labels=c("20/12/2017", "30/12/2017", "10/01/2018",'20/01/2018','30/01/2018','10/02/2018')
######Exceedance plots########################################
# head(mid.reef)
quantile(mg.cm2.d$average, probs = c(0.01, 0.20, 0.5, 0.8, 0.9, .99), na.rm=T) # quartile
# hist(mid.reef$dep.rate, breaks = 120, xlim = c(0, 300))

mg.cm2.d
cdf.fun=ecdf(mg.cm2.d$average)  #create a cumlative distribution function
summary(cdf.fun)
icx = seq(0.1, max(mg.cm2.d$average), 1)
cum.icx=cdf.fun(icx)  #run fucntion on defined values
exceed.icx=(1 - cum.icx)*100
exceed.icx
plot(icx,exceed.icx, log = 'x', type="l", col="black",ylim = c(0,100), xlim = c(0.1,max(mg.cm2.d$average)), main = 'Site', ylab = 'Cumulative Percentage (%)', xlab = 'Deposition rate (mg/cm^2/d)')
library(ggplot2)
p4 = ggplot(NULL)
p4 = p4+ scale_x_log10(limits=c(0.1,300))
p4 = p4+ geom_line(aes(x = icx, y = exceed.icx), color = "black")
p4 = p4 + scale_y_continuous(limits=c(0,100))
p4 = p4 + theme_classic()
p4 = p4 + labs(x=expression(Deposition~rate~(mg~"cm"^{-2}~'d'^{-1})),
              y=expression(Cumulative~Percentages~("%")))
p4

#fb.cfd = p4
#gb.cfd = p4
#pb.cfd = p4
#mr.cfd = p4
data1.cfd = p4

#require(cowplot)
#plot_grid(fb.cfd, gb.cfd, pb.cfd, mr.cfd, data1.cfd,labels  = c("fb", "gb", "pb", "mr", "data1"), ncol = 1, align = 'v')

###########################################
setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/6 Post-settlement/1 2018/2018 field/2 water quality/AIMS data")
windspeed <- read.table(file="2018 windspeed.txt", header= TRUE,dec=",", na.strings=c("")) 
tail(windspeed)
windspeed[complete.cases(windspeed), ]

windspeed$date.time2 <- as.numeric(as.character(windspeed$date.time2))
windspeed$wind.speed <- as.numeric(as.character(windspeed$wind.speed))
windspeed$wind.max <- as.numeric(as.character(windspeed$wind.max))
windspeed$mean.kots = windspeed$wind.speed/1.852
wind.speed = windspeed$wind.speed

p2 = ggplot(NULL)
p2 = p2+ geom_line(data=windspeed, aes(x = date.time2, y = wind.speed, color = 'mean')) 
p2 = p2+ scale_x_continuous(name ="Date", limits=c(43424,43522), breaks=c(43425, 43450, 43475, 43500, 43525), labels=c("21/11/2018", "16/12/2018", "10/01/2019", "4/02/2019", "1/03/2019")) 
p2 = p2 + geom_line(data=windspeed, aes(x = date.time2, y = wind.max, color = 'max'))+scale_y_continuous(name ="Wind speed (km/h)")
p2 = p2 + scale_color_manual( name = "Wind speed", labels = c("max", 'mean'), values = c('red','blue'))
p2 = p2 + theme_sleek()
p2 = p2+ theme(legend.position = c(0.9, 0.85))
p2   #, breaks=c(43425, 43450, 43475, 43500, 43525), labels=c("21/11/2018	,16/12/2018,	10/01/2019,	4/02/2019,	1/03/2019')


# #grid.arrange(p1, p2, nrow = 2)
# require(cowplot)
# plot_grid(p3, p2, labels = "AUTO", ncol = 1, align = 'v')


#Correlation between windspeed and dep sed
#lm(dep.rate~ wind.speed, family = gaussian)


