#ms9 test
##1) Import data
#setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/5 Settlement/2 2018/4 light intensity_spectra")
#irrad1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2020%20ms9%20test", header= TRUE,dec=",", na.strings=c("",".","NA"))
#irrad1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2020%20ms9%20neal%20test", header= TRUE,dec=",", na.strings=c("",".","NA"))
#ms9 0013 SN: 0049; 415.7,439.7,490.3,511.7,548.8,569.5,610.5,658.5,699.3
#irrad1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2020%20ms9%200013%200049", header= TRUE,dec=",", na.strings=c("",".","NA"))  
#ms9 0012 SN: 0057; 408.6,439.0,490.0,510.6,550.6,569.5,610.5,658.5,700.2
irrad1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2020%20ms9%200012%200057%202", header= TRUE,dec=",", na.strings=c("",".","NA")) 

head(irrad1)
options(scipen = 999)  # turn off scientific notation

####2)Organising and wrangling####
str(irrad1) #check data type is correct
#irrad1 = dplyr::select(irrad1,c(date, time, date.time, depth, tilt, ch.1:ch.9))  #remove column. Make sure have package on front 
irrad1[] <- lapply(irrad1, function(x) as.numeric(as.character(x)))   #convert all to numeric
str(irrad1) #check data type is correct
nrow(irrad1)
irrad1 = irrad1[complete.cases(irrad1), ] #complete cases
irrad1$date.fac <- as.factor(irrad1$date)
head(irrad1)
library(dplyr)
#nm = c(415.7,439.7,490.3,511.7,548.8,569.5,610.5,658.5,699.3)
nm = c(408.6,439.0,490.0,510.6,550.6,569.5,610.5,658.5,700.2)
names = round(nm) %>% as.character()  #add channels

names2 <- paste('x',names, sep='' )  #joining columns. Use sep=''  to remove space
colnames(irrad1)[5:13] <- names2  #0049
#colnames(irrad1)[6:14] <- names2

#filter for dates deployed
irrad1 = dplyr::filter(irrad1, date.time > 44147) #deployed on the 12/11/20 (44147) - 14/12/20   (44179)
irrad1 = dplyr::filter(irrad1, date.time < 44179)

library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek1")
theme_set(theme_sleek1())
p1 = ggplot() + geom_point(data = irrad1, aes(x = date.time, y = x440  ))+theme_sleek1()
p1

######Convert to PAR/nm#####
#PAR = Irradiance * nm / con.fac    # Note: This website uses 11961.72 https://www.berthold-bio.com/service-support/support-portal/knowledge-base/how-do-i-convert-irradiance-into-photon-flux.html
con.fac = 11973.78
df.par = irrad1[5:13] * nm / con.fac
nrow(df.par)
str(df.par)

#auc linear
MESS::auc(nm, df.par[1,], type = 'linear', from = 400, to = 700, rule = 2)
plot(nm, df.par[1,])

#auc spline
auc1 = auc(nm, df.par[1,], type = 'spline', from = 400, to = 700, rule = 2) #or use linear ses stats:splinefun and stats:integrate
auc1
plot(nm, df.par[1,])
lines(spline(nm, df.par[1,], method = 'fmm'))

#using newton-cotes
md1 = splinefun(nm, df.par[1,], method = 'fmm')
pracma::cotes(md1, 400, 700, 20, 5)

#using trap and approx
micromolespersec_ipol = approx(nm, df.par[1,],  xout=400:700, rule=2)
caTools::trapz(400:700, micromolespersec_ipol$y)
plot(nm, df.par[1,])
lines(approx(nm, df.par[1,],  xout=400:700, rule=2))  #some extrapolation

############all integrations############################
tot.par = vector("numeric", nrow(df.par))  #placeholder
for(i in 1:nrow(df.par)) { 
  bob <- MESS::auc(nm, df.par[i,], type = 'spline', from = 400, to = 700, rule = 2)   
  tot.par[i] <- bob
} # 
df.par$tot.par = tot.par  #add to main df
df.par$date.time = irrad1$date.time
df.par$time = irrad1$time
df.par$par = irrad1$par
head(df.par)  #all good to here
#plot(df.par$date.time, df.par$tot.par)
quantile(df.par$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile of tot par inc night
min(df.par$tot.par)

df2 = data.frame(tpar = df.par$tot.par, spar = df.par$par)
df2$ratio = df2$tpar/df2$spar
df2[sapply(df2, is.infinite)] <- NA
(1-mean(df2$ratio, na.rm = T))*100  #linear 9% higher, spline = 8.5% off


p1 = ggplot()+geom_point(df.par, mapping = aes(x = date.time-44146, y = tot.par),position = position_jitter(width = .01), alpha = 0.01,size = 3 )+theme_sleek1()
p1 = p1 + geom_point(df.par, mapping = aes(x = date.time-44146, y = par, col = 'red'),position = position_jitter(width = .01), alpha = 0.01,size = 3 )
p1 = p1 + scale_y_continuous( limits = c(0, 1500)) 
p1  #my intergration


p1 = ggplot()+geom_point(df.par, mapping = aes(x = date.time-44146, y = par),position = position_jitter(width = .01), alpha = 0.50,size = 3 )+theme_sleek1()
p1 = p1 + scale_y_continuous( limits = c(0, 1500)) 
p1   #slight difference using their integ


#More data cleaning
data3 = df.par[which(df.par[,10]<0),]  #remove all negative PAR
df.par = df.par %>% dplyr::anti_join(data3)  #subtracts one data.frame from another

df.par = dplyr::filter(df.par, tot.par > 1) #filter at 1 PAR

#also reset based on minimum values (see Light.calcs)



irrad1_long = irrad1 %>% pivot_longer(-vec.x,  names_to = "factors" ,values_to = "meas")  #keep vec.x, add all other columns to factors , add all their values to meas)

