
##1) Import data
#setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/5 Settlement/2 2018/4 light intensity_spectra")
irrad1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2020%20ms9%20neal%20test", header= TRUE,dec=",", na.strings=c("",".","NA"))
head(irrad1)
options(scipen = 999)  # turn off scientific notation

####2)Organising and wrangling####
str(irrad1) #check data type is correct
irrad2 = dplyr::select(irrad1,c( date.time, ch.1:ch.9))  #remove column. Make sure have package on front 
irrad2[] <- lapply(irrad2, function(x) as.numeric(as.character(x)))   #convert all to numeric
str(irrad2) #check data type is correct
nrow(irrad2)
irrad2 = irrad2[complete.cases(irrad2), ] #complete cases
irrad2$date.fac <- as.factor(irrad2$date)
head(irrad2)
names = round(c(364.0,385.7,440.1,490.6,550.5,589.8,635.5,659.7,700.2)) %>% as.character()  #add channels
  names2 <- paste('x',names, sep='' )  #joining columns. Use sep=''  to remove space
colnames(irrad2)[2:10] <- names2

#filter for dates deployed
# irrad1 = dplyr::filter(irrad1, date.time > 44140)
# irrad1 = dplyr::filter(irrad1, date.time < 44142)

library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek")
theme_set(theme_sleek())
p1 = ggplot() + geom_point(data = irrad2, aes(x = date.time, y = x364))+theme_sleek()
p1

#######Convert to PAR/nm################
#PAR = Irradiance * nm / con.fac    # Note: This website uses 11961.72 https://www.berthold-bio.com/service-support/support-portal/knowledge-base/how-do-i-convert-irradiance-into-photon-flux.html
con.fac = 11973.78
nm = c(364.0,385.7,440.1,490.6,550.5,589.8,635.5,659.7,700.2)
df.par = irrad2[2:10] * nm / con.fac
nrow(df.par)
correction.factor = 1.05  #approx fudge factor for ms9 from Matt's talk

#calc total PAR for one row using auc
library(MESS)
auc(nm, df.par[1,], type = 'linear', from = 400, to = 700, rule = 2) #or use linear ses stats:splinefun and stats:integrate
plot(nm, df.par[1,])
lines(spline(nm, df.par[1,], method = 'fmm'))


#calc total PAR for one row using trap
plot(nm, df.par[1,])
lines(approx(nm, df.par[1,],  xout=400:700, rule=2))  #some extrapolation
micromolespersec_ipol = approx(nm, df.par[1,],  xout=400:700, rule=2)
caTools::trapz(400:700, micromolespersec_ipol$y)


#all
tot.par = vector("numeric", nrow(df.par))  #placeholder
for(i in 1:nrow(df.par)) { 
  bob <- MESS::auc(nm, df.par[i,], type = 'linear', from = 400, to = 700, rule = 2)   
  tot.par[i] <- bob
} # 
df.par$tot.par = tot.par*correction.factor  #add to main df
df.par$date.time = irrad1$date.time
df.par$time = irrad1$time
df.par$par.mayb = irrad1$par.maybe %>% as.numeric(as.character())

plot(df.par$par.mayb/df.par$tot.par)
lines(0:40,rep(1,41), col = 'red')

(1 - (mean(df.par$par.mayb)/mean(df.par$tot.par)))*100

head(df.par)  #all good to here
#plot(df.par$date.time, df.par$tot.par)
quantile(df.par$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile of tot par inc night
min(df.par$tot.par)

# #More data cleaning
# data3 = df.par[which(df.par[,10]<0),]  #remove all negative PAR
# df.par = df.par %>% dplyr::anti_join(data3)  #subtracts one data.frame from another
# 
# df.par = dplyr::filter(df.par, tot.par > 1) #filter at 1 PAR
# 
# #also reset based on minimum values (see Light.calcs)
# 






irrad1_long = irrad1 %>% pivot_longer(-vec.x,  names_to = "factors" ,values_to = "meas")  #keep vec.x, add all other columns to factors , add all their values to meas)
