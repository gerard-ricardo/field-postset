#ms8 test


##1) Import data
#setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/5 Settlement/2 2018/4 light intensity_spectra")
#irrad1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2020%20ms9%20test", header= TRUE,dec=",", na.strings=c("",".","NA"))
irrad1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2017%20flo%20deep%20simp2", header= TRUE,dec=",", na.strings=c("",".","NA", " "))  #2017 flo simp

head(irrad1)
options(scipen = 999)  # turn off scientific notation


####2)Organising and wrangling####
str(irrad1) #check data type is correct
irrad1 = dplyr::select(irrad1,c(date, time, date.time, X425nm :X695nm))  #remove column. Make sure have package on front 
irrad1[] <- lapply(irrad1, function(x) as.numeric(as.character(x)))   #convert all to numeric
str(irrad1) #check data type is correct
nrow(irrad1)
irrad1 = irrad1[complete.cases(irrad1), ] #complete cases
irrad1$date.fac <- as.factor(irrad1$date)
head(irrad1)
#colnames(irrad1)[4:11] = c('425','455','485','515','555','615','660','695')  #add channels

#filter for dates deployed
#irrad1 = dplyr::filter(irrad1, date.time > 44140)
#irrad1 = dplyr::filter(irrad1, date.time < 44142)

library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek")
theme_set(theme_sleek())
p0 = ggplot()+geom_point(irrad1, mapping = aes(x = date.time, y = X425nm),position = position_jitter(width = .02), alpha = 0.50,size = 3 )+theme_sleek()
#p0= p0+ scale_y_continuous( limits = c(0, 1)) 
p0


irrad1[1, 4:11]



#Convert to PAR/nm
#PAR = Irradiance * nm / con.fac    # Note: This website uses 11961.72 https://www.berthold-bio.com/service-support/support-portal/knowledge-base/how-do-i-convert-irradiance-into-photon-flux.html
con.fac = 11973.78
nm = c(425, 455, 485, 515, 555, 615, 660, 695)
df.par = irrad1[4:11] * nm / con.fac
nrow(df.par)

#single auc
MESS::auc(nm, df.par[1,], type = 'linear', from = 400, to = 700, rule = 2)  #linear = 8.8, spline = 9.14
plot(nm, df.par[1,])
lines(spline(nm, df.par[1,]))

#using trap
plot(nm2, df.par[1,])
lines(approx(nm2, df.par[1,], xout=400:700, rule=2))  #some extrapolation
df = approx(nm2, df.par[1,], xout=400:700, rule=2)
sum(df$y)  #9.19634  but extrapolated
caTools::trapz(400:700, micromolespersec_ipol$y)   #11.98

# #calc total PAR
# library(MESS)
# test1 = auc(nm, df.par[1,], type = 'linear') #or use linear ses stats:splinefun and stats:integrate
# test1
# plot(nm, df.par[1,])
# lines(spline(nm, df.par[1,], method = 'fmm'))

#all
tot.par = vector("numeric", nrow(df.par))  #placeholder
for(i in 1:nrow(df.par)) { 
  bob <- auc(nm, df.par[i,], type = 'linear')   
  tot.par[i] <- bob
} # 
df.par$tot.par = tot.par  #add to main df
df.par$date.time = irrad1$date.time
df.par$time = irrad1$time
head(df.par)  #all good to here
#plot(df.par$date.time, df.par$tot.par)
quantile(df.par$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile of tot par inc night
min(df.par$tot.par)

#More data cleaning
data3 = df.par[which(df.par[,10]<0),]  #remove all negative PAR
df.par = df.par %>% dplyr::anti_join(data3)  #subtracts one data.frame from another

df.par = dplyr::filter(df.par, tot.par > 1) #filter at 1 PAR

#also reset based on minimum values (see Light.calcs)







irrad1_long = irrad1 %>% pivot_longer(-vec.x,  names_to = "factors" ,values_to = "meas")  #keep vec.x, add all other columns to factors , add all their values to meas)
