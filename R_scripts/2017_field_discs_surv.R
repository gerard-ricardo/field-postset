#Field recruitment

library(reshape2)
library(ggplot2)
library(lattice)
library(MASS) 
library(lme4) #GLM
library(RVAideMemoire) #GLMM overdispersion test

####import######
setwd("C:/Users/g_ric/OneDrive/1 Work/3 Results/6 Post-settlement/3 2017/1 2017 field/3 analyses")
data1 <- read.table(file="field.surv.17.txt", header= TRUE,dec=",") 
tail(data1)

####2)Organising and wrangling####
data1[complete.cases(data1), ]
colnames(data1)[2] <- 'tot'
colnames(data1)[6] <- 'suc'
str(data1)
data1$plug <- as.factor(as.character(data1$plug))
data1$tot <- as.numeric(as.character(data1$tot))
data1$site <- as.factor(as.character(data1$site))
data1$suc <- as.numeric(as.character(data1$suc))
data1$struc <- as.factor(as.character(data1$struc))
data1$aspect <- as.factor(as.character(data1$aspect))
#data1$surv = data1$set.t1/data1$set.t0*100
head(data1)
#levels(data1$site) <- c("Florence Bay 1", "Florence Bay 2", "Picnic Bay 1", 'Picnic Bay 2')
levels(data1$aspect)
data1$aspect <- factor(data1$aspect,levels = c("upwards", "vertical","downwards")) #Set levels in order
data1$obs <- factor(formatC(1:nrow(data1), flag="0", width = 3))# unique tank ID for later on
# plot(data1$aspect, data1$surv,  col = data1$site)
# plot(data1$surv ~ data1$aspect + data1$site, type = 'l')
# plot(aspect, surv, data = data1, by = site)
# qplot(aspect, surv, data = data1, facet_grid(site), na.rm )

#Column statistics
#Comside=aggregate(Settled ~ Replicate+Nominal, data = data10, sum) #Total settlement/prism
#rm(mean)
# col.stat=aggregate(surv ~ aspect + site, data = data1, mean) #Mean settlement/nomianl
# SDS=aggregate(surv ~ aspect+site, data = data1, sd) #SD settlement/nomianl
# (aggregate(set.t1 ~ site, data = data1, sum)) / (aggregate(set.t0 ~ site, data = data1, sum))  #total survived by site
# SES=(SDS$surv)/sqrt(8) #Stand. error
# col.stat$SES=SES
# col.stat$SeUP = col.stat$surv + SES
# col.stat$SeLO = col.stat$surv - SES
# col.stat
# 
# data2 = data.frame(data1$site, data1$aspect, data1$surv)
# data3 = melt(data2, id.vars = c("data1.aspect", 'data1.site'), value.name = "data1.surv", na.rm=T)
# colnames(data3) <- c("aspect", "site", 'variable', 'surv')

#######3)Data exploration####
##Visualize data - plot data split at every factor
library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek1")  #set theme in code
ggplot(NULL)+ geom_point(data=data3, aes(x = data1.aspect, y = data1.surv))+ facet_wrap(~data1.site)  #by site

ggplot(NULL)+geom_point(data = data3, aes(x = data1.aspect, y = data1.surv),position = position_jitter(width = .02),color = grey(0.3),size = 2) 

p0 = ggplot()+geom_point(data1, mapping = aes(x = aspect, y = suc/tot),position = position_jitter(width = .02, height = .02), alpha = 0.50,size = 3 )+theme_sleek1()
p0 = p0 + facet_wrap(~site)#+scale_x_log10(name ="XXXX")#+geom_smooth(data1, mapping = aes(x = raw.x, y = suc/tot))
#p0= p0+ scale_y_continuous( limits = c(0, 1)) 
p0

####################
#####4) Model####
library(lme4)
md1 <- glmer(cbind(suc,(tot - suc)) ~ aspect*site +(1|obs),family = binomial (link = logit),data = data1) 
summary(md1)

plot(md1)
sum(resid(md1, type = "pearson")^2) / (nrow(data1) - length(coef(md1))) #(overdispsrion glm Zuur)
library(RVAideMemoire) #GLMM overdispersion test
overdisp.glmer(md1) #Overdispersion for GLMM
library("performance")
#check_overdispersion(md1)  #only for count data
plot(fitted(md1), resid(md1))  #fitted vs residuals
abline(h = 0)
check_singularity(md1)
AIC(md1) #if the better model is 2 or less, use the simpler model
MuMIn::AICc(md1)
r2(md1)
icc(md1)  #Intraclass Correlation Coefficient

####6) Fitting and predicting###
df1 <- expand.grid(aspect  = levels(data1$aspect), 
                   site     = levels(data1$site))   #this creates a df with every treatment combination to predict. make sure names are the same as model
mm <- model.matrix(~aspect*site, df1)  # build model matrix. Dummy codes each combination 
eta <- mm %*% fixef(md1)
df1$prediction  <- as.vector(exp(eta) / (1 + exp(eta)))
se    <- sqrt(diag(mm %*% vcov(md1) %*% t(mm)))
df1$upper  <- as.vector(exp(eta + 1.96 *se) /(1 + exp(eta  + 1.96 *se)))
df1$lower  <- as.vector(exp(eta - 1.96 *se) /(1 + exp(eta  - 1.96 *se)))

#######################
# #rm(SeUp)
# p1 <- ggplot()
# #mean data points
# p1 <- p1 + geom_point(data = col.stat, aes(x = aspect, y = surv, size = 6),    col = ("black")) 
# p1 <- p1 + geom_errorbar(data = col.stat,aes(x = aspect, ymax = SeUP, ymin = SeLO), width=0.2) #add errorbars
# #p1 <- p1 + geom_point(data = data3, aes(x = data1.aspect, y = data1.surv),position = position_jitter(width = .02),color = 'grey',size = 2)+facet_grid(. ~ site, scales = "fixed") #add data points
# p1 <- p1 + xlab("Surface Orientation") + ylab("Recruit survival (%)") #labels
# #p1 <- p1 + theme(text = element_text(size=6)) #change theme size
# #data10$Nomorder <- factor(data10$Nomorder,
# #levels = c("None", "VL", "Low", "Med", "High","VH"))
# p1 <- p1 + facet_grid(. ~ site, scales = "fixed") #seperate by pooled plot factor (here site)
# p1 <- p1 + theme(legend.position="none") #remove legend
# p1

#Categorical
library(ggplot2)
p0 = ggplot()+geom_point(data1, mapping = aes(x = aspect, y = suc/tot),color = 'steelblue',alpha = 0.5, size = data1$tot/max(data1$tot)*4, position=position_jitter(width = .01))
p0 = p0 + geom_point(df1,  mapping = aes(x = aspect, y = prediction), color = 'grey60', size = 4)
# p0 = p0 + geom_line(df1,  mapping = aes(x = log.x, y = upper))
# p0 = p0 + geom_line(df1,  mapping = aes(x = log.x, y = lower))
p0 = p0 + geom_errorbar(df1, mapping = aes(x =aspect, ymin=lower, ymax=upper), width = 0, size = 1, color = 'grey60')
p0 = p0 + scale_x_discrete(name ="Surface Orientation", label = c("Upwards", 'Vertical', "Downwards")) 
p0 = p0 + scale_y_continuous(name ="Recruit survival (prop.)") #label = c("Downwards", "Upwards")) 
hum.names <- as_labeller(c(`fl` = "Florence Bay", `pb` = "Picnic Bay")) 
p0 = p0 +facet_wrap(site~. ,labeller=hum.names)
p0 = p0 +theme_sleek1()
#p0 = p0 +scale_x_log10(name ="dep sed")
p0

#####panel##########
library(gridExtra)
grid.arrange(arrangeGrob(p0, p1, ncol = 1))

# ############################################
# #Data exploration
# 
# #Relationships
# boxplot(surv ~ site, data = data1, xlab = "Site")
# boxplot(surv ~ struc, data = data1, xlab = "data1untry")
# boxplot(surv ~ aspect, data = data1, xlab = "Habitat")
# 
# tail(data1)
# data1$obs <- factor(formatC(1:nrow(data1), flag="0", width = 3))# first need to make an observation row to soon randomise
# md1a <- glmer(cbind(set.t1,(set.t0 - set.t1)) ~ site *  struc + (1|obs),family = binomial (link = logit),data = data1) 
# md1b <- glmer(cbind(set.t1,(set.t0 - set.t1)) ~ site *  aspect + (1|obs),family = binomial (link = logit), data = data1) #b is the best
# md1c <- glmer(cbind(set.t1,(set.t0 - set.t1)) ~ site +  aspect + (1|obs),family = binomial (link = logit),nAGQ=0, data = data1) #nAGQ helped
# md1d <- glmer(cbind(set.t1,(set.t0 - set.t1)) ~ site *  aspect * struc+(1|obs),family = binomial (link = logit),nAGQ=0,  data = data1)
# anova(md1a, md1b, md1c, md1d)  #b is best
# overdisp.glmer(md1b) #0.655, oversipesion is okay
# summary(md1b)
# 
# boxplot(E0 ~ site,   ylab = "Normalized residuals",data = data1, xlab = "site")
# abline(h = 0, lty = 2)   #look good at site
# 
# boxplot(E0 ~ struc,   ylab = "Normalized residuals",data = data1, xlab = "structure")
# abline(h = 0, lty = 2)   #look good at struc
# 
# boxplot(E0 ~ aspect,   ylab = "Normalized residuals",data = data1, xlab = "aspect")
# abline(h = 0, lty = 2)   #look good at aspect
# 
# library(DHARMa)
# dat.sim <- simulateResiduals(md1b)
# plot(md1b)   #might be issues - need to diagnose what this means
# 
# #Pearson residuals
# summary(md1b)
# dat.resid <- sum(resid(md1b, type = "pearson")^2)
# 1 - pchisq(dat.resid, 89)   #89 is data1$df.resid   . #0.999 is very good fit, no sig patterns left in the residuals
# library(MuMIn)
# r.squaredGLMM(md1b)  #the r^2 sucks
# 
# library(lsmeans)
# ls.posthoc = lsmeans(md1b, pairwise ~  aspect | site)
# ls.posthoc.df = data.frame(ls.posthoc$lsmeans)
# ls.posthoc.df$prediction  <- as.vector(exp(ls.posthoc.df$lsmean) / (1 + exp(ls.posthoc.df$lsmean)))*100
# ls.posthoc.df$upper  <- exp(ls.posthoc.df$lsmean + 1.96 *ls.posthoc.df$SE) /(1 + exp(ls.posthoc.df$lsmean  + 1.96 *ls.posthoc.df$SE))*100
# ls.posthoc.df$lower  <- exp(ls.posthoc.df$lsmean - 1.96 *ls.posthoc.df$SE) /(1 + exp(ls.posthoc.df$lsmean  - 1.96 *ls.posthoc.df$SE))*100
# fl.df = subset(ls.posthoc.df, site == 'fl')
# fl.df.raw = subset(data1, site == 'fl')
# pb.df = subset(ls.posthoc.df, site == 'pb')
# pb.df.raw = subset(data1, site == 'pb')
# 
# p1 =ggplot()
# #p1 = p1 + geom_line(data = df2, aes(x = vec.x, y = prediction, color = 'mean'))
# p1 = p1 + geom_point(data = fl.df.raw, aes(x = aspect, y = surv, color = 'mean'))
# p1 <- p1 + geom_errorbar(data = fl.df,aes(x = aspect, ymax = upper, ymin = lower), width=0.2) #add errorbars
# p1 = p1 + geom_point(data = fl.df, aes(x = aspect, y = prediction, color = 'mean', size = 6))
# #p1 = p1 +  geom_ribbon( data = df2,aes(x = vec.x, ymin=lower, ymax=upper), fill="steelblue", alpha=0.2)
# p1 = p1+ annotate(geom="text", x=1, y=100, label="r2 = XXXX",color="grey34", size = 5)
# p1 = p1+ scale_x_discrete(name ="Surface orientation") 
# p1 = p1+ scale_y_continuous(name ="Survivorship (%)") 
# p1 = p1 + scale_color_manual( name = "Wind speed", labels = c("mean", "mean", "bdb"), values = c('grey78','#BCBDDC','#BCBDDC' ))  #from brewerpal output purples
# p1 =  p1 + theme_sleek()
# p1 = p1 + theme(legend.position="none")
# p1
# 
# p2 =ggplot()
# p2 = p2 + geom_point(data = pb.df.raw, aes(x = aspect, y = surv, color = 'mean'))
# #p2 = p2 + geom_line(data = df2, aes(x = vec.x, y = prediction, color = 'mean'))
# p2 <- p2 + geom_errorbar(data = pb.df,aes(x = aspect, ymax = upper, ymin = lower), width=0.2) #add errorbars
# p2 = p2 + geom_point(data = pb.df, aes(x = aspect, y = prediction, color = 'mean', size = 6))
# #p2 = p2 +  geom_ribbon( data = df2,aes(x = vec.x, ymin=lower, ymax=upper), fill="steelblue", alpha=0.2)
# p2 = p2+ annotate(geom="text", x=1, y=70, label="r2 = XXXX",color="grey34", size = 5)
# p2 = p2+ scale_x_discrete(name ="Surface orientation") 
# p2 = p2+ scale_y_continuous(name ="Survivorship (%)") 
# p2 = p2 + scale_color_manual( name = "Wind speed", labels = c("mean", "mean", "bdb"), values = c('grey78','#BCBDDC','#BCBDDC' ))  #from brewerpal output purples
# p2 =  p2 + theme_sleek()
# p2 = p2 + theme(legend.position="none")
# p2
# 
# #See growth for all plots
# #plot_grid(p1, p2, labels = "AUTO", nrow = 1, align = 'v')
# 
# 
# 
