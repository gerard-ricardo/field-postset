#Greg in field 17, survivors only

#1) Import data
setwd("C:/Users/g_ric/OneDrive/1 Work/3 Results/6 Post-settlement/3 2017/1 2017 field/3 analyses")
data1 <- read.table(file="field.greg17.txt", header= TRUE, dec=",")   #this appears to be only the survivors
head(data1)
nrow(data1)

#2)Organising and wrangling
str(data1)
data1$recruit.no <- as.numeric(as.character(data1$recruit.no)) 
data1$t0 <- as.integer(as.character(data1$t0)) 
data1$t1 <- as.integer(as.character(data1$t1)) #is aggregations at settlement
data1$struc <- as.factor(as.character(data1$struc)) 
data1$site <- as.factor(as.character(data1$site)) 
data1$aspect <- as.factor(as.character(data1$aspect)) 
head(data1)
data1$flat = data1$t1-data1$t0  #is increase
str(data1)
data1$aspect <- factor(data1$aspect,levels = c("upwards", "vertical","downwards")) #Set levels in order


#Visualize data - plot data split at every factor
#3)Data exploration
library(ggplot2)
p0 = ggplot()+geom_point(data1, mapping = aes(x = t0, y = flat),position = position_jitter(width = .02), alpha = 0.50,size = 2)+facet_wrap(~aspect+site)#+scale_x_log10(name ="dep sed")
p0

#4) Model
md1 <- glm(t1 ~ t0 *site *struc*aspect ,family="poisson", data=data1)

library(glmmTMB)
md1 <- glmmTMB(t1 ~ t0+site +aspect, family = 'nbinom2' ,data = data1)  #t0+site+aspect best (+ simple)
AIC(md1)
summary(md1)
#drop1(md1)
#plot(md1)
sum(residuals(md1, type = "deviance")^2)/md1$df.residual # >1.5, overdispersed

#################Fitting and plotting
#vec.x = seq(min(data1$t0), max(data1$t0), length = 100)
# df1 <- expand.grid(t0     = vec.x, 
#                    site     = levels(data1$site), 
#                    aspect = levels(data1$aspect))   #make sure names are the same as model
# mm <- model.matrix(~ t0 +site*aspect, df1)  # build model matrix
# eta <- mm %*% coef(M2nb)
# df1$prediction  <- as.vector(exp(eta))
# se    <- sqrt(diag(mm %*% vcov(M2nb) %*% t(mm)))
# df1$upper  <- as.vector(exp(eta + 1.96 *se) )
# df1$lower  <- as.vector(exp(eta - 1.96 *se))

vec.x = seq(min(data1$t0), max(data1$t0), length = 100)
df1 <- expand.grid(t0     = vec.x,
                   site     = levels(data1$site),
                   aspect = levels(data1$aspect))   #make sure names are the same as mode  #this creates a df with every treatment combination to predict. make sure names are the same as model
mm <- model.matrix(~t0+site +aspect, df1)  # build model matrix. Dummy codes each combination 
eta <- mm %*% fixef(md1)$cond
df1$prediction  <- as.vector(exp(eta))
se    <- sqrt(diag(mm %*% vcov(md1)$cond %*% t(mm)))
df1$upper  <- as.vector(exp(eta + 1.96 *se))
df1$lower  <- as.vector(exp(eta - 1.96 *se))

#############################
# library(ggplot2)
# p0 = ggplot()+geom_point(data1, mapping = aes(x = t0, y = t1),position = position_jitter(width = .02), alpha = 0.50,size = 2)+facet_wrap(~aspect+site)#+scale_x_log10(name ="dep sed")
# p0 = p0 + geom_line(df1,  mapping = aes(x = t0, y = prediction))
# # p0 = p0 + geom_line(df1,  mapping = aes(x = log.x, y = upper))
# # p0 = p0 + geom_line(df1,  mapping = aes(x = log.x, y = lower))
# p0 = p0 + geom_ribbon(df1, mapping = aes(x =t0, ymin=lower, ymax=upper))
# #p0 = p0 +facet_wrap(~time+spec)
# #p0 = p0 +scale_x_log10(name ="dep sed")
# p0

#Regression
library(ggplot2)
p0= ggplot()
p0= p0+ geom_point(data = data1, aes(x = t0, y = t1, alpha = 0.8), color = 'steelblue', size = 3, position=position_jitter(width = .01))
#p0= p0+ geom_point(data = data1, aes(x = raw.x, y = prop), alpha = 0.9, size = 2)
p0= p0+ geom_line(data = df1, aes(x =  t0, y = prediction), color = 'grey30', size=1)
p0= p0+  geom_ribbon(data = df1, aes(x = t0, ymin=lower, ymax=upper,fill='grey'),  alpha=0.2)
#p0= p0+ scale_x_log10()
p0 = p0+ labs(x=expression(Aggregate~(no.~of~settlers)),
              y=expression(Polyps/recruit~(no.)))
#p0= p0+ scale_y_continuous( limits = c(0, 1)) 
p0= p0+ theme_sleek1()
p0= p0+ scale_fill_manual( values = c("grey","khaki2"))
p0= p0+ theme(legend.position="none")
hum.names <- as_labeller(c(`fl` = "Florence Bay", `pb` = "Picnic Bay",`upwards` = "Upwards", `downwards` = "Downwards", `vertical` = "Vertical" )) 
p0 = p0 +facet_wrap(~aspect+site ,labeller=hum.names,  nrow = 3)
p0

#############################################################################################

#Greg, all recruits (inc dead?)

#1) Import data
setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/6 Post-settlement/3 2017/1 2017 field/1 discs/3 analysis")
data1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/greg17%20all", header= TRUE, dec=",")   #this appears to be only the survivors
nrow(data1)

#2)Organising and wrangling
str(data1)
data1$plug <- as.numeric(as.character(data1$recruit.no)) 
data1$t0 <- as.integer(as.character(data1$t0)) 
data1$t1 <- as.integer(as.character(data1$t1)) 
data1$surv <- as.integer(as.character(data1$surv)) 
data1$plug <- as.factor(as.character(data1$plug)) 
data1$site <- as.factor(as.character(data1$site))
data1$struc <- as.factor(as.character(data1$struc))
data1$aspect <- as.factor(as.character(data1$aspect))
head(data1)
data1$flat = data1$t1-data1$t0
str(data1)

#Visulise data - plot data split at every factor
#3)Data exploration
library(ggplot2)
p0 = ggplot()+geom_point(data1, mapping = aes(x = t0, y = surv),position = position_jitter(height = 0.05, width = 0.05), alpha = 0.50,size = 2)+facet_wrap(~aspect+site)#+scale_x_log10(name ="dep sed")
p0

M2p <- glm(surv ~ aspect ,family="binomial", data=data1)
summary(M2p)
drop1(M2p)
#plot(M2p)
sum(residuals(M2p, type = "deviance")^2)/M2p$df.residual # >1.5, overdispersed
library(MASS)
M2nb <- glm.nb(t1 ~ t0 +site*aspect, data=data1)
summary(M2nb)
#drop1(M2nb)
# M2lm <- lm(t1 ~ t0 +site+aspect, data=data1)
# summary(M2lm)
# 
# M2nb <- glm.nb(t1 ~ t0, data=data1)
# summary(M2nb)
# 
# library(car)
# Anova(M2nb)

#Fitting and plotting
vec.x = seq(min(data1$t0), max(data1$t0), length = 100)
df1 <- expand.grid(t0     = vec.x, 
                   site     = levels(data1$site), 
                   aspect = levels(data1$aspect))   #make sure names are the same as model
mm <- model.matrix(~ t0 +site*aspect, df1)  # build model matrix
eta <- mm %*% coef(M2nb)
df1$prediction  <- as.vector(exp(eta))
se    <- sqrt(diag(mm %*% vcov(M2nb) %*% t(mm)))
df1$upper  <- as.vector(exp(eta + 1.96 *se) )
df1$lower  <- as.vector(exp(eta - 1.96 *se))
library(ggplot2)
p0 = ggplot()+geom_point(data1, mapping = aes(x = t0, y = t1),position = position_jitter(width = .02), alpha = 0.50,size = 2)+facet_wrap(~aspect+site)#+scale_x_log10(name ="dep sed")
p0 = p0 + geom_line(df1,  mapping = aes(x = t0, y = prediction))
# p0 = p0 + geom_line(df1,  mapping = aes(x = log.x, y = upper))
# p0 = p0 + geom_line(df1,  mapping = aes(x = log.x, y = lower))
p0 = p0 + geom_ribbon(df1, mapping = aes(x =t0, ymin=lower, ymax=upper))
#p0 = p0 +facet_wrap(~time+spec)
#p0 = p0 +scale_x_log10(name ="dep sed")
p0


library(ggplot2)
p0= ggplot()
p0= p0+ geom_point(data = data1, aes(x = t0, y = t1), color = 'grey85',alpha = 0.8, size = 2, position=position_jitter(width = .01))
#p0= p0+ geom_point(data = data1, aes(x = t0, y = t1), alpha = 0.9, size = 2)
p0= p0+ geom_line(data = df1, aes(x =  t0, y = prediction), color = 'grey30', size=1)
p0= p0+  geom_ribbon(data = df1, aes(x = t0, ymin=lower, ymax=upper,fill='grey'),  alpha=0.2)
#p0= p0+ scale_x_log10()
p0 = p0+ labs(x=expression(Aggregate~(mg~"cm"^{-2})),
              y=expression(Polyp~number~(t1.)))
#p0= p0+ scale_y_continuous( limits = c(0, 1)) 
p0= p0+ theme_sleek()
#mid = median(data1$t0)
#p0= p0+ scale_color_gradient(low="grey20",  high="grey80" ,trans = "log")
p0= p0+ scale_fill_manual( values = c("grey","khaki2"))
p0= p0+ theme(legend.position="none")
p0= p0+ facet_wrap(~aspect+site, nrow = 2)
p0


library(emmeans)
lsmeans(M2nb, pairwise ~  t0+site) 

library(questionr)
odds.ratio(M2p)


library(MuMIn)
model.avg(get.models(dredge(M2p, na.action = "na.fail")))
