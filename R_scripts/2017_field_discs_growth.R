#Field recruitment

library(reshape2)
library(ggplot2)
library(lme4)
library(RVAideMemoire) #GLMM overdispersion test

setwd("C:/Users/gerar/OneDrive/1 Work/3 Results/6 Post-settlement/3 2017/1 2017 field/3 analyses")
data1 <- read.table(file="field.polyps.17.txt", header= TRUE,dec=",") 
tail(data1)

####2)Organising and wrangling####
data1[complete.cases(data1), ]
colnames(data1)[5] <- 'counts'
str(data1)
data1$plug <- as.factor(as.character(data1$plug))
#data1$tot <- as.numeric(as.character(data1$tot))
data1$site <- as.factor(as.character(data1$site))
data1$counts <- as.integer(as.character(data1$counts))
data1$struc <- as.factor(as.character(data1$struc))
data1$aspect <- as.factor(as.character(data1$aspect))
#data1$surv = data1$set.t1/data1$set.t0*100
head(data1)
#levels(data1$site) <- c("Florence Bay 1", "Florence Bay 2", "Picnic Bay 1", 'Picnic Bay 2')
levels(data1$aspect)
data1$aspect <- factor(data1$aspect,levels = c("upwards", "vertical","downwards")) #Set levels in order
data1$obs <- factor(formatC(1:nrow(data1), flag="0", width = 3))# unique tank ID for later on

#######3)Data exploration####
##Visualize data - plot data split at every factor
library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek1")  #set theme in code
#ggplot(NULL)+ geom_point(data=data3, aes(x = data1.aspect, y = data1.surv))+ facet_wrap(~data1.site)  #by site
#ggplot(NULL)+geom_point(data = data3, aes(x = data1.aspect, y = data1.surv),position = position_jitter(width = .02),color = grey(0.3),size = 2) 

p0 = ggplot()+geom_point(data1, mapping = aes(x = aspect, y = counts),position = position_jitter(width = .02, height = .02), alpha = 0.50,size = 3 )+theme_sleek1()
p0 = p0 + facet_wrap(~site)#+scale_x_log10(name ="XXXX")#+geom_smooth(data1, mapping = aes(x = raw.x, y = suc/tot))
#p0= p0+ scale_y_continuous( limits = c(0, 1)) 
p0

####################
#####4) Model####
#library(lme4)
#md1 <- glmer(counts ~ aspect*site +(1|plug),family = poisson(),data = data1) #failed to converge
library(glmmTMB)
md1 <- glmmTMB(counts ~ aspect*site +(1|plug), family = 'nbinom2' ,data = data1)
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
eta <- mm %*% fixef(md1)$cond
df1$prediction  <- as.vector(exp(eta))
se    <- sqrt(diag(mm %*% vcov(md1)$cond %*% t(mm)))
df1$upper  <- as.vector(exp(eta + 1.96 *se))
df1$lower  <- as.vector(exp(eta - 1.96 *se))

#Categorical
library(ggplot2)
p1 = ggplot()+geom_point(data1, mapping = aes(x = aspect, y = counts),color = 'steelblue',alpha = 0.5, size = 3, position=position_jitter(width = .01))
p1 = p1 + geom_point(df1,  mapping = aes(x = aspect, y = prediction), color = 'grey60', size = 4)
# p1 = p1 + geom_line(df1,  mapping = aes(x = log.x, y = upper))
# p1 = p1 + geom_line(df1,  mapping = aes(x = log.x, y = lower))
p1 = p1 + geom_errorbar(df1, mapping = aes(x =aspect, ymin=lower, ymax=upper), width = 0, size = 1, color = 'grey60')
p1 = p1 + scale_x_discrete(name ="Surface Orientation", label = c("Upwards", 'Vertical', "Downwards")) 
p1 = p1 + scale_y_continuous(name ="Recruit growth (no. polyps)") #label = c("Downwards", "Upwards")) 
hum.names <- as_labeller(c(`fl` = "Florence Bay", `pb` = "Picnic Bay")) 
p1 = p1 +facet_wrap(site~. ,labeller=hum.names)
p1 = p1 +theme_sleek1()
#p1 = p1 +scale_x_log10(name ="dep sed")
p1

######################################
# data1[complete.cases(data1), ]
# data1$total.polyp <- as.numeric(as.character(data1$total.polyp))
# #data1$set.t1 <- as.numeric(as.character(data1$set.t1))
# #data1$total.polyp = data1$set.t1/data1$set.t0*100
# head(data1)
# # levels(data1$site) <- c("fl", "pb")
# # levels(data1$struc) <- c("1", "2")
# # levels(data1$aspect) <- c("upwards", "vertical", "downwards")
# data1$aspect <- factor(data1$aspect,levels = c("upwards", "vertical","downwards")) #Set levels in order
# 
# plot(data1$aspect, data1$total.polyp,  col = data1$site)
# plot(data1$total.polyp ~ data1$aspect + data1$site, type = 'l')
# 
# plot(aspect, total.polyp, data = data1, by = site)
# 
# qplot(aspect, total.polyp, data = data1, facet_grid(site), na.rm )
# 
# 
# ##########################################################################ignore below
# 
# 
# #Column statistics
# #Comside=aggregate(Settled ~ Replicate+Nominal, data = data10, sum) #Total settlement/prism
# #rm(mean)
# col.stat=aggregate(total.polyp ~ aspect + site, data = data1, mean) #Mean settlement/nomianl
# SDS=aggregate(total.polyp ~ aspect+site, data = data1, sd) #SD settlement/nomianl
# aggregate(total.polyp ~ site, data = data1, sum)  #total survived by site
# SES=(SDS$total.polyp)/sqrt(8) #Stand. error
# col.stat$SES=SES
# col.stat$SeUP = col.stat$total.polyp + SES
# col.stat$SeLO = col.stat$total.polyp - SES
# col.stat
# 
# data2 = data.frame(data1$site, data1$aspect, data1$total.polyp)
# data3 = melt(data2, id.vars = c("data1.aspect", 'data1.site'), value.name = "data1.total.polyp", na.rm=T)
# ggplot(NULL)+ geom_point(data=data3, aes(x = data1.aspect, y = data1.total.polyp))+ facet_wrap(~data1.site)
# 
# ggplot(NULL)+geom_point(data = data3, 
#            aes(x = data1.aspect, y = data1.total.polyp),
#            position = position_jitter(width = .02),
#            color = grey(0.3),
#            size = 2) 
# 
# #######################
# #rm(SeUp)
# p <- ggplot()
# 
# #mean data points
# p <- p + geom_point(data = col.stat, 
#                     aes(x = aspect, 
#                         y = total.polyp, 
#                         size = 6),    
#                     col = ("black")) 
# 
# p <- p + geom_errorbar(data = col.stat,
#                        aes(x = aspect, 
#                            ymax = SeUP, 
#                            ymin = SeLO), 
#                        width=0.2) #add errorbars
# 
# #p <- p + geom_point(data = data3, aes(x = data1.aspect, y = data1.total.polyp),position = position_jitter(width = .02),color = 'grey',size = 2)+facet_grid(. ~ site, scales = "fixed") #add data points
# 
# p <- p + xlab("Surface Orientation") + ylab("No. of polyps") #labels
# p <- p + theme(text = element_text(size=6)) #change theme size
# #data10$Nomorder <- factor(data10$Nomorder,
# #levels = c("None", "VL", "Low", "Med", "High","VH"))
# p <- p + facet_grid(. ~ site, 
#                     scales = "fixed") #seperate by pooled plot factor (here site)
# p=p+theme_bw()
# p <- p + theme(legend.position="none") #remove legend
# p
# 
# ############################################
# #Analysis
# 
# #Relationships
# tail(data1)
# boxplot(total.polyp ~ site, data = data1, xlab = "Site")
# boxplot(total.polyp ~ struc, data = data1, xlab = "data1untry")
# boxplot(total.polyp ~ aspect, data = data1, xlab = "Habitat")
# 
# 
# tail(data1)
# data1$obs <- factor(formatC(1:nrow(data1), flag="0", width = 3))# first need to make an observation row to soon randomise
# md1a <- glmer(total.polyp ~ site *  struc *aspect +(1|obs),family = poisson,data = data1) 
# md1b <- glmer(total.polyp ~ site +  struc +aspect +(1|obs),family = poisson,data = data1)
# md1c <- glmer(total.polyp ~ site +  struc + (1|obs),family = poisson,data = data1)  
# md1d <- glmer(total.polyp ~ site *  aspect + (1|obs),family = poisson,data = data1)  #d or e. is has more df but slightly higher AIC/BIC
# md1e <- glmer(total.polyp ~ site +  aspect + (1|obs),family = poisson,data = data1)  # e is simplier, but under-dispersed at 0.2 (i.e no error bars)
# md1f <- glmer(total.polyp ~ site +  aspect + (1|struc) + (1|obs),family = poisson,data = data1)  #singular fit
# md1g <- glmer(total.polyp ~ site  + (1|obs),family = poisson,data = data1) 
# 
# anova(md1a, md1b, md1c, md1d, md1e, md1g)  
# overdisp.glmer(md1d) #
# summary(md1e)
# library(MuMIn)
# r.squaredGLMM(md1e)
# #r-squares are low and best models are underdispesed. Will try neg binom
# 
# #GLM Neg. Binomial
# library(glmmTMB)
# mdnba <- glmmTMB(total.polyp ~ site *  struc *aspect + (1|obs),family="nbinom2", data = data1) 
# mdnbb <- glmmTMB(total.polyp ~ site +  struc +aspect +(1|obs),family="nbinom2",data = data1)
# mdnbc <- glmmTMB(total.polyp ~ site +  struc + (1|obs),family="nbinom2",data = data1)  
# mdnbd <- glmmTMB(total.polyp ~ site *  aspect + (1|obs),family="nbinom2",data = data1)  #
# mdnbe <- glmmTMB(total.polyp ~ site +  aspect + (1|obs),family="nbinom2",data = data1)  # best model by far, although a bit overdispersed
# mdnbf <- glmmTMB(total.polyp ~ site +  aspect + (1|struc) + (1|obs),family="nbinom2",data = data1)  
# mdnbg <- glmmTMB(total.polyp ~ site  + (1|obs),family="nbinom2",data = data1)
# 
# anova(mdnba, mdnbb, mdnbc, mdnbd, mdnbe, mdnbf, mdnbg)  
# summary(mdnbe)
# library(MuMIn)
# r.squaredGLMM(mdnbe)
# sum((resid(mdnbe, type = "pearson"))^2) / ((nrow(data1)) - (length(coef(mdnbe)) + 1))  #0.8 overdispersion =  different summary over disp
# 
# #CHecking for zero-inflation (i.e 20% zeros of more)
# hist(data1$total.polyp, breaks = 100)
# sum(data1$total.polyp == 0)  #Number of zeros
# 100 * sum(data1$total.polyp == 0) / nrow(data1)  #% of zeros is only 3 %
# 
# 
# # drop1(md1a, test = "Chi")
# # anova(glm(md1a, family=poisson, data=data1), test = "Chi")
# # anova(md1d, md1e, test = "Chi")
# 
# 
# 
# #####This is where I am up to....see code below!!!!!!!!!!
# 
# # boxplot(E0 ~ site,   
# #         ylab = "Normalized residuals",
# #         data = data1, xlab = "site")
# # abline(h = 0, lty = 2)   #look good at site
# # 
# # boxplot(E0 ~ struc,   
# #         ylab = "Normalized residuals",
# #         data = data1, xlab = "structure")
# # abline(h = 0, lty = 2)   #look good at struc
# # 
# # boxplot(E0 ~ aspect,   
# #         ylab = "Normalized residuals",
# #         data = data1, xlab = "aspect")
# # abline(h = 0, lty = 2)   #look good at aspect
# # 
# # library(DHARMa)
# # dat.sim <- simulateResiduals(md1b)
# # plot(md1b)   #might be issues - need to diagnose what this means
# # 
# # #Pearson residuals
# # dat.resid <- sum(resid(md1b, type = "pearson")^2)
# # 1 - pchisq(dat.resid, 89)   #89 is data1$df.resid   . #0.999 is very good fit, no sig patterns left in the residuals
# # 
# # 
# # 
# library(lsmeans)
# ls.posthoc.g = emmeans(mdnbe, pairwise ~  aspect+site)
# emmip(mdnbe, aspect ~ site)  #interpret as pretty much the same slope (slight difference on upwards)
# ls.posthoc.g.df = data.frame(ls.posthoc.g$emmeans)
# ls.posthoc.g.df$prediction  <- as.vector(exp(ls.posthoc.g.df$emmean))  #log back-transormed
# ls.posthoc.g.df$upper  <- exp(ls.posthoc.g.df$emmean + 1.96 *ls.posthoc.g.df$SE)
# ls.posthoc.g.df$lower  <- exp(ls.posthoc.g.df$emmean - 1.96 *ls.posthoc.g.df$SE) 
# ls.posthoc.g.df  #the difference here from the raw must be the random compnent?
# fl.df.g = subset(ls.posthoc.g.df, site == 'fl')
# fl.df.raw.g = subset(data1, site == 'fl')
# pb.df.g = subset(ls.posthoc.g.df, site == 'pb')
# pb.df.raw.g = subset(data1, site == 'pb')
# 
# 
# p3 =ggplot()
# #p3 = p3 + geom_line(data = df2, aes(x = vec.x, y = prediction, color = 'mean'))
# p3 = p3 + geom_point(data = fl.df.raw.g, aes(x = aspect, y = total.polyp, color = 'mean'))
# p3 = p3 + geom_errorbar(data = fl.df.g,aes(x = aspect, ymax = upper, ymin = lower), width=0.2) #add errorbars
# p3 = p3 + geom_point(data = fl.df.g, aes(x = aspect, y = prediction, color = 'mean', size = 6))
# #p3 = p3 +  geom_ribbon( data = df2,aes(x = vec.x, ymin=lower, ymax=upper), fill="steelblue", alpha=0.2)
# p3 = p3+ annotate(geom="text", x=1, y=150, label="r2 = XXXX",color="grey34", size = 5)
# p3 = p3+ scale_x_discrete(name ="Surface orientation") 
# p3 = p3+ scale_y_continuous(name ="Growth (total polyps/disc)") 
# p3 = p3 + scale_color_manual( name = "Wind speed", labels = c("mean", "mean", "bdb"), values = c('grey78','#BCBDDC','#BCBDDC' ))  #from brewerpal output purples
# p3 =  p3 + theme_sleek()
# p3 = p3 + theme(legend.position="none")
# p3
# 
# p4 =ggplot()
# #p4 = p4 + geom_line(data = df2, aes(x = vec.x, y = prediction, color = 'mean'))
# p4 = p4 + geom_point(data = pb.df.raw.g, aes(x = aspect, y = total.polyp, color = 'mean'))
# p4 = p4 + geom_errorbar(data = pb.df.g,aes(x = aspect, ymax = upper, ymin = lower), width=0.2) #add errorbars
# p4 = p4 + geom_point(data = pb.df.g, aes(x = aspect, y = prediction, color = 'mean', size = 6))
# #p4 = p4 +  geom_ribbon( data = df2,aes(x = vec.x, ymin=lower, ymax=upper), fill="steelblue", alpha=0.2)
# p4 = p4+ annotate(geom="text", x=1, y=70, label="r2 = XXXX",color="grey34", size = 5)
# p4 = p4+ scale_x_discrete(name ="Surface orientation") 
# p4 = p4+ scale_y_continuous(name ="Growth (total polyps/disc)") 
# p4 = p4 + scale_color_manual( name = "Wind speed", labels = c("mean", "mean", "bdb"), values = c('grey78','#BCBDDC','#BCBDDC' ))  #from brewerpal output purples
# p4 =  p4 + theme_sleek()
# p4 = p4 + theme(legend.position = c(0.7, 0.85))
# p4
# 
# 
# #All plots - load 
# source(file = "field discs surv 17.R")
# 
# # setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/6 Post-settlement/2017/2017 field/figures")
# # png("surv n growth panel.png", width =1000, height = 750)
# # library(cowplot)
# # plot_grid(p1, p2, p3, p4, labels = "AUTO", nrow = 2, align = 'v')
# # dev.off()
#  