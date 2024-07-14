#2018 field


#setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/6 Post-settlement/1 2018/2018 field/3 analysis")
data1 <- read.table(file="https://pastebin.com/raw/H8VCcFCV", header= TRUE,dec=",", na.strings=c("",".","NA"))
tail(data1, 100)
options(scipen = 999)  # turn off scientific notation

#write.csv(data1,'2018 field surv (not original).csv')

#2)Organising and wrangling
str(data1) #check data type is correct
#data1$raw.x <- as.numeric(as.character(data1$raw.x))
#data1$time <- as.factor(as.character(data1$time))
data1$disc <- as.factor(as.character(data1$disc))
data1$site.no <- as.numeric(as.character(data1$site.no))
data1$site.dist <- as.numeric(as.character(data1$site.dist))
#data1$site.dist <- log10(data1$site.dist)
data1$site <- as.factor(as.character(data1$site))
#data1$rep <- as.factor(as.character(data1$rep))
data1$struc <- as.factor(as.character(data1$struc))
data1$orient <- as.factor(as.character(data1$orient))
#data1$counts <- as.numeric(as.character(data1$counts))
data1$set <- as.integer(as.character(data1$set))
data1$suc = data1$pre.ble
data1$suc1 = data1$ble
data1$suc <- as.integer(as.character(data1$suc))
data1$suc1 <- as.integer(as.character(data1$suc1))
data1$t0 <- as.integer(as.character(data1$t0))
data1$tot = data1$t0
data1$prop <- data1$suc/data1$tot
data1$prop1 <- data1$suc1/data1$tot
data1$obs <- factor(formatC(1:nrow(data1), flag="0", width = 3))# unique tank ID for later on
nrow(data1)
str(data1)
table(data1$orient)
data1 = data1[complete.cases(data1), ]  #make sure import matches NA type
#data1$site.dist <- ifelse(data1$site.dist < 4.24, 0.424, data1$site.dist)  #Rreplace neg with zero

data1$suc1 <- ifelse(data1$suc1 > data1$tot, data1$tot, data1$suc1)  #Note: some bleached > t0. So I have temporarily 
#flattened these to tot. Need to investigate why. 
data1$orient <- factor(data1$orient,levels = c("t", "s","b")) #Set levels in order


#####Visulise data - plot data split at every factor#######
#3)Data exploration
hist(data1$tot)  #unbalanaced lowest grouping factor
# library(tidyr)
# #data2 = data1[-c(4:8)]
# #data2.long = gather(data2, class, prop, titan.p:bleached.p)
library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek1")
p0 = ggplot()+geom_point(data1, mapping = aes(x = site.dist, y = suc/tot),alpha = 0.20)+facet_wrap(~orient)#+scale_x_log10(name ="dep sed")
p0

p1 = ggplot()+geom_point(data1, mapping = aes(x = site.dist, y = prop1))+facet_wrap(~orient)#+scale_x_log10(name ="dep sed")
p1

#####fit model####
library(lme4)
# md1 <- glmer(cbind(suc,(tot - suc)) ~ site.dist*orient +(1|disc),family = binomial (link = logit),data = data1) 
# #md1 <- glmer(cbind(counts,(tot - counts)) ~ scale(raw.x)*time*spec +(1|obs),family = binomial (link = logit),data = data1)
# summary(md1)
library(glmmTMB)
md3 <- glmmTMB(cbind(suc,(tot - suc)) ~ site.dist*orient +(1|disc), family='binomial', data= data1)
summary(md3)
library(RVAideMemoire) #GLMM overdispersion test
#overdisp.glmer(md1) #Overdispersion for GLMM
summary(md3)
# md1 <- glmer(cbind(counts,(tot - counts)) ~ log.x*time*spec +(1|obs),family = binomial (link = logit),data = data1)
# md1 <- glmer(cbind(counts,(tot - counts)) ~ log.x*time*spec-1 +(1|obs),family = binomial (link = logit),data = data1)
AIC(md1)
library(emmeans)
#emmeans (md3,  ~ site.dist * orient) 
cont = contrast(emmeans(md3,  ~ site.dist * orient))

######Fitting and plotting#####
vec.x = seq(min(data1$site.dist), max(data1$site.dist), length = 100)
df1 <- expand.grid(site.dist  = vec.x, 
                   orient     = levels(data1$orient))
mm <- model.matrix(~site.dist*orient, df1)  # build model matrix
eta <- mm %*% fixef(md3)$cond
df1$prediction  <- as.vector(exp(eta) / (1 + exp(eta)))
se    <- sqrt(diag(mm %*% vcov(md3)$cond %*% t(mm)))
df1$upper  <- as.vector(exp(eta + 1.96 *se) /(1 + exp(eta  + 1.96 *se)))
df1$lower  <- as.vector(exp(eta - 1.96 *se) /(1 + exp(eta  - 1.96 *se)))

######Regression########
p0= ggplot()
p0= p0+ geom_point(data = data1, mapping = aes(x = site.dist, y = suc/tot, alpha = 0.8), color = 'steelblue', size = data1$tot/max(data1$tot)*3, position=position_jitter(width = .02))
#p0= p0+ geom_point(data = data1, aes(x = raw.x, y = prop), alpha = 0.9, size = 2)
p0= p0+ geom_line(data = df1, aes(x =  site.dist, y = prediction), color = 'grey30', size=1)
p0= p0+  geom_ribbon(data = df1, aes(x = site.dist, ymin=lower, ymax=upper,fill='grey'),  alpha=0.2)
#p0= p0+ scale_x_log10()
p0 = p0+ labs(x=expression(Site~distance~(km)),
              y=expression(Postsettlement~survival~(prop.)))
p0= p0+ scale_y_continuous( limits = c(0, 1)) 
p0= p0+ theme_sleek()
#mid = median(data1$raw.x)
#p0= p0+ scale_color_gradient(low="grey20",  high="grey80" ,trans = "log")
p0= p0+ scale_fill_manual( values = c("grey","khaki2"))
p0= p0+ theme(legend.position="none")
# p0 = p0  + theme(legend.position = c(0.9, 0.9))
orient.names <- c('b'="Downward",'s'="Vertical", 't' = 'Upward')
# p0= p0+ facet_wrap(~spec, nrow = 2, labeller  = as_labeller(spec.names))
p0= p0+ facet_wrap(~orient, nrow = 1, labeller  = as_labeller(orient.names))
panel.label = data.frame(c('a', 'b', 'c'))
#p0 = p0+ geom_text(panel.label, mapping = aes(x = -Inf, y = Inf, label = panel.label, group = panel.label),size = 5,inherit.aes = FALSE)
#p0 = p0 +geom_label(data = panel.label, aes(label=panel.label), x = Inf, y = -Inf, hjust=1, vjust=0,inherit.aes = FALSE)
p0


####analysis
sum(data1$tot)/sum(data1$set)  #number of recruits at t0
