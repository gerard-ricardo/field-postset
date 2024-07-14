
setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/6 Post-settlement/1 2018/2018 field/3 analysis")
data1 <- read.table(file="https://pastebin.com/raw/NE5VQCM1", header= TRUE,dec=",", na.strings=c("",".","NA"))
data2 <- read.table(file="https://pastebin.com/raw/yc0Duhs3", header= TRUE,dec=",", na.strings=c("",".","NA"))
head(data1)
options(scipen = 999)  # turn off scientific notation

#2)Organising and wrangling
str(data1) #check data type is correct
#data1$raw.x <- as.numeric(as.character(data1$raw.x))
data1$struc <- as.factor(as.character(data1$struc))
str(data2)
data1$site = factor(data1$site,levels = c("Fb", "Ge", "Pb", "Mi", "VS")) #Set levels in order
# data1$log.x <- log10(data1$raw.x)
# data1$time <- as.factor(as.character(data1$time))
# data1$tank <- as.factor(as.character(data1$tank))
# data1$rep <- as.factor(as.character(data1$rep))
data1$site.dist <- as.numeric(as.character(data1$site.dist))
# data1$suc <- as.integer(as.character(data1$suc))
# data1$tot <- as.integer(as.character(data1$tot))
# data1$prop <- data1$counts/data1$tot
data1$obs <- factor(formatC(1:nrow(data1), flag="0", width = 3))# unique tank ID for later on
nrow(data1)
str(data1)

#3)Visulise data - plot data split at every factor
#3)Data exploration
data3 = data.frame(data1, data2)
str(data3)
data3.long = gather(data3, class, count, Macro.algae:detritus)
library(ggplot2)
p0 = ggplot()+geom_point(data3.long, mapping = aes(x = site.dist, y = count))+facet_wrap(~orient+class)
p0


# #4a) Cluster analysis model
# library(vegan)
# md1 = adonis(data2 ~ site*orient,method="bray",  data=data1, permutations=9999)  #do I need arcsine transomration?
# print(md1)  #need pair-wise
# dis <- vegdist(data2)  ## Bray-Curtis distances between samples
# mod <- betadisper(dis, data1$site)  ## Calculate multivariate dispersions
# mod
# #NMDS
# mod1 <- metaMDS(data2, distance = "bray", trace = FALSE, autotransform = FALSE)  #if you have exteme values, turn transformation on
# mod1  #A rule of thumb: stress < 0.05 provides an excellent representation in reduced dimensions, < 0.1 is great, <0.2 is good/ok, and stress < 0.3 provides a poor representation. 
# plot(mod1)
# data.scores <- as.data.frame(scores(mod1))  
# data.scores$site <- rownames(data.scores)  
# data.scores$Site<-data1$site
# ggplot(data=data.scores) + stat_ellipse(aes(x=NMDS1,y=NMDS2,colour=Site),level = 0.50) +
#   geom_point(aes(x=NMDS1,y=NMDS2,shape=Site,colour=Site),size=4) +theme_sleek()


#4b) categorical
library(DirichletReg)
data4 = data1  #subset the predicitors
data4$props = DR_data(data2)   #group the proportions and add back to dataframe
mod0 <- DirichReg(props ~ 1, data4)
mod1 <- DirichReg(props ~ site, data4)
mod2 <- DirichReg(props ~ site+orient, data4)
mod3 <- DirichReg(props ~ site*orient, data4)
anova(mod0, mod1, mod2, mod3)  #most sig best. Interestingly now int is sig. (maybe because of)
AIC( mod3)  #
summary(mod3)

#Fitting and plotting
#vec.x = seq(0.01, 300, 10)
exp.df2 <- expand.grid(site  = levels(data4$site), 
                       orient     = levels(data4$orient))
ff1 = as.data.frame(predict(mod3, newdata = exp.df2))
colnames(ff1) = colnames(data2)
df1 = bind_cols(exp.df2, ff1)  #cobine two dataframes to make wide prediction df
#colnames(df1) <- c("raw.x", "time","titan.p", 'peys.p', 'dead.p', 'bleached.p')
library(tidyr)
df1.long = gather(df1, class, prop, Macro.algae:detritus)
library(ggplot2)
p0 = ggplot(data3.long, mapping = aes(x = site, y = count/100))+geom_point(data3.long, mapping = aes(x = site, y = count/100))
p0 = p0 + geom_point(df1.long,  mapping = aes(x = site, y = prop, col = 'red'))
p0 = p0 +facet_wrap(~orient+class)
#p0 = p0 +scale_x_log10(name ="dep sed")
p0

#4b) continous
library(DirichletReg)
data4 = data1  #subset the predicitors
data4$props = DR_data(data2)   #group the proportions and add back to dataframe
mod0 <- DirichReg(props ~ 1, data4)
mod1 <- DirichReg(props ~ site.dist, data4)
mod2 <- DirichReg(props ~ site.dist+orient, data4)
mod3 <- DirichReg(props ~ site.dist*orient, data4)
anova(mod0, mod1, mod2, mod3)  #most sig best. Additive model is best
AIC( mod0)
AIC( mod1)
AIC( mod2)
AIC( mod3)# Mod1 most parsimonious 
summary(mod2)


#Fitting and plotting
vec.x = seq(min(data4$site.dist), max(data3$site.dist), 0.1)
exp.df2 <- expand.grid(site.dist  = vec.x, 
                       orient     = levels(data4$orient))
ff1 = as.data.frame(predict(mod2, newdata = exp.df2))
colnames(ff1) = colnames(data2)
df1 = bind_cols(exp.df2, ff1)  #cobine two dataframes to make wide prediction df
#colnames(df1) <- c("raw.x", "time","titan.p", 'peys.p', 'dead.p', 'bleached.p')
library(tidyr)
df1.long = gather(df1, class, prop, Macro.algae:detritus)
library(ggplot2)
p0 = ggplot(data3.long, mapping = aes(x = site.dist, y = count/100))+geom_point(data3.long, mapping = aes(x = site.dist, y = count/100))
p0 = p0 + geom_line(df1.long,  mapping = aes(x = site.dist, y = prop, col = 'red'))
p0 = p0 +facet_wrap(~orient+class)
#p0 = p0 +scale_x_log10(name ="dep sed")
p0







# Vegan default example
# library(vegan)
# data(dune)
# data(dune.env)
# adonis(dune ~ Management*A1, data=dune.env, permutations=99)
# 
# ### Example of use with strata, for nested (e.g., block) designs.
# 
# dat <- expand.grid(rep=gl(2,1), NO3=factor(c(0,10)),field=gl(3,1) )
# dat
# Agropyron <- with(dat, as.numeric(field) + as.numeric(NO3)+2) +rnorm(12)/2
# Schizachyrium <- with(dat, as.numeric(field) - as.numeric(NO3)+2) +rnorm(12)/2
# total <- Agropyron + Schizachyrium
# library(lattice)
# dotplot(total ~ NO3, dat, jitter.x=TRUE, groups=field,
#         type=c('p','a'), xlab="NO3", auto.key=list(columns=3, lines=TRUE) )
# 
# Y <- data.frame(Agropyron, Schizachyrium)
# mod <- metaMDS(Y)
# plot(mod)
# ### Hulls show treatment
# ordihull(mod, group=dat$NO3, show="0")
# ordihull(mod, group=dat$NO3, show="10", col=3)
# ### Spider shows fields
# ordispider(mod, group=dat$field, lty=3, col="red")
# 
# ### Correct hypothesis test (with strata)
# adonis(Y ~ NO3, data=dat, strata=dat$field, perm=1e3)
# 
# ### Incorrect (no strata)
# adonis(Y ~ NO3, data=dat, perm=1e3)
