###This R code conducts analysis on the data "rt.csv", which was collected with the R file getdata.R

rm(list=ls())

setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/country-year-database")
library(plyr)
library(ggplot2)
library(reshape2)
library("MASS")
library("xtable")
library(plm)

# load data
rt <- read.csv("rt.csv")
names(rt)
rt$X <- NULL

rt$pop.wdi <- as.integer(rt$pop.wdi)
rt$nyt.lagged <- as.integer(rt$nyt.lagged)

rt.all <- rt
rt <- subset(rt,!rt$country=="United States",)

############################################
##### Checking Region Counts to Varify #####
#############################################

### Counts

# number of articles per region-year
n.region.year <- ddply(.data=rt, .variables=.(year,region), .fun=summarize,"count"=sum(nyt))
n.region.year

# write to csv
casted <- dcast(data = n.region.year, formula = year ~ region, value.var = "count")
casted
write.csv(casted,"region_year_counts.csv")

# plot
ggplot(data=n.region.year, aes(x=year,y=count,group=region,color=region)) + geom_line()

#### means

# mean number of articles for obs. per region-year
n.region.year.mean <- ddply(.data=rt, .variables=.(year,region), .fun=summarize,"mean"=mean(nyt))
n.region.year.mean

# plot
ggplot(data=n.region.year.mean, aes(x=year,y=mean,group=region,color=region)) + geom_line()

# write csv
casted.mean <- dcast(data = n.region.year.mean, formula = year ~ region, value.var = "mean")
casted.mean
write.csv(casted.mean,"region_year_means.csv")

#################################################
##### Pre and Post 2001 Regression Analysis #####
#################################################


pre.2001 <- rt[rt$year<2002,]
post.2001 <- rt[rt$year>2002 & rt$year < 2011,]
names(rt)
set.seed(1234)

# PLM
pre.2001.plm <- plm.data(pre.2001, c("ccode","year"))
plm.pre <- plm(nyt ~ nyt.lagged+polity+autoc+physint+speech+new_empinx+log(gdp.pc.wdi)+pop.wdi+statedept+milper+cinc+domestic9+amnesty.uas,data = test,model = "within")
summary(plm.pre)

# GLM v. PLM
glm.pre<-glm(nyt ~ nyt.lagged+polity+autoc+physint+speech+new_empinx+log(gdp.pc.wdi)+pop.wdi+statedept+milper+cinc+domestic9+amnesty.uas,data = test, na.action=na.omit) 
summary(lm.pre)
fixef(plm.pre)
pFtest(plm.pre, lm.pre) 
summary(rt$rights)

# GLM
glm.pre<-glm.nb(nyt ~ nyt.lagged+polity+autoc+physint+speech+new_empinx+log(gdp.pc.wdi)+pop.wdi+statedept+milper+cinc+domestic9+amnesty.uas+(relevel(region,4)), data = pre.2001, na.action=na.omit)
summary(glm.pre)

glm.post <- glm.nb(nyt ~ nyt.lagged+polity+autoc+physint+speech+new_empinx+log(gdp.pc.wdi)+pop.wdi+statedept+milper+cinc+domestic9+amnesty.uas+(relevel(region,5)), data = post.2001, na.action=na.omit) 
summary(glm.post)

#### create xtable

glm.2.table <- xtable(summary(glm.2),caption="Determinants of Media Coverage, 2001–2010", align="ccccc")
print(glm.2.table)

glm.1.table <- xtable(summary(glm.1),caption="Determinants of Media Coverage, 1980–2001", align="ccccc")
print(glm.1.table)

names(rt)

##########################
##### Amnesty Analysis #####
##########################

glm.amnesty <- glm.nb(am.mentions ~ amnesty.uas, data = rt, na.action=na.omit) 
summary(glm.amnesty)
