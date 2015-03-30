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

n.region.year <- ddply(.data=rt, .variables=.(year,region), .fun=summarize,"count"=sum(nyt))
n.region.year

ggplot(data=n.region.year, aes(x=year,y=count,group=region,color=region)) + geom_line()

casted <- dcast(data = n.region.year, formula = year ~ region, value.var = "count")

write.csv(casted,"region_year_counts.csv")


#################################################
##### Pre and Post 2001 Regression Analysis #####
#################################################


pre.2001 <- rt[rt$year<2002,]
post.2001 <- rt[rt$year>2002 & rt$year < 2011,]
names(rt)
set.seed(1234)

# PLM
test <- plm.data(pre.2001, c("ccode","year"))
plm.1 <- plm(nyt ~ nyt.lagged+polity+amnesty.uas+gdp.pc.wdi+pop.wdi+cinc+domestic9+physint+speech+(relevel(region,4)),data = test,model = "within")
summary(plm.1)
fixef(plm.1)
pFtest(plm.1, lm.1) 

# LM
lm.1<-lm(nyt ~ nyt.lagged+polity+amnesty.uas+gdp.pc.wdi+pop.wdi+cinc+domestic9+physint+speech+(relevel(region,4)), data = pre.2001, na.action=na.omit) 
summary(lm.1)

# GLM
glm.2<-glm.nb(nyt ~ nyt.lagged+polity+amnesty.uas+gdp.pc.wdi+pop.wdi+cinc+domestic9+physint+speech+muslim+(relevel(region,5)), data = post.2001,na.action=na.omit) 
summary(glm.2)

glm.3 <- glm.nb(nyt ~ polity+democ+autoc+physint+speech+new_empinx+wecon+wopol+wosoc+elecsd+gdp.pc.wdi+pop.wdi+statedept+milper+cinc+bdeadbest+domestic9+amnesty.uas+(relevel(region,5)), data = post.2001, na.action=na.omit) 
summary(glm.3)

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
