###This R code conducts analysis on the data "rt.csv", which was collected with the R file getdata.R

rm(list=ls())

setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/country-year-database")

rt <- read.csv("rt.csv")
rt$X <- NULL
rt$X.1 <- NULL

rt$pop.wdi <- as.integer(rt$pop.wdi)
rt$nyt.lagged <- as.integer(rt$nyt.lagged)

library("MASS")
library("xtable")

############################################
##### Checking Region Counts to Varify #####
#############################################

# define function
region.per.year <- function(x,y){
  subset.data <- subset(rt,region==x & year==y)
  return(as.integer(sum(subset.data$nyt,na.rm=TRUE)))
}


# create dataframe
regions <- unique(rt$region[!is.na(rt$region)])
regions
number.news <- data.frame(regions)

# fill in cells

start <-1980
end <- 2010
for(i in start:end){
  number.news <- cbind(number.news,unlist(lapply(regions,region.per.year,y=i)))
}
names(number.news) <- c("regions",start:end)

write.csv(number.news,"region_year_counts.csv")

# Plot change in number over time

rownames(number.news) <- number.news$regions
number.news$regions
number.news <- number.news[, !(colnames(number.news) %in% c("regions"))]
rownames(number.news)
x <- seq(1980,2010)
m <- number.news[2,]
l <- number.news[4,]
c <- number.news[1,]
a <- number.news[6,]
w <- number.news[5,]
f <- number.news[3,]

plot(x,m,
     xlab="year",
     ylab="number of articles in NYT", # Change this for your data
     main="Human Rights Violations Articles Over Time",
     type="l",
     col="red"
)
lines(x, l, type="l",col="green" )
lines(x, c, type="l",col="yellow" )
lines(x, a, type="l",col="blue" )
lines(x, w, type="l",col="orange" )
lines(x, f, type="l",col="purple" )

legend("topleft", c("Middle East", "Latin America", "Former Soviet Union","Asia","West","Africa"), col = c("red", "green","yellow","blue","orange","purple"),
       text.col = "black", lty = 1,
       merge = TRUE, bg = "gray90")

##########################
##### Summary states #####
##########################

mean(post.2001$nyt[post.2001$region=="MENA"],na.rm=TRUE)
mean(post.2001$nyt[post.2001$region=="Asia"],na.rm=TRUE)
mean(post.2001$nyt[post.2001$region=="Africa"],na.rm=TRUE)
mean(post.2001$nyt[post.2001$region=="LA"],na.rm=TRUE)
mean(post.2001$nyt[post.2001$region=="EECA"],na.rm=TRUE)
mean(post.2001$nyt[post.2001$region=="West"],na.rm=TRUE)

#################################################
##### Pre and Post 2001 Regression Analysis #####
#################################################


pre.2001 <- rt[rt$year<2002,]
pre.2001 <- pre.2001[-which(rt$country=="United States"),]
post.2001 <- rt[rt$year>2002 & rt$year < 2011,]
post.2001 <- post.2001[-which(rt$country=="United States"),]
names(rt)
set.seed(1234)

glm.1<-glm.nb(nyt ~ nyt.lagged+polity+amnesty.uas+gdp.pc.wdi+pop.wdi+cinc+domestic9+physint+speech+(relevel(region,4)), data = pre.2001, na.action=na.omit) 
summary(glm.1)

glm.2<-glm.nb(nyt ~ nyt.lagged+polity+amnesty.uas+gdp.pc.wdi+pop.wdi+cinc+domestic9+physint+speech+(relevel(region,5)), data = post.2001,na.action=na.omit) 
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

glm.amnesty <- glm.nb(am.mentions ~ nyt+polity+democ+autoc+physint+speech+new_empinx+wecon+wopol+wosoc+elecsd+gdp.pc.wdi+pop.wdi+statedept+milper+cinc+bdeadbest+domestic9+amnesty.uas+(relevel(region,4)), data = rt, na.action=na.omit) 
summary(glm.amnesty)
