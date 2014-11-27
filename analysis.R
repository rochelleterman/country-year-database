###This R code conducts analysis on the data "rt.csv", which was collected with the R file getdata.R

setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/country-year-database")

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


#################################################
##### Pre and Post 2001 Regression Analysis #####
#################################################


pre.2001 <- rt[rt$year<2002,]
pre.2001 <- pre.2001[-which(rt$country=="United States"),]
post.2001 <- rt[rt$year>2002,]
post.2001 <- post.2001[-which(rt$country=="United States"),]
names(rt)
glm.1<-glm.nb(nyt ~ polity+polity2+democ+autoc+physint+speech+new_empinx+wecon+wopol+wosoc+elecsd+gdp.pc.wdi+pop.wdi+amnesty+statedept+milper+cinc+bdeadbest+INGO_uia+domestic9+amnesty.uas+(relevel(region,4)), data = pre.2001) 
summary(glm.1)

glm.2<-glm(nyt ~ polity+polity2+democ+autoc+physint+speech+new_empinx+wecon+wopol+wosoc+elecsd+gdp.pc.wdi+pop.wdi+amnesty+statedept+milper+cinc+bdeadbest+domestic9+amnesty.uas+(relevel(region,5)), data = post.2001) 
summary(glm.2)

names(rt)

