### This R code builds my brand new spanking database with the following variables:
### 1. Country name (country): countrycode package
### 2. Numeric Country Code (ccode): COW
### 3. Alpha Country Code (scode): COW
### 4. 2 Letter WDI code (X2lcode): WB
### 5. 3 letter WDI code (worldbank): WB
### 6. UN numeric code (un): UN
### 7. Year (year): polity
### 8. Institutionalized Democracy: (democ): polity
### 9. Institutionalized Autocracy (autoc): polity
### 10. Combined Policy Score (polity + polity 2): polity 
### 11. GDP per capita, current US$ (gdp.pc.wdi): World Bank Development Indicators
### 12. GDP per capita, current US$ (gdp.pc.un): UN data
### 13. UN region (unreg): CIRI
### 14. CIRI physical integrity score (physint): CIRI
### 15. CIRI freedom of speech score (speech): CIRI
### 16. CIRI Empowerment Rights Index (new_empinx): CIRI
### 17. CIRI Women’s Economic Rights (wecon): CIRI
### 18. CIRI Women’s Political Rights (wopol): CIRI
### 19. CIRI Women's Social Rights (wosoc): CIRI
### 20. CIRI Electoral Self-Determination: CIRI
### 21. PTS Amnesty (amnesty): PTS
### 22. PTS State Department (statedept): PTS
### 23. WDI population mid-year estimates (pop.wdi): World Bank Development Indicators
### 24. Size of Military, (military personnel, thousands) (milper): Correlates of War, National Military Capabilities 
### 25. Composite Index of Military Capabilities (cinc): Correlates of War, National Military Capabilities
### 26. Number of battle deaths (bdeadbest): Bethany Lacina & Nils Petter Gleditsch, 2005. ‘Monitoring Trends in Global Combat: A New Dataset of Battle Deaths’, European Journal of Population 21(2–3): 145–166.
### 27. Number of NGO ties (INGO_uia): Hafner-Burton and Tsutsui’s (2005)
### 28. Domestic Conflict / Stability Index (domestic9): Cross National Time Series Data, Banks (2012)
### 29. Amnesty Internatioanl UA's
### 30. NYT human rights articles
### 31. Region

library("foreign")
library("WDI")
library("countrycode")
rm(list=ls())
setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/country-year-database")

###### Load other people's data to get started

ron <- read.dta("Data/RonRamosThoms/file49964_ramos_ron_thoms_jpr_2007.dta")
murdie <- read.dta("Data/MurdiePeksendata/MurdiePeksenJOPDataFile.dta") # 1945 - 2005
meernick <- read.csv ("Data/Meernick/Merge\ of\ JCR\ data\ for\ AIUA\ Paper\ and\ COW\ State\ list.csv") # 1997 - 2007 / 2009
hafner <- read.dta("Data/Hafner/data_paradox.dta") # INGO ties, 1966 - 1999
polity <- read.csv("Data/Polity/p4v2013.csv")
ciri <- read.csv("Data/CIRI/CIRI_1981_2011.csv")
codes <- read.csv("Data/WDI/country.codes.csv")
un.gdp <- read.csv("Data/UNdata/UNdata_gdp_per_capita.csv") # read data
cinc <- read.csv("Data/COW/NMC_v4_0.csv") # only to 2007
battle <- read.csv("Data/BattleDeaths/PRIO_bd3.0.csv")
cnts <- read.csv("Data/CNTS/CNTSDATA.csv")
pts <- load("/Users/rterman/Dropbox/berkeley/Dissertation/Data and Analyais/Git Repos/country-year-database/Data/PTS/PTS2012.RData") # load data
amnesty <- read.csv("Data/Amnesty/total_amnesty2.csv")
wdi.gdp <- read.csv("Data/WDI/wdi-gdp.csv")
wdi.pop <- read.csv("Data/WDI/wdi-pop.csv")
problem.countries <- read.csv("Data/problematic_countries.csv")
counts <- read.csv("Data/NYT/country_year_counts.csv")

rt.old<- rt
write.csv(rt.ciri,"rt.ciri.csv")

#############################
##### Start with Polity #####
#############################

names(polity)
rt.polity <- subset(polity, year>1979 & year < 2013,select=c(ccode,scode,country,year,polity,polity2,democ,autoc))
rt <- rt.polity

# delete some weird countries

# Sudan-North
rt <- rt[-which(rt$country=="Sudan-North"),]

###############
#### Codes ####
###############

### write function for problematic countries
get.code <- function(index,code){
  rtindex <- which(names(rt)==code) # get the index for the code column in rt
  pcindex <- which(names(problem.countries)==code)
  country <- rt$country[index]
  country <- as.character(country)
  return(problem.countries[problem.countries$country==country,pcindex])
}

as.character(get.code(4497,"worldbank"))

#### UN ####
############

rt$un <- countrycode(rt$ccode,"cown","un") #UN

#### Missing UN codes
index <- which(is.na(rt$un))
rt$un[index] <- unlist(lapply(index,get.code,code="un"))

# Sudan post 2011
rt$un[rt$country=="Sudan" & rt$year >2010] <- 29


unique(rt$country[is.na(rt$un)])

#### WB ####
############

### WB 3L Code

rt$worldbank <- countrycode(rt$un,"un","wb") #worldbank
unique(rt$country[is.na(rt$worldbank)])

## Missing Codes
index <- which(is.na(rt$worldbank))
rt$worldbank[index] <- as.character(unlist(lapply(index,get.code,code="worldbank")))

### WB Region

rt$wbregion <- countrycode(rt$worldbank,"wb","region") #worldbank region
unique(rt$wbregion)

## Missing Codes
index <- which(is.na(rt$wbregion))
rt$wbregion[index] <- as.character(unlist(lapply(index,get.code,code="wbregion")))

unique(rt$country[is.na(rt$wbregion)])

#### iso2c ####
###############

rt$iso2c <- countrycode(rt$worldbank,"wb","iso2c") #iso2c

##### Missing codes
index <- which(is.na(rt$iso2c))
rt$iso2c[index] <- as.character(unlist(lapply(index,get.code,code="iso2c")))

unique(rt$country[is.na(rt$iso2c)])

#### iso3c ####
###############

rt$iso3c <- countrycode(rt$iso2c,"iso2c","iso3c") #iso2
unique(rt$country[is.na(rt$iso3c)])

##### Missing codes
index <- which(is.na(rt$iso3c))
rt$iso3c[index] <- as.character(unlist(lapply(index,get.code,code="iso3c")))

##### Re-Order Columns
rt <- rt[,c(4,3,1,2,9,10,11,12,13,5,6,7,8)]

write.csv(rt,"rt.csv")

###############
#### CIRI ####
###############

names(ciri)
ciri.subset <- subset(ciri, YEAR > 1979 & YEAR < 2013, select=c(YEAR,COW,UNREG,PHYSINT,SPEECH,NEW_EMPINX,WECON,WOPOL,WOSOC,ELECSD))
names(ciri.subset) <- c("year","ccode","unreg","physint","speech","new_empinx","wecon","wopol","wosoc","elecsd")
rt.merge <- merge(rt,ciri.subset,by=c("year","ccode"),all.x=TRUE,incomparables=NA)
x<- data.frame(cbind(rt.merge$year,rt.merge$ccode))
x <- which(duplicated(x))
rt.merge <- rt.merge[-x,]
rt <- rt.merge

###############
##### GDP #####
###############

# From World Bank Development Indicators

WDIsearch(string="gdp per capita")
wdi.gdp <- WDI(country = "all", indicator = c("NY.GDP.PCAP.CD"), start = 1980, end = 2012) #download data
names(wdi.gdp) # GDP per capita (current US$)
write.csv(wdi.gdp,file="Data/WDI/wdi-gdp.csv") #write csv for later use
rt$gdp.pc.wdi <- NA # initialize variable

# write function to get gdp in wdi.gdp based on iso2c (2letter county codes) + year in rt
get.gdp.wdi <- function(x,y){
  gdp <- wdi.gdp$NY.GDP.PCAP.CD[wdi.gdp$iso2c==x & wdi.gdp$year==y]
  return(gdp[1])
} 

rt$gdp.pc.wdi <- mapply(get.gdp.wdi,rt$iso2c,rt$year) # apply to my data
#rt$gdp.pc.wdi <- as.character(rt$gdp.pc.wdi)
#rt$gdp.pc.wdi[rt$gdp.pc.wdi == "numeric(0)"] <- NA #get rid of that weird shit.
#rt$gdp.pc.wdi <- as.factor(rt$gdp.pc.wdi)
summary(rt$gdp.pc.wdi)

# From UN Data

rt$gdp.pc.un <- NA #initialize variable
names(un.gdp)

# write function to get data
get.gdp.un <- function(x,y){
  gdp <- un.gdp$Value[un.gdp$Country.or.Area.Code==x & un.gdp$Year==y]
  return(unlist(gdp))
}

# apply to my dataframe
rt$gdp.pc.un <- mapply(get.gdp.un,rt$un,rt$year) # apply to my data
rt$gdp.pc.un[rt$gdp.pc.un == "numeric(0)"] <- NA #get rid of that weird shit.
rt$gdp.pc.un[grep("NA",rt$gdp.pc.un)] <- NA
rt$gdp.pc.un <- as.double(rt$gdp.pc.un)
summary(rt$gdp.pc.un)


######################
##### Population #####
######################

# From World Bank Development Indicators

WDIsearch(string="SP.POP.TOTL")
wdi.pop <- WDI(country = "all", indicator = c("SP.POP.TOTL"), start = 1980, end = 2013) #download data
names(wdi.pop) # GDP per capita (current US$)
write.csv(wdi.pop,file="Data/WDI/wdi-pop.csv") #write csv for later use
rt$pop.wdi <- NA # initialize variable

# write function to get gdp in wdi.gdp based on country + year in rt
get.pop.wdi <- function(x,y){
  pop <- wdi.pop$SP.POP.TOTL[wdi.pop$iso2c==x & wdi.pop$year==y]
  return(pop)
}

# apply to data
rt$pop.wdi <- mapply(get.pop.wdi,rt$iso2c,rt$year) # apply to my data
rt$pop.wdi[rt$pop.wdi == "numeric(0)"] <- NA #get rid of that weird shit.
rt$pop.wdi <- as.double(rt$pop.wdi)
summary(rt$pop.wdi)

###############
##### PTS #####
###############

names(PTS)
pts <- subset(PTS,Year>1979 & Year<2013, select=c("COW","Year","Amnesty","StateDept"))
names(pts) <- c("ccode","year","amnesty","statedept")
rt.merge <- merge(rt,pts,by=c("ccode","year"), all.x = TRUE)
rt <- rt.merge

#########################################
#### National Military Capabilities #####
#########################################

cinc.sub <- subset(cinc,year>1979 & year<2013,select=c("ccode","year","milper","cinc"))
rt.merge <- merge(rt,cinc.sub,by=c("ccode","year"), all.x = TRUE)
rt.merge$milper[rt.merge$milper=="-9"] <- NA # replace -9 with NA
rt <- rt.merge

########################
#### Battle Deaths #####
########################

battle.sub <- subset(battle,year>1979 & year<2013,select=c("year","bdeadbes","location"))
names(battle.sub) <- c("year","bdeadbest","country")
summary(battle.sub$country) # note there are some multiple countries here that I don't know what to do with - return to it later.

rt.merge <-merge(rt,battle.sub,by=c("country","year"), all.x = TRUE)
x<- data.frame(cbind(rt.merge$year,rt.merge$ccode))
x <- which(duplicated(x))
x
rt.merge <- rt.merge[-x,]
rt.merge$bdeadbest[is.na(rt.merge$bdeadbest)] <- 0
rt.merge$bdeadbest[rt.merge$bdeadbest == "-999"] <- NA 
rt <- rt.merge
summary(rt$bdeadbest)

#####################
##### INGO ties #####
#####################

rt.merge <- merge(rt,hafner,by=c("country","year"),all.x = TRUE)
rt <- rt.merge

unique(rt$country[is.na(rt$INGO_uia)])

#####################################
##### CNTS - domestic stability #####
#####################################

names(cnts)
cnts.sub <- subset(cnts,year>1979 & year<2013,select=c("year","Wbcode","domestic9"))
names(cnts.sub) <- c("year","worldbank","domestic9")
rt.merge <- merge(rt,cnts.sub,by=c("year","worldbank"),all.x=TRUE)
x<- data.frame(cbind(rt.merge$year,rt.merge$ccode))
x <- which(duplicated(x))
x
rt.merge <- rt.merge[-x,]
rt <- rt.merge

unique(rt$country[is.na(rt$domestic9)])


##################################
##### Amnesty Urgent Actions #####
##################################

#define function
amnesty$countrycode <- as.integer(amnesty$countrycode)
amnesty$year <- as.integer(amnesty$year)

get.uas <- function(x,y){
  subset.data <- subset(amnesty,countrycode==x & year==y)
  return(as.integer(nrow(subset.data)))
}

get.uas(516,2002)
rt$amnesty.uas <- mapply(get.uas,x=rt$ccode,y=rt$year)
summary(rt$amnesty.uas)

# Assign NAs for all years < 1985
rt$amnesty.uas[rt$year<1985] <- NA


##############################
##### NYT Media Coverage #####
##############################

rt$nyt <- NA
x <- list()
n <- nrow(counts)
n
for(i in 1:n){
  country <- counts$iso3c[i]
  year <- counts$year[i]
  count <- counts$count[i]
  index <- which(rt$iso3c==country & rt$year==year)
  if (length(index) > 0){
    rt$nyt[index] <- count
    } else x <- append(x,index)
}
x

# turn NAs to 0
rt$nyt[which(is.na(rt$nyt))] <- 0
rt$nyt[rt$year>2010] <- NA


###### Writing, reading, loving.
rt$pop.wdi <- as.character(rt$pop.wdi)
write.csv(rt,"rt.csv")
