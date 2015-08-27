### This R code builds my brand new spanking database with the following variables:
### 1. year: 1980-2013
### 2. country: Country name (countrycode package)
### 3. ccode: COW Numeric Country Code (countrycode package)
### 4. scode: COW Alpha Country Code (countrycode package)
### 5. un: UN county code (countrycode package)
### 6. worldbank: 3 letter World Bank country code (countrycode package)
### 7. wbregion: World Bank region (countrycode package)
### 8. iso2c: 2 Letter iso code (countrycode package)
### 9. iso3c: 3 letter iso code (countrycode package)
### 10. polity: combined polity score (polity)
### 11. polity2: combined polity score 2 (polity)
### 12. democ: Institutionalized Democracy: (polity)
### 13. autoc: Institutionalized Autocracy (polity)
### 14. unreg: UN region (ciri)
### 15. physint: CIRI physical integrity score (ciri)
### 16. speech: CIRI freedom of speech score (ciri)
### 17. new_empinx: CIRI Empowerment Rights Index (ciri)
### 18. wecon: CIRI Women’s Economic Rights (ciri)
### 19. wepol: CIRI Women’s Political Rights (ciri)
### 20. wosoc: CIRI Women's Social Rights (ciri)
### 21. elecsd: CIRI Electoral Self-Determination: (ciri)
### 22. gdp.pc.wdi: GDP per capita, current US$ (World Bank Development Indicators)
### 23. gdp.pc.un: GDP per capita, current US$ (UN data)
### 24. pop.wdi: WDI population mid-year estimates (World Bank Development Indicators)
### 25. amnesty: PTS Amnesty (PTS)
### 26. statedept: PTS State Department (PTS)
### 27. milper: Size of Military, (military personnel, thousands) (Correlates of War, National Military Capabilities, cinc)
### 28. cinc: Composite Index of Military Capabilities (Correlates of War, National Military Capabilities, cinc)
### 29. bdeadbest: Number of battle deaths. (Bethany Lacina & Nils Petter Gleditsch, 2005. ‘Monitoring Trends in Global Combat: A New Dataset of Battle Deaths’, European Journal of Population 21(2–3): 145–166.)
### 30. INGO_uia: Number of NGO ties (Hafner-Burton and Tsutsui’s (2005))
### 31. domestic9: Domestic Conflict / Stability Index (Cross National Time Series Data, Banks (2012))
### 32. amnesty.uas: Number of Amnesty Internatioanl UA's (Terman)
### 33. nyt: Number of NYT human rights articles (Terman)
### 34. Region (based on Hafner-Burton and Ron's (2012) categories)

library("foreign")
library("WDI")
library("countrycode")
library(statar) # for missing values
rm(list=ls())
setwd("~/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/country-year-database")

###### Load other people's data to get started

meernick <- read.csv ("Data/Meernick/Merge\ of\ JCR\ data\ for\ AIUA\ Paper\ and\ COW\ State\ list.csv") # 1997 - 2007 / 2009
ron <- read.dta("Data/RonRamosThoms/file49964_ramos_ron_thoms_jpr_2007.dta")
murdie <- read.dta("Data/MurdiePeksendata/MurdiePeksenJOPDataFile.dta") # 1945 - 2005
hafner <- read.dta("Data/Hafner/data_paradox.dta") # INGO ties, 1966 - 1999
polity <- read.csv("Data/Polity/p4v2013.csv")
ciri <- read.csv("Data/CIRI/CIRI_1981_2011.csv")
codes <- read.csv("Data/WDI/country.codes.csv")
un.gdp <- read.csv("Data/UNdata/UNdata_gdp_per_capita.csv") # read data
cinc <- read.csv("Data/COW/NMC_v4_0.csv") # only to 2007
battle <- read.csv("Data/BattleDeaths/PRIO_bd3.0.csv")
cnts <- read.csv("Data/CNTS/CNTSDATA.csv")
pts <- load("Data/PTS/PTS2012.RData") # load data
amnesty <- read.csv("Data/Amnesty/total_amnesty2.csv")
wdi.gdp <- read.csv("Data/WDI/wdi-gdp.csv")
wdi.pop <- read.csv("Data/WDI/wdi-pop.csv")
nyt <- read.csv("Data/NYT_violations/nyt.violations.csv")
am <- read.csv("Data/Amnesty/amnesty_processed.csv")
problem.countries <- read.csv("problematic_countries.csv")
countries <- read.csv("country_codes.csv")
muslim <- read.csv("Data/muslim-pop.csv")

#############################
##### Start with Voeten #####
#############################

load("Data/Idealpoints.RData")
rt <- x[x$Year>1978,c(1,2,6,8)]
names(rt) <- c("year","ccode","country","idealpoint" )
rt <- arrange(rt, ccode, year)

###############
#### Codes ####
###############

codes <- countrycode_data

#### UN ####
############

rt$un <- countrycode(rt$ccode,"cown","un") #UN

#### Missing UN codes
unique(rt$country[is.na(rt$un)])
# index <- which(is.na(rt$un))
# rt$un[index] <- unlist(lapply(index,get.code,code="un"))
# Sudan post 2011
# rt$un[rt$country=="Sudan" & rt$year >2010] <- 29

#### WB ####
############

rt$worldbank <- countrycode(rt$un,"un","wb") #worldbank
unique(rt$country[is.na(rt$worldbank)])

#### iso2c ####
###############

rt$iso2c <- countrycode(rt$worldbank,"wb","iso2c") #iso2c
unique(rt$country[is.na(rt$iso2c)])

#### iso3c ####
###############

rt$iso3c <- countrycode(rt$iso2c,"iso2c","iso3c") #iso2
unique(rt$country[is.na(rt$iso3c)])

##### Re-Order Columns
rt <- rt[,c(1,2,3,5,6,7,8,4)]

#######################
##### Add Polity #####
######################

polity.sub <- subset(polity, year>1978 & year < 2015,select=c(ccode,year,polity,polity2,democ,autoc))
rt <- merge(rt,polity.sub,by=c("year","ccode"),all.x=TRUE)
rt$polity[rt$polity < -10] <- NA
rt$polity2[rt$polity2 < -10] <- NA
summary(rt$polity)

#write.csv(rt,"rt.csv")

###############
#### CIRI ####
###############

names(ciri)
ciri.subset <- subset(ciri, YEAR > 1978 & YEAR < 2015, select=c(YEAR,COW,UNREG,PHYSINT,SPEECH,NEW_EMPINX,WECON,WOPOL,WOSOC,ELECSD))
names(ciri.subset) <- c("year","ccode","unreg","physint","speech","new_empinx","wecon","wopol","wosoc","elecsd")
rt.merge <- merge(rt,ciri.subset,by=c("year","ccode"),all.x=TRUE)
x<- data.frame(cbind(rt.merge$year,rt.merge$ccode))
x <- which(duplicated(x))
rt.merge <- rt.merge[-x,]
rt <- rt.merge

rt$wopol[rt$wopol<0] <-NA
rt$speech[rt$speech<0] <- NA
rt$wecon[rt$wecon<0] <-NA
rt$wosoc[rt$wosoc<0] <-NA
rt$elecsd[rt$elecsd<0] <-NA

###############
##### GDP #####
###############

# From World Bank Development Indicators
WDIsearch(string="gdp per capita")
wdi.gdp <- WDI(country = "all", indicator = c("NY.GDP.PCAP.CD"), start = 1979, end = 2014) #download data
names(wdi.gdp) # GDP per capita (current US$)
wdi.gdp$country <- NULL
#write.csv(wdi.gdp,file="Data/WDI/wdi-gdp.csv") 
rt <- merge(rt,wdi.gdp,by=c("year","iso2c"),all.x=TRUE) # TODO: switch to "wb"?
names(rt)[21] <- "gdp.pc.wdi"
summary(rt$gdp.pc.wdi)

# From UN Data
un.gdp$Item <- NULL
un.gdp$Country.or.Area <- NULL
names(un.gdp)
names(un.gdp) <- c("un","year","gdp.pc.un")
rt <- merge(rt,un.gdp,by=c("year","un"),all.x=TRUE)

######################
##### Population #####
######################

# From World Bank Development Indicators
WDIsearch(string="SP.POP.TOTL")
wdi.pop <- WDI(country = "all", indicator = c("SP.POP.TOTL"), start = 1979, end = 2014) #download data
names(wdi.pop) # GDP per capita (current US$)
#write.csv(wdi.pop,file="Data/WDI/wdi-pop.csv") #write csv for later use

# subset
wdi.pop$country <- NULL
rt <- merge(rt,wdi.pop,by=c("year","iso2c"),all.x=TRUE)
names(rt)[23] <- "pop.wdi"
summary(rt$pop.wdi)

###############
##### PTS #####
###############

names(PTS)
pts <- subset(PTS,Year>1978 & Year<2015, select=c("COW","Year","Amnesty","StateDept"))
names(pts) <- c("ccode","year","amnesty","statedept")
rt.merge <- merge(rt,pts,by=c("ccode","year"), all.x = TRUE)
rt <- rt.merge

#########################################
#### National Military Capabilities #####
#########################################

cinc.sub <- subset(cinc,year>1978 & year<2015,select=c("ccode","year","milper","cinc"))
rt.merge <- merge(rt,cinc.sub,by=c("ccode","year"), all.x = TRUE)
rt.merge$milper[rt.merge$milper=="-9"] <- NA # replace -9 with NA
rt <- rt.merge

########################
#### Battle Deaths #####
########################

battle.sub <- subset(battle,year>1978 & year<2015,select=c("year","bdeadbes","location"))
names(battle.sub) <- c("year","bdeadbest","country")
summary(battle.sub$country) # note there are some multiple countries here that I don't know what to do with - return to it later.
names(rt)

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

rt.saved <- rt
rt <- rt.saved

names(cnts)
cnts.sub <- subset(cnts,year>1978 & year<2015,select=c("year","Wbcode","domestic9"))
names(cnts.sub) <- c("year","worldbank","domestic9")
rt.merge <- merge(rt,cnts.sub,by=c("year","worldbank"),all.x=TRUE)
rt <- rt.merge
summary(rt$domestic9)

##############################
##### NYT Media Coverage #####
##############################

## NEEDS WORK

## make new code column
rt$rt_code <- rt$iso3c
rt$rt_code <- as.character(rt$rt_code)
rt$rt_code[rt$country=="Macedonia"] <- "MAC"

#define function
nyt$YEAR <- as.integer(nyt$YEAR)

get.nyt <- function(x,y){
  subset.data <- subset(nyt,COUNTRY_CODE==x & YEAR==y)
  return(as.integer(nrow(subset.data)))
}

get.nyt("MKD",1999)
nyt.old <- rt$nyt
rt$nyt <- mapply(get.nyt,x=as.character(rt$rt_code),y=rt$year)
# rt$nyt[rt$country=="United States"] <- NA
rt$nyt <- as.numeric(rt$nyt)
summary(rt$nyt)

rt$nyt[rt$year>2010] <- NA

##################################
##### Amnesty Urgent Actions #####
##################################

## NEEDS WORK

#define function
amnesty$countrycode <- as.character(amnesty$countrycode)
amnesty$year <- as.integer(amnesty$year)

get.uas <- function(x,y){
  subset.data <- subset(amnesty,countrycode==x & year==y)
  return(as.integer(nrow(subset.data)))
}

get.uas("USA",2000)
rt$amnesty.uas <- mapply(get.uas,x=rt$rt_code,y=rt$year)
summary(rt$amnesty.uas)

# Assign NAs for all years < 1985
rt$amnesty.uas[rt$year<1985] <- NA

##################
##### Region #####
##################

## NEEDS WORK

rt$region<- NA
n <- nrow(countries)
for (i in 1:n){
  country <- countries$iso3c[i]
  rt$region[rt$rt_code==country]<-as.character(countries$Region[i])
}
unique(rt$country[is.na(rt$region)])

rt$region[rt$country=="Czechoslovakia"] <- "EECA"
rt$region[rt$country=="Yemen South"] <- "MENA"
rt$region[rt$country=="Germany East"] <- "EECA"
rt$region[rt$country=="Taiwan"] <- "Asia"
rt$region[rt$country=="Serbia and Montenegro"] <- "EECA"
rt$region[rt$country=="Serbia"] <- "EECA"
rt$region[rt$country=="Yugoslavia"] <- "EECA"
rt$region[rt$country=="Macedonia"] <- "EECA"
rt$region[rt$country=="Kosovo"] <- "EECA"
rt$region[rt$country=="Montenegro"] <- "EECA"

rt$region <- as.factor(rt$region)
summary(rt$region)

#################################
##### Murdie - Media Exp  #######
#################################

murdie.subset<- subset(murdie,select=c("ccode","year","lnreportcount"))
rt <- merge(rt,murdie.subset,by=c("year","ccode"),all.x=TRUE)
summary(murdie$lnreportcount)

####################
##### Muslim #######
####################
muslim.org <- muslim
muslim <- muslim.org

muslim$ccode<- countrycode(muslim$Country, "country.name", "cown")
muslim <- muslim[,c(3,5,8)]
names(muslim) <- c("1990", "2010","ccode")
muslim <- melt(muslim, id.var = "ccode", variable.name = "year")

rt.sub <- rt[,c("ccode","year")]
row.names(rt.sub) <- 1:nrow(rt.sub)
muslim <- merge(rt.sub, muslim, all.x = T)
x<- data.frame(cbind(muslim$year,muslim$ccode))
x <- which(duplicated(x))
x
muslim <- muslim[-x,]

muslim <- arrange(muslim, ccode)

impute <- function(var){
  DT <- data.table(
    id    = muslim$ccode,
    date  = as.numeric(as.character(muslim$year)),
    value = var
  )
  setna(DT, along_with = date, by = id, roll = "nearest")
  return(DT$value)
}

muslim$value2<- impute(muslim$value)
muslim.x <- muslim[,c("ccode","year","value2")]
names(muslim.x) <- c("ccode","year","muslim")
rt.merge <- merge(rt, muslim.x, all.x = T)

# test
rt.merge$muslim[rt.merge$country=="United Kingdom"]
names(rt.merge)

rt <- rt.merge

#rt$pop.wdi <- as.character(rt$pop.wdi)
#rt$nyt.lagged <-  as.character(rt$nyt.lagged)
#rt$muslim <- as.character(rt$muslim)

write.csv(rt,"rt.csv", row.names = F)

rt.no.us <- subset(rt,!rt$country=="United States")
write.csv(rt.no.us,"rt.no.us.csv", row.names = F)

