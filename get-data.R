### This R code builds my brand new spanking database with the following variables:
### 1. Alpha Country name (country): polity
### 2. Numeric Country Code (ccode): polity
### 3. Alpha Country Code (scode): polity
### 4. 2 Letter WDI code (X2lcode): WDI
### 5. 3 letter WDI code (X3lcode): WDI
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

##################################
##### Start with CIRI scores #####
##################################

names(ciri)
rt <- subset(ciri, select=c(CTRY,YEAR,UNCTRY,COW,UNREG,PHYSINT,SPEECH,NEW_EMPINX,WECON,WOPOL,WOSOC,ELECSD))
names(rt) <- c("country","year","un","ccode","unreg","physint","speech","new_empinx","wecon","wopol","wosoc","elecsd")
names(rt)


#################################
##### Country Coding Issues #####
#################################

##### Fill in missing or wrong UN codes

#Angola un code is 24
rt$un[rt$country=="Angola"] <- 24
#Namibia
rt$un[rt$country=="Namibia"] <- 516
#Yugoslavia
rt$un[rt$country=="Yugoslavia"] <- 807
#Germany East 276
rt$un[rt$country=="Germany East"] <- 276
#Vietnam
rt$un[rt$country=="Vietnam"] <- 704
#Ethiopia
rt$un[rt$country=="Ethiopia"] <- 231
#South Sudan
rt$un[rt$country=="South Sudan"] <- 728
#Yemen South
rt$un[rt$country=="Yemen, South"] <- 720
#Montenegro
rt$un[rt$country=="Montenegro"] <- 499
# USSR
rt$un[rt$country=="Soviet Union"] <- 810
#Kosovo
rt$un[rt$country=="Kosovo"] <-NA
# Taiwan
rt$un[rt$country=="Taiwan"] <- NA
#Serbia
rt$un[rt$country=="Serbia"] <- 688
#Serbia and Montenegro
rt$un[rt$country=="Serbia and Montenegro"] <- 891

###### Merge with 2-letter and 3-letter codes number for WDI

names(codes)
codes.sub <- subset(codes,select=c(X2lcode,X3lcode,un))
names(codes.sub)

rt.merge <- merge(codes.sub,rt,by="un",all.y=TRUE)
rt <- rt.merge

no.code.countries <- unique(rt$country[is.na(rt$X2lcode)])
no.code.countries <- subset(rt,is.na(rt$X2lcode),select=c("country","year","un","ccode"))

####### Add 1980 to the mix

data1980 <- subset(rt,year==1981)
data1980$year <- 1980
data1980[,8:14] <- NA

rt <- rbind(rt,data1980)
rt <- rt[order(rt$country),] # order by country name

write.csv(rt,"rt1a.csv")

##### Deal with problematic countries

rt <- rt[-which(rt$country=="Czech Republic" & rt$year<1993),]
rt.merge <- rt[-which(rt$country=="Czechoslovakia" & rt$year>1992),]

rt <- rt[-which(rt$country=="Serbia" & rt$year<2006),]
rt <- rt[-which(rt$country=="Serbia and Montenegro" & rt$year>2005),]

##### Merge with polity data to get country, year, ccode, polity, polity2, autoc, democ

names(polity)
polity.sub <- subset(polity,year > 1979, select=c(year,ccode,scode,polity,polity2,democ,autoc))
rt <- merge(rt,polity.sub,by = c("ccode","year"),all.x = TRUE) # merge

###############
##### GDP #####
###############

# From World Bank Development Indicators

WDIsearch(string="gdp per capita")
wdi.gdp <- WDI(country = "all", indicator = c("NY.GDP.PCAP.CD"), start = 1980, end = 2013) #download data
names(wdi.gdp) # GDP per capita (current US$)
write.csv(wdi.gdp,file="Data/WDI/wdi-gdp.csv") #write csv for later use
rt$gdp.pc.wdi <- NA # initialize variable

# write function to get gdp in wdi.gdp based on iso2c (2letter county codes) + year in rt
get.gdp.wdi <- function(x,y){
  gdp <- wdi.gdp$NY.GDP.PCAP.CD[wdi.gdp$iso2c==x & wdi.gdp$year==y]
  return(gdp)
} 

rt$gdp.pc.wdi <- mapply(get.gdp.wdi,rt$X2lcode,rt$year) # apply to my data
rt$gdp.pc.wdi <- as.character(rt$gdp.pc.wdi)
rt$gdp.pc.wdi[rt$gdp.pc.wdi == "numeric(0)"] <- NA #get rid of that weird shit.
rt$gdp.pc.wdi <- as.factor(rt$gdp.pc.wdi)

# From UN Data

rt$gdp.pc.un <- NA #initialize variable
names(un.gdp)

# write function to get data
get.gdp.un <- function(x,y){
  gdp <- un.gdp$Value[un.gdp$Country.or.Area.Code==x & un.gdp$Year==y]
  return(gdp)
}

# apply to my dataframe
rt$country <- as.character(rt$country) # necessary
rt$gdp.pc.un <- mapply(get.gdp.un,rt$un,rt$year) # apply to my data
rt$gdp.pc.un <- as.character(rt$gdp.pc.un)
rt$gdp.pc.un[rt$gdp.pc.un == "numeric(0)"] <- NA #get rid of that weird shit.
rt$gdp.pc.un <- as.factor(rt$gdp.pc.un)
summary(rt$gdp.pc.un)

###############
##### PTS #####
###############

names(PTS)
pts <- subset(PTS,Year>1979, select=c("COW","Year","Amnesty","StateDept"))
names(pts) <- c("ccode","year","amnesty","statedept")
rt.merge <- merge(rt,pts,by=c("ccode","year"), all.x = TRUE)
rt <- rt.merge

######################
##### Population #####
######################

# From World Bank Development Indicators

WDIsearch(string="SP.POP.TOTL")
wdi.pop <- WDI(country = "all", indicator = c("SP.POP.TOTL"), start = 1980, end = 2013) #download data
names(wdi.pop) # GDP per capita (current US$)
write.csv(wdi.gdp,file="Data/WDI/wdi-pop.csv") #write csv for later use
rt$pop.wdi <- NA # initialize variable

# write function to get gdp in wdi.gdp based on country + year in rt
get.pop.wdi <- function(x,y){
  pop <- wdi.pop$SP.POP.TOTL[wdi.pop$iso2c==x & wdi.pop$year==y]
  return(pop)
} 

rt$pop.wdi <- mapply(get.pop.wdi,rt$X2lcode,rt$year) # apply to my data
rt$pop.wdi <- as.character(rt$pop.wdi)
rt$pop.wdi[rt$pop.wdi == "numeric(0)"] <- NA #get rid of that weird shit.
rt$pop.wdi <- as.factor(rt$pop.wdi)

#########################################
#### National Military Capabilities #####
#########################################

cinc.sub <- subset(cinc,year>1979,select=c("ccode","year","milper","cinc"))
rt.merge <- merge(rt,cinc.sub,by=c("ccode","year"), all.x = TRUE)
rt.merge$milper[rt.merge$milper=="-9"] <- NA # replace -9 with NA
rt <- rt.merge

########################
#### Battle Deaths #####
########################

battle.sub <- subset(battle,year>1979,select=c("year","bdeadbes","location"))
names(battle.sub) <- c("year","bdeadbest","country")
summary(battle.sub$country) # note there are some multiple countries here that I don't know what to do with - return to it later.
rt.merge <- merge(rt,battle.sub,by=c("country","year"), all.x = TRUE)
rt.merge$bdeadbest[is.na(rt.merge$bdeadbest)] <- 0
rt.merge$bdeadbest[rt.merge$bdeadbest == "-999"] <- NA 
rt <- rt.merge

#####################
##### INGO ties #####
#####################

rt.merge <- merge(rt,hafner,by=c("country","year"),all.x = TRUE)
rt <- rt.merge

#####################################
##### CNTS - domestic stability #####
#####################################

names(cnts)
cnts.sub <- subset(cnts,year>1979,select=c("year","Wbcode","domestic9"))
names(cnts.sub) <- c("year","X3lcode","domestic9")
rt.merge <- merge(rt,cnts.sub,by=c("year","X3lcode"),all.x=TRUE)
rt <- rt.merge

##################################
##### Amnesty Urgent Actions #####
##################################

#define function
amnesty$countrycode <- as.integer(amnesty$countrycode)
amnesty$year <- as.integer(amensty$year)

get.uas <- function(x,y){
  subset.data <- subset(amnesty,countrycode==x & year==y)
  return(as.integer(nrow(subset.data)))
}

get.uas(516,2002)
rt$amnesty.uas <- mapply(get.uas,x=rt$ccode,y=rt$year)
summary(rt$amnesty.uas)

##############################
##### NYT Media Coverage #####
##############################

#define function
total$COUNTRY_CODE <- as.integer(total$COUNTRY_CODE)
total$YEAR <- as.integer(total$YEAR)

get.nyt <- function(x,y){
  subset.data <- subset(total,COUNTRY_CODE==x & YEAR==y)
  return(as.integer(nrow(subset.data)))
}

get.uas(2,2002)
rt$nyt <- mapply(get.nyt,x=rt$ccode,y=rt$year)
summary(rt$amnesty.uas)


##################
##### Region #####
##################

countries <- read.csv("Data/country_codes.csv")

##Adding 3-letter codes to my country code list.

get.letters <- function(x,y,z){
  country.index <- which(z$Code==x)
  z$X3lcode[country.index] <- as.character(y)
  return(z$X3lcode)
}

countries$X3lcode <- NA

for(i in 1:nrow(rt)){
  countries$X3lcode <- get.letters(rt$ccode[i],rt$X3lcode[i],countries)
}

# export
write.csv(countries,"country_codes.csv")

rt$region <- NA

get.region <- function(x,y,z){
  country.index <- which(z$ccode==x)
  z$region[country.index] <- as.character(y)
  return(z$region)
}


# Apply function to all countries in the key-value list
n <- nrow(countries)
for(i in 2:n){
  rt$region <- get.region(countries$Code[i],countries$Region[i],rt)
}

### remove duplicate entries
x <- which(duplicated(subset(rt,select=c("year","country"))))
rt <- rt[-x,]

#### Writing, reading, loving.
rt$pop.wdi <- as.character(rt$pop.wdi)
write.csv(rt,"rt2.csv")
rt <- read.csv("rt2.csv")
names(rt)
rt$X <- NULL

