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


library("foreign")

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

##### First subset polity data to get country, year, ccode, polity, polity2, autoc, democ

names(polity)
rt <- subset(polity,year > 1979, select=c(year,ccode,country,scode,polity,polity2,democ,autoc))

##### Add 2-letter and 3-letter codes number for WDI

names(codes)
codes.sub <- subset(codes,select=c(X2lcode,X3lcode,un))
names(rt)
rt <- merge(codes.sub,rt,by = "un",all.y = TRUE) # merge

# NA?

no.code.countries <- cbind(unique(rt$country[is.na(rt$X2lcode)]),rt$year[]
no.code.countries <- subset(rt,is.na(rt$X2lcode),select=c("country","year","un","ccode"))

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
rt$un[rt$country=="Yemen South"] <- 720
#Montenegro
rt$un[rt$country=="Montenegro"] <- 499
# USSR
rt$un[rt$country=="USSR"] <- 810
#Kosovo
rt$un[rt$country=="Kosovo"] <-NA
# Taiwan
rt$un[rt$country=="Taiwan"] <- NA
#Serbia
rt$un[rt$country=="Serbia"] <- 688
#Serbia and Montenegro
rt$un[rt$country=="Serbia and Montenegro"] <- 891

##### GDP

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

##### Add UN code number from CIRI

un.cow <- unique(data.frame(cbind(ciri$COW,ciri$UNCTRY)))
names(un.cow) <- c("ccode","un")
rt <- merge(un.cow,rt,by = "ccode",all.y = TRUE) # merge

###### CIRI Scores

names(ciri)
ciri.sub <- subset(ciri, select=c(COW,YEAR,UNREG,PHYSINT,SPEECH,NEW_EMPINX,WECON,WOPOL,WOSOC,ELECSD))
names(ciri.sub) <- c("ccode","year","unreg","physint","speech","new_empinx","wecon","wopol","wosoc","elecsd")
names(ciri.sub)

rt.merge <- merge(rt,ciri.sub, by = c("ccode","year"), all.x = TRUE)
rt.merge <- unique(rt.merge)

rt <- read.csv("rt.merge.csv") # I had to remove weird duplicate records by hand so I'm reading this back in

####### PTS

pts <- load("/Users/rterman/Dropbox/berkeley/Dissertation/Data and Analyais/Git Repos/country-year-database/Data/PTS/PTS2012.RData") # load data
names(PTS)
pts <- subset(PTS,Year>1979, select=c("COW","Year","Amnesty","StateDept"))
names(pts) <- c("ccode","year","amnesty","statedept")
rt.merge <- merge(rt,pts,by=c("ccode","year"), all.x = TRUE)

rt <- rt.merge
write.csv(ciri,"Data/CIRI/CIRI_1981_2011.csv")

######### Population

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

#### National Military Capabilities

cinc.sub <- subset(cinc,year>1979,select=c("ccode","year","milper","cinc"))
rt.merge <- merge(rt,cinc.sub,by=c("ccode","year"), all.x = TRUE)
rt.merge$milper[rt.merge$milper=="-9"] <- NA # replace -9 with NA
rt <- rt.merge

#### Battle Deaths

battle.sub <- subset(battle,year>1979,select=c("year","bdeadbes","location"))
names(battle.sub) <- c("year","bdeadbest","country")
summary(battle.sub$country) # note there are some multiple countries here that I don't know what to do with - return to it later.
rt.merge <- merge(rt,battle.sub,by=c("country","year"), all.x = TRUE)
rt.merge$bdeadbest[is.na(rt.merge$bdeadbest)] <- 0
rt.merge$bdeadbest[rt.merge$bdeadbest == "-999"] <- NA 
rt <- rt.merge

##### INGO ties

rt.merge <- merge(rt,hafner,by=c("country","year"),all.x = TRUE)
rt <- rt.merge

### CNTS

names(cnts)
cnts.sub <- subset(cnts,year>1979,select=c("year","Wbcode","domestic9"))
names(cnts.sub) <- c("year","X3lcode","domestic9")
rt.merge <- merge(rt,cnts.sub,by=c("year","X3lcode"),all.x=TRUE)
rt <- rt.merge

#### Writing, reading, loving.
rt$pop.wdi <- as.character(rt$pop.wdi)
write.csv(rt,"rt.csv")
rt <- read.csv("rt.csv")
rt$X <- NULL
