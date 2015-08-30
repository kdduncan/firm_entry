################# required packages ################# 
require(xlsx)
require(dplyr)
###### loading data #####

# tota of 3144 county-equivalent locations in the US, we exclude Alaska, Hawaii, and DC to get a target total of
# 3144 - 30 - 5-1 = 3108
# with 11 years of data from 1999-2009, implying a target of34166 is the ideal master merge file size

# tax data 1977-2008
taxdat <- read.csv("~/papers/firm_entry/build/input/all_state_taxes.csv")
taxdat$year_L1 <- taxdat$year+1
taxdat <- taxdat[,-c(1,13)]

# uitax data is not propery normaized in 'all_state_taxes', which we fix below
uidat <- read.xlsx("~/papers/firm_entry/build/input/uitax_fix.xlsx",1)
uidat$year_L1 <- uidat$Year+1
uidat <- uidat[,-c(1:3)]

# now replace original uitax data
taxdat <- merge(taxdat,uidat,by.x=c("year_L1","stfip"),by.y = c("year_L1","st_fips"))
rm(uidat)

# government spending per capita, 1994-2010
govdat <- read.xlsx("~/papers/firm_entry/build/input/govtexp_L1.xlsx", "Sheet1")
govdat$stfip <- govdat$stfip_sub
govdat <- govdat[,-c(1,3,4)]

# amenities
amenities <- read.xlsx("~/papers/firm_entry/build/input/natamenf_1_.xls", 1, startRow = 105)
amenities <- amenities[,-c(2,5:14)]
amenities$FIPS.Code <- as.numeric(as.character(amenities$FIPS.Code))

# fips codes for all US states
fips<- read.csv("~/papers/firm_entry/build/input/BorderFips.csv")
fips$county <- tolower(fips$county)
fips$state <- tolower(fips$state)
fips$cofip <- fips$statefips*1000+fips$countyfips
fips <- fips[fips$county != "state total" & fips$county != "statewide",]

tmp <- read.csv("~/papers/firm_entry/build/input/Edited_County_Files/county_naicssector_emplchange_1998-1999_edited.csv",
  header = FALSE)
tmp <- tmp[tmp$V4 == "--" & tmp$V3 != "STATE TOTAL"
           & tmp$V3 != "STATEWIDE" & tmp$V2 != "ALASKA"
           & tmp$V2 != "HAWAII",]
tmp <- tmp[,-c(4)]
colnames(tmp) <- c("year","state","county","base","births","deaths","expansions","contractions","stable")
tmp$state <- tolower(tmp$state)
tmp$county <- tolower(tmp$county)
master<- merge(fips,tmp, by = c("state","county"), all.x = TRUE)
master$year[is.na(master$year)] <- 1999
master[is.na(master)] <- 0

for (i in 2000:2010){
  tmp <- read.csv(paste(
    "~/papers/firm_entry/build/input/Edited_County_Files/county_naicssector_emplchange_",i-1,"-",i,"_edited.csv",sep=""),
    header = FALSE)
  tmp <- tmp[tmp$V4 == "--" & tmp$V3 != "STATE TOTAL"
             & tmp$V3 != "STATEWIDE" & tmp$V2 != "ALASKA"
             & tmp$V2 != "HAWAII",]
  tmp <- tmp[,-c(4)]
  colnames(tmp) <- c("year","state","county","base","births","deaths","expansions","contractions","stable")
  tmp$state <- tolower(tmp$state)
  tmp$county <- tolower(tmp$county)
  tmp<- merge(fips,tmp, by = c("state","county"), all.x = TRUE)
  tmp$year[is.na(tmp$year)] <- i
  tmp[is.na(tmp)] <- 0
  master <- rbind(master,tmp)
}
rm(tmp)

master <- merge(master,govdat, by.x=c("statefips","year"),by.y=c("stfip","year"))
rm(govdat)
master <- merge(amenities, master, by.x = "FIPS.Code",by.y="cofip")
rm(amenities)
master <- merge(master, taxdat, by.x = c("statefips","year"), by.y = c("stfip","year_L1"))
rm(taxdat)
#34166?? Not 37272??

master <- master[c("statefips","countyfips","FIPS.Code","year","state","county",
                   "base","births","deaths","expansions","contractions",
                   "educ_pc_L1","hwy_pc_L1","welfare_pc_L1",
                   "ptax","inctax","capgntax","salestax","corptax","wctaxfixed","uitaxrate",
                   "hsplus","realfuelpr","unionmem","popdensity","pctmanuf",
                   "JAN.TEMP...Z","JAN.SUN...Z","JUL.TEMP...Z","JUL.HUM...Z","TOPOG...Z","LN.WATER..AREA...Z")]

write.csv(master, file = "~/papers/firm_entry/build/output/master.csv")
