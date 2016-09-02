# This file creates my master files for both my count data and regression discontinuity models.


# required packages 
require(xlsx)
require(dplyr)


# load in raw data

# note, we can calculate the ideal number of counties we want to get:
# there is a total of 3144 county-equivalent locations in the US
# we exclude Alaska, Hawaii, and DC to get a target total of
# 3144 - 30 - 5-1 = 3108
# with 11 years of data from 1999-2009, implying a target of34166 is the ideal master merge file size

# tax data 1977-2008
taxdat <- read.csv("~/papers/firm_entry/build/input/all_state_taxes.csv")
taxdat$year_L1 <- taxdat$year+1
taxdat$ptax <- taxdat$ptax*100
taxdat <- taxdat[,-c(1,13)]

# uitax data is not propery normaized in 'all_state_taxes', which we fix below
uidat <- read.xlsx("~/papers/firm_entry/build/input/uitax_fix.xlsx",1)

uidat$uitaxrate <- uidat$uitaxrate*100
uidat$year_L1 <- uidat$Year+1
uidat <- uidat[,-c(1:3)]

michdat <- read.csv("~/papers/firm_entry/build/input/michigan_corp.csv")
michdat$year_L1 <- michdat$year+1
michdat <- michdat[,-c(1)]

# now replace original uitax data
taxdat <- merge(taxdat,uidat,by.x=c("year_L1","stfip"),by.y = c("year_L1","st_fips"))

# rm(uidat)

# length(michdat$ctax) = 32
# length(taxdat$stfip == michdat$stfip_nbr & taxdat$year_L1 == michdat$year)
taxdat$corptax <- replace(taxdat$corptax,taxdat$corptax[taxdat$stfip == michdat$stfip_nbr & taxdat$year_L1 == michdat$year], michdat$ctax)

rm(uidat)
rm(michdat)

# government spending per capita, 1994-2010
govdat <- read.xlsx("~/papers/firm_entry/build/input/govtexp_L1.xlsx", "Sheet1")

govdat$stfip <- govdat$stfip_sub
govdat <- govdat[,-c(1,3,4)]

# amenities
amenities <- read.xlsx("~/papers/firm_entry/build/input/natamenf_1_.xls", 1, startRow = 105)
# need to check for duplicates

amenities <- amenities[,-c(2,5:14)]
amenities$FIPS.Code <- as.numeric(as.character(amenities$FIPS.Code))




# fips codes for all US states
fips<- read.csv("~/papers/firm_entry/build/input/BorderFips.csv")
# need to check for duplicates

fips$county <- tolower(fips$county)
fips$state <- tolower(fips$state)
fips$cofip <- fips$statefips*1000+fips$countyfips
fips <- fips[fips$county != "state total" & fips$county != "statewide",]

# the population data is weirdly formatted, where for each fips code for years before and after
# 1999 the different files do not line up, so we have to fix it
# http://www.nber.org/data/census-intercensal-county-population.html
population <- read.csv("~/papers/firm_entry/build/input/county_population.csv")
# need to check for duplicates

pop <- c()
for (i in 1:length(fips$cofip)){
  tmp <- population[population$fips == fips$cofip[i],]
  out <- tmp[1,1:10]
  
  for (j in 11:length(tmp)){
    if (is.na(tmp[1,j])){
      tmp[1,j] <- 0
    }
    if (is.na(tmp[2,j])){
      tmp[2,j] <- 0
    }
    out <- cbind(out, tmp[1,j]+tmp[2,j])
  }
  # this doesn't seem to be working as intended
  # THIS SEEMS TO BE FUCKED gets error "subscript out of bounds"
  pop <- rbind(pop, out)
}
pop <- as.data.frame(pop)
names(pop)[11:58] = names(population)[11:58]
drop <- c("county_fips", "areaname", "state_name", "county_name", "fipsst", "fipsco", "region", "division",
          "pop19904", "pop20104", "base20104" )
pop <- pop[,!names(pop) %in% drop]
rm(population)

# note, population figures are from 1970-2014, while my study is from ~1998-2010
# so while this searches over all years, it doesn't have to.
# 3108*44 = 136752 is the target
pop_out <- c()
# note data actually begins in 1970, we start in 1971 to create a lag variable!
year <- 1971
for (i in 1:length(pop[,1])) {
  for (j in 20:44){
    tmp <- cbind(year+j,pop[i,1],pop[i,2],pop[i,j+2])
    pop_out <- rbind(pop_out, tmp)
  }
}
pop_out <- as.data.frame(pop_out)
names(pop_out) = c("year","cofip","stfip","pop")
write.csv(pop_out, file = "~/papers/firm_entry/build/output/pop_1990-2014.csv")

# pairs right next to each other
border_pairs <- read.csv("~/papers/firm_entry/build/input/border_fipsCodes.csv")
border_pairs <- unique(border_pairs)

# pairs extending the bandwidth by "1" on the nbr's side ("nbr's nbr")
increased_band <- read.csv("~/papers/firm_entry/build/output/increased_bandwidth.csv") 
names(increased_band)[2] <- "year"

# NAICS codes
naics <- c("--", "11","21","22","23","31-33","42","44-45","48-49", 
           "51","52","53","54","55","56","61","62","71","72","81","95","99")

# iterate over NAICS codes
for (m in 1:length(naics)){
  # load in that years firm births/deaths characteristics
  tmp <- read.csv("~/papers/firm_entry/build/input/Edited_County_Files/county_naicssector_emplchange_1998-1999_edited.csv",
                  header = FALSE)
  # cut out the states we don't use
  tmp <- tmp[tmp$V4 == naics[m] & tmp$V3 != "STATE TOTAL"
             & tmp$V3 != "STATEWIDE" & tmp$V2 != "ALASKA"
             & tmp$V2 != "HAWAII",]
  tmp <- tmp[,-c(4)]
  # semi-normalize names
  colnames(tmp) <- c("year","state","county","base","births","deaths","expansions","contractions","stable")
  tmp$state <- tolower(tmp$state) # make terms lower case to normalize for merging
  tmp$county <- tolower(tmp$county)
  master<- merge(fips,tmp, by = c("state","county"), all.x = TRUE) # merging, while allowing there to exist 0's
  master$year[is.na(master$year)] <- 1999 # replace missing years with the year
  master[is.na(master)] <- 0 # replace everything else with a 0 for the missing values
  
  # repeat while iterating over every other year
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
  
  # start merging together the master file with government data, amenities, tax data
  master <- merge(master,govdat, by.x=c("statefips","year"),by.y=c("stfip","year"))
  master <- merge(amenities, master, by.x = c("FIPS.Code"), by.y = c("cofip"))
  master <- merge(master, taxdat, by.x = c("statefips","year"), by.y = c("stfip","year_L1"))
  master <- merge(master, pop_out, by.x = c("FIPS.Code","year"), by.y = c("cofip","year"))
  
  # master over all counties
  master <- master[c("statefips","countyfips","FIPS.Code","year","state","county",
                     "base","births","deaths","expansions","contractions",
                     "educ_pc_L1","hwy_pc_L1","welfare_pc_L1",
                     "ptax","inctax","capgntax","salestax","corptax","wctaxfixed","uitaxrate",
                     "hsplus","realfuelpr","unionmem","popdensity","pctmanuf",
                     "JAN.TEMP...Z","JAN.SUN...Z","JUL.TEMP...Z","JUL.HUM...Z","TOPOG...Z","LN.WATER..AREA...Z", "pop")]
  
  # create the master for subject and neighbor terms by copying the master and appending "sub" or "nbr" onto the end
  master_sub <- master
  names(master_sub)[7:33] <- paste(names(master_sub)[7:33],"sub",sep="_")
  
  master_nbr <- master
  names(master_nbr)[7:33] <- paste(names(master_nbr)[7:33],"nbr",sep="_")
  
  # merge together with the border_pairs
  border_master <- merge(border_pairs, master_sub, by.x = c("year","cofip_sub"), by.y = c("year","FIPS.Code"))
  border_master <- merge(border_master, master_nbr, by.x = c("year","cofip_nbr"), by.y = c("year","FIPS.Code"))
  
  # merge together with the increased_band pairs
  band_master <- merge(increased_band, master_sub, by.x = c("year","cofip_sub"), by.y = c("year","FIPS.Code"))
  band_master <- merge(band_master, master_nbr, by.x = c("year","cofip_nbr_nbr"), by.y = c("year","FIPS.Code"))
  
  write.csv(master, file = paste("~/papers/firm_entry/build/output/",naics[m],"master.csv", sep = "_")) 
  write.csv(border_master, file = paste("~/papers/firm_entry/build/output/",naics[m],"border_master.csv", sep = "_"))
  write.csv(band_master, file = paste("~/papers/firm_entry/build/output/",naics[m],"band_master.csv", sep = "_"))
}

