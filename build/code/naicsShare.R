year <- c("98","99","00","01","02","03","04","05","06","07","08")
yearfull <- c("1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008")

ag <- c()
manuf <- c()
retail <- c()
finance <- c()

for (i in 1:length(year)){
  # load in each years cbp file
  # merge with raw fips file to get all counties
  # calculate the total number of establishments per naics grouping
  # estimate shares per county
  # add to mega-file
  
  tmp <- read.csv(paste("~/Downloads/cbp/cbp",year[i],"co.txt",sep=""))
  
  tmp <- tmp[,c("fipstate","fipscty","naics","est")]
  tmp1 <- tmp[tmp$naics  == "11----",]
  tmp2 <- tmp[tmp$naics  %in% c("31----","32----","33----"),]
  tmp3 <- tmp[tmp$naics  %in% c("44----","45----"),]
  tmp4 <- tmp[tmp$naics  == "52----",]
  
  fips<- read.csv("~/papers/firm_entry/build/input/BorderFips.csv")
  
  tmp1 <- merge(tmp1,fips,by.x = c("fipstate","fipscty"),by.y = c("statefips","countyfips"), all.y = TRUE)
  tmp1 <- tmp1[!(tmp1$fipscty %in% c(0,999)),c("fipstate","fipscty","naics","est")]
  tmp1$naics <- "11"
  tmp1$year <- yearfull[i]
  tmp1$est[is.na(tmp1$est)] <- 0
  
  tmp2 <- merge(tmp2,fips,by.x = c("fipstate","fipscty"),by.y = c("statefips","countyfips"), all.y = TRUE)
  tmp2 <- tmp2[!(tmp2$fipscty %in% c(0,999)),c("fipstate","fipscty","naics","est")]
  tmp2$naics <- "31-33"
  tmp2$year <- yearfull[i]
  tmp2$est[is.na(tmp2$est)] <- 0
  
  tmp3 <- merge(tmp3,fips,by.x = c("fipstate","fipscty"),by.y = c("statefips","countyfips"), all.y = TRUE)
  tmp3 <- tmp3[!(tmp3$fipscty %in% c(0,999)),c("fipstate","fipscty","naics","est")]
  tmp3$naics <- "44-45"
  tmp3$year <- yearfull[i]
  tmp3$est[is.na(tmp3$est)] <- 0
  
  tmp4 <- merge(tmp4,fips,by.x = c("fipstate","fipscty"),by.y = c("statefips","countyfips"), all.y = TRUE)
  tmp4 <- tmp4[!(tmp4$fipscty %in% c(0,999)),c("fipstate","fipscty","naics","est")]
  tmp4$naics <- "52"
  tmp4$year <- yearfull[i]
  tmp4$est[is.na(tmp4$est)] <- 0
  
  # add up total number of est across all counties
  agTotal <- sum(tmp1$est)
  manufTotal <- sum(tmp2$est)
  retailTotal <- sum(tmp3$est)
  financeTotal <- sum(tmp4$est)
  
  # calculate shares
  tmp1$agShare <- tmp1$est/agTotal*100
  tmp2$manufShare <- tmp2$est/manufTotal*100
  tmp3$retailShare <- tmp3$est/retailTotal*100
  tmp4$financeShare <- tmp4$est/financeTotal*100
  
  ag <- rbind(ag,tmp1)
  manuf <- rbind(manuf,tmp2)
  retail <- rbind(retail,tmp3)
  finance <- rbind(finance,tmp4)
}

ag <- ag[,-c(3)]
manuf <- manuf[,-c(3)]
retail <- retail[,-c(3)]
finance <- finance[,-c(3)]

total <- merge(ag,manuf,by = c("fipstate","fipscty","year"))
drops <- c("est.x","est.y")
total <- total[,!(names(total) %in% drops)]
total <- merge(total,retail,by = c("fipstate","fipscty","year"))
total <- total[,!(names(total) %in% drops)]
total <- merge(total,finance,by = c("fipstate","fipscty","year"))
total <- total[,!(names(total) %in% drops)]
total$cofip <- total$fipstate*1000+total$fipscty
total <- total[,-c(1,2)]
total$year <- as.numeric(total$year) - 1

write.csv(total, file ="~/papers/firm_entry/build/output/share.csv") 
