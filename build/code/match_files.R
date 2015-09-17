# US county congruency data
match_counties <- read.csv("~/papers/firm_entry/build/input/all_county_matches.csv")

# assigns missing cofip variables that are NA's in the original data set
tmp <- 0;
for (i in 1:length(match_counties$cofip)){
  if (is.na(match_counties$cofip[i])) {
    match_counties$cofip[i] <- tmp
  } else {
    tmp <- match_counties$cofip[i] 
  }
}
rm(tmp)

# my border-matched county data
border_pairs <- read.csv("~/papers/firm_entry/build/input/border_fipsCodes.csv")

# all already established neighbors
all_nbr <- border_pairs$cofip_nbr[border_pairs$year == 1999]
all_sub <- border_pairs$cofip_sub[border_pairs$year == 1999]

# algorithm structure
# 1. search existing subject counties for all their neighbors
# 2. search from all match_counties for all neighbors of the neighbor
# 3. eliminate neighbors that are already in either my all_sub or all_nbr categories

# final array's to be assigned to
sub_final <- c() # arrays that track the subject county
nbr_final <- c() # arrays that track the neighbor county
nbr_nbr_final <- c() # array that tracks the neighbor's neighbor counties.

# only need to search over unique cofip_sub values
for (i in 1:length(unique(border_pairs$cofip_sub))){
  # pick a subject county
  border_tmp <- border_pairs[border_pairs$cofip_sub == unique(border_pairs$cofip_sub)[i] & border_pairs$year == 1999,]
  
  # create an array of all subject specific neighbor counties
  nbr_tmp <- border_tmp$cofip_nbr
  
  # an array of neighbor's neighbors.
  nbr_nbr <- c()
  nbr <- c()
  
  # search over all neighbors
  for (k in 1:length(nbr_tmp)){
    # search over the congruency table to find match pairs
    for (j in 1:length(match_counties[,1])) {
      if (match_counties$cofip[j] == nbr_tmp[k]){
        nbr_nbr <- rbind(nbr_nbr, match_counties$nbrfip[j])
        nbr <- rbind(nbr, match_counties$cofip[j])
      }
    }
  }
  
  # remove all pairs that are already matched someplace else
  for (k in 1:length(nbr_nbr)){
    included <- FALSE
    for (j in 1:length(all_nbr)){
      if (nbr_nbr[k] == all_nbr[j]){
        included <- TRUE
      }
    }
    for (j in 1:length(all_sub)){
      if (nbr_nbr[k] == all_sub[j]){
        included <- TRUE
      }
    }
    if (included == FALSE){
      nbr_nbr_final <- rbind(nbr_nbr_final, nbr_nbr[k])
      nbr_final <- rbind(nbr_final, nbr[k])
      sub_final <- rbind(sub_final, unique(border_pairs$cofip_sub)[i])
    }
  }
}

# write as data frame
master <- as.data.frame(sub_final)
master$nbr_nbr <- nbr_nbr_final

# remove existing doubles from the setup
master$id <- sub_final*100000+nbr_nbr_final
master <- master[!duplicated(master),]
names(master)[1:2] <- c("cofip_sub","cofip_nbr_nbr")

bandwidth <- c()
# extend the data set over every year.
for (i in 1:length(unique(border_pairs$year))){
  for (j in 1:length(master[,1])){
    bandwidth <- rbind(bandwidth, c(border_pairs$year[i], master[j,1:2]))
  }
}
names(bandwidth)[1] <- "year"

# write to a csv to be used elsewhere
write.csv(bandwidth, file = "~/papers/firm_entry/build/output/increased_bandwidth.csv") 

# stuff.

test <- county.fips[ county.fips[,1] %in% master]
