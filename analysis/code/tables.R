library(multiwayvcov) # clustered standard errors
library(lfe) #felm fixed effect linear models
library(stargazer) #stargazer
library(car) #linearhypothesis
library(lmtest) #coeftest


naics <- c("--", "11","21","22","23","31-33","42","44-45","48-49", 
           "51","52","53","54","55","56","61","62","71","72","81","95","99")
naicsf <- c("00", "11","21","22","23","31_33","42","44_45","48_49", 
            "51","52","53","54","55","56","61","62","71","72","81","95","99")

naics_names <- c("Total", "Agriculture", "Mining", "Utilities",
                 "Construction", "Manufacturing", "Wholesale trade", "Retail Trade", "Transportation and Warehousing",
                 "Information", "Finance and insurance", "Real estate and rental and leasing",
                 "Professional, scientific, and technical services", "Management of companies and enterprises",
                 "Administrative and support and waste management and remediation serv", "Educational services",
                 "Health care and social assistance", "Arts, entertainment, and recreation", "Accommodation and foodservices",
                 "Other services (except public administration)", "Auxiliaries, exc corp, subsidiary, and regional managing offices",
                 "Unclassified")

stor <- matrix(,nrow = 13761,ncol = length(naics))

for (m in 1:length(naics)){  
  master <- read.csv(paste("~/papers/firm_entry/build/output/",naics[m],"border_master.csv", sep = "_"))

  # log dep. var's
  master$lnbirths_sub <- log(master$births_sub)
  master$lnbirths_nbr <- log(master$births_nbr)

  master$births_ratio <- master$lnbirths_sub - master$lnbirths_nbr
 I
  assign(paste("master",naics[m],sep="_"), master)
  stor[,m] <- master$births_ratio 
}

stor <- as.data.frame(stor[,c(1,2,6,8,11)])

dim <- dim(stor)
y <- unlist(stor)
y[ is.infinite(y)] <- NaN
stor <- matrix(y, dim)
stor <- as.data.frame(stor)
names(stor) <- naics_names[c(1,2,6,8,11)]

tmp <- cor(stor,use="pairwise.complete.obs")

stargazer(tmp, font.size = "tiny",
          title = "Correlation Between Industry Firm Entry", 
          out = "~/papers/firm_entry/analysis/output/entryCorrelations.tex" )


master <- read.csv("~/papers/firm_entry/build/output/_--_border_master.csv")

# log dep. var's
master$lnbase_sub <- log(master$base_sub)
master$lnbirths_sub <- log(master$births_sub)
master$lndeaths_sub <- log(master$deaths_sub)
master$lnexpan_sub <- log(master$expansions_sub)
master$lncontr_sub <- log(master$contractions_sub)

master$lnbase_nbr <- log(master$base_nbr)
master$lnbirths_nbr <- log(master$births_nbr)
master$lndeaths_nbr <- log(master$deaths_nbr)
master$lnexpan_nbr <- log(master$expansions_nbr)
master$lncontr_nbr <- log(master$contractions_nbr)

# ratio and raw diff dep. var's
master$base_diff <- master$base_sub - master$base_nbr
master$base_ratio <- master$lnbase_sub - master$lnbase_nbr


master$births_diff <- master$births_sub - master$births_nbr
master$births_ratio <- master$lnbirths_sub - master$lnbirths_nbr
master$deaths_diff <- master$deaths_sub-master$deaths_nbr
master$deaths_ratio <- master$lndeaths_sub - master$lndeaths_nbr
master$expansions_diff <- master$expansions_sub - master$expansions_nbr
master$expansions_ratio <- master$lnexpan_sub - master$lnexpan_nbr
master$contractions_diff <- master$lncontr_sub - master$lncontr_nbr
master$contractions_ratio <- master$contractions_sub - master$contractions_nbr
master$net_diff <- master$births_sub - master$deaths_sub - (master$births_nbr- master$deaths_nbr)



# diff tax rates and exp
master$educ_pc_L1_diff <- master$educ_pc_L1_sub - master$educ_pc_L1_nbr
master$hwy_pc_L1_diff <- master$hwy_pc_L1_sub - master$hwy_pc_L1_nbr
master$welfare_pc_L1_diff <- master$welfare_pc_L1_sub - master$welfare_pc_L1_nbr
master$ptax_diff <- master$ptax_sub - master$ptax_nbr
master$inctax_diff <- master$inctax_sub - master$inctax_nbr
master$capgntax_diff <- master$capgntax_sub - master$capgntax_nbr
master$salestax_diff <- master$salestax_sub - master$salestax_nbr
master$corptax_diff <- master$corptax_sub - master$corptax_nbr
master$wctax_diff <- master$wctaxfixed_sub - master$wctaxfixed_nbr
master$uitax_diff <- master$uitaxrate_sub - master$uitaxrate_nbr

# diff state controls
master$hsplus.cont_diff <- master$hsplus_sub - master$hsplus_nbr
master$realfuelpr.cont_diff <- master$realfuelpr_sub - master$realfuelpr_nbr
master$unionmem.cont_diff <- master$unionmem_sub - master$unionmem_nbr
master$popdensity.cont_diff <- master$popdensity_sub - master$popdensity_nbr
master$pctmanuf.cont_diff <- master$pctmanuf_sub - master$pctmanuf_nbr

# diff county geography amenities
master$JAN.TEMP.Z_diff <- master$JAN.TEMP...Z_sub - master$JAN.TEMP...Z_nbr
master$JAN.SUN.Z_diff <- master$JAN.SUN...Z_sub - master$JAN.SUN...Z_nbr
master$JUL.TEMP.Z_diff <- master$JUL.TEMP...Z_sub - master$JUL.TEMP...Z_nbr
master$JUL.HUM.Z_diff <- master$JUL.HUM...Z_sub - master$JUL.HUM...Z_nbr
master$TOPOG.Z_diff <- master$TOPOG...Z_sub - master$TOPOG...Z_nbr
master$WATER.AR.Z_diff <- master$LN.WATER..AREA...Z_sub - master$LN.WATER..AREA...Z_nbr

master$pop_diff <- log(master$pop_sub)- log(master$pop_nbr)

master$stpr_id <- paste(master$statefips.x,master$statefips.y, by = "")
# eclude data we only see once
master <- master[master$births_sub > 0 & master$births_nbr > 0,] # 13115
master <- master[table(master$stpr_id) != 11,] # 13249 for the 'all firm' case
master$fips <- master$cofip_sub*100000+master$cofip_nbr



library(data.table)

master$ptax_diff <- master$ptax_sub - master$ptax_nbr
master$inctax_diff <- master$inctax_sub - master$inctax_nbr
master$capgntax_diff <- master$capgntax_sub - master$capgntax_nbr
master$salestax_diff <- master$salestax_sub - master$salestax_nbr
master$corptax_diff <- master$corptax_sub - master$corptax_nbr
master$wctax_diff <- master$wctaxfixed_sub - master$wctaxfixed_nbr
master$uitax_diff <- master$uitaxrate_sub - master$uitaxrate_nbr

dt <- data.table(master)
dt[, c("ptaxL1_diff","inctaxL1_diff","capgntaxL1_diff","salestaxL1_diff","corptaxL1_diff","wctaxL1_diff","uitaxL1_diff")
   := list(c(NA, diff(ptax_diff)),c(NA, diff(inctax_diff)),c(NA,diff(capgntax_diff)),
           c(NA, diff(salestax_diff)),c(NA,diff(corptax_diff)),
           c(NA, diff(wctax_diff)),c(NA,diff(uitax_diff))),by = fips]

for (i in 1:length(unique(dt$stpr_id))){
  tmp <- dt[dt$stpr_id == unique(dt$stpr_id)[i],]
  plot(tmp$ptaxL1_diff + tmp$capgntaxL1_diff + tmp$salestaxL1_diff
       + tmp$corptaxL1_diff + tmp$wctaxL1_diff + tmp$uitaxL1_diff)
}

test <- lm(births_ratio ~ ptax_diff + capgntax_diff + salestax_diff + corptax_diff + wctax_diff + uitax_diff + 
             ptaxL1_diff + capgntaxL1_diff + salestaxL1_diff + corptaxL1_diff + wctaxL1_diff + uitaxL1_diff, data = dt)
# set up mean border areas to use
mean_births_sub <- c()
mean_births_nbr <- c()
dep_var <- c();
# array to use target should be 107*11 = 1177
k = 1
for (i in 1:length(unique(master$stpr_id))){
  for (j in 1:length(unique(master$year))){
    # first pull out just state-pair information
    tmp <- master[master$stpr_id == unique(master$stpr_id)[i] & master$year == unique(master$year)[j],]
    # restrict each side to only have unique county observations on each side
    tmp_sub <- tmp[!duplicated(tmp$cofip_sub),]
    tmp_nbr <- tmp[!duplicated(tmp$cofip_nbr),]
    
    # calculate means
    dep_var <- rbind(dep_var,tmp_sub[1,c(2,110)])
    mean_births_sub[k] <- mean(tmp_sub$births_sub)
    mean_births_nbr[k] <- mean(tmp_nbr$births_nbr)
    k <- k +1
  }
}
dl_master <- as.data.frame(dep_var)
dl_master$births_ratio <- log(mean_births_sub) - log(mean_births_nbr)
dl_master <- dl_master[is.finite(dl_master$births_ratio),]

# preliminary cross-correlations
var <- data.frame(master$ptax_diff, master$inctax_diff, master$capgntax_diff, 
                  master$salestax_diff, master$corptax_diff, master$wctax_diff, master$uitax_diff)
names(var) <- c("ptax diff", "inctax diff", "capgtax diff", "salestax diff", "corptax diff",
                "wctax diff", "uitax diff")
png(filename="~/papers/firm_entry/analysis/output/_--_pairs.png")
scatterplotMatrix( ~ ptax_diff + inctax_diff + capgntax_diff + 
                  salestax_diff + corptax_diff + wctax_diff + uitax_diff, data = master,
                  main = "Tax Variable Cross Correlation")
dev.off()

# imposing equality across borders
pols_namen_nc_real <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                         + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                         + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                         data = master)
stprid_c_vcov <- cluster.vcov(pols_namen_nc_real, master$stpr_id)
pols_namen_nc_r_coef <- coeftest(pols_namen_nc_real, vcov = stprid_c_vcov) # results

### RANKINGS ####
# weighted aggregate tax differentials between states
# first I look at the starting difference
firstdat <- master[ master$year == 1999,]
firstdat <- firstdat[!duplicated(firstdat$stpr_id), ]
firstdat$start <- abs(pols_namen_nc_r_coef[2]*firstdat$ptax_diff + pols_namen_nc_r_coef[3]*firstdat$inctax_diff  + pols_namen_nc_r_coef[4]*firstdat$capgntax_diff
                      + pols_namen_nc_r_coef[5]*firstdat$salestax_diff  + pols_namen_nc_r_coef[6]*firstdat$corptax_diff  + pols_namen_nc_r_coef[7]*firstdat$wctax_diff
                      + pols_namen_nc_r_coef[8]*firstdat$uitax_diff)
firstdat <-data.frame(firstdat$stpr_id, firstdat$start, firstdat$state.x, firstdat$state.y)[order(firstdat$start, decreasing = TRUE),]

# then I look at the ending difference
finaldat <- master[master$year == 2009,]
finaldat <- finaldat[ !duplicated(finaldat$stpr_id) & finaldat$stpr_id != "35 49 ", ]
#  for (i in 1:length(finaldat$stpr_id)) {
#    if (!(finaldat$stpr_id[i] %in% firstdat$firstdat.stpr_id)){
#      print(finaldat$stpr_id[i])
#    }
#  }



finaldat$finish <- abs(pols_namen_nc_r_coef[2]*finaldat$ptax_diff + pols_namen_nc_r_coef[3]*finaldat$inctax_diff  + pols_namen_nc_r_coef[4]*finaldat$capgntax_diff
                       + pols_namen_nc_r_coef[5]*finaldat$salestax_diff  + pols_namen_nc_r_coef[6]*finaldat$corptax_diff  + pols_namen_nc_r_coef[7]*finaldat$wctax_diff
                       + pols_namen_nc_r_coef[8]*finaldat$uitax_diff)

finaldat <- merge(finaldat,dl_master[dl_master$year == 2009,], by.x = "finaldat.stpr_id", by.y = "stpr_id")

par(mfrow=c(1,1))
png(filename="~/papers/firm_entry/analysis/output/_--_weightedtax.png")
qplot(abs(pols_namen_nc_r_coef[2]*ptax_diff + pols_namen_nc_r_coef[3]*inctax_diff  + pols_namen_nc_r_coef[4]*capgntax_diff
               + pols_namen_nc_r_coef[5]*salestax_diff  + pols_namen_nc_r_coef[6]*corptax_diff  + pols_namen_nc_r_coef[7]*wctax_diff
               + pols_namen_nc_r_coef[8]*uitax_diff), data=finaldat, 
           xlab = "absolute valued weighted tax differential", geom="histogram",  main = "Weighted Tax Differentials in 2009")
dev.off()

png(filename="~/papers/firm_entry/analysis/output/_--_taxdiff.png")
qplot(finish/abs(births_ratio), data=finaldat, binwidth = 0.10,
      xlab = "pct firm births ratio explained", geom="histogram",  main = "Percent Firm Births Ratio explained 2009")
dev.off()

png(filename="~/papers/firm_entry/analysis/output/_--_birthsdiff.png")
qplot(abs(births_ratio), data=finaldat,
              xlab = "Mean Firm Births Ratio", geom="histogram",  main = "Mean Firm Births Ratio in 2009")
dev.off()

tmp <- finaldat$finish/abs(finaldat$births_ratio)
hist(tmp)

# want to see which areas have seen the biggest improvement.

favors <- function(x){
  result <- c()
  if (length(x) > 1){
    for (i in 1:length(x)){
      if (x[i] > 0){
        result[i] <- "sub"
      } else{
        result[i] <- "nbr"
      }
    }
  } else{
    if (x > 0){
      result <- "sub"
    } else{
      result <- "nbr"
    }
  }
  return(result)
}

preftax <- favors(pols_namen_nc_r_coef[2]*finaldat$ptax_diff + pols_namen_nc_r_coef[3]*finaldat$inctax_diff  + pols_namen_nc_r_coef[4]*finaldat$capgntax_diff
                  + pols_namen_nc_r_coef[5]*finaldat$salestax_diff  + pols_namen_nc_r_coef[6]*finaldat$corptax_diff  + pols_namen_nc_r_coef[7]*finaldat$wctax_diff
                  + pols_namen_nc_r_coef[8]*finaldat$uitax_diff)


finaldat <- data.frame(finaldat$stpr_id,finaldat$finish, finaldat$state.x, finaldat$state.y)[order(finaldat$finish, decreasing = TRUE),]

datmerge <- merge(firstdat, finaldat, by.x = c("firstdat.stpr_id","firstdat.state.x","firstdat.state.y"), 
                  by.y = c("finaldat.stpr_id","finaldat.state.x","finaldat.state.y"))
datmerge <- datmerge[order(datmerge$firstdat.start, decreasing = TRUE),-c(1)]
names(datmerge) <- c("sub state", "nbr state","weighted tax 1999", "weighted tax 2009")

stargazer(datmerge[1:50,], summary = FALSE, rownames = FALSE, font.size = "tiny",
          title = "Top 50 Weighted Tax Differentials for Total Firm Births", 
          out = "~/papers/firm_entry/analysis/output/_--_wtr.tex")

mean_starts <- c(1:(length(unique(master$stpr_id))-1))

for (i in 1:length(mean_starts)){
  temp_dat <- master[master$stpr_id == unique(master$stpr_id)[i],]
  mean_starts[i] <- sum(temp_dat$births_ratio[complete.cases(temp_dat$births_ratio)])/length(temp_dat[,1])
}


prefstart <- favors(mean_starts)
dif <- function(x,y) {
  results <- c()
  for (i in 1:length(x)){
    if (x[i] == y[i]) {
      results[i] <- "same"
    } else{
      results[i] <- "different"
    }
  }
  return(results)
}

difbin <- function(x,y) {
  results <- c()
  for (i in 1:length(x)){
    if (x[i] == y[i]) {
      results[i] <- 1
    } else{
      results[i] <- 0
    }
  }
  return(results)
}

# what percent of results line up with our test
sum(difbin(prefstart, preftax))/107

# this is what is throwing the error
mean <- data.frame(abs(mean_starts), prefstart, finaldat$finaldat.finish, preftax, dif(prefstart,preftax), finaldat$finaldat.state.x, finaldat$finaldat.state.y)[order(abs(mean_starts), decreasing = TRUE),]
names(mean) <- c("mean firm entry", "preffered side", "abs weighted tax", "preferred side", "same?","sub state", "nbr state")
stargazer(mean[1:10,], summary = FALSE, rownames = FALSE, font.size = "tiny",
          title = "Result Comparison for Total Firm Births", 
          out = "~/papers/firm_entry/analysis/output/_--_meantable.tex")
names(mean) <- c("V1","V2","V3","V4","V5","V6","V7")
mean <- setorder(mean, -V3)
names(mean) <- c("mean firm entry", "preffered side", "abs weighted tax", "preferred side", "same?","sub state", "nbr state")
stargazer(mean[1:10,], summary = FALSE, rownames = FALSE, font.size = "tiny",
          title = "Result Comparison for Estimated Firm Enry", 
          out = "~/papers/firm_entry/analysis/output/_--_taxtable.tex")

