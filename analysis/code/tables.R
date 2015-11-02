library(multiwayvcov) # clustered standard errors
library(lfe) #felm fixed effect linear models
library(stargazer) #stargazer
library(car) #linearhypothesis
library(lmtest) #coeftest

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


par(mfrow=c(1,1))
plot=qplot(abs(pols_namen_nc_r_coef[2]*ptax_diff + pols_namen_nc_r_coef[3]*inctax_diff  + pols_namen_nc_r_coef[4]*capgntax_diff
               + pols_namen_nc_r_coef[5]*salestax_diff  + pols_namen_nc_r_coef[6]*corptax_diff  + pols_namen_nc_r_coef[7]*wctax_diff
               + pols_namen_nc_r_coef[8]*uitax_diff), data=finaldat, 
           xlab = "absolute valued weighted tax differential", geom="histogram",  main = "Weighted Tax Differentials in 2008") 
ggsave(plot,file="~/papers/firm_entry/analysis/output/_--_weightedtax.png")

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
