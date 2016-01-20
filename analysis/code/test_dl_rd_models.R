library(multiwayvcov) # clustered standard errors
library(lfe) #felm fixed effect linear models
library(stargazer) #stargazer
library(car) #linearhypothesis
library(lmtest) #coeftest
library(boot) # bootstrap

naics <- c("--", "11","21","22","23","31-33","42","44-45","48-49", 
           "51","52","53","54","55","56","61","62","71","72","81","95","99")
naicsf <- c("00", "11","21","22","23","31_33","42","44_45","48_49", 
            "51","52","53","54","55","56","61","62","71","72","81","95","99")

naics_names <- c("Total", "Agriculture, forestry, fishing, and hunting", "Mining", "Utilities",
                 "Construction", "Manufacturing", "Wholesale trade", "Retail Trade", "Transportation and Warehousing",
                 "Information", "Finance and insurance", "Real estate and rental and leasing",
                 "Professional, scientific, and technical services", "Management of companies and enterprises",
                 "Administrative and support and waste management and remediation serv", "Educational services",
                 "Health care and social assistance", "Arts, entertainment, and recreation", "Accommodation and foodservices",
                 "Other services (except public administration)", "Auxiliaries, exc corp, subsidiary, and regional managing offices",
                 "Unclassified")

taxes <- c("property","income","capital gains","sales","corporate","workers compensation", "unemployment insurance")

m = 1
master <- read.csv(paste("~/papers/firm_entry/build/output/",naics[m],"border_master.csv", sep = "_"))
  
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

#scatterplot.matrix(~births_ratio+ptax_diff + inctax_diff + capgntax_diff + salestax_diff  + corptax_diff  +wctax_diff+ uitax_diff, data = master)
  
  #######################################################
  # morph dataset into donald and lang (2007) estimator
  #######################################################
  
# regular estimates
pols_namen_nc_real <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                         + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                         + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                         data = master)
stprid_c_vcov <- cluster.vcov(pols_namen_nc_real, master$stpr_id)
pols_namen_nc_r_coef <- coeftest(pols_namen_nc_real, vcov = stprid_c_vcov) # results  


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
    dep_var <- rbind(dep_var,tmp_sub[1,87:110])
    mean_births_sub[k] <- mean(tmp_sub$births_sub)
    mean_births_nbr[k] <- mean(tmp_nbr$births_nbr)
    k <- k +1
  }
}
dl_master <- as.data.frame(dep_var)
dl_master$births_ratio <- log(mean_births_sub) - log(mean_births_nbr)
dl_master <- dl_master[is.finite(dl_master$births_ratio),]
  
#scatterplot.matrix(~births_ratio+ptax_diff + inctax_diff + capgntax_diff + salestax_diff  + corptax_diff  +wctax_diff+ uitax_diff, data = dl_master)
  
dl_namen_nc_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                    + salestax_diff  + corptax_diff + wctax_diff  + uitax_diff
                    + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff, data = dl_master)
stprid_c_vcov <- cluster.vcov(dl_namen_nc_r,  dl_master$stpr_id)
dl_namen_nc_r_coef <- coeftest(dl_namen_nc_r, vcov = stprid_c_vcov)
  
# resampling

# cluster bootstrap function
clusbootreg <- function(formula, data, cluster, reps){
  reg1 <- lm(formula, data)
  clusters <- names(table(cluster))
  sterrs <- matrix(NA, nrow=reps, ncol=length(coef(reg1)))
  for(i in 1:reps){
    index <- sample(1:length(clusters), length(clusters), replace=TRUE)
    aa <- clusters[index]
    bb <- table(aa)
    bootdat <- NULL
    for(j in 1:max(bb)){
      cc <- data[cluster %in% names(bb[bb %in% j]),]
      for(k in 1:j){
        bootdat <- rbind(bootdat, cc)
      }
    }
    sterrs[i,] <- coef(lm(formula, bootdat))
  }
  val <- cbind(apply(sterrs,2,mean),apply(sterrs,2,sd),coef(reg1)/apply(sterrs,2,sd))
  colnames(val) <- c("Estimate","Std. Error","t-stat")
  return(val)
}

dl_boot <- clusbootreg(formula = births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
            + salestax_diff  + corptax_diff   + wctax_diff + uitax_diff
            + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff, 
            data = dl_master, cluster = dl_master$stpr_id, reps = 1000)

ado_boot <-clusbootreg(formula = births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                       + salestax_diff  + corptax_diff   + wctax_diff + uitax_diff
                       + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff, 
                       data = master, cluster = master$stpr_id, reps = 1000)

dl_boot
ado_boot

# old code
loop <- 4000
dlTaxrslt <- matrix(, nrow = 7, ncol = loop)

  
for (i in 1:loop){
  tmpDat <- dl_master[sample(nrow(dl_master), 500),]
  tmp_dl <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                      + salestax_diff  + corptax_diff   + wctax_diff + uitax_diff
                      + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff, data = tmpDat)
  stprid_c_vcov <- cluster.vcov(tmp_dl,  tmpDat$stpr_id)
  dlTaxrslt[,i] <- coeftest(tmp_dl, vcov = stprid_c_vcov)[2:8]
}

taxrslt <- matrix(, nrow = 7, ncol = loop)
  
for (i in 1:loop){
  tmpDat <- master[sample(nrow(master), 3000),]
  tmp_dl <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
               + salestax_diff  + corptax_diff   + wctax_diff + uitax_diff
               + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff, data = tmpDat)
  stprid_c_vcov <- cluster.vcov(tmp_dl,  tmpDat$stpr_id)
  taxrslt[,i] <- coeftest(tmp_dl, vcov = stprid_c_vcov)[2:8]
}
  
for (i in 1:7){
  hist(taxrslt[i,], main = paste(naics[m], "Regular Estimates", taxes[i], sep = " "))
  hist(dlTaxrslt[i,], main = paste(naics[m], "DL estimates", taxes[i], sep = " "))
}
  
mean <- c(1:7)
sd <- c(1:7)
CI_upper <- c(1:7)
CI_lower <- c(1:7)

dlMean <- c(1:7)
dlSD <- c(1:7)
dlCI_upper <- c(1:7)
dlCI_lower <- c(1:7)
print("results for resample")
for (i in 1:7){
  mean[i] <- mean(taxrslt[i,])
  sd[i] = sqrt(1/(loop-1)*sum((taxrslt[i,]-mean[i])^(2)))
  CI_upper[i] <- mean[i] + 1.97*sd[i]
  CI_lower[i] <- mean[i] - 1.97*sd[i]
  
  dlMean[i] <- mean(dlTaxrslt[i,])    
  dlSD[i] = sqrt(1/(loop-1)*sum((dlTaxrslt[i,]-dlMean[i])^(2)))
  dlCI_upper[i] <- dlMean[i] + 1.97*dlSD[i]
  dlCI_lower[i] <- dlMean[i] - 1.97*dlSD[i]
  
  if (dl_namen_nc_r_coef[i] > quantile(taxrslt[i,], probs = c(0.025, 0.975))[1] & dl_namen_nc_r_coef[i] < quantile(taxrslt[i,], probs = c(0.025, 0.975))[2]){
    print("dl in reg result")
  } else{
    print("dl not in reg result")
  }
  
  if (pols_namen_nc_r_coef[i] > quantile(dlTaxrslt[i,], probs = c(0.025, 0.975))[1] & pols_namen_nc_r_coef[i] < quantile(dlTaxrslt[i,], probs = c(0.025, 0.975))[2]){
    print("reg reslt in dl")
  } else{
    print("reg reslt not in dl")
  }
}

print("Original CI results")
for (i in 2:8){  
  if (dl_namen_nc_r_coef[i,1] > pols_namen_nc_r_coef[i,1]-1.97*pols_namen_nc_r_coef[i,2] & dl_namen_nc_r_coef[i,1] < pols_namen_nc_r_coef[i,1]+1.97*pols_namen_nc_r_coef[i,2]){
    print("dl in reg result")
  } else{
    print("dl not in reg result")
  }
  
  if (pols_namen_nc_r_coef[i,1] > dl_namen_nc_r_coef[i,1]-1.97*dl_namen_nc_r_coef[i,2] & pols_namen_nc_r_coef[i] < dl_namen_nc_r_coef[i,1]+1.97*dl_namen_nc_r_coef[i,2]){
    print("reg reslt in dl")
  } else{
    print("reg reslt not in dl")
  }
}


rslt <- matrix(,nrow = 14, ncol = 4)
for (i in 1:7){
  rslt[i,1] <- mean[i]
  rslt[i,3] <- sd[i]
  rslt[i,2] <- pols_namen_nc_r_coef[i+1,1]
  rslt[i,4] <- pols_namen_nc_r_coef[i+1,2]
  
  rslt[i+7,1] <- dlMean[i]
  rslt[i+7,3] <- dlSD[i]
  rslt[i+7,2] <- dl_namen_nc_r_coef[i+1,1]
  rslt[i+7,4] <- dl_namen_nc_r_coef[i+1,2]
}
rslt <- as.data.frame(rslt)
names(rslt) <- c("bootstrap mean", "in sample fit", "bootstrap SD", "in sample SD")

rslt

bootstrapCI <- matrix(,nrow=7,ncol = 8)
for(i in 1:7){
  mean[i] <- mean(taxrslt[i,])
  sd[i] = sqrt(1/(loop-1)*sum((taxrslt[i,]-mean[i])^(2)))
  CI_upper[i] <- mean[i] + 1.97*sd[i]
  CI_lower[i] <- mean[i] - 1.97*sd[i]
  
  dlMean[i] <- mean(dlTaxrslt[i,])    
  dlSD[i] = sqrt(1/(loop-1)*sum((dlTaxrslt[i,]-dlMean[i])^(2)))
  dlCI_upper[i] <- dlMean[i] + 1.97*dlSD[i]
  dlCI_lower[i] <- dlMean[i] - 1.97*dlSD[i]
  
  bootstrapCI[i,1] <- mean[i]
  bootstrapCI[i,2] <- CI_lower[i]
  bootstrapCI[i,3] <- CI_upper[i]
  bootstrapCI[i,4] <- mean[i]/sd[i]
  bootstrapCI[i,5] <- dlMean[i]
  bootstrapCI[i,6] <- dlCI_lower[i]
  bootstrapCI[i,7] <- dlCI_upper[i]
  bootstrapCI[i,8] <- dlMean[i]/dlSD[i]
}

bootstrapCI <- as.data.frame(bootstrapCI)
names(bootstrapCI) <- c("ADO mean", "ADO lower CI", "ADO upper CI", "t-stat",
                        "DL mean", "DL lower CI", "DL upper CI", "t - stat")
bootstrapCI$tax <- c("property", "income","capital gains","sales", "corporate","wc","unemp. ins.")

bootstrapCI
