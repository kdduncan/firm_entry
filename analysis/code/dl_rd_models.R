library(multiwayvcov) # clustered standard errors
library(lfe) #felm fixed effect linear models
library(stargazer) #stargazer
library(car) #linearhypothesis
library(lmtest) #coeftest
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

for (m in 1:length(naics)){
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
      rm(tmp)
      
      # calculate means
      dep_var <- rbind(dep_var,tmp_sub[87:110])
      mean_births_sub[k] <- mean(tmp_sub$births_sub)
      mean_births_nbr[k] <- mean(tmp_nbr$births_nbr)
      k <- k +1
    }
  }
  dl_master <- as.data.frame(dep_var)
  dl_master$births_ratio <- log(mean_births_sub) - log(mean_births_nbr)
  dl_master <- dl_master[is.finite(dl_master$births_ratio),]
  
  scatterplot.matrix(~births_ratio+ptax_diff + inctax_diff + capgntax_diff + salestax_diff  + corptax_diff  +wctax_diff+ uitax_diff, data = dl_master)
  
  dl_amen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                    + salestax_diff  + corptax_diff  + uitax_diff
                    + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                    + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                    + popdensity.cont_diff + pctmanuf.cont_diff + JAN.TEMP.Z_diff
                    + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                    + TOPOG.Z_diff + WATER.AR.Z_diff, data = dl_master)
  stprid_c_vcov <- cluster.vcov(dl_amen_c_r, dl_master$stpr_id)
  dl_amen_c_r_coef <- coeftest(dl_amen_c_r, vcov = stprid_c_vcov)
  
  dl_namen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                     + salestax_diff  + corptax_diff + uitax_diff
                     + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                     + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                     + popdensity.cont_diff + pctmanuf.cont_diff, data = dl_master)
  stprid_c_vcov <- cluster.vcov(dl_namen_c_r,  dl_master$stpr_id)
  dl_namen_c_r_coef <- coeftest(dl_namen_c_r, vcov = stprid_c_vcov)
  
  dl_amen_nc_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                     + salestax_diff  + corptax_diff  + uitax_diff
                     + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                     + JAN.TEMP.Z_diff + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                     + TOPOG.Z_diff + WATER.AR.Z_diff, data = dl_master)
  stprid_c_vcov <- cluster.vcov(dl_amen_nc_r, dl_master$stpr_id)
  dl_amen_nc_r_coef <- coeftest(dl_amen_nc_r, vcov = stprid_c_vcov)
  
  dl_namen_nc_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                      + salestax_diff  + corptax_diff   + uitax_diff
                      + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff, data = dl_master)
  stprid_c_vcov <- cluster.vcov(dl_namen_nc_r,  dl_master$stpr_id)
  dl_namen_nc_r_coef <- coeftest(dl_namen_nc_r, vcov = stprid_c_vcov)
  
  # iv estimation
  simple.pop.1s<- lm(pop_diff ~ WATER.AR.Z_diff, data = dl_master)
  # very significant, but R^2 only about 0.04, so probably a weak instrument?
  
  dl_master$pop.pred<- predict(simple.pop.1s, newdata = dl_master)
  dl.pop.2s<- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                 + salestax_diff  + corptax_diff  + uitax_diff
                 + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                 + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                 + popdensity.cont_diff + pctmanuf.cont_diff + pop.pred, data = dl_master)
  stprid_c_vcov <- cluster.vcov(dl.pop.2s,  dl_master$stpr_id)
  dl.pop.2s_coef <- coeftest(dl.pop.2s, vcov = stprid_c_vcov)
  
  # fixed effect model
  dl_master$stpr_fe <- factor(dl_master$stpr_id)
  
  dl_amen_fe  <- felm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                      + salestax_diff  + corptax_diff   + uitax_diff
                      + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                      + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                      + popdensity.cont_diff + pctmanuf.cont_diff  + JAN.TEMP.Z_diff
                      + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                      + TOPOG.Z_diff + WATER.AR.Z_diff  | stpr_fe | 0 | 0, data = dl_master)
  
  dl_fe <- felm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                + salestax_diff  + corptax_diff  + uitax_diff
                + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                + popdensity.cont_diff + pctmanuf.cont_diff | stpr_fe | 0 | 0, data = dl_master)
  
  write(stargazer(dl_amen_c_r_coef, dl_amen_nc_r_coef, dl_namen_c_r_coef,dl_namen_nc_r_coef, dl_amen_fe, dl_fe,
                  dep.var.labels = c("births ratio"), model.names = FALSE,
                  covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                       "Sales Tax Difference", "Corp Tax Difference",
                                       "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                       "Welfare Spending Per Cap Diff"),
                  omit = c("cont","Z"), omit.labels = c('controls', 'amenities'), omit.yes.no = c("Yes", "No"), omit.table.layout = "sn",
                  column.labels = c("OLS","OLS","OLS","OLS","FE"),
                  no.space = TRUE, title = paste("Regression Discontinuity Models for ", naics_names[m], "Firm Births using Donald and Lang (2007)", sep = " ")), 
        paste("~/papers/firm_entry/analysis/output/", naics[m], "dl_rd_results.tex", sep = "_"))

  # resampling
  loop <- 4000
  dlTaxrslt <- matrix(, nrow = 7, ncol = loop)
  library(boot)
  
  for (i in 1:loop){
    tmpDat <- dl_master[sample(nrow(dl_master), 500),]
    tmp_dl <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                        + salestax_diff  + corptax_diff   + wctax_diff + uitax_diff
                        + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff, data = tmpDat)
    stprid_c_vcov <- cluster.vcov(tmp_dl,  tmpDat$stpr_id)
    dlTaxrslt[,i] <- coeftest(tmp_dl, vcov = stprid_c_vcov)[1:7]
  }

  taxrslt <- matrix(, nrow = 7, ncol = loop)
  
  for (i in 1:loop){
    tmpDat <- master[sample(nrow(master), 3000),]
    tmp_dl <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                 + salestax_diff  + corptax_diff   + wctax_diff + uitax_diff
                 + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff, data = tmpDat)
    stprid_c_vcov <- cluster.vcov(tmp_dl,  tmpDat$stpr_id)
    taxrslt[,i] <- coeftest(tmp_dl, vcov = stprid_c_vcov)[1:7]
  }
  
  for (i in 1:7){
    hist(taxrslt[i,], main = paste(naics[m], "Regular Estimates", sep = " "))
  }
  
  mean <- c(1:7)
  for (i in 1:7){
    mean[i] <- mean(taxrslt[i,])
  }
  
  mse <- c(1:7)
  for (i in 1:7){
    mse[i] = sqrt(1/(loop-1)*sum((taxrslt[i,]-mean[i])^(2)))
  }
  
  CI_upper <- c(1:7)
  CI_lower <- c(1:7)
  for (i in 1:7){
    CI_upper[i] <- mean[i] + 1.97*mse[i]
    CI_lower[i] <- mean[i] - 1.97*mse[i]
  }
  
  for (i in 1:7){
    hist(taxrslt[i,])
  }
  
  dlMean <- c(1:7)
  for (i in 1:7){
    dlMean[i] <- mean(dlTaxrslt[i,])
  }
  
  dlMSE <- c(1:7)
  for (i in 1:7){
    dlMSE[i] = sqrt(1/(loop-1)*sum((dlTaxrslt[i,]-dlMean[i])^(2)))
  }

  dlCI_upper <- c(1:7)
  dlCI_lower <- c(1:7)
  for (i in 1:7){
    dlCI_upper[i] <- dlMean[i] + 1.97*dlMSE[i]
    dlCI_lower[i] <- dlMean[i] - 1.97*dlMSE[i]
  }
  for (i in 1:7){
    if (dl_namen_nc_r_coef[i] > quantile(taxrslt[i,], probs = c(0.025, 0.975))[1] & dl_namen_nc_r_coef[i] < quantile(taxrslt[i,], probs = c(0.025, 0.975))[2]){
      print(TRUE)
    } else{
      print(FALSE)
    }
  }
  
  for (i in 1:7){
    if (pols_namen_nc_r_coef[i] > quantile(dlTaxrslt[i,], probs = c(0.025, 0.975))[1] & pols_namen_nc_r_coef[i] < quantile(dlTaxrslt[i,], probs = c(0.025, 0.975))[2]){
      print(TRUE)
    } else{
      print(FALSE)
    }
  }
  
  ##### EXTEND THE BANDWIDTH
  
  # now we repeat for the expanded rd-design
  master <- read.csv(paste("~/papers/firm_entry/build/output/",naics[m],"band_master.csv", sep = "_"))
  
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
  
  #### morph dataset into donald and lang (2007) estimator
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
      rm(tmp)
      
      # calculate means
      dep_var <- rbind(dep_var,tmp_sub[88:110])
      mean_births_sub[k] <- mean(tmp_sub$births_sub) 
      mean_births_nbr[k] <- mean(tmp_nbr$births_nbr)
      k <- k +1
    }
  }
  dl_master <- as.data.frame(dep_var)
  dl_master$births_ratio <- log(mean_births_sub) - log(mean_births_nbr)
  dl_master <- dl_master[is.finite(dl_master$births_ratio),]
  
  dl_amen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                    + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                    + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                    + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                    + popdensity.cont_diff + pctmanuf.cont_diff + JAN.TEMP.Z_diff
                    + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                    + TOPOG.Z_diff + WATER.AR.Z_diff, data = dl_master)
  stprid_c_vcov <- cluster.vcov(dl_amen_c_r, dl_master$stpr_id)
  dl_amen_c_r_coef <- coeftest(dl_amen_c_r, vcov = stprid_c_vcov)
  
  dl_namen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                     + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                     + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                     + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                     + popdensity.cont_diff + pctmanuf.cont_diff, data = dl_master)
  stprid_c_vcov <- cluster.vcov(dl_namen_c_r,  dl_master$stpr_id)
  dl_namen_c_r_coef <- coeftest(dl_namen_c_r, vcov = stprid_c_vcov)
  
  dl_amen_nc_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                     + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                     + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                     + JAN.TEMP.Z_diff + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                     + TOPOG.Z_diff + WATER.AR.Z_diff, data = dl_master)
  stprid_c_vcov <- cluster.vcov(dl_amen_nc_r, dl_master$stpr_id)
  dl_amen_nc_r_coef <- coeftest(dl_amen_nc_r, vcov = stprid_c_vcov)
  
  dl_namen_nc_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                      + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                      + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff, data = dl_master)
  stprid_c_vcov <- cluster.vcov(dl_namen_nc_r,  dl_master$stpr_id)
  dl_namen_nc_r_coef <- coeftest(dl_namen_nc_r, vcov = stprid_c_vcov)
  
  # iv estimation
  simple.pop.1s<- lm(pop_diff ~ WATER.AR.Z_diff, data = dl_master)
  # very significant, but R^2 only about 0.04, so probably a weak instrument?
  
  dl_master$pop.pred<- predict(simple.pop.1s, newdata = dl_master)
  dl.pop.2s<- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                 + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                 + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                 + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                 + popdensity.cont_diff + pctmanuf.cont_diff + pop.pred, data = dl_master)
  stprid_c_vcov <- cluster.vcov(dl.pop.2s,  dl_master$stpr_id)
  dl.pop.2s_coef <- coeftest(dl.pop.2s, vcov = stprid_c_vcov)
  
  # fixed effect model
  dl_master$stpr_fe <- factor(dl_master$stpr_id)
  
  dl_amen_fe  <- felm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                      + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                      + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                      + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                      + popdensity.cont_diff + pctmanuf.cont_diff  + JAN.TEMP.Z_diff
                      + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                      + TOPOG.Z_diff + WATER.AR.Z_diff  | stpr_fe | 0 | 0, data = dl_master)
  
  dl_fe <- felm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                + popdensity.cont_diff + pctmanuf.cont_diff | stpr_fe | 0 | 0, data = dl_master)
  
  write(stargazer(dl_amen_c_r_coef, dl_amen_nc_r_coef, dl_namen_c_r_coef, dl_namen_nc_r_coef,   dl_amen_fe, dl_fe, 
                  dep.var.labels = c("births ratio"),
                  covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                       "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                       "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                       "Welfare Spending Per Cap Diff"),
                  omit = c("cont","Z"), omit.labels = c("controls", "amenities"), omit.table.layout = "sn",
                  column.labels = c("OLS","OLS","OLS","OLS","FE","FE"),
                  no.space = TRUE, title = paste("DL Extended Bandwidth Models for ", naics_names[m], "Firm Births using Donald and Lang (2007)", sep = " ")), 
        paste("~/papers/firm_entry/analysis/output/", naics[m], "dl_eb_results.tex", sep = "_"))
  rm(master)
}

