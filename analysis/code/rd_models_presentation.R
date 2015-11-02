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

  
  sum <- subset(master, select = c("births_ratio", "ptax_diff", "inctax_diff", "capgntax_diff",
                                   "salestax_diff","corptax_diff","wctax_diff","uitax_diff",
                                   "educ_pc_L1_diff","hwy_pc_L1_diff","welfare_pc_L1_diff",
                                   "hsplus.cont_diff","realfuelpr.cont_diff","unionmem.cont_diff",
                                   "popdensity.cont_diff","pctmanuf.cont_diff","JAN.TEMP.Z_diff",
                                   "JAN.SUN.Z_diff","JUL.TEMP.Z_diff","JUL.HUM.Z_diff",
                                   "TOPOG.Z_diff","WATER.AR.Z_diff"))
  stargazer(sum, out = paste("~/papers/firm_entry/analysis/output/presentation/", naics[m], "summary_table.tex", sep = "_"),
            label = paste(naics[m],"summary", sep = ""),
            covariate.labels = c("Births Ratio","Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                 "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                 "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                 "Welfare Spending Per Cap Diff","Pct Highschool","Real Fuel Price","Pct Union",
                                 "Pop Density", "Pct Manuf", "Jan Temp Z Diff", "Jan Sun Z Diff", "Jul Temp Z Diff",
                                 "Jul Hum Z Diff", "Topog Z Diff", "Ln Water Z Diff"),
            no.space = TRUE, title = paste("Summary Table for ", naics_names[m], "Firm Births", sep = " "))
  
  #######################
  # BIRTHS RATIO MODELS #
  #######################
  # normal likelihood, real valued dep var, with amenities, not imposing equality
  pols1_amen_real <- lm(births_ratio ~  ptax_sub + ptax_nbr + inctax_sub +  inctax_nbr
                        + capgntax_sub + capgntax_nbr + salestax_sub + salestax_nbr +
                          corptax_sub + corptax_nbr +  wctaxfixed_sub + wctaxfixed_nbr +
                          uitaxrate_sub + uitaxrate_nbr 
                        + educ_pc_L1_sub+educ_pc_L1_nbr+ hwy_pc_L1_sub+ 
                          hwy_pc_L1_nbr + welfare_pc_L1_sub +  welfare_pc_L1_nbr +
                          JAN.TEMP...Z_sub + JAN.TEMP...Z_nbr
                        + JAN.SUN...Z_sub + JAN.SUN...Z_nbr
                        + JUL.TEMP...Z_sub + JUL.TEMP...Z_nbr
                        + JUL.HUM...Z_sub + JUL.HUM...Z_nbr
                        + TOPOG...Z_sub + TOPOG...Z_nbr
                        + LN.WATER..AREA...Z_sub + LN.WATER..AREA...Z_nbr, data = master)
  stprid_c_vcov <- cluster.vcov(pols1_amen_real, master$stpr_id)
  pols1_amen_r_coef <- coeftest(pols1_amen_real, vcov = stprid_c_vcov) # results
  
  
  # without imposing equality, real valued, no amenities
  pols1_real <- lm(births_ratio ~  ptax_sub + ptax_nbr + inctax_sub +  inctax_nbr
                   + capgntax_sub + capgntax_nbr + salestax_sub + salestax_nbr +
                     corptax_sub + corptax_nbr +  wctaxfixed_sub + wctaxfixed_nbr +
                     uitaxrate_sub + uitaxrate_nbr 
                   + educ_pc_L1_sub+educ_pc_L1_nbr+ hwy_pc_L1_sub+ 
                     hwy_pc_L1_nbr + welfare_pc_L1_sub +  welfare_pc_L1_nbr, data = master)
  stprid_c_vcov <- cluster.vcov(pols1_real, master$stpr_id)
  pols1_r_coef <- coeftest(pols1_real, vcov = stprid_c_vcov)
  
  # tests for equality
  ptaxjoint <- linearHypothesis(pols1_real, c("ptax_sub = -ptax_nbr"), vcov = stprid_c_vcov)
  inctaxjoint <- linearHypothesis(pols1_real, c("inctax_sub = -inctax_nbr"), vcov = stprid_c_vcov)
  capgntaxjoint <- linearHypothesis(pols1_real, c("capgntax_sub = -capgntax_nbr"), vcov = stprid_c_vcov)
  salestaxjoint <- linearHypothesis(pols1_real, c("salestax_sub = -salestax_nbr"), vcov = stprid_c_vcov)
  corptaxjoint <- linearHypothesis(pols1_real, c("corptax_sub = -corptax_nbr"), vcov = stprid_c_vcov)
  wctaxjoint <- linearHypothesis(pols1_real, c("wctaxfixed_sub = -wctaxfixed_nbr"), vcov = stprid_c_vcov)
  uitaxjoint <- linearHypothesis(pols1_real, c("uitaxrate_sub = -uitaxrate_nbr"), vcov = stprid_c_vcov)
  
  ftest <- matrix(c("ptax_sub = -ptax_nbr","inctax_sub = -inctax_nbr",
                       "capgntax_sub = -capgntax_nbr","salestax_sub = -salestax_nbr",
                       "corptax_sub = -corptax_nbr","wctaxfixed_sub = -wctaxfixed_nbr",
                       "uitaxrate_sub = -uitaxrate_nbr",round(c(ptaxjoint[2,3],inctaxjoint[2,3], capgntaxjoint[2,3],
                                                                +         salestaxjoint[2,3], corptaxjoint[2,3], wctaxjoint[2,3], uitaxjoint[2,3],
                                                                +         ptaxjoint[2,4],inctaxjoint[2,4], capgntaxjoint[2,4],
                                                                +         salestaxjoint[2,4], corptaxjoint[2,4], wctaxjoint[2,4], 
                                                                uitaxjoint[2,4]), digits = 4)), 
                  nrow = 7, byrow = FALSE, dimnames =  list(c(),c("Test","F-Stat", "P(>F)")))
  
  stargazer(ftest, title = paste("F-Tests for Symmetry of Coefficients for", naics_names[m], "Firm Start Ups", sep = " ")
            , label = paste(naics[m],"Ftests", sep = ""), colnames = TRUE, digits = 3,
            out = paste("~/papers/firm_entry/analysis/output/presentation/",naics[m],"Ftests.tex", sep = "_"))
  
  write(stargazer(pols1_amen_r_coef, pols1_r_coef, label = paste(naics[m],"noequality", sep =""),
                  dep.var.labels = c("births ratio"), model.names = FALSE,
                  covariate.labels = c("Property Tax Sub", "Property Tax Nbr", "Income Tax Sub", "Income Tax Nbr", 
                                       "Capital Gains Tax Sub", "Capital Gains Tax nbr", "Sales Tax Sub", "Sales Tax Nbr",
                                       "Corp Tax Sub", "Corp Tax Nbr", "Workers Comp Tax Sub", "Workers Comp Tax Nbr",
                                       "Unemp. Tax Sub","Unemp. Tax Nbr","Educ Spending Per Cap Sub", "Educ Spending Per Cap Nbr",
                                       "Highway Spending Per Cap Sub", "Highway Spending Per Cap Nbr",
                                       "Welfare Spending Per Cap Sub","Welfare Spending Per Cap Sub"),
                  omit = c("Z"), omit.labels = c("amenities"), omit.yes.no = c("Yes", "No"), omit.table.layout = "sn",
                  column.labels = c("OLS","OLS","OLS","OLS","FE", "FE","IV"),
                  no.space = TRUE, title = paste("Not Symmetric Effects for ", naics_names[m], "Firm Births", sep = " ")),
        paste("~/papers/firm_entry/analysis/output/presentation/", naics[m], "no_equality_rd_results.tex", sep = "_"))
  
  
  
  
  
  # imposing equality across borders
  pols_namen_nc_real <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                           + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                           + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                           data = master)
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_real, master$stpr_id)
  pols_namen_nc_r_coef <- coeftest(pols_namen_nc_real, vcov = stprid_c_vcov) # results
  
  assign(paste("pols_na_nc",naicsf[m], sep = "_"), coeftest(pols_namen_nc_real, vcov = stprid_c_vcov))
  
  pols_amen_nc_real <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                          + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                          + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                          + JAN.TEMP.Z_diff + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                          + TOPOG.Z_diff + WATER.AR.Z_diff, 
                          data = master)
  stprid_c_vcov <- cluster.vcov(pols_amen_nc_real , master$stpr_id)
  pols_amen_nc_r_coef <- coeftest(pols_amen_nc_real , vcov = stprid_c_vcov) # results
  
  pols_namen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                       + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                       + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                       + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                       + popdensity.cont_diff + pctmanuf.cont_diff, 
                       data = master)
  stprid_c_vcov <- cluster.vcov(pols_namen_c_r,  master$stpr_id)
  pols_namen_c_r_coef <- coeftest(pols_namen_c_r, vcov = stprid_c_vcov)
  assign(paste("pols_na_c",naicsf[m], sep = "_"), coeftest(pols_namen_c_r, vcov = stprid_c_vcov))
  
  linearHypothesis(pols_namen_c_r, c("ptax_diff + inctax_diff + capgntax_diff
                    + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  linearHypothesis(pols_namen_c_r, c("capgntax_diff + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  
  
  pols_amen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                      + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                      + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                      + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                      + popdensity.cont_diff + pctmanuf.cont_diff + JAN.TEMP.Z_diff
                      + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                      + TOPOG.Z_diff + WATER.AR.Z_diff, 
                      data = master)
  stprid_c_vcov <- cluster.vcov(pols_amen_c_r, master$stpr_id)
  pols_amen_c_r_coef <- coeftest(pols_amen_c_r, vcov = stprid_c_vcov)
  
  # iv estimation
  simple.pop.1s<- lm(pop_diff ~ WATER.AR.Z_diff, data = master)
  # very significant, but R^2 only about 0.04, so probably a weak instrument?
  
  master$pop.pred<- predict(simple.pop.1s, newdata = master)
  simple.pop.2s<- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                     + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                     + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                     + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                     + popdensity.cont_diff + pctmanuf.cont_diff + pop.pred, 
                     data = master)
  stprid_c_vcov <- cluster.vcov(simple.pop.2s,  master$stpr_id)
  simple.pop.2s_coef <- coeftest(simple.pop.2s, vcov = stprid_c_vcov)
  
  # fixed effect model
  master$stpr_fe <- factor(master$stpr_id)
  master <- master[is.finite(master$births_ratio),]
  
  pols_amen_fe  <- felm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                        + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                        + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                        + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                        + popdensity.cont_diff + pctmanuf.cont_diff  + JAN.TEMP.Z_diff
                        + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                        + TOPOG.Z_diff + WATER.AR.Z_diff  | stpr_fe | 0 |0, data = master)
  
  pols_fe <- felm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                  + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                  + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                  + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                  + popdensity.cont_diff + pctmanuf.cont_diff | stpr_fe | 0 | 0, data = master)
  
  # write tables
  write(stargazer(pols_amen_c_r_coef, pols_namen_c_r_coef, pols_namen_nc_r_coef, pols_fe,
                  label = paste(naics[m], "rd", sep = ""), dep.var.labels = c("births ratio"), model.names = FALSE,
                  covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                       "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                       "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                       "Welfare Spending Per Cap Diff"),
                  omit = c("cont","Z"), omit.labels = c('controls', 'amenities'), omit.yes.no = c("Yes", "No"), omit.table.layout = "sn",
                  column.labels = c("OLS","OLS","OLS","OLS","FE", "FE"),
                  no.space = TRUE, title = paste("Regression Discontinuity Models for ", naics_names[m], "Firm Births", sep = " ")),
        paste("~/papers/firm_entry/analysis/output/presentation/", naics[m], "rd_results.tex", sep = "_"))
  
  # Run regressions for each year
  for (i in 1999:2009)  {
    sub <- master[master$year == i,]
    pols_year <- lm(sub$births_ratio ~ sub$ptax_diff + sub$inctax_diff + sub$capgntax_diff
                    + sub$salestax_diff  + sub$corptax_diff  + sub$wctax_diff  + sub$uitax_diff
                    + sub$educ_pc_L1_diff + sub$hwy_pc_L1_diff + sub$welfare_pc_L1_diff
                    + sub$hsplus.cont_diff + sub$realfuelpr.cont_diff + sub$unionmem.cont_diff
                    + sub$popdensity.cont_diff + sub$pctmanuf.cont_diff , data = sub)
    stprid_c_vcov <- cluster.vcov(pols_year, sub$stpr_id)
    assign(paste("pols",i,sep="_"), coeftest(pols_year, vcov = stprid_c_vcov)) 
  }
  rm(sub)
  
  write(stargazer(pols_1999,pols_2001,pols_2003,pols_2005,pols_2007,pols_2009,
                  dep.var.labels = c("births ratio"), label = paste(naics[m],"year",sep=""), font.size = "small",
                  covariate.labels = c("Prop Tax Diff", "Inc Tax Diff", "Cap Tax Diff",
                                       "Sal Tax Diff", "Corp Tax Diff", "Work Comp Diff",
                                       "Unemp. Tax Diff", "Ln Educ Diff", "Ln Hwy Diff",
                                       "Ln Welf. Diff"),
                  omit = c("cont","Z"), omit.labels = c("controls", "amenities"), omit.table.layout = "sn",
                  column.labels = c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009"),
                  no.space = TRUE, title = paste("Test for Stability over Time for ", naics_names[m], "Firm Births", sep = " ")), 
        paste("~/papers/firm_entry/analysis/output/presentation/", naics[m], "year_rd_results.tex", sep = "_"))
  
  
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
      dep_var <- rbind(dep_var,tmp_sub[1,87:112])
      mean_births_sub[k] <- sum(tmp_sub$births_sub)/length(tmp_sub[,1]) 
      mean_births_nbr[k] <- sum(tmp_nbr$births_nbr)/length(tmp_nbr[,1])
      k <- k +1
    }
  }
  dl_master <- as.data.frame(dep_var)
  dl_master$mean_births_sub <- mean_births_sub
  dl_master$mean_births_nbr <- mean_births_nbr
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
  
  write(stargazer(dl_amen_c_r_coef,  dl_namen_c_r_coef,dl_namen_nc_r_coef, dl_amen_fe,
                  dep.var.labels = c("births ratio"), model.names = FALSE,
                  covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                       "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                       "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                       "Welfare Spending Per Cap Diff"),
                  omit = c("cont","Z"), omit.labels = c('controls', 'amenities'), omit.yes.no = c("Yes", "No"), omit.table.layout = "sn",
                  column.labels = c("OLS","OLS","OLS","OLS","FE"),
                  no.space = TRUE, title = paste("Regression Discontinuity Models for ", naics_names[m], "Firm Births using Donald and Lang (2007)", sep = " ")), 
        paste("~/papers/firm_entry/analysis/output/presentation/", naics[m], "dl_rd_results.tex", sep = "_"))
  
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
  
  # eclude data we only see once
  master <- master[table(master$stpr_id) != 11,]
  master <- master[master$births_sub > 0 & master$births_nbr > 0 ,]
  
  pols_namen_nc_real <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                           + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                           + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                           data = master)
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_real, master$stpr_id)
  pols_namen_nc_r_coef <- coeftest(pols_namen_nc_real, vcov = stprid_c_vcov) # results
  
  pols_amen_nc_real <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                          + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                          + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                          + JAN.TEMP.Z_diff + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                          + TOPOG.Z_diff + WATER.AR.Z_diff, 
                          data = master)
  stprid_c_vcov <- cluster.vcov(pols_amen_nc_real , master$stpr_id)
  pols_amen_nc_r_coef <- coeftest(pols_amen_nc_real , vcov = stprid_c_vcov) # results
  
  pols_namen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                       + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                       + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                       + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                       + popdensity.cont_diff + pctmanuf.cont_diff, 
                       data = master)
  stprid_c_vcov <- cluster.vcov(pols_namen_c_r,  master$stpr_id)
  pols_namen_c_r_coef <- coeftest(pols_namen_c_r, vcov = stprid_c_vcov)
  
  linearHypothesis(pols_namen_c_r, c("ptax_diff + inctax_diff + capgntax_diff
                                 + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  linearHypothesis(pols_namen_c_r, c("capgntax_diff + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  
  
  pols_amen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                      + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                      + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                      + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                      + popdensity.cont_diff + pctmanuf.cont_diff + JAN.TEMP.Z_diff
                      + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                      + TOPOG.Z_diff + WATER.AR.Z_diff, 
                      data = master)
  stprid_c_vcov <- cluster.vcov(pols_amen_c_r, master$stpr_id)
  pols_amen_c_r_coef <- coeftest(pols_amen_c_r, vcov = stprid_c_vcov)
  
  # iv estimation
  simple.pop.1s<- lm(pop_diff ~ WATER.AR.Z_diff, data = master)
  # very significant, but R^2 only about 0.04, so probably a weak instrument?
  
  master$pop.pred<- predict(simple.pop.1s, newdata = master)
  simple.pop.2s<- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                     + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                     + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                     + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                     + popdensity.cont_diff + pctmanuf.cont_diff + pop.pred, 
                     data = master)
  stprid_c_vcov <- cluster.vcov(simple.pop.2s,  master$stpr_id)
  simple.pop.2s_coef <- coeftest(simple.pop.2s, vcov = stprid_c_vcov)
  
  # fixed effect model
  master$stpr_fe <- factor(master$stpr_id)
  
  master <- master[is.finite(master$births_ratio),]
  
  pols_amen_fe  <- felm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                        + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                        + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                        + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                        + popdensity.cont_diff + pctmanuf.cont_diff  + JAN.TEMP.Z_diff
                        + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                        + TOPOG.Z_diff + WATER.AR.Z_diff  | stpr_fe | 0 |0, data = master)
  
  pols_fe <- felm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                  + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                  + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                  + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                  + popdensity.cont_diff + pctmanuf.cont_diff | stpr_fe | 0 | 0, data = master)
  
  # write tables
  write(stargazer(pols_amen_c_r_coef, pols_namen_c_r_coef, pols_namen_nc_r_coef, pols_amen_fe,
                  label = paste(naics[m], "eb", sep = ""), dep.var.labels = c("births ratio"), model.names = FALSE,
                  covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                       "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                       "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                       "Welfare Spending Per Cap Diff"),
                  omit = c("cont","Z"), omit.labels = c('controls', 'amenities'), omit.yes.no = c("Yes", "No"), omit.table.layout = "sn",
                  column.labels = c("OLS","OLS","OLS","OLS","FE", "FE"),
                  no.space = TRUE, title = paste("Extended Bandwidth Models for ", naics_names[m], "Firm Births", sep = " ")), 
        paste("~/papers/firm_entry/analysis/output/presentation/", naics[m], "eb_results.tex", sep = "_"))
  
  # Run regressions for each year
  for (i in 1999:2009)  {
    sub <- master[master$year == i,]
    pols_year <- lm(sub$births_ratio ~ sub$ptax_diff + sub$inctax_diff + sub$capgntax_diff
                    + sub$salestax_diff  + sub$corptax_diff  + sub$wctax_diff  + sub$uitax_diff
                    + sub$educ_pc_L1_diff + sub$hwy_pc_L1_diff + sub$welfare_pc_L1_diff
                    + sub$hsplus.cont_diff + sub$realfuelpr.cont_diff + sub$unionmem.cont_diff
                    + sub$popdensity.cont_diff + sub$pctmanuf.cont_diff , 
                    subset = sub$births_sub > 0 & sub$births_nbr > 0)
    stprid_c_vcov <- cluster.vcov(pols_year, sub$stpr_id[sub$births_sub > 0 & sub$births_nbr > 0])
    assign(paste("pols",i,sep="_"), coeftest(pols_year, vcov = stprid_c_vcov)) 
  }
  rm(sub)
  
  write(stargazer(pols_1999,pols_2001,pols_2003,pols_2005,pols_2007,pols_2009, 
                  dep.var.labels = c("births ratio"), label = paste(naics[m], "ebyear"), font.size = "small",
                  covariate.labels = c("Prop Tax Diff", "Inc Tax Diff", "Capital Tax Diff",
                                       "Sal Tax Diff", "Corp Tax Diff", "Work Comp Diff",
                                       "Unemp. Tax Diff", "Ln Educ Diff", "Ln Hwy Diff",
                                       "Ln Welf. Diff"),
                  omit = c("cont","Z"), omit.labels = c("controls", "amenities"), omit.table.layout = "sn",
                  column.labels = c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009"),
                  no.space = TRUE, title = paste("Extended Bandwidth for Stability over Time for ", naics_names[m], "Firm Births", sep = " ")), 
        paste("~/papers/firm_entry/analysis/output/presentation/", naics[m], "year_eb_results.tex", sep = "_"))
  
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
      dep_var <- rbind(dep_var,tmp_sub[1,88:112])
      mean_births_sub[k] <- sum(tmp_sub$births_sub)/length(tmp_sub[,1]) 
      mean_births_nbr[k] <- sum(tmp_nbr$births_nbr)/length(tmp_nbr[,1])
      k <- k +1
    }
  }
  dl_master <- as.data.frame(dep_var)
  dl_master$mean_births_sub <- mean_births_sub
  dl_master$mean_births_nbr <- mean_births_nbr
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
  
  write(stargazer(dl_amen_c_r_coef, dl_namen_c_r_coef, dl_namen_nc_r_coef,   dl_amen_fe, 
                  dep.var.labels = c("births ratio"),
                  covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                       "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                       "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                       "Welfare Spending Per Cap Diff"),
                  omit = c("cont","Z"), omit.labels = c("controls", "amenities"), omit.table.layout = "sn",
                  column.labels = c("OLS","OLS","OLS","OLS","FE","FE"),
                  no.space = TRUE, title = paste("DL Extended Bandwidth Models for ", naics_names[m], "Firm Births using Donald and Lang (2007)", sep = " ")), 
        paste("~/papers/firm_entry/analysis/output/presentation/", naics[m], "dl_eb_results.tex", sep = "_"))
  rm(master)
}

stargazer(pols_na_nc_11,pols_na_nc_31_33, pols_na_nc_44_45,pols_na_nc_52,
          dep.var.labels = c("births ratio"), font.size = "small", label = "naics",
          covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                               "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                               "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                               "Welfare Spending Per Cap Diff"),
          omit = c("cont","Z"), omit.labels = c("controls", "amenities"), omit.table.layout = "sn",
          column.labels = c("Farming","Farming","Manuf","Manuf","Retail","Retail","Finance","Finance"),
          no.space = TRUE, title = "Results for Firm Entry across NAICS Subcodes for ",
          out = "~/papers/firm_entry/analysis/output/presentation/naics_subcodes.tex")

