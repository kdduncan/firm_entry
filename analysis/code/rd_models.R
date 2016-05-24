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
  stargazer(sum, out = paste("~/papers/firm_entry/analysis/output/", naics[m], "summary_table.tex", sep = "_"),
            label = paste(naics[m],"summary", sep = ""), omit.stat = c("n"),
            notes  = "All variables are for the difference between our subject and neighbor counties. At the state level, this is 1177 observations. Further, all tax variables are scaled to be between 0 and 100 rather than 0 and 1. For each variable we observe them 13,115 times when not accounting for positive or negative infinify values in firm start up rates.",
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
            out = paste("~/papers/firm_entry/analysis/output/",naics[m],"Ftests.tex", sep = "_"))
  
  write(stargazer(pols1_amen_real, pols1_real,
                  se = list(pols1_amen_r_coef[,2], pols1_r_coef[,2]),
                  label = paste(naics[m],"noequality", sep =""), font.size = "footnotesize",
                  dep.var.labels = c("births ratio"), model.names = FALSE,
                  covariate.labels = c("Property Tax Sub", "Property Tax Nbr", "Income Tax Sub", "Income Tax Nbr", 
                                       "Capital Gains Tax Sub", "Capital Gains Tax nbr", "Sales Tax Sub", "Sales Tax Nbr",
                                       "Corp Tax Sub", "Corp Tax Nbr", "Workers Comp Tax Sub", "Workers Comp Tax Nbr",
                                       "Unemp. Tax Sub","Unemp. Tax Nbr","Educ Spending Per Cap Sub", "Educ Spending Per Cap Nbr",
                                       "Highway Spending Per Cap Sub", "Highway Spending Per Cap Nbr",
                                       "Welfare Spending Per Cap Sub","Welfare Spending Per Cap Sub"),
                  notes = c("Each model is estimated with Ordinary Least Squares",
                  "with clustered standard errors at the state-pair level.",
                  "coefficient values and standard errors are reported."),
                  omit = c("Z"), omit.labels = c("amenities"), omit.yes.no = c("Yes", "No"), omit.stat = c("f","adj.rsq","ser"),
                  column.labels = c("OLS","OLS","OLS","OLS","FE", "FE","IV"),
                  no.space = TRUE, title = paste("Not Symmetric Effects for ", naics_names[m], "Firm Births", sep = " ")),
        paste("~/papers/firm_entry/analysis/output/", naics[m], "no_equality_rd_results.tex", sep = "_"))
  
  
  
  
  
  # imposing equality across borders
  
  
  pols_namen_nc_real <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                           + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                           + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                           data = master)
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_real, master$stpr_id)
  pols_namen_nc_r_coef <- coeftest(pols_namen_nc_real, vcov = stprid_c_vcov) # results
  
  nanctax <- linearHypothesis(pols_namen_nc_real, c("ptax_diff + inctax_diff + capgntax_diff
                                                    + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  nancexp <- linearHypothesis(pols_namen_nc_real, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"), vcov = stprid_c_vcov)
  
  
  assign(paste("pols_na_nc",naicsf[m],"reg",sep = "_"), pols_namen_nc_real)
  assign(paste("pols_na_nc",naicsf[m], sep = "_"), pols_namen_nc_r_coef)
  
  pols_amen_nc_real <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                          + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                          + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                          + JAN.TEMP.Z_diff + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                          + TOPOG.Z_diff + WATER.AR.Z_diff, 
                          data = master)
  stprid_c_vcov <- cluster.vcov(pols_amen_nc_real , master$stpr_id)
  pols_amen_nc_r_coef <- coeftest(pols_amen_nc_real , vcov = stprid_c_vcov) # results
  
  anctax <- linearHypothesis(pols_amen_nc_real, c("ptax_diff + inctax_diff + capgntax_diff
                                                  + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  ancexp <- linearHypothesis(pols_amen_nc_real, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"), vcov = stprid_c_vcov)
  
  
  pols_namen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                       + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                       + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                       + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                       + popdensity.cont_diff + pctmanuf.cont_diff, 
                       data = master)
  stprid_c_vcov <- cluster.vcov(pols_namen_c_r,  master$stpr_id)
  pols_namen_c_r_coef <- coeftest(pols_namen_c_r, vcov = stprid_c_vcov)
  
  nactax <- linearHypothesis(pols_namen_c_r, c("ptax_diff + inctax_diff + capgntax_diff
                                               + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  nacexp <- linearHypothesis(pols_namen_c_r, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"), vcov = stprid_c_vcov)
  
  assign(paste("pols_na_c",naicsf[m],"reg",sep = "_"), pols_namen_c_r)
  assign(paste("pols_na_c",naicsf[m], sep = "_"), pols_namen_c_r_coef)
  
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
  
  actax <- linearHypothesis(pols_amen_c_r, c("ptax_diff + inctax_diff + capgntax_diff
                                             + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  acexp <- linearHypothesis(pols_amen_c_r, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"), vcov = stprid_c_vcov)
  

  
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
  
  fe1tax <- linearHypothesis(pols_amen_fe, c("ptax_diff + inctax_diff + capgntax_diff
                                             + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"))
  fe1exp <- linearHypothesis(pols_amen_fe, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"))
  
  pols_fe <- felm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                  + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                  + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                  + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                  + popdensity.cont_diff + pctmanuf.cont_diff | stpr_fe | 0 | 0, data = master)
  
  fe2tax <- linearHypothesis(pols_fe, c("ptax_diff + inctax_diff + capgntax_diff
                                             + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"))
  fe2exp <- linearHypothesis(pols_fe, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"))
  
  # write tables
  
  Ftest <- matrix(c("No Amenities, No Controls Taxes","No Amenities, No Controls Expenditures",
                    "No Amenities, Controls Taxes", "No Amenities, Controls Expenditures",
                    "Amenities, No Controls Taxes", "Amenities, No Controls Expenditures",
                    "Amenities, Controls Taxes", "Amenities, Controls Expenditures",
                    "FE No Amenities, Controls Taxes", "FE No Amenities, Controls Expenditures",
                    "FE Amenities, Controls Taxes", "FE Amenities, Controls Expenditures",
                    round(c(nanctax[2,3],nancexp[2,3],anctax[2,3], ancexp[2,3],nactax[2,3], nacexp[2,3],
                            actax[2,3], acexp[2,3],fe2tax[2,3],fe2exp[2,3],fe1tax[2,3],fe1exp[2,3],
                            nanctax[2,4],nancexp[2,4],anctax[2,4], ancexp[2,4],nactax[2,4], nacexp[2,4],
                            actax[2,4], acexp[2,4], fe2tax[2,4], fe2exp[2,4],fe1tax[2,4],fe1exp[2,4]), digits =4)), 
                  nrow = 12, byrow = FALSE, dimnames =  list(c(),c("Test","F-Stat", "P(>F)")))
  
  stargazer(Ftest, title = paste("F-Tests for Joint Tax and Expenditure Effects for", naics_names[m], "Firm Start Ups", sep = " ")
            , label = paste(naics[m],"Ftests", sep = ""), colnames = TRUE, digits = 3,
            out = paste("~/papers/firm_entry/analysis/output/",naics[m],"rd_Ftests.tex", sep = "_"))
  
  write(stargazer(pols_amen_c_r, pols_namen_c_r, pols_amen_nc_real, pols_namen_nc_real, pols_amen_fe, pols_fe,
                  se = list(pols_amen_c_r_coef[,2],pols_namen_c_r_coef[,2],pols_amen_nc_r_coef[,2],pols_namen_nc_r_coef[,2],NULL, NULL),
                  label = paste(naics[m], "rd", sep = ""), dep.var.labels = c("births ratio"), model.names = FALSE,
                  font.size = "footnotesize",
                  covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                       "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                       "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                       "Welfare Spending Per Cap Diff"),
                  notes = c("The first four columns are estimated with OLS and clustered standard",
                            " errors at the state-pair level. Columns 5 and 6 are estimated with",
                            "a fixed effect estimator at the state-pair level with homoskedastic", "standard errors."),
                  omit = c("cont","Z"), omit.labels = c('controls', 'amenities'), omit.yes.no = c("Yes", "No"), omit.stat = c("f","adj.rsq","ser"),
                  column.labels = c("OLS","OLS","OLS","OLS","FE", "FE"),
                  no.space = TRUE, title = paste("Regression Discontinuity Models for ", naics_names[m], "Firm Births", sep = " ")),
        paste("~/papers/firm_entry/analysis/output/", naics[m], "rd_results.tex", sep = "_"))
  
  # Run regressions for each year
  for (i in 1999:2009)  {
    sub <- master[master$year == i,]
    pols_year <-  lm(sub$births_ratio ~ sub$ptax_diff + sub$inctax_diff + sub$capgntax_diff
                     + sub$salestax_diff  + sub$corptax_diff  + sub$wctax_diff  + sub$uitax_diff
                     + sub$educ_pc_L1_diff + sub$hwy_pc_L1_diff + sub$welfare_pc_L1_diff
                     + sub$hsplus.cont_diff + sub$realfuelpr.cont_diff + sub$unionmem.cont_diff
                     + sub$popdensity.cont_diff + sub$pctmanuf.cont_diff , data = sub)
    stprid_c_vcov <- cluster.vcov(pols_year, sub$stpr_id)
    assign(paste("pols",i,sep="_"), pols_year)
    assign(paste("pols",i,"vcv",sep="_"), coeftest(pols_year, vcov = stprid_c_vcov)) 
  }
  rm(sub)
  
  write(stargazer(pols_1999,pols_2000,pols_2001,pols_2002,pols_2003,pols_2004,
                  se = list(pols_1999_vcv[,2],pols_2001_vcv[,2],pols_2002_vcv[,2],pols_2003_vcv[,2],pols_2004_vcv[,2]),
                  notes = c("All models are estimated with Ordinary Least Squares",
                            "and clustered standard errors at the state-pair level."),
                  dep.var.labels = c("births ratio"), label = paste(naics[m],"year",sep=""), font.size = "small",
                  covariate.labels = c("Prop Tax Diff", "Inc Tax Diff", "Cap Tax Diff",
                                       "Sal Tax Diff", "Corp Tax Diff", "Work Comp Diff",
                                       "Unemp. Tax Diff", "Ln Educ Diff", "Ln Hwy Diff",
                                       "Ln Welf. Diff"),
                  omit = c("cont","Z"), omit.labels = c("controls", "amenities"), omit.stat = c("f","adj.rsq","ser"),
                  column.labels = c("1999","2000","2001","2002","2003","2004"),
                  no.space = TRUE, title = paste("Psuedo-RD for Stability over Time for ", naics_names[m],
                                                 "Firm Births Pt I", sep = " ")), paste("~/papers/firm_entry/analysis/output/", naics[m], "year_rd_results1.tex", sep = "_"))
  
  write(stargazer(pols_2005,pols_2006,pols_2007,pols_2008,pols_2009,
                  se = list(pols_2005_vcv[,2],pols_2006_vcv[,2],pols_2007_vcv[,2],pols_2008_vcv[,2],pols_2009_vcv[,2]),
                  notes = c("All models are estimated with Ordinary Least Squares",
                            "and clustered standard errors at the state-pair level."),
                  dep.var.labels = c("births ratio"), label = paste(naics[m],"year",sep=""), font.size = "small",
                  covariate.labels = c("Prop Tax Diff", "Inc Tax Diff", "Cap Tax Diff",
                                       "Sal Tax Diff", "Corp Tax Diff", "Work Comp Diff",
                                       "Unemp. Tax Diff", "Ln Educ Diff", "Ln Hwy Diff",
                                       "Ln Welf. Diff"),
                  omit = c("cont","Z"), omit.labels = c("controls", "amenities"), omit.stat = c("f","adj.rsq","ser"),
                  column.labels = c("2005","2006","2007","2008","2009"),
                  no.space = TRUE, title = paste("Psuedo-RD for Stability over Time for ", naics_names[m],
                                                 "Firm Births Pt II", sep = " ")), paste("~/papers/firm_entry/analysis/output/", naics[m], "year_rd_results2.tex", sep = "_"))
  
  
  
  
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
  
  nanctax <- linearHypothesis(pols_namen_nc_real, c("ptax_diff + inctax_diff + capgntax_diff
                                                    + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  nancexp <- linearHypothesis(pols_namen_nc_real, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"), vcov = stprid_c_vcov)
  
  pols_amen_nc_real <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                          + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                          + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                          + JAN.TEMP.Z_diff + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                          + TOPOG.Z_diff + WATER.AR.Z_diff, 
                          data = master)
  stprid_c_vcov <- cluster.vcov(pols_amen_nc_real , master$stpr_id)
  pols_amen_nc_r_coef <- coeftest(pols_amen_nc_real , vcov = stprid_c_vcov) # results
  
  anctax <- linearHypothesis(pols_amen_nc_real, c("ptax_diff + inctax_diff + capgntax_diff
                                                  + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  ancexp <- linearHypothesis(pols_amen_nc_real, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"), vcov = stprid_c_vcov)
  
  
  pols_namen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                       + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                       + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                       + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                       + popdensity.cont_diff + pctmanuf.cont_diff, 
                       data = master)
  stprid_c_vcov <- cluster.vcov(pols_namen_c_r,  master$stpr_id)
  pols_namen_c_r_coef <- coeftest(pols_namen_c_r, vcov = stprid_c_vcov)
  
  nactax <- linearHypothesis(pols_namen_c_r, c("ptax_diff + inctax_diff + capgntax_diff
                                               + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  nacexp <- linearHypothesis(pols_namen_c_r, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"), vcov = stprid_c_vcov)
  
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
  
  actax <- linearHypothesis(pols_amen_c_r, c("ptax_diff + inctax_diff + capgntax_diff
                                             + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  acexp <- linearHypothesis(pols_amen_c_r, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"), vcov = stprid_c_vcov)
  
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
  
  fe1tax <- linearHypothesis(pols_amen_fe, c("ptax_diff + inctax_diff + capgntax_diff
                                             + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"))
  fe1exp <- linearHypothesis(pols_amen_fe, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"))
  
  pols_fe <- felm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                  + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                  + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                  + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                  + popdensity.cont_diff + pctmanuf.cont_diff | stpr_fe | 0 | 0, data = master)
  
  fe2tax <- linearHypothesis(pols_fe, c("ptax_diff + inctax_diff + capgntax_diff
                                             + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"))
  fe2exp <- linearHypothesis(pols_fe, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"))
  
  
  # write tables
  Ftest <- matrix(c("No Amenities, No Controls Taxes","No Amenities, No Controls Expenditures",
                    "No Amenities, Controls Taxes", "No Amenities, Controls Expenditures",
                    "Amenities, No Controls Taxes", "Amenities, No Controls Expenditures",
                    "Amenities, Controls Taxes", "Amenities, Controls Expenditures",
                    "FE No Amenities, Controls Taxes", "FE No Amenities, Controls Expenditures",
                    "FE Amenities, Controls Taxes", "FE Amenities, Controls Expenditures",
                    round(c(nanctax[2,3],nancexp[2,3],anctax[2,3], ancexp[2,3],nactax[2,3], nacexp[2,3],
                            actax[2,3], acexp[2,3],fe2tax[2,3],fe2exp[2,3],fe1tax[2,3],fe1exp[2,3],
                            nanctax[2,4],nancexp[2,4],anctax[2,4], ancexp[2,4],nactax[2,4], nacexp[2,4],
                            actax[2,4], acexp[2,4], fe2tax[2,4], fe2exp[2,4],fe1tax[2,4],fe1exp[2,4]), digits =4)), 
                  nrow = 12, byrow = FALSE, dimnames =  list(c(),c("Test","F-Stat", "P(>F)")))
  
  stargazer(Ftest, title = paste("F-Tests for Joint Tax and Expenditure Effects for Extended Bandwith", naics_names[m], "Firm Start Ups", sep = " ")
            , label = paste(naics[m],"Ftests", sep = ""), colnames = TRUE, digits = 3,
            out = paste("~/papers/firm_entry/analysis/output/",naics[m],"eb_Ftests.tex", sep = "_"))
  
  write(stargazer(pols_amen_c_r, pols_namen_c_r, pols_amen_nc_real, pols_namen_nc_real, pols_amen_fe, pols_fe,
                  se = list(pols_amen_c_r_coef[,2],pols_namen_c_r_coef[,2],pols_amen_nc_r_coef[,2],pols_namen_nc_r_coef[,2],NULL, NULL),
                  label = paste(naics[m], "eb", sep = ""), dep.var.labels = c("births ratio"), model.names = FALSE, font.size = "footnotesize",
                  covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                       "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                       "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                       "Welfare Spending Per Cap Diff"),
                  notes = c("The first four columns are estimated with OLS and clustered standard",
                            " errors at the state-pair level. Columns 5 and 6 are estimated with",
                            "a fixed effect estimator at the state-pair level with homoskedastic", "standard errors."),
                  omit = c("cont","Z"), omit.labels = c('controls', 'amenities'), omit.yes.no = c("Yes", "No"), omit.stat = c("f","adj.rsq","ser"),
                  column.labels = c("OLS","OLS","OLS","OLS","FE", "FE"),
                  no.space = TRUE, title = paste("Extended Bandwidth Discontinuity Models for ", naics_names[m], "Firm Births", sep = " ")),
        paste("~/papers/firm_entry/analysis/output/", naics[m], "eb_results.tex", sep = "_"))
  
  # Run regressions for each year
  for (i in 1999:2009)  {
    sub <- master[master$year == i,]
    pols_year <-  lm(sub$births_ratio ~ sub$ptax_diff + sub$inctax_diff + sub$capgntax_diff
                     + sub$salestax_diff  + sub$corptax_diff  + sub$wctax_diff  + sub$uitax_diff
                     + sub$educ_pc_L1_diff + sub$hwy_pc_L1_diff + sub$welfare_pc_L1_diff
                     + sub$hsplus.cont_diff + sub$realfuelpr.cont_diff + sub$unionmem.cont_diff
                     + sub$popdensity.cont_diff + sub$pctmanuf.cont_diff , data = sub)
    stprid_c_vcov <- cluster.vcov(pols_year, sub$stpr_id)
    assign(paste("pols",i,sep="_"), pols_year)
    assign(paste("pols",i,"vcv",sep="_"), coeftest(pols_year, vcov = stprid_c_vcov)) 
  }
  rm(sub)
  
  write(stargazer(pols_1999,pols_2000,pols_2001,pols_2002,pols_2003,pols_2004,
                  se = list(pols_1999_vcv[,2],pols_2001_vcv[,2],pols_2002_vcv[,2],pols_2003_vcv[,2],pols_2004_vcv[,2]),
                  notes = c("All models are estimated with Ordinary Least Squares",
                            "and clustered standard errors at the state-pair level."),
                  dep.var.labels = c("births ratio"), label = paste(naics[m],"year_eb",sep=""), font.size = "small",
                  covariate.labels = c("Prop Tax Diff", "Inc Tax Diff", "Cap Tax Diff",
                                       "Sal Tax Diff", "Corp Tax Diff", "Work Comp Diff",
                                       "Unemp. Tax Diff", "Ln Educ Diff", "Ln Hwy Diff",
                                       "Ln Welf. Diff"),
                  omit = c("cont","Z"), omit.labels = c("controls", "amenities"), omit.stat = c("f","adj.rsq","ser"),
                  column.labels = c("1999","2000","2001","2002","2003","2004"),
                  no.space = TRUE, title = paste("Extended Bandwidth for Stability over Time for ", naics_names[m],
                                                 "Firm Births Pt I", sep = " ")), paste("~/papers/firm_entry/analysis/output/", naics[m], "year_eb_results1.tex", sep = "_"))
  
  write(stargazer(pols_2005,pols_2006,pols_2007,pols_2008,pols_2009,
                  se = list(pols_2005_vcv[,2],pols_2006_vcv[,2],
                            pols_2007_vcv[,2],pols_2008_vcv[,2],pols_2009_vcv[,2]),
                  notes = c("All models are estimated with Ordinary Least Squares",
                            "and clustered standard errors at the state-pair level."),
                  dep.var.labels = c("births ratio"), label = paste(naics[m],"year_eb",sep=""), font.size = "small",
                  covariate.labels = c("Prop Tax Diff", "Inc Tax Diff", "Cap Tax Diff",
                                       "Sal Tax Diff", "Corp Tax Diff", "Work Comp Diff",
                                       "Unemp. Tax Diff", "Ln Educ Diff", "Ln Hwy Diff",
                                       "Ln Welf. Diff"),
                  omit = c("cont","Z"), omit.labels = c("controls", "amenities"), omit.stat = c("f","adj.rsq","ser"),
                  column.labels = c("2005","2006","2007","2008","2009"),
                  no.space = TRUE, title = paste("Extended Bandwidth for Stability over Time for ", naics_names[m],
                                                 "Firm Births Pt I", sep = " ")), paste("~/papers/firm_entry/analysis/output/", naics[m], "year_eb_results2.tex", sep = "_"))
  rm(master)
}

stargazer(pols_na_c_11_reg, pols_na_nc_11_reg,pols_na_c_31_33_reg, pols_na_nc_31_33_reg,pols_na_c_44_45_reg,
          pols_na_nc_44_45_reg,pols_na_c_52_reg,pols_na_nc_52_reg,
          se = list(pols_na_c_11[,2], pols_na_nc_11[,2],pols_na_c_31_33[,2], pols_na_nc_31_33[,2],pols_na_c_44_45[,2],
                    pols_na_nc_44_45[,2],pols_na_c_52[,2],pols_na_nc_52[,2]),
          notes = c("All models are estimated with Ordinary Least Squares",
                    "and clustered standard errors at the state-pair level."),
          dep.var.labels = c("births ratio"), font.size = "small", label = "naics",
          covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                               "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                               "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                               "Welfare Spending Per Cap Diff"),
          omit = c("cont","Z"), omit.labels = c("controls", "amenities"), omit.stat = c("f","adj.rsq","ser"),
          column.labels = c("Farming","Farming","Manuf","Manuf","Retail","Retail","Finance","Finance"),
          no.space = TRUE, title = "Results for Firm Entry across NAICS Subcodes for ",
          out = "~/papers/firm_entry/analysis/output/naics_subcodes.tex")