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
  master$inctax_diff <- (master$inctax_sub - master$inctax_nbr)
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
  

  recip <- read.csv("~/papers/firm_entry/build/input/recipricol.csv")
  recip <- recip[,-c(3:5)]
  recip$stpr_id <- paste(recip$stateFIPS_sub,recip$stateFIPS_nbr, by = "")
  
  recip_master <- merge(master, recip, by.x = c("statefips.x", "statefips.y"),by.y = c("stateFIPS_sub","stateFIPS_nbr"))
  recip_master$stpr_id <- paste(recip_master$statefips.x,recip_master$statefips.y, by = "")
  
  norecip_master <- master[!(master$stpr_id %in% recip$stpr_id),]
  
  #######################
  # BIRTHS RATIO MODELS #
  #######################

  # imposing equality across borders
  pols_namen_nc_real <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                           + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                           + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                           data = recip_master)
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_real, recip_master$stpr_id)
  pols_namen_nc_r_coef <- coeftest(pols_namen_nc_real, vcov = stprid_c_vcov) # results
  
  assign(paste("pols_na_nc",naicsf[m], sep = "_"), coeftest(pols_namen_nc_real, vcov = stprid_c_vcov))
  
  pols_amen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                      + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                      + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                      + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                      + popdensity.cont_diff + pctmanuf.cont_diff + JAN.TEMP.Z_diff
                      + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                      + TOPOG.Z_diff + WATER.AR.Z_diff, 
                      data = recip_master)
  stprid_c_vcov <- cluster.vcov(pols_amen_c_r, recip_master$stpr_id)
  pols_amen_c_r_coef <- coeftest(pols_amen_c_r, vcov = stprid_c_vcov)
  
  norecip_namen_nc_real <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                           + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                           + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                           data = norecip_master)
  stprid_c_vcov <- cluster.vcov(norecip_namen_nc_real, norecip_master$stpr_id)
  norecip_namen_nc_r_coef <- coeftest(pols_namen_nc_real, vcov = stprid_c_vcov) # results

  norecip_amen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                      + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                      + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                      + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                      + popdensity.cont_diff + pctmanuf.cont_diff + JAN.TEMP.Z_diff
                      + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                      + TOPOG.Z_diff + WATER.AR.Z_diff, 
                      data = norecip_master)
  stprid_c_vcov <- cluster.vcov(norecip_amen_c_r, norecip_master$stpr_id)
  norecip_amen_c_r_coef <- coeftest(pols_amen_c_r, vcov = stprid_c_vcov)
  
  
  # write tables
  write(stargazer(pols_amen_c_r, pols_namen_nc_real,norecip_amen_c_r, norecip_namen_nc_real,
    se = list(pols_amen_c_r_coef[,2], pols_namen_nc_r_coef[,2], norecip_amen_c_r_coef[,2], norecip_namen_nc_r_coef[,2]),
                  label = paste(naics[m], "rd", sep = ""), dep.var.labels = c("births ratio"), model.names = FALSE,
                  covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                       "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                       "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                       "Welfare Spending Per Cap Diff"),
                  omit = c("cont","Z"), omit.labels = c('controls', 'amenities'), omit.yes.no = c("Yes", "No"), omit.stat = c("f","adj.rsq","ser"),
                  column.labels = c("Recipricol","Recipricol","No Recipricol","No Recipricol"),
                  no.space = TRUE, title = paste("Counties with Income Tax Agreements for ", naics_names[m], "Firm Births", sep = " ")),
        paste("~/papers/firm_entry/analysis/output/", naics[m], "agreements_rd_results.tex", sep = "_"))
}

