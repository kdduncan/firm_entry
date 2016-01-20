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
  
  regions <- read.csv("~/papers/firm_entry/build/input/state_geocodes_v2011.csv")
  
  regions_sub <- regions[,1:3]
  names(regions_sub)[1:2] <- paste(names(regions_sub)[1:2],"sub",sep="_")
  
  regions_nbr <- regions[,1:3]
  names(regions_nbr)[1:2] <- paste(names(regions_nbr)[1:2],"nbr",sep="_")
  
  master <- merge(master,regions_sub, by.x = "statefips.x", by.y = "stateFIPS")
  master <- merge(master, regions_nbr, by.x = "statefips.y", by.y = "stateFIPS")
  
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
  
  #######################
  # BIRTHS RATIO MODELS #
  #######################
  # imposing equality across borders
  pols_namen_nc_real_1 <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                           + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                           + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                           data = master, subset = (master$Region_sub == 1))
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_real_1, master$stpr_id[(master$Region_sub == 1)])
  pols_na_nc_1 <- coeftest(pols_namen_nc_real_1, vcov = stprid_c_vcov)
  
  pols_namen_nc_real_2 <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                             + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                             + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                             data = master, subset = (master$Region_sub == 2))
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_real_2, master$stpr_id[(master$Region_sub == 2)])
  pols_na_nc_2 <- coeftest(pols_namen_nc_real_2, vcov = stprid_c_vcov)
  
  pols_namen_nc_real_3 <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                             + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                             + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                             data = master, subset = (master$Region_sub == 3))
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_real_3, master$stpr_id[(master$Region_sub == 3)])
  pols_na_nc_3 <- coeftest(pols_namen_nc_real_3, vcov = stprid_c_vcov)
  
  pols_namen_nc_real_4 <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                             + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                             + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                             data = master, subset = (master$Region_sub == 4))
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_real_4, master$stpr_id[(master$Region_sub == 4)])
  pols_na_nc_4 <- coeftest(pols_namen_nc_real_4, vcov = stprid_c_vcov)
  
  stargazer(pols_namen_nc_real_1,pols_namen_nc_real_2,pols_namen_nc_real_3,pols_namen_nc_real_4,
    se = list(pols_na_nc_1[,2], pols_na_nc_2[,2], pols_na_nc_3[,2], pols_na_nc_4[,2]),
            dep.var.labels = c("births ratio"), model.names = FALSE,
            covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                 "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                 "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                 "Welfare Spending Per Cap Diff"), omit.stat = c("f","adj.rsq","ser"),
            column.labels = c("Northeast", "Midwest", "South", "West"),
            no.space = TRUE, title = paste("Regional Estates for ", naics_names[m], "Firm Births", sep = " "),
            out = paste("~/papers/firm_entry/analysis/output/", naics[m], "region_rd_results.tex", sep = "_"))
  
  # write tables
 
}

