library(multiwayvcov) # clustered standard errors
library(lfe) #felm fixed effect linear models
library(stargazer) #stargazer
library(car) #linearhypothesis
library(lmtest) #coeftest

# used "counties in MSA's from http://www.bea.gov/regional/docs/msalist.cfm?mlist=45

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
  
  metro <- read.csv("~/papers/firm_entry/build/input/metrolist.csv")
  names(metro) <- c("metro","metro name","cofip","county name")
  
  metro_sub <- metro
  names(metro_sub) <- paste(names(metro_sub),"sub",sep="_")
  
  metro_nbr <- metro
  names(metro_nbr) <- paste(names(metro_nbr),"nbr",sep="_")
  
  master <- merge(master,metro_sub, by = "cofip_sub")
  master <- merge(master, metro_nbr, by = "cofip_nbr")
  
 
  
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

  same <- master[master$metro_sub == master$metro_nbr,]
  
  #######################
  # BIRTHS RATIO MODELS #
  #######################
  # imposing equality across borders
  pols_namen_nc_real_1 <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                           + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                           + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                           data = master)
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_real_1, master$stpr_id)
  pols_na_nc_1 <- coeftest(pols_namen_nc_real_1, vcov = stprid_c_vcov)
  
  nanctax1 <- linearHypothesis(pols_namen_nc_real, c("ptax_diff + inctax_diff + capgntax_diff
                                     + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  nancexp1 <- linearHypothesis(pols_namen_nc_real, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"), vcov = stprid_c_vcov)
  
  
  pols_namen_nc_real_2 <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                             + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                             + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                             data = same)
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_real_2, same$stpr_id)
  pols_na_nc_2 <- coeftest(pols_namen_nc_real_2, vcov = stprid_c_vcov)
  
  nanctax2 <- linearHypothesis(pols_namen_nc_real_2, c("ptax_diff + inctax_diff + capgntax_diff
                                     + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  nancexp2 <- linearHypothesis(pols_namen_nc_real_2, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"), vcov = stprid_c_vcov)
  

  master <- read.csv(paste("~/papers/firm_entry/build/output/",naics[m],"border_master.csv", sep = "_"))
  
  # dens <- read.csv("~/papers/firm_entry/build/input/PctUrbanRural_County.csv")
  rucc <- read.csv("~/papers/firm_entry/build/input/ruralurbancodes2003.csv")
  names(rucc) <- c("cofip", "state","countyName", "rucc1993","rucc2004","pop2000","pctCommute","description")
  
  # dens_sub <- dens
  # names(dens_sub) <- paste(names(dens_sub),"sub",sep="_")
  # dens_nbr <- dens
  # names(dens_nbr) <- paste(names(dens_nbr),"nbr",sep="_")
  
  rucc_sub <- rucc
  names(rucc_sub) <- paste(names(rucc_sub),"sub",sep="_")
  rucc_nbr <- rucc
  names(rucc_nbr) <- paste(names(rucc_nbr),"nbr",sep="_")
  
  # master_dens <- merge(master,dens_sub, by = "cofip_sub")
  # master_dens <- merge(master_dens, dens_nbr, by = "cofip_nbr")
  
  master <- merge(master,rucc_sub, by = "cofip_sub")
  master <- merge(master, rucc_nbr, by = "cofip_nbr")
  
  
  
  # log dep. var's
  master$urban <- as.numeric(master$rucc2004_sub & master$rucc2004_nbr < 7)
  master$rural <- as.numeric(master$rucc2004_sub & master$rucc2004_nbr > 6)
  
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
  
  master_urban <- master[master$urban == 1,]
  master_rural <- master[master$rural == 1,]
  
  #######################
  # BIRTHS RATIO MODELS #
  #######################
  # imposing equality across borders
  pols_namen_nc_dens_1 <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                             + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                             + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                             data = master_urban)
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_dens_1, master_urban$stpr_id)
  pols_na_nc_dens1 <- coeftest(pols_namen_nc_dens_1, vcov = stprid_c_vcov)
  
  nanctax3 <- linearHypothesis(pols_namen_nc_dens_1, c("ptax_diff + inctax_diff + capgntax_diff
                                   + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  nancexp3 <- linearHypothesis(pols_namen_nc_dens_1, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"), vcov = stprid_c_vcov)
  
  
  
  pols_namen_nc_dens_2 <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                             + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                             + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                             data = master_rural)
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_dens_2, master_rural$stpr_id)
  pols_na_nc_dens2 <- coeftest(pols_namen_nc_dens_2, vcov = stprid_c_vcov)
  
  nanctax4 <- linearHypothesis(pols_namen_nc_dens_2, c("ptax_diff + inctax_diff + capgntax_diff
                                   + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff = 0"), vcov = stprid_c_vcov)
  nancexp4 <- linearHypothesis(pols_namen_nc_dens_2, c(" educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff = 0"), vcov = stprid_c_vcov)
  
  
  Ftest <- matrix(c("In MSA Taxes","In MSA Exp","Same MSA Taxes","Same MSA Exp",
                    "Jointly Urban Taxes","Jointly Urban Exp", "Jointly Rural Taxes","Jointly Rural Exp",round(c(nanctax1[2,3],nancexp1[2,3],
                                                                                            + nanctax2[2,3], nancexp2[2,3],
                                                                                            + nanctax3[2,3], nancexp3[2,3],
                                                                                            + nanctax4[2,3], nancexp4[2,3],
                                                                                            +nanctax1[2,4],nancexp1[2,4],
                                                                                            + nanctax2[2,4], nancexp2[2,4],
                                                                                            + nanctax3[2,4], nancexp3[2,4],
                                                                                            + nanctax4[2,4], nancexp[2,4]), digits =4)), 
                  nrow = 8, byrow = FALSE, dimnames =  list(c(),c("Test","F-Stat", "P(>F)")))
  
  stargazer(Ftest, title = paste("F-Tests for Density Joint Tax and Expenditure Effects for", naics_names[m], "Firm Start Ups", sep = " ")
            , label = paste(naics[m],"Ftests", sep = ""), colnames = TRUE, digits = 3,
            out = paste("~/papers/firm_entry/analysis/output/",naics[m],"metrord_Ftests.tex", sep = "_"))
  
  stargazer(pols_namen_nc_real_1,pols_namen_nc_real_2,pols_namen_nc_dens_1,pols_namen_nc_dens_2, 
    se = list(pols_na_nc_1[,2], pols_na_nc_2[,2], pols_na_nc_dens1[,2], pols_na_nc_dens2[,2]),
    label = paste(naics[m], "metro", sep = ""),
    notes = c("All models are estimated with Ordinary Least Squares",
              "and clustered standard errors at the state-pair level."),
            dep.var.labels = c("births ratio"), model.names = FALSE,
            covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                 "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                 "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                 "Welfare Spending Per Cap Diff"), omit.stat = c("f","adj.rsq","ser"),
            column.labels = c("In a MSA", "Same MSA","Jointly Urban", "Jointly Rural"),
            no.space = TRUE, title = paste("MSA Estates for ", naics_names[m], "Firm Births", sep = " "),
            out = paste("~/papers/firm_entry/analysis/output/", naics[m], "metro_rd_results.tex", sep = "_"))
  
  # write tables
 
}

