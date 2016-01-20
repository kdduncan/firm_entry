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
  
  emp <- read.csv("~/papers/firm_entry/build/input/NAICS_EMP_WORKPLACE_1990-2012.csv")
  
  emp_sub <- emp
  names(emp_sub) <- paste(names(emp_sub),"sub",sep="_")
  
  emp_nbr <- emp
  names(emp_nbr) <- paste(names(emp_nbr),"nbr",sep="_")
  
  master <- merge(master, emp_sub, by.x = c("cofip_sub","year"), by.y = c("fips_sub","year_sub"))
  master <- merge(master, emp_nbr, by.x =  c("cofip_nbr","year"), by.y = c("fips_nbr","year_nbr")) #13442 obs for all case
  
  # log dep. var's
  master$lnwandsemp_ratio = log(as.numeric(master$wandsemp_sub)) - log(as.numeric(master$wandsemp_nbr))
  master$wandsemp_ratio = as.numeric(master$wandsemp_sub) - as.numeric(master$wandsemp_nbr)
  master$lntotemp_ratio = log(as.numeric(master$totemp_sub)) - log(as.numeric(master$totemp_nbr))
  master$totemp_ratio = as.numeric(master$totemp_sub) - as.numeric(master$totemp_nbr)
  
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
  # master <- master[master$lntotemp_ratio > 0,] # 13115
  master <- master[table(master$stpr_id) != 11,] # 12942 for all case ?

  #######################
  # BIRTHS RATIO MODELS #
  #######################
  # imposing equality across borders
  pols_namen_nc_real <- lm(lntotemp_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                           + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                           + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff,
                           data = master)
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_real, master$stpr_id)
  pols_namen_nc_r_coef <- coeftest(pols_namen_nc_real, vcov = stprid_c_vcov) # results
  
  assign(paste("pols_na_nc",naicsf[m], sep = "_"), coeftest(pols_namen_nc_real, vcov = stprid_c_vcov))
  
  pols_amen_nc_real <- lm(lntotemp_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                          + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                          + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                          + JAN.TEMP.Z_diff + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                          + TOPOG.Z_diff + WATER.AR.Z_diff, 
                          data = master)
  stprid_c_vcov <- cluster.vcov(pols_amen_nc_real , master$stpr_id)
  pols_amen_nc_r_coef <- coeftest(pols_amen_nc_real , vcov = stprid_c_vcov) # results
  
  pols_namen_c_r <- lm(lntotemp_ratio ~ ptax_diff + inctax_diff + capgntax_diff
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
  
  
  pols_amen_c_r <- lm(lntotemp_ratio ~ ptax_diff + inctax_diff + capgntax_diff
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
  simple.pop.2s<- lm(lntotemp_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                     + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                     + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                     + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                     + popdensity.cont_diff + pctmanuf.cont_diff + pop.pred, 
                     data = master)
  stprid_c_vcov <- cluster.vcov(simple.pop.2s,  master$stpr_id)
  simple.pop.2s_coef <- coeftest(simple.pop.2s, vcov = stprid_c_vcov)
  
  # fixed effect model
  master$stpr_fe <- factor(master$stpr_id)
  master <- master[is.finite(master$lntotemp_ratio),]
  
  pols_amen_fe  <- felm(lntotemp_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                        + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                        + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                        + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                        + popdensity.cont_diff + pctmanuf.cont_diff  + JAN.TEMP.Z_diff
                        + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                        + TOPOG.Z_diff + WATER.AR.Z_diff  | stpr_fe | 0 |0, data = master)
  
  pols_fe <- felm(lntotemp_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                  + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                  + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                  + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                  + popdensity.cont_diff + pctmanuf.cont_diff | stpr_fe | 0 | 0, data = master)
  
  # write tables
  write(stargazer(pols_amen_c_r_coef, pols_namen_nc_r_coef, pols_amen_fe, pols_fe,
                  label = paste(naics[m], "rd", sep = ""), dep.var.labels = c("total employment ratio"), model.names = FALSE,
                  covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                       "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                       "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                       "Welfare Spending Per Cap Diff"),
                  omit = c("cont","Z"), omit.labels = c('controls', 'amenities'), omit.yes.no = c("Yes", "No"), omit.table.layout = "sn",
                  column.labels = c("OLS","OLS","FE", "FE"),
                  no.space = TRUE, title = paste("Regression Discontinuity Models for ", naics_names[m], "New Employment", sep = " ")),
        paste("~/papers/firm_entry/analysis/output/", naics[m], "emp_rd_results.tex", sep = "_"))
}