library(multiwayvcov) # clustered standard errors
library(lfe) #felm fixed effect linear models
library(stargazer) #stargazer
library(car) #linearhypothesis

naics <- c("--", "11","21","22","23","31-33","42","44-45","48-49", 
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
 master$hsplus_diff <- master$hsplus_sub - master$hsplus_nbr
 master$realfuelpr_diff <- master$realfuelpr_sub - master$realfuelpr_nbr
 master$unionmem_diff <- master$unionmem_sub - master$unionmem_nbr
 master$popdensity_diff <- master$popdensity_sub - master$popdensity_nbr
 master$pctmanuf_diff <- master$pctmanuf_sub - master$pctmanuf_nbr
 
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
 
 # count data, not imposing equality, amenities
 pols1_amen_count <- lm(master$births_diff ~  master$ptax_sub + master$ptax_nbr + master$inctax_sub +  master$inctax_nbr
                        + master$capgntax_sub + master$capgntax_nbr + master$salestax_sub + master$salestax_nbr +
                          master$corptax_sub + master$corptax_nbr +  master$wctaxfixed_sub + master$wctaxfixed_nbr +
                          master$uitaxrate_sub + master$uitaxrate_nbr 
                        + master$educ_pc_L1_sub+master$educ_pc_L1_nbr+ master$hwy_pc_L1_sub+ 
                          master$hwy_pc_L1_nbr + master$welfare_pc_L1_sub +  master$welfare_pc_L1_nbr +
                          master$JAN.TEMP...Z_sub + master$JAN.TEMP...Z_nbr
                       + master$JAN.SUN...Z_sub + master$JAN.SUN...Z_nbr
                       + master$JUL.TEMP...Z_sub + master$JUL.TEMP...Z_nbr
                       + master$JUL.HUM...Z_sub + master$JUL.HUM...Z_nbr
                       + master$TOPOG...Z_sub + master$TOPOG...Z_nbr
                       + master$LN.WATER..AREA...Z_sub + master$LN.WATER..AREA...Z_nbr)
 stprid_c_vcov <- cluster.vcov(pols1_amen_count, master$stpr_id)
 pols1_amen_c_coef <- coeftest(pols1_amen_count, vcov = stprid_c_vcov) # results
 
 
 
 pols1_namen_count <- lm(master$births_diff ~  master$ptax_sub + master$ptax_nbr + master$inctax_sub +  master$inctax_nbr
                         + master$capgntax_sub + master$capgntax_nbr + master$salestax_sub + master$salestax_nbr +
                           master$corptax_sub + master$corptax_nbr +  master$wctaxfixed_sub + master$wctaxfixed_nbr +
                           master$uitaxrate_sub + master$uitaxrate_nbr 
                         + master$educ_pc_L1_sub+master$educ_pc_L1_nbr+ master$hwy_pc_L1_sub+ 
                           master$hwy_pc_L1_nbr + master$welfare_pc_L1_sub +  master$welfare_pc_L1_nbr)
 stprid_c_vcov <- cluster.vcov(pols1_namen_count, master$stpr_id)
 pols1_namen_c_coef <- coeftest(pols1_namen_count, vcov = stprid_c_vcov) # results
 
 # count data model without amenities testing for equality
 pols1_count <- lm(master$births_diff ~  master$ptax_sub + master$ptax_nbr + master$inctax_sub +  master$inctax_nbr
                   + master$capgntax_sub + master$capgntax_nbr + master$salestax_sub + master$salestax_nbr +
                     master$corptax_sub + master$corptax_nbr +  master$wctaxfixed_sub + master$wctaxfixed_nbr +
                     master$uitaxrate_sub + master$uitaxrate_nbr 
                   + master$educ_pc_L1_sub+master$educ_pc_L1_nbr+ master$hwy_pc_L1_sub+ 
                     master$hwy_pc_L1_nbr + master$welfare_pc_L1_sub +  master$welfare_pc_L1_nbr)
 stprid_c_vcov <- cluster.vcov(pols1_count, master$stpr_id)
 pols1_stpr_c_coef <- coeftest(pols1_count, vcov = stprid_c_vcov)
 
 # tests for equality
 linearHypothesis(pols1_count, c("master$ptax_sub = -master$ptax_nbr"), vcov = stprid_c_vcov)
 linearHypothesis(pols1_count, c("master$inctax_sub = -master$inctax_nbr"), vcov = stprid_c_vcov)
 linearHypothesis(pols1_count, c("master$capgntax_sub = -master$capgntax_nbr"), vcov = stprid_c_vcov)
 linearHypothesis(pols1_count, c("master$salestax_sub = -master$salestax_nbr"), vcov = stprid_c_vcov)
 linearHypothesis(pols1_count, c("master$corptax_sub = -master$corptax_nbr"), vcov = stprid_c_vcov)
 linearHypothesis(pols1_count, c("master$wctaxfixed_sub = -master$wctaxfixed_nbr"), vcov = stprid_c_vcov)
 linearHypothesis(pols1_count, c("master$uitaxrate_sub = -master$uitaxrate_nbr"), vcov = stprid_c_vcov)
 
 # normal likelihood, real valued dep var, with amenities, not imposing equality
 pols1_amen_real <- lm(master$births_ratio ~  master$ptax_sub + master$ptax_nbr + master$inctax_sub +  master$inctax_nbr
                       + master$capgntax_sub + master$capgntax_nbr + master$salestax_sub + master$salestax_nbr +
                         master$corptax_sub + master$corptax_nbr +  master$wctaxfixed_sub + master$wctaxfixed_nbr +
                         master$uitaxrate_sub + master$uitaxrate_nbr 
                       + master$educ_pc_L1_sub+master$educ_pc_L1_nbr+ master$hwy_pc_L1_sub+ 
                         master$hwy_pc_L1_nbr + master$welfare_pc_L1_sub +  master$welfare_pc_L1_nbr +
                         master$JAN.TEMP...Z_sub + master$JAN.TEMP...Z_nbr
                       + master$JAN.SUN...Z_sub + master$JAN.SUN...Z_nbr
                       + master$JUL.TEMP...Z_sub + master$JUL.TEMP...Z_nbr
                       + master$JUL.HUM...Z_sub + master$JUL.HUM...Z_nbr
                       + master$TOPOG...Z_sub + master$TOPOG...Z_nbr
                       + master$LN.WATER..AREA...Z_sub + master$LN.WATER..AREA...Z_nbr, 
                       subset = master$births_sub > 0 & master$births_nbr > 0)
 stprid_c_vcov <- cluster.vcov(pols1_amen_real, master$stpr_id[master$births_sub > 0 & master$births_nbr > 0])
 pols1_amen_r_coef <- coeftest(pols1_amen_real, vcov = stprid_c_vcov) # results
 

 # without imposing equality, real valued, no amenities
 pols1_real <- lm(master$births_ratio ~  master$ptax_sub + master$ptax_nbr + master$inctax_sub +  master$inctax_nbr
                  + master$capgntax_sub + master$capgntax_nbr + master$salestax_sub + master$salestax_nbr +
                    master$corptax_sub + master$corptax_nbr +  master$wctaxfixed_sub + master$wctaxfixed_nbr +
                    master$uitaxrate_sub + master$uitaxrate_nbr 
                  + master$educ_pc_L1_sub+master$educ_pc_L1_nbr+ master$hwy_pc_L1_sub+ 
                    master$hwy_pc_L1_nbr + master$welfare_pc_L1_sub +  master$welfare_pc_L1_nbr, 
                  subset = master$births_sub > 0 & master$births_nbr > 0)
 stprid_c_vcov <- cluster.vcov(pols1_real, master$stpr_id[master$births_sub > 0 & master$births_nbr > 0])
 pols1_r_coef <- coeftest(pols1_real, vcov = stprid_c_vcov)
 
# write(stargazer(pols1_amen_r_coef, pols1_r_coef, no.space = TRUE, title = paste("Pseudo-RD for Stability of Terms over Borders for ", naics[m], sep = " ")), paste("~/papers/firm_entry/analysis/output/", naics[m], "no_equality_rd_results.tex", sep = "_"))
 
 # tests for equality
 linearHypothesis(pols1_real, c("master$ptax_sub = -master$ptax_nbr"), vcov = stprid_c_vcov)
 linearHypothesis(pols1_real, c("master$inctax_sub = -master$inctax_nbr"), vcov = stprid_c_vcov)
 linearHypothesis(pols1_real, c("master$capgntax_sub = -master$capgntax_nbr"), vcov = stprid_c_vcov)
 linearHypothesis(pols1_real, c("master$salestax_sub = -master$salestax_nbr"), vcov = stprid_c_vcov)
 linearHypothesis(pols1_real, c("master$corptax_sub = -master$corptax_nbr"), vcov = stprid_c_vcov)
 linearHypothesis(pols1_real, c("master$wctaxfixed_sub = -master$wctaxfixed_nbr"), vcov = stprid_c_vcov)
 linearHypothesis(pols1_real, c("master$uitaxrate_sub = -master$uitaxrate_nbr"), vcov = stprid_c_vcov)
 
 
 # imposing equality across borders

 # count data, difference, with amenities
 pols_amen_c <- lm(master$births_diff ~ master$ptax_diff + master$inctax_diff + master$capgntax_diff
                   + master$salestax_diff  + master$corptax_diff  + master$wctax_diff  + master$uitax_diff
                   + master$educ_pc_L1_diff + master$hwy_pc_L1_diff + master$welfare_pc_L1_diff
                   + master$hsplus_diff + master$realfuelpr_diff + master$unionmem_diff
                   + master$popdensity_diff + master$pctmanuf_dif + master$JAN.TEMP.Z_diff
                   + master$JAN.SUN.Z_diff + master$JUL.TEMP.Z_diff + master$JUL.HUM.Z_diff
                   + master$TOPOG.Z_diff + master$WATER.AR.Z_diff)
 stprid_c_vcov <- cluster.vcov(pols_amen_c, master$stpr_id)
 pols_amen_c_coef <- coeftest(pols_amen_c, vcov = stprid_c_vcov)
 
 
 pols_namen_c <- lm(master$births_diff ~ master$ptax_diff + master$inctax_diff + master$capgntax_diff
                   + master$salestax_diff  + master$corptax_diff  + master$wctax_diff  + master$uitax_diff
                   + master$educ_pc_L1_diff + master$hwy_pc_L1_diff + master$welfare_pc_L1_diff
                   + master$hsplus_diff + master$realfuelpr_diff + master$unionmem_diff
                   + master$popdensity_diff + master$pctmanuf_dif )
 stprid_c_vcov <- cluster.vcov(pols_namen_c, master$stpr_id)
 pols_namen_c_coef <- coeftest(pols_namen_c, vcov = stprid_c_vcov)
 
 pols_amen_r <- lm(master$births_ratio ~ master$ptax_diff + master$inctax_diff + master$capgntax_diff
                  + master$salestax_diff  + master$corptax_diff  + master$wctax_diff  + master$uitax_diff
                  + master$educ_pc_L1_diff + master$hwy_pc_L1_diff + master$welfare_pc_L1_diff
                  + master$hsplus_diff + master$realfuelpr_diff + master$unionmem_diff
                  + master$popdensity_diff + master$pctmanuf_dif + master$JAN.TEMP.Z_diff
                  + master$JAN.SUN.Z_diff + master$JUL.TEMP.Z_diff + master$JUL.HUM.Z_diff
                  + master$TOPOG.Z_diff + master$WATER.AR.Z_diff, 
                  subset = master$births_sub > 0 & master$births_nbr > 0)
 stprid_c_vcov <- cluster.vcov(pols_amen_r, master$stpr_id[master$births_sub > 0 & master$births_nbr > 0])
 pols_amen_r_coef <- coeftest(pols_amen_r, vcov = stprid_c_vcov)

 pols_namen_r <- lm(master$births_ratio ~ master$ptax_diff + master$inctax_diff + master$capgntax_diff
                    + master$salestax_diff  + master$corptax_diff  + master$wctax_diff  + master$uitax_diff
                    + master$educ_pc_L1_diff + master$hwy_pc_L1_diff + master$welfare_pc_L1_diff
                    + master$hsplus_diff + master$realfuelpr_diff + master$unionmem_diff
                    + master$popdensity_diff + master$pctmanuf_dif, 
                    subset = master$births_sub > 0 & master$births_nbr > 0)
 stprid_c_vcov <- cluster.vcov(pols_namen_r,  master$stpr_id[master$births_sub > 0 & master$births_nbr > 0])
 pols_namen_r_coef <- coeftest(pols_namen_r, vcov = stprid_c_vcov)
 
# write(stargazer(pols_amen_r_coef, pols_namen_r_coef, no.space = TRUE, title = paste("Pseudo-RD Base for ", naics[m], sep = " ")), paste("~/papers/firm_entry/analysis/output/", naics[m], "equality_rd_results.tex", sep = "_"))
 
 
 ## Robustness checks
 # Run regressions for each year
 for (i in 1999:2009)  {
   sub <- master[master$year == i,]
   pols_year <- lm(sub$births_ratio ~ sub$ptax_diff + sub$inctax_diff + sub$capgntax_diff
                   + sub$salestax_diff  + sub$corptax_diff  + sub$wctax_diff  + sub$uitax_diff
                   + sub$educ_pc_L1_diff + sub$hwy_pc_L1_diff + sub$welfare_pc_L1_diff
                   + sub$hsplus_diff + sub$realfuelpr_diff + sub$unionmem_diff
                   + sub$popdensity_diff + sub$pctmanuf_dif , 
                   subset = sub$births_sub > 0 & sub$births_nbr > 0)
   stprid_c_vcov <- cluster.vcov(pols_year, sub$stpr_id[sub$births_sub > 0 & sub$births_nbr > 0])
   assign(paste("pols",i,sep="_"), coeftest(pols_year, vcov = stprid_c_vcov)) 
 }
 rm(sub)
 
 write(stargazer(pols_1999, pols_2003, pols_2006, pols_2009, no.space = TRUE, title = paste("Psuedo-RD for Stability over Time for ", naics_names[m], "Firm Births", sep = " ")), paste("~/papers/firm_entry/analysis/output/", naics[m], "year_rd_results.tex", sep = "_"))
 
 # iv estimation
 simple.pop.1s<- lm(pop_diff ~ WATER.AR.Z_diff, data = master)
 # very significant, but R^2 only about 0.04, so probably a weak instrument?

 master$pop.pred<- predict(simple.pop.1s, data = master)
 simple.pop.2s<- lm(master$births_ratio ~ master$ptax_diff + master$inctax_diff + master$capgntax_diff
                   + master$salestax_diff  + master$corptax_diff  + master$wctax_diff  + master$uitax_diff
                   + master$educ_pc_L1_diff + master$hwy_pc_L1_diff + master$welfare_pc_L1_diff
                   + master$hsplus_diff + master$realfuelpr_diff + master$unionmem_diff
                   + master$popdensity_diff + master$pctmanuf_dif + master$pop.pred, 
                   subset = master$births_sub > 0 & master$births_nbr > 0)
 stprid_c_vcov <- cluster.vcov(simple.pop.2s,  master$stpr_id[master$births_sub > 0 & master$births_nbr > 0])
 simple.pop.2s_coef <- coeftest(simple.pop.2s, vcov = stprid_c_vcov)

 # fixed effect model
 master$stpr_fe <- factor(master$stpr_id)

 master <- master[is.finite(master$births_ratio),]

 pols_amen_fe  <- felm(master$births_ratio ~ master$ptax_diff + master$inctax_diff + master$capgntax_diff
                       + master$salestax_diff  + master$corptax_diff  + master$wctax_diff  + master$uitax_diff
                       + master$educ_pc_L1_diff + master$hwy_pc_L1_diff + master$welfare_pc_L1_diff
                       + master$hsplus_diff + master$realfuelpr_diff + master$unionmem_diff
                       + master$popdensity_diff + master$pctmanuf_dif  + master$JAN.TEMP.Z_diff
                       + master$JAN.SUN.Z_diff + master$JUL.TEMP.Z_diff + master$JUL.HUM.Z_diff
                       + master$TOPOG.Z_diff + master$WATER.AR.Z_diff  | master$stpr_fe | 0 | master$stpr_id)

 pols_fe <- felm(master$births_ratio ~ master$ptax_diff + master$inctax_diff + master$capgntax_diff
                 + master$salestax_diff  + master$corptax_diff  + master$wctax_diff  + master$uitax_diff
                 + master$educ_pc_L1_diff + master$hwy_pc_L1_diff + master$welfare_pc_L1_diff
                 + master$hsplus_diff + master$realfuelpr_diff + master$unionmem_diff
                 + master$popdensity_diff + master$pctmanuf_dif | master$stpr_fe | 0 | master$stpr_id)

 # write tables
 write(stargazer(pols_amen_r_coef, pols_namen_r_coef, pols_amen_fe, pols_fe, simple.pop.2s_coef, 
                 dep.var.labels = c("births ratio"),
                 covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                      "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                      "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                      "Welfare Spending Per Cap Diff", "Pct Highschool Diff", "Real Fuel Price Diff",
                                      "Pct Union Diff", "Pop Density Diff", "Pct Manufacturing Diff", "Population Diff"),
                 omit = c("Z"), omit.labels = c("amenities"), omit.table.layout = "sn",
                 column.labels = c("OLS","OLS","FE", "FE","IV"),
                 no.space = TRUE, title = paste("Pseudo-Regression Models for ", naics_names[m], "Firm Births", sep = " ")), paste("~/papers/firm_entry/analysis/output/", naics[m], "rd_results.tex", sep = "_"))
 
 rm(master)

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
  master$hsplus_diff <- master$hsplus_sub - master$hsplus_nbr
  master$realfuelpr_diff <- master$realfuelpr_sub - master$realfuelpr_nbr
  master$unionmem_diff <- master$unionmem_sub - master$unionmem_nbr
  master$popdensity_diff <- master$popdensity_sub - master$popdensity_nbr
  master$pctmanuf_diff <- master$pctmanuf_sub - master$pctmanuf_nbr
  
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

  # imposing equality across borders
  pols_amen_r <- lm(master$births_ratio ~ master$ptax_diff + master$inctax_diff + master$capgntax_diff
                    + master$salestax_diff  + master$corptax_diff  + master$wctax_diff  + master$uitax_diff
                    + master$educ_pc_L1_diff + master$hwy_pc_L1_diff + master$welfare_pc_L1_diff
                    + master$hsplus_diff + master$realfuelpr_diff + master$unionmem_diff
                    + master$popdensity_diff + master$pctmanuf_dif + master$JAN.TEMP.Z_diff
                    + master$JAN.SUN.Z_diff + master$JUL.TEMP.Z_diff + master$JUL.HUM.Z_diff
                    + master$TOPOG.Z_diff + master$WATER.AR.Z_diff, 
                    subset = master$births_sub > 0 & master$births_nbr > 0)
  stprid_c_vcov <- cluster.vcov(pols_amen_r, master$stpr_id[master$births_sub > 0 & master$births_nbr > 0])
  pols_amen_r_coef <- coeftest(pols_amen_r, vcov = stprid_c_vcov)
  
  pols_namen_r <- lm(master$births_ratio ~ master$ptax_diff + master$inctax_diff + master$capgntax_diff
                     + master$salestax_diff  + master$corptax_diff  + master$wctax_diff  + master$uitax_diff
                     + master$educ_pc_L1_diff + master$hwy_pc_L1_diff + master$welfare_pc_L1_diff
                     + master$hsplus_diff + master$realfuelpr_diff + master$unionmem_diff
                     + master$popdensity_diff + master$pctmanuf_dif, 
                     subset = master$births_sub > 0 & master$births_nbr > 0)
  stprid_c_vcov <- cluster.vcov(pols_namen_r,  master$stpr_id[master$births_sub > 0 & master$births_nbr > 0])
  pols_namen_r_coef <- coeftest(pols_namen_r, vcov = stprid_c_vcov)

  # iv estimation
  simple.pop.1s<- lm(master$pop_diff ~ master$WATER.AR.Z_diff)
  
  master$pop.pred<- predict(simple.pop.1s, newdata = master)
  simple.pop.2s<- lm(master$births_ratio ~ master$ptax_diff + master$inctax_diff + master$capgntax_diff
                     + master$salestax_diff  + master$corptax_diff  + master$wctax_diff  + master$uitax_diff
                     + master$educ_pc_L1_diff + master$hwy_pc_L1_diff + master$welfare_pc_L1_diff
                     + master$hsplus_diff + master$realfuelpr_diff + master$unionmem_diff
                     + master$popdensity_diff + master$pctmanuf_dif + master$pop.pred, 
                     subset = master$births_sub > 0 & master$births_nbr > 0)
  stprid_c_vcov <- cluster.vcov(simple.pop.2s,  master$stpr_id[master$births_sub > 0 & master$births_nbr > 0])
  simple.pop.2s_coef <- coeftest(simple.pop.2s, vcov = stprid_c_vcov)
  
  # fixed effect model
  master$stpr_fe <- factor(master$stpr_id)
  
  master <- master[is.finite(master$births_ratio),]
  
  pols_amen_fe  <- felm(master$births_ratio ~ master$ptax_diff + master$inctax_diff + master$capgntax_diff
                        + master$salestax_diff  + master$corptax_diff  + master$wctax_diff  + master$uitax_diff
                        + master$educ_pc_L1_diff + master$hwy_pc_L1_diff + master$welfare_pc_L1_diff
                        + master$hsplus_diff + master$realfuelpr_diff + master$unionmem_diff
                        + master$popdensity_diff + master$pctmanuf_dif  + master$JAN.TEMP.Z_diff
                        + master$JAN.SUN.Z_diff + master$JUL.TEMP.Z_diff + master$JUL.HUM.Z_diff
                        + master$TOPOG.Z_diff + master$WATER.AR.Z_diff  | master$stpr_fe | 0 | master$stpr_id)
  
  pols_fe <- felm(master$births_ratio ~ master$ptax_diff + master$inctax_diff + master$capgntax_diff
                  + master$salestax_diff  + master$corptax_diff  + master$wctax_diff  + master$uitax_diff
                  + master$educ_pc_L1_diff + master$hwy_pc_L1_diff + master$welfare_pc_L1_diff
                  + master$hsplus_diff + master$realfuelpr_diff + master$unionmem_diff
                  + master$popdensity_diff + master$pctmanuf_dif | master$stpr_fe | 0 | master$stpr_id)
  
  write(stargazer(pols_amen_r_coef, pols_namen_r_coef, pols_amen_fe, pols_fe, simple.pop.2s_coef, 
                  dep.var.labels = c("births ratio"),
                  covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                       "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                       "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                       "Welfare Spending Per Cap Diff", "Pct Highschool Diff", "Real Fuel Price Diff",
                                       "Pct Union Diff", "Pop Density Diff", "Pct Manufacturing Diff", "Population Diff"),
                  omit = c("Z"), omit.labels = c("amenities"), omit.table.layout = "sn",
                  column.labels = c("OLS","OLS","FE", "FE","IV"),
                  no.space = TRUE, title = paste("Extended Bandwidth Models for ", naics_names[m], "Firm Births", sep = " ")), paste("~/papers/firm_entry/analysis/output/", naics[m], "extended_rd_results.tex", sep = "_"))
  
  rm(master)
}