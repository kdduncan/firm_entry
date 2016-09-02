library(multiwayvcov) # clustered standard errors
library(lfe) #felm fixed effect linear models
library(stargazer) #stargazer
library(car) #linearhypothesis
library(lmtest) #coeftest
naics <- c("--")
naicsf <- c("00")

naics_names <- c("Total")

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
  
  # master = 13115 obs prior to merge
  share <- read.csv("~/papers/firm_entry/build/output/share.csv")
  test <- cbind(share$cofip,share$year)
  
  share_sub <- share
  names(share_sub)[3:7] <- paste(names(share_sub)[3:7],"sub",sep="_")
  share_nbr <- share
  names(share_nbr)[3:7] <- paste(names(share_nbr)[3:7],"nbr",sep="_")
  
  master <- merge(master,share_sub,by = c("year","cofip_sub"))
  # merge = 10728, => 13115 - 10728 = 2387, => 13115/2387 = 5.49344, so not a clear divisor
  master <- merge(master,share_nbr,by = c("year","cofip_nbr"))
  
  master$agshare_diff <- master$agShare_sub - master$agShare_nbr
  master$manufshare_diff <- master$manufShare_sub - master$manufShare_nbr
  master$retailshare_diff <- master$retailShare_sub - master$retailShare_nbr
  master$financeshare_diff <- master$financeShare_sub - master$financeShare_nbr
  
  
  pols_namen_nc_real <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                           + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                           + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                           + agshare_diff + manufshare_diff + retailshare_diff + financeshare_diff,
                           data = master)
  stprid_c_vcov <- cluster.vcov(pols_namen_nc_real, master$stpr_id)
  pols_namen_nc_r_coef <- coeftest(pols_namen_nc_real, vcov = stprid_c_vcov) # results

  pols_amen_nc_real <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                          + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                          + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                          + agshare_diff + manufshare_diff + retailshare_diff + financeshare_diff
                          + JAN.TEMP.Z_diff + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                          + TOPOG.Z_diff + WATER.AR.Z_diff, 
                          data = master)
  stprid_c_vcov <- cluster.vcov(pols_amen_nc_real , master$stpr_id)
  pols_amen_nc_r_coef <- coeftest(pols_amen_nc_real , vcov = stprid_c_vcov) # results
  
  pols_namen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                       + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                       + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                       + agshare_diff + manufshare_diff + retailshare_diff + financeshare_diff
                       + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                       + popdensity.cont_diff + pctmanuf.cont_diff, 
                       data = master)
  stprid_c_vcov <- cluster.vcov(pols_namen_c_r,  master$stpr_id)
  pols_namen_c_r_coef <- coeftest(pols_namen_c_r, vcov = stprid_c_vcov)

  pols_amen_c_r <- lm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                      + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                      + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                      + agshare_diff + manufshare_diff + retailshare_diff + financeshare_diff
                      + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                      + popdensity.cont_diff + pctmanuf.cont_diff + JAN.TEMP.Z_diff
                      + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                      + TOPOG.Z_diff + WATER.AR.Z_diff, 
                      data = master)
  stprid_c_vcov <- cluster.vcov(pols_amen_c_r, master$stpr_id)
  pols_amen_c_r_coef <- coeftest(pols_amen_c_r, vcov = stprid_c_vcov)
  
  # fixed effect model
  master$stpr_fe <- factor(master$stpr_id)
  master <- master[is.finite(master$births_ratio),]
  
  pols_amen_fe  <- felm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                        + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                        + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                        + agshare_diff + manufshare_diff + retailshare_diff + financeshare_diff
                        + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                        + popdensity.cont_diff + pctmanuf.cont_diff  + JAN.TEMP.Z_diff
                        + JAN.SUN.Z_diff + JUL.TEMP.Z_diff + JUL.HUM.Z_diff
                        + TOPOG.Z_diff + WATER.AR.Z_diff  | stpr_fe | 0 |0, data = master)
  
  pols_fe <- felm(births_ratio ~ ptax_diff + inctax_diff + capgntax_diff
                  + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                  + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                  + agshare_diff + manufshare_diff + retailshare_diff + financeshare_diff
                  + hsplus.cont_diff + realfuelpr.cont_diff + unionmem.cont_diff
                  + popdensity.cont_diff + pctmanuf.cont_diff | stpr_fe | 0 | 0, data = master)
  
  
  write(stargazer(pols_amen_c_r, pols_namen_c_r, pols_amen_nc_real, pols_namen_nc_real, pols_amen_fe, pols_fe,
                  se = list(pols_amen_c_r_coef[,2],pols_namen_c_r_coef[,2],pols_amen_nc_r_coef[,2],pols_namen_nc_r_coef[,2],NULL, NULL),
                  label = paste(naics[m], "rd", sep = ""), dep.var.labels = c("births ratio"), model.names = FALSE,
                  font.size = "footnotesize",
                  covariate.labels = c("Property Tax Difference", "Income Tax Difference", "Capital Gains Tax Difference",
                                       "Sales Tax Difference", "Corp Tax Difference", "Workers Comp Tax Difference",
                                       "Unemp. Tax Difference", "Educ Spending Per Cap Diff", "Highway Spending Per Cap Diff",
                                       "Welfare Spending Per Cap Diff","ag share", "manuf share", "retail share", "finance share"),
                  notes = c("The first four columns are estimated with OLS and clustered standard",
                            " errors at the state-pair level. Columns 5 and 6 are estimated with",
                            "a fixed effect estimator at the state-pair level with homoskedastic", "standard errors."),
                  omit = c("cont","Z"), omit.labels = c('controls', 'amenities'), omit.yes.no = c("Yes", "No"), omit.stat = c("f","adj.rsq","ser"),
                  column.labels = c("OLS","OLS","OLS","OLS","FE", "FE"),
                  no.space = TRUE, title = paste("Regression Discontinuity Models for ", naics_names[m], "Firm Births", sep = " ")),
        paste("~/papers/firm_entry/analysis/output/", naics[m], "agg_rd_results.tex", sep = "_"))
  
  
  
  
  
  
  
  ptax <- lm( ptax_diff ~ births_ratio + inctax_diff + capgntax_diff
              + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
              + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
              + agshare_diff + manufshare_diff + retailshare_diff + financeshare_diff,
              data = master)
  stprid_c_vcov <- cluster.vcov(ptax, master$stpr_id)
  ptax_coef <- coeftest(ptax, vcov = stprid_c_vcov) # results
  
  inctax <- lm(inctax_diff ~ births_ratio + ptax_diff + capgntax_diff
               + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
               + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
               + agshare_diff + manufshare_diff + retailshare_diff + financeshare_diff,
               data = master)
  stprid_c_vcov <- cluster.vcov(inctax, master$stpr_id)
  inctax_coef <- coeftest(inctax, vcov = stprid_c_vcov) # results
  
  capgntax <- lm(capgntax_diff ~ births_ratio + ptax_diff + inctax_diff
                 + salestax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                 + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                 + agshare_diff + manufshare_diff + retailshare_diff + financeshare_diff,
                 data = master)
  stprid_c_vcov <- cluster.vcov(capgntax, master$stpr_id)
  capgntax_coef <- coeftest(capgntax, vcov = stprid_c_vcov) # results
  
  salestax <- lm(salestax_diff ~ births_ratio + ptax_diff + inctax_diff
                 + capgntax_diff  + corptax_diff  + wctax_diff  + uitax_diff
                 + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                 + agshare_diff + manufshare_diff + retailshare_diff + financeshare_diff,
                 data = master)
  stprid_c_vcov <- cluster.vcov(salestax, master$stpr_id)
  salestax_coef <- coeftest(salestax, vcov = stprid_c_vcov) # results
  
  corptax <- lm(corptax_diff ~ births_ratio + ptax_diff + inctax_diff
                + capgntax_diff  + salestax_diff  + wctax_diff  + uitax_diff
                + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
                + agshare_diff + manufshare_diff + retailshare_diff + financeshare_diff,
                data = master)
  stprid_c_vcov <- cluster.vcov(corptax, master$stpr_id)
  corptax_coef <- coeftest(corptax, vcov = stprid_c_vcov) # results
  
  wctax <- lm( wctax_diff ~ births_ratio + ptax_diff + inctax_diff
               + capgntax_diff  + salestax_diff  + corptax_diff + uitax_diff
               + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
               + agshare_diff + manufshare_diff + retailshare_diff + financeshare_diff,
               data = master)
  stprid_c_vcov <- cluster.vcov(wctax, master$stpr_id)
  wctax_coef <- coeftest(wctax, vcov = stprid_c_vcov) # results
  
  uitax <- lm(uitax_diff ~ births_ratio + ptax_diff + inctax_diff
              + capgntax_diff  + salestax_diff  + corptax_diff + wctax_diff
              + educ_pc_L1_diff + hwy_pc_L1_diff + welfare_pc_L1_diff
              + agshare_diff + manufshare_diff + retailshare_diff + financeshare_diff,
              data = master)
  stprid_c_vcov <- cluster.vcov(uitax, master$stpr_id)
  uitax_coef <- coeftest(uitax, vcov = stprid_c_vcov) # results
  
  png(filename=paste("~/papers/firm_entry/analysis/output/agg_absresidualtaxplots", naics[m], ".png", sep = "_"))
  par(mfrow = c(3,3))
  plot(abs(master$ptax_diff),master$ptax_diff-predict(ptax), main = "property tax")
  plot(abs(master$inctax_diff),master$inctax_diff-predict(inctax), main = "income tax")
  plot(abs(master$salestax_diff),master$salestax_diff-predict(salestax), main = "sales tax")
  plot(abs(master$capgntax_diff),master$capgntax_diff-predict(capgntax), main = "capital gains tax")
  plot(abs(master$corptax_diff),master$corptax_diff-predict(corptax), main = "corporate tax")
  plot(abs(master$wctax_diff),master$wctax_diff-predict(wctax), main = "workers compensation tax")
  plot(abs(master$uitax_diff),master$uitax_diff-predict(uitax), main = "unemployment insurance tax")
  dev.off()
  
  png(filename=paste("~/papers/firm_entry/analysis/output/agg_predictedtaxplots", naics[m], ".png", sep = "_"))
  par(mfrow = c(3,3))
  plot(master$ptax_diff,predict(ptax), main = "property tax")
  plot(master$inctax_diff,predict(inctax), main = "income tax")
  plot(master$salestax_diff,predict(salestax), main = "sales tax")
  plot(master$capgntax_diff,predict(capgntax), main = "capital gains tax")
  plot(master$corptax_diff,predict(corptax), , main = "corporate tax")
  plot(master$wctax_diff,predict(wctax), main = "workers compensation tax")
  plot(master$uitax_diff,predict(uitax), main = "unemployment insurance tax")
  dev.off()
  
  stargazer(ptax, inctax, capgntax, salestax,  corptax, wctax, uitax,
            se = list(ptax_coef[,2],inctax_coef[,2],capgntax_coef[,2],salestax_coef[,2],corptax_coef[,2], wctax_coef[,2],uitax_coef[,2]),
            label = paste(naics[m], "rd", sep = ""), dep.var.labels = c("births ratio"), model.names = FALSE,
            font.size = "footnotesize", notes = c("The first four columns are estimated with OLS and clustered standard",
                                                  " errors at the state-pair level. Columns 5 and 6 are estimated with",
                                                  "a fixed effect estimator at the state-pair level with homoskedastic", "standard errors."),
            omit.stat = c("f","adj.rsq","ser"), column.labels = c("ptax","inctax","capgntax","sales","corp", "wc", "ui"),
            no.space = TRUE, title = paste("Inverted Model for ", naics_names[m], "Firm Births", sep = " "),
            paste("~/papers/firm_entry/analysis/output/", naics[m], "agg_invrd_results.tex", sep = "_"))
}