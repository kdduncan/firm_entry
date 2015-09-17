require(ggplot2) # plotting
require(MASS) # glm.nb
require(stargazer) # build latex tables
require(lme4) # glmer
require(AER) # dispersion test



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
  master <- read.csv(paste("~/papers/firm_entry/analysis/input/", naics[m],"master.csv", sep = "_"))
  
  state_master <-matrix(c(1:11088), nrow = 528, ncol = 21)
  s_births <-  c(1:528)
  k <- 1
  for (i in 1:length(unique(master$statefips))){
    for (j in 1:length(unique(master$year))){
      tmp <- master[master$statefips == unique(master$statefips)[i] & master$year == unique(master$year)[j],]
      s_births[k] <- sum(tmp$births)
      state_master[k,] <- as.matrix(tmp[1,c(13:33)])
      k <- k+1
    }
  }

  state_master <- as.data.frame(state_master)
  state_master$s_births <- s_births
  names(state_master)[1:21] <- names(master[,13:33])
  
  # state levels
  

  pois_amen_st <- glm(state_master$s_births ~  state_master$educ_pc_L1 + state_master$hwy_pc_L1 + state_master$welfare_pc_L1 
                      + state_master$ptax + state_master$inctax + state_master$capgntax + state_master$salestax
                      + state_master$corptax + state_master$wctaxfixed  + state_master$uitaxrate
                      + state_master$hsplus + state_master$realfuelpr + state_master$unionmem + state_master$popdensity
                      + state_master$pctmanuf + state_master$JAN.TEMP...Z + state_master$JAN.SUN...Z + state_master$JUL.TEMP...Z
                      + state_master$JUL.HUM...Z + state_master$TOPOG...Z + state_master$LN.WATER..AREA...Z, family = poisson(link = log))
  
  dispersiontest(pois_amen_st,trafo=1)
  
  nb_amen_st <- glm.nb(state_master$s_births ~  state_master$educ_pc_L1 + state_master$hwy_pc_L1 + state_master$welfare_pc_L1 
                       + state_master$ptax + state_master$inctax + state_master$capgntax + state_master$salestax
                       + state_master$corptax + state_master$wctaxfixed  + state_master$uitaxrate
                       + state_master$hsplus + state_master$realfuelpr + state_master$unionmem + state_master$popdensity
                       + state_master$pctmanuf + state_master$JAN.TEMP...Z + state_master$JAN.SUN...Z + state_master$JUL.TEMP...Z
                       + state_master$JUL.HUM...Z + state_master$TOPOG...Z + state_master$LN.WATER..AREA...Z)
  norm_amen_st <- lm(log(state_master$s_births) ~  state_master$educ_pc_L1 + state_master$hwy_pc_L1 + state_master$welfare_pc_L1 
                     + state_master$ptax + state_master$inctax + state_master$capgntax + state_master$salestax
                     + state_master$corptax + state_master$wctaxfixed  + state_master$uitaxrate
                     + state_master$hsplus + state_master$realfuelpr + state_master$unionmem + state_master$popdensity
                     + state_master$pctmanuf + state_master$JAN.TEMP...Z + state_master$JAN.SUN...Z + state_master$JUL.TEMP...Z
                     + state_master$JUL.HUM...Z + state_master$TOPOG...Z + state_master$LN.WATER..AREA...Z, subset = state_master$s_births > 0)
  
  
  # regression based test for overdispersion

  
  write(stargazer(pois_amen_st, nb_amen_st, norm_amen_st, 
                  dep.var.labels = c("state firm births", "log state firm births"),
                  covariate.labels = c("Educ Spending Per Cap", "Highway Spending Per Cap ",
                                       "Welfare Spending Per Cap ", "Property Tax", "Income Tax", "Capital Gains Tax",
                                       "Sales Tax", "Corp Tax", "Workers Comp Tax",
                                       "Unemp. Tax",  "Pct Highschool", "Real Fuel Price",
                                       "Pct Union", "Pop Density", "Pct Manufacturing",
                                       "Jan Temp Z Score", "Jan Sun Z Score", "Jul Tmp Z Score",
                                       "Jul Hum Z Score", "Top Z Score", "Ln Area Water"),
                  no.space = TRUE, title = paste("Count Data Models for",naics_names[m],"Firm Births",sep = " ")), paste("~/papers/firm_entry/analysis/output/", naics[m], "state_cdm_results.tex", sep = "_"))
 
  rm(master)
  rm(state_master)
}
