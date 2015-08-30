require(ggplot2) # plotting
require(MASS) # glm.nb
require(stargazer) # build latex tables
require(lme4) # glmer
require(AER) # dispersion test

master <- read.csv("~/papers/firm_entry/analysis/input/master.csv")
attach(master)

state_master <-matrix(c(1:11088), nrow = 528, ncol = 21)
s_births <-  c(1:528)
k <- 1
for (i in 1:length(unique(statefips))){
  for (j in 1:length(unique(year))){
    tmp <- master[statefips == unique(statefips)[i] & year == unique(year)[j],]
    s_births[k] <- sum(tmp$births)
    state_master[k,] <- as.matrix(tmp[1,c(13:33)])
    k <- k+1
  }
}

state_master <- statemaster[,-c(1)]
# state levels
pois_amen_st <- glm(s_births ~ state_master, family = poisson(link = log))
nb_amen_st <- glm.nb(s_births ~ state_master)

pois_amen <- glm(births ~  ptax+inctax+capgntax+salestax+corptax+wctaxfixed+uitaxrate+
                   educ_pc_L1+hwy_pc_L1+welfare_pc_L1+
                 hsplus+realfuelpr+unionmem+popdensity+pctmanuf+
                   JAN.TEMP...Z+JAN.SUN...Z+JUL.TEMP...Z+JUL.HUM...Z+TOPOG...Z+LN.WATER..AREA...Z, family = poisson(link=log))
write(stargazer(pois_amen), "~/papers/firm_entry/analysis/output/pois_amen_results.tex")

# regression based test for overdispersion
dispersiontest(pois_amen,trafo=1)

nb_amen <- glm.nb(births ~  ptax+inctax+capgntax+salestax+corptax+wctaxfixed+uitaxrate+
                    educ_pc_L1+hwy_pc_L1+welfare_pc_L1+
                    hsplus+realfuelpr+unionmem+popdensity+pctmanuf+
                    JAN.TEMP...Z+JAN.SUN...Z+JUL.TEMP...Z+JUL.HUM...Z+TOPOG...Z+LN.WATER..AREA...Z)

pois_amen_fixed <- glmer(births ~  ptax+inctax+capgntax+salestax+corptax+wctaxfixed+uitaxrate+
                           educ_pc_L1+hwy_pc_L1+welfare_pc_L1+
                           hsplus+realfuelpr+unionmem+popdensity+pctmanuf+
                           JAN.TEMP...Z+JAN.SUN...Z+JUL.TEMP...Z+JUL.HUM...Z+TOPOG...Z+LN.WATER..AREA...Z+
                           (1|statefips),family=poisson(link=log))

nb_amen-fixed <- glmer.nb(births ~  ptax+inctax+capgntax+salestax+corptax+wctaxfixed+uitaxrate+
                            educ_pc_L1+hwy_pc_L1+welfare_pc_L1+
                            hsplus+realfuelpr+unionmem+popdensity+pctmanuf+
                            JAN.TEMP...Z+JAN.SUN...Z+JUL.TEMP...Z+JUL.HUM...Z+TOPOG...Z+LN.WATER..AREA...Z+
                            (1|statefips))