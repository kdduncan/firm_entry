################# required packages ################# 
require(ggplot2) # plotting
require(plm) # pooled linear model
require(lmtest) # linear model tests
require(sandwich) # sandwich estimators
require(multiwayvcov) # multivariate
require(stats4) # mle estimation
require(car) # F tests
require(MASS) # glm.nb
require(stargazer) # build latex tables
require(lme4) # glmer
require(AER)

#################  data files ################# 
df <- read.csv("~/papers/mscthesis/dataset_MSc_Thesis.csv")
attach(df)

vars <- data.frame(cofip_sub, cofip_nbr, births_ratio, births_diff,ptax_ratio_L1, inctax_ratio_L1, capgntax_ratio_L1, 
                   stax_ratio_L1,corptax_ratio_L1, wctax_ratio_L1, uitax_ratio_L1, minwage_L1_ratio,
                   rtw_L1_ratio, educ_ratio_L1, hwy_ratio_L1, welfare_ratio_L1,
                   hs_L1_diff, fuel_L1_diff, union_L1_diff, density_L1_diff, manuf_L1_diff)

amen_sub <- read.csv("~/papers/mscthesis/natamenf_sub.csv")
amen_nbr <- read.csv("~/papers/mscthesis/natamenf_nbr.csv")

# merge together data sets
vars <- merge(vars, amen_sub, by = "cofip_sub")
vars <- merge(vars, amen_nbr, by = "cofip_nbr")

vars$jan_sun_z_ratio <- vars$jan_sun_z_sub- vars$jan_sun_z_nbr
df$jan_sun_z_ratio <- vars$jan_sun_z_sub- vars$jan_sun_z_nbr
vars$jul_temp_z_ratio <- vars$jul_temp_z_sub - vars$jul_temp_z_nbr
df$jul_temp_z_ratio <- vars$jul_temp_z_sub - vars$jul_temp_z_nbr
vars$jul_hum_z_ratio <- vars$jul_hum_z_sub -vars$jul_hum_z_nbr
df$jul_hum_z_ratio <- vars$jul_hum_z_sub -vars$jul_hum_z_nbr
vars$top_z_ratio <- vars$top_z_sub - vars$top_z_nbr
df$top_z_ratio <- vars$top_z_sub - vars$top_z_nbr
vars$pct_water_z_ratio <- vars$pct_water_z_sub - vars$pct_water_z_nbr
df$pct_water_z_ratio <- vars$pct_water_z_sub - vars$pct_water_z_nbr
detach(df)
attach(df)

################# Data Analysis ################# 

# number of unique state pairs, county pairs
stpr_num <- unique(df$stpr_id)
cofip_num <- unique(df$id)

length(stpr_num) # unique state pairs
length(cofip_num) # unique county pairs

# degrees of freedom
table(df$stpr_id)

# correlations between terms
cor(vars)

# aggregate firm start ups
par(mfrow = c(1,2))
hist(c(df$births_sub,df$births_nbr), breaks = 100, main = "Number of Firm Start Ups",
     xlab = "Number of Firms", ylab = "Frequency")

# firm start ups closer together
hist(c(df$births_sub[df$births_sub < 1000],df$births_nbr[df$births_nbr < 1000]), breaks = 100, main = "Number of Firm Start Ups",
     xlab = "Number of Firms", ylab = "Frequency")

# his of birth ratio
hist(df$births_ratio, main = "Log Firm Entry Difference", breaks = 50)

# max discrepency between counties
max(abs(df$births_ratio), na.rm = TRUE)

# aggregate (unweighted) tax burden
hist(df$ptax_ratio_L1 + df$inctax_ratio_L1  +df$capgntax_ratio_L1 + df$stax_ratio_L1  + df$corptax_ratio_L1  + df$wctax_ratio_L1  + df$uitax_ratio_L1, prob = TRUE, main = "Sum of Taxes Difference", breaks = 50)

#find min and max values of aggregate (unweighted) tax burden
max(df$ptax_ratio_L1 + df$inctax_ratio_L1  +df$capgntax_ratio_L1 + df$stax_ratio_L1  + df$corptax_ratio_L1  + df$wctax_ratio_L1  + df$uitax_ratio_L1)
min(df$ptax_ratio_L1 + df$inctax_ratio_L1  +df$capgntax_ratio_L1 + df$stax_ratio_L1  + df$corptax_ratio_L1  + df$wctax_ratio_L1  + df$uitax_ratio_L1)


# scatter plots
pairs(~df$births_ratio+ df$ptax_ratio_L1 + df$inctax_ratio_L1  + df$capgntax_ratio_L1
      + df$stax_ratio_L1  + df$corptax_ratio_L1  + df$wctax_ratio_L1  + df$uitax_ratio_L1, main = "Tax Variables")

hist(df$births_ratio,df$ptax_ratio_L1 + df$inctax_ratio_L1  + df$capgntax_ratio_L1
     + df$stax_ratio_L1  + df$corptax_ratio_L1  + df$wctax_ratio_L1  + df$uitax_ratio_L1)
cor(df$births_ratio,df$ptax_ratio_L1 + df$inctax_ratio_L1  + df$capgntax_ratio_L1
    + df$stax_ratio_L1  + df$corptax_ratio_L1  + df$wctax_ratio_L1  + df$uitax_ratio_L1)

#################  REGRESSIONS ################# 

### count data models ###

allstarts <- c(df$births_sub,df$births_nbr)

iden <- rep(1,length(df$ptax_sub_L1))

x_sub <- matrix(c(iden, df$ptax_sub_L1, df$inctax_sub_L1, 
                df$capgntax_sub_L1, df$salestax_sub_L1, df$corptax_sub_L1, 
                df$wctax_sub_L1, df$uitaxrate_sub_L1, df$minwage_L1_sub, 
                df$rtw_L1_sub, df$educ_pc_L1_sub, df$hwy_pc_L1_sub, df$welfare_pc_L1_sub, df$hsplus_sub_L1, 
                df$realfuelpr_sub_L1, df$unionmem_sub_L1, df$popdensity_sub_L1, df$pctmanuf_sub_L1, 
                vars$jan_sun_z_sub,  vars$jul_temp_z_sub,
                vars$jul_hum_z_sub,	vars$top_z_sub,	vars$pct_water_z_sub), nrow = length(df$ptax_sub_L1))

x_nbr <- matrix(c(iden, df$ptax_nbr_L1, df$inctax_nbr_L1, 
                df$capgntax_nbr_L1, df$salestax_nbr_L1, df$corptax_nbr_L1, 
                df$wctax_nbr_L1, df$uitaxrate_nbr_L1, df$minwage_L1_nbr, 
                df$rtw_L1_nbr, df$educ_pc_L1_nbr, df$hwy_pc_L1_nbr, df$welfare_pc_L1_nbr, 
                df$hsplus_nbr_L1, df$realfuelpr_nbr_L1, df$unionmem_nbr_L1, df$popdensity_nbr_L1, df$pctmanuf_nbr_L1, 
                vars$jan_sun_z_nbr,  vars$jul_temp_z_nbr,
                vars$jul_hum_z_nbr,  vars$top_z_nbr,	vars$pct_water_z_nbr), nrow = length(df$ptax_sub_L1))

stprid <- c(df$stpr_id,df$stpr_id)
x_total <- rbind(x_sub,x_nbr)

pois_amen <- glm(allstarts ~ 0+x_total, family = poisson(link=log))

dispersiontest(pois_amen,trafo=1)

pois_amen_fixed <- glmer(allstarts~0+x_total+(1|stprid),family=poisson(link=log))

nb_amen <- glm.nb(allstarts ~ 0+x_total)

nb_amen-fixed <- glmer.nb(allstarts~0+x_total+(1|stprid))

lnall <- log(allstarts)[log(allstarts) != -Inf]
x_total <- x_total[log(allstarts) != -Inf,]
norm_amen <- lm(lnall ~ 0+x_total)

norm_amen_fixed <- lmer(lnall ~ 0+x_total+(1|stprid))

x_sub2 <- matrix(c(iden, df$ptax_sub_L1, df$inctax_sub_L1, 
                   df$capgntax_sub_L1, df$salestax_sub_L1, df$corptax_sub_L1, 
                   df$wctax_sub_L1, df$uitaxrate_sub_L1, df$minwage_L1_sub, 
                   df$rtw_L1_sub, df$educ_pc_L1_sub, df$hwy_pc_L1_sub, df$welfare_pc_L1_sub, df$hsplus_sub_L1, 
                   df$realfuelpr_sub_L1, df$unionmem_sub_L1, df$popdensity_sub_L1, df$pctmanuf_sub_L1), nrow = length(df$ptax_sub_L1))

x_nbr2 <- matrix(c(iden, df$ptax_nbr_L1, df$inctax_nbr_L1, 
                   df$capgntax_nbr_L1, df$salestax_nbr_L1, df$corptax_nbr_L1, 
                   df$wctax_nbr_L1, df$uitaxrate_nbr_L1, df$minwage_L1_nbr, 
                   df$rtw_L1_nbr, df$educ_pc_L1_nbr, df$hwy_pc_L1_nbr, df$welfare_pc_L1_nbr, 
                   df$hsplus_nbr_L1, df$realfuelpr_nbr_L1, df$unionmem_nbr_L1, df$popdensity_nbr_L1, df$pctmanuf_nbr_L1), nrow = length(df$ptax_sub_L1))
x_total <- rbind(x_sub2,x_nbr2)

pois_namen <- glm(allstarts ~ 0+x_total, family = poisson(link=log))
pois_namen_coef <- coeftest(pois_namen)[,1]
pois_namen_sigma <- cov2cor(vcov(pois_namen))

nb_namen <- glm.nb(allstarts ~ 0+x_total)

lnall <- log(allstarts)[log(allstarts) != -Inf]
x_total <- x_total[log(allstarts) != -Inf,]
norm_namen <- lm(lnall ~ 0+x_total)


### differenced-count data models ###

neglog <- function(beta1, beta2) {
  -sum(df$births_sub-df$births_nbr-
         exp(beta1+beta2*ptax_sub_L1)
       +exp(beta1+beta2*ptax_nbr_L1))
}

mle(neglog, start = (beta1 = 1, beta2 = .002))
# start = list(beta0 = pois_namen_coef[1], beta1 = pois_namen_coef[2], beta2 = pois_namen_coef[3],
beta3 = pois_namen_coef[4], beta4 = pois_namen_coef[5], beta5 = pois_namen_coef[6],
beta6 = pois_namen_coef[7], beta7 = pois_namen_coef[8])

#### Normal Regressions ###

## State-pair Clustering ##
# normal likelihood, integer valued dep var, with amenities, not imposing equality
pols1_amen_count <- lm(df$births_diff ~  df$ptax_sub_L1 + df$ptax_nbr_L1 + df$inctax_sub_L1 +  df$inctax_nbr_L1 +
                          df$capgntax_sub_L1 + df$capgntax_nbr_L1 + df$salestax_sub_L1 + df$salestax_nbr_L1 +
                          df$corptax_sub_L1 + df$corptax_nbr_L1 +  df$wctax_sub_L1 + df$wctax_nbr_L1 +
                          df$uitaxrate_sub_L1 + df$uitaxrate_nbr_L1 + df$minwage_L1_sub +  df$minwage_L1_nbr +
                          df$rtw_L1_sub + df$rtw_L1_nbr + df$educ_pc_L1_sub+df$educ_pc_L1_nbr+ df$hwy_pc_L1_sub+ 
                         df$hwy_pc_L1_nbr+df$welfare_pc_L1_sub+  df$welfare_pc_L1_nbr +
                        vars$jan_sun_z_sub+ vars$jan_sun_z_nbr+ vars$jul_temp_z_sub +vars$jul_temp_z_nbr
                        + vars$jul_hum_z_sub +vars$jul_hum_z_nbr+ vars$top_z_sub + vars$top_z_nbr
                        + vars$pct_water_z_sub + vars$pct_water_z_nbr)
stprid_c_vcov <- cluster.vcov(pols1_amen_count, df$stpr_id)
pols1_stpr_amen_c_coef <- coeftest(pols1_amen_count, vcov = stprid_c_vcov) # results



pols1_namen_count <- lm(df$births_diff ~  df$ptax_sub_L1 + df$ptax_nbr_L1 + df$inctax_sub_L1 +  df$inctax_nbr_L1 +
                         df$capgntax_sub_L1 + df$capgntax_nbr_L1 + df$salestax_sub_L1 + df$salestax_nbr_L1 +
                         df$corptax_sub_L1 + df$corptax_nbr_L1 +  df$wctax_sub_L1 + df$wctax_nbr_L1 +
                         df$uitaxrate_sub_L1 + df$uitaxrate_nbr_L1 + df$minwage_L1_sub +  df$minwage_L1_nbr +
                         df$rtw_L1_sub + df$rtw_L1_nbr+ df$educ_pc_L1_sub+df$educ_pc_L1_nbr+ df$hwy_pc_L1_sub+ 
                          df$hwy_pc_L1_nbr+df$welfare_pc_L1_sub+  df$welfare_pc_L1_nbr)
stprid_c_vcov <- cluster.vcov(pols1_namen_count, df$stpr_id)
pols1_stpr_namen_c_coef <- coeftest(pols1_namen_count, vcov = stprid_c_vcov) # results

# normal likelihood, real valued dep var, with amenities, not imposing equality
pols1_amen_real <- lm(df$births_ratio ~  df$ptax_sub_L1 + df$ptax_nbr_L1 + df$inctax_sub_L1 +  df$inctax_nbr_L1 +
                         df$capgntax_sub_L1 + df$capgntax_nbr_L1 + df$salestax_sub_L1 + df$salestax_nbr_L1 +
                         df$corptax_sub_L1 + df$corptax_nbr_L1 +  df$wctax_sub_L1 + df$wctax_nbr_L1 +
                         df$uitaxrate_sub_L1 + df$uitaxrate_nbr_L1 + df$minwage_L1_sub +  df$minwage_L1_nbr +
                         df$rtw_L1_sub + df$rtw_L1_nbr+ df$educ_pc_L1_sub+df$educ_pc_L1_nbr+ df$hwy_pc_L1_sub+ 
                        df$hwy_pc_L1_nbr+df$welfare_pc_L1_sub+  df$welfare_pc_L1_nbr
                        + vars$jan_sun_z_sub+ vars$jan_sun_z_nbr+ vars$jul_temp_z_sub +vars$jul_temp_z_nbr
                       + vars$jul_hum_z_sub +vars$jul_hum_z_nbr+ vars$top_z_sub + vars$top_z_nbr
                       + vars$pct_water_z_sub + vars$pct_water_z_nbr)
stprid_c_vcov <- cluster.vcov(pols1_amen_real, df$stpr_id)
pols1_stpr_amen_r_coef <- coeftest(pols1_amen_real, vcov = stprid_c_vcov) # results

pols1_namen_real <- lm(df$births_ratio ~  df$ptax_sub_L1 + df$ptax_nbr_L1 + df$inctax_sub_L1 +  df$inctax_nbr_L1 +
                        df$capgntax_sub_L1 + df$capgntax_nbr_L1 + df$salestax_sub_L1 + df$salestax_nbr_L1 +
                        df$corptax_sub_L1 + df$corptax_nbr_L1 +  df$wctax_sub_L1 + df$wctax_nbr_L1 +
                        df$uitaxrate_sub_L1 + df$uitaxrate_nbr_L1 + df$minwage_L1_sub +  df$minwage_L1_nbr +
                        df$rtw_L1_sub + df$rtw_L1_nbr+ df$educ_pc_L1_sub+df$educ_pc_L1_nbr+ df$hwy_pc_L1_sub+ 
                         df$hwy_pc_L1_nbr+df$welfare_pc_L1_sub+  df$welfare_pc_L1_nbr)
stprid_c_vcov <- cluster.vcov(pols1_namen_real, df$stpr_id)
pols1_stpr_namen_r_coef <- coeftest(pols1_namen_real, vcov = stprid_c_vcov) # results

#### IMPOSING EQUALITY

# real valued, imposing equality, amenities
pols_real_amen <- lm(df$births_ratio ~  df$ptax_sub_L1 + df$ptax_nbr_L1 + df$inctax_sub_L1 +  df$inctax_nbr_L1 +
                   df$capgntax_sub_L1 + df$capgntax_nbr_L1 + df$salestax_sub_L1 + df$salestax_nbr_L1 +
                   df$corptax_sub_L1 + df$corptax_nbr_L1 +  df$wctax_sub_L1 + df$wctax_nbr_L1 +
                   df$uitaxrate_sub_L1 + df$uitaxrate_nbr_L1 + df$minwage_L1_sub +  df$minwage_L1_nbr +
                   df$rtw_L1_sub + df$educ_pc_L1_sub+df$educ_pc_L1_nbr+ df$hwy_pc_L1_sub+ 
                     df$hwy_pc_L1_nbr+df$welfare_pc_L1_sub+  df$welfare_pc_L1_nbr
                   + df$rtw_L1_nbr+vars$jan_sun_z_ratio+vars$jul_temp_z_ratio+
                   vars$jul_hum_z_ratio + vars$top_z_ratio + vars$pct_water_z_ratio)
stprid_c_vcov <- cluster.vcov(pols_real_amen, df$stpr_id)
pols_amen_stpr_r_coef <- coeftest(pols_real_amen, vcov = stprid_c_vcov)

# real valued, imposing equality, no amenities
pols1_real <- lm(df$births_ratio ~  df$ptax_sub_L1 + df$ptax_nbr_L1 + df$inctax_sub_L1 +  df$inctax_nbr_L1 +
                    df$capgntax_sub_L1 + df$capgntax_nbr_L1 + df$salestax_sub_L1 + df$salestax_nbr_L1 +
                    df$corptax_sub_L1 + df$corptax_nbr_L1 +  df$wctax_sub_L1 + df$wctax_nbr_L1 +
                    df$uitaxrate_sub_L1 + df$uitaxrate_nbr_L1 + df$minwage_L1_sub +  df$minwage_L1_nbr +
                    df$rtw_L1_sub + df$rtw_L1_nbr+ df$educ_pc_L1_sub+df$educ_pc_L1_nbr+ df$hwy_pc_L1_sub+ 
                   df$hwy_pc_L1_nbr+df$welfare_pc_L1_sub+  df$welfare_pc_L1_nbr)
stprid_c_vcov <- cluster.vcov(pols1_real, df$stpr_id)
pols1_stpr_r_coef <- coeftest(pols1_real, vcov = stprid_c_vcov)

linearHypothesis(pols1_real, c("df$ptax_sub_L1 = -df$ptax_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_real, c("df$inctax_sub_L1 = -df$inctax_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_real, c("df$capgntax_sub_L1 = -df$capgntax_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_real, c("df$salestax_sub_L1 = -df$salestax_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_real, c("df$corptax_sub_L1 = -df$corptax_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_real, c("df$wctax_sub_L1 = -df$wctax_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_real, c("df$uitaxrate_sub_L1 = -df$uitaxrate_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_real, c("df$minwage_L1_sub = -df$minwage_L1_nbr"), vcov = stprid_c_vcov)
linearHypothesis(pols1_real, c("df$rtw_L1_sub = -df$rtw_L1_nbr"), vcov = stprid_c_vcov)


pols1_count <- lm(df$births_diff ~ df$ptax_sub_L1 + df$ptax_nbr_L1 + df$inctax_sub_L1 +  df$inctax_nbr_L1 +
                     df$capgntax_sub_L1 + df$capgntax_nbr_L1 + df$salestax_sub_L1 + df$salestax_nbr_L1 +
                     df$corptax_sub_L1 + df$corptax_nbr_L1 +  df$wctax_sub_L1 + df$wctax_nbr_L1 +
                     df$uitaxrate_sub_L1 + df$uitaxrate_nbr_L1 + df$minwage_L1_sub +  df$minwage_L1_nbr +
                     df$rtw_L1_sub + df$rtw_L1_nbr+ df$educ_pc_L1_sub+df$educ_pc_L1_nbr+ df$hwy_pc_L1_sub+ 
                    df$hwy_pc_L1_nbr+df$welfare_pc_L1_sub+  df$welfare_pc_L1_nbr)
stprid_c_vcov <- cluster.vcov(pols1_count, df$stpr_id)
pols1_stpr_c_coef <- coeftest(pols1_count, vcov = stprid_c_vcov)

linearHypothesis(pols1_count, c("df$ptax_sub_L1 = -df$ptax_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_count, c("df$inctax_sub_L1 = -df$inctax_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_count, c("df$capgntax_sub_L1 = -df$capgntax_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_count, c("df$salestax_sub_L1 = -df$salestax_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_count, c("df$corptax_sub_L1 = -df$corptax_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_count, c("df$wctax_sub_L1 = -df$wctax_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_count, c("df$uitaxrate_sub_L1 = -df$uitaxrate_nbr_L1"), vcov = stprid_c_vcov)
linearHypothesis(pols1_count, c("df$minwage_L1_sub = -df$minwage_L1_nbr"), vcov = stprid_c_vcov)
linearHypothesis(pols1_count, c("df$rtw_L1_sub = -df$rtw_L1_nbr"), vcov = stprid_c_vcov)

pols_amen_c <- lm(df$births_diff ~ df$ptax_ratio_L1 + df$inctax_ratio_L1  +df$capgntax_ratio_L1
                  + df$stax_ratio_L1  + df$corptax_ratio_L1  + df$wctax_ratio_L1  + df$uitax_ratio_L1
                  + df$minwage_L1_ratio  + df$rtw_L1_ratio  + df$educ_ratio_L1  + df$hwy_ratio_L1
                  + df$welfare_ratio_L1 + df$hs_L1_diff + df$fuel_L1_diff + df$union_L1_diff
                  + df$density_L1_diff + df$manuf_L1_diff+ df$jan_sun_z_ratio + df$jul_temp_z_ratio + df$jul_hum_z_ratio +
                    df$top_z_ratio+df$pct_water_z_ratio)
stprid_c_vcov <- cluster.vcov(pols_amen_c, df$stpr_id)
pols_amen_c_coef <- coeftest(pols_amen_c, vcov = stprid_c_vcov)

pols2_count <- lm(df$births_diff ~ df$ptax_ratio_L1 + df$inctax_ratio_L1  +df$capgntax_ratio_L1
                   + df$stax_ratio_L1  + df$corptax_ratio_L1  + df$wctax_ratio_L1  + df$uitax_ratio_L1
                   + df$minwage_L1_ratio  + df$rtw_L1_ratio  + df$educ_ratio_L1  + df$hwy_ratio_L1
                   + df$welfare_ratio_L1 + df$hs_L1_diff + df$fuel_L1_diff + df$union_L1_diff
                   + df$density_L1_diff + df$manuf_L1_diff)
stprid_c_vcov <- cluster.vcov(pols2_count, df$stpr_id)
pols_namen_c_coef <- coeftest(pols2_count, vcov = stprid_c_vcov)

pols_namen_r <- lm(df$births_ratio ~ df$ptax_ratio_L1 + df$inctax_ratio_L1  +df$capgntax_ratio_L1
                  + df$stax_ratio_L1  + df$corptax_ratio_L1  + df$wctax_ratio_L1  + df$uitax_ratio_L1
                  + df$minwage_L1_ratio  + df$rtw_L1_ratio  + df$educ_ratio_L1  + df$hwy_ratio_L1
                  + df$welfare_ratio_L1 + df$hs_L1_diff + df$fuel_L1_diff + df$union_L1_diff
                  + df$density_L1_diff + df$manuf_L1_diff)
stprid_c_vcov <- cluster.vcov(pols_amen_r, df$stpr_id)
pols_namen_r_coef <- coeftest(pols_namen_r, vcov = stprid_c_vcov)


pols_amen_r <- lm(df$births_ratio ~ df$ptax_ratio_L1 + df$inctax_ratio_L1  +df$capgntax_ratio_L1
                  + df$stax_ratio_L1  + df$corptax_ratio_L1  + df$wctax_ratio_L1  + df$uitax_ratio_L1
                  + df$minwage_L1_ratio  + df$rtw_L1_ratio  + df$educ_ratio_L1  + df$hwy_ratio_L1
                  + df$welfare_ratio_L1 + df$hs_L1_diff + df$fuel_L1_diff + df$union_L1_diff
                  + df$density_L1_diff + df$manuf_L1_diff+df$jan_sun_z_ratio + 
                   df$jul_temp_z_ratio + df$jul_hum_z_ratio +
                   df$top_z_ratio+df$pct_water_z_ratio)
stprid_c_vcov <- cluster.vcov(pols_amen_r, df$stpr_id)
pols_amen_r_coef <- coeftest(pols_amen_r, vcov = stprid_c_vcov)

## Robustness check $$

# Run regressions for each year

for (i in 1999:2008)  {
  sub <- df[df$year == i,]
  pols_year <- lm(sub$births_ratio ~ sub$ptax_ratio_L1 + sub$inctax_ratio_L1  +sub$capgntax_ratio_L1
                                + sub$stax_ratio_L1  + sub$corptax_ratio_L1  + sub$wctax_ratio_L1  + sub$uitax_ratio_L1
                                + sub$minwage_L1_ratio  + sub$rtw_L1_ratio  + sub$educ_ratio_L1  + sub$hwy_ratio_L1
                                + sub$welfare_ratio_L1 + sub$hs_L1_diff + sub$fuel_L1_diff + sub$union_L1_diff
                                + sub$density_L1_diff + sub$manuf_L1_diff)
  stprid_c_vcov <- cluster.vcov(pols_year, df$stpr_id[df$year == i])
  assign(paste("pols",i,sep="_"), coeftest(pols_year, vcov = stprid_c_vcov))
}

# Regression with interaction term
ptax_educ <- df$ptax_sub_L1*df$educ_pc_L1_sub - df$ptax_nbr_L1*df$educ_pc_L1_nbr
ptax_hwy <- df$ptax_sub_L1*df$hwy_pc_L1_sub - df$ptax_nbr_L1*df$hwy_pc_L1_nbr
ptax_welfare <- df$ptax_sub_L1*df$welfare_pc_L1_sub - df$ptax_nbr_L1*df$welfare_pc_L1_nbr

inctax_educ <- df$inctax_sub_L1*df$educ_pc_L1_sub - df$inctax_nbr_L1*df$educ_pc_L1_nbr
inctax_hwy <- df$inctax_sub_L1*df$hwy_pc_L1_sub - df$inctax_nbr_L1*df$hwy_pc_L1_nbr
inctax_welfare <- df$inctax_sub_L1*df$welfare_pc_L1_sub - df$inctax_nbr_L1*df$welfare_pc_L1_nbr

salestax_educ <- df$salestax_sub_L1*df$educ_pc_L1_sub - df$salestax_nbr_L1*df$educ_pc_L1_nbr
salestax_hwy <- df$salestax_sub_L1*df$hwy_pc_L1_sub - df$salestax_nbr_L1*df$hwy_pc_L1_nbr
salestax_welfare <- df$salestax_sub_L1*df$welfare_pc_L1_sub - df$salestax_nbr_L1*df$welfare_pc_L1_nbr

pols_int <- lm(df$births_ratio ~ df$ptax_ratio_L1+df$inctax_ratio_L1+df$stax_ratio_L1
               +df$educ_ratio_L1 + df$hwy_ratio_L1 + df$welfare_ratio_L1
               + ptax_educ + ptax_hwy + ptax_welfare
               + inctax_educ + inctax_hwy + inctax_welfare
               + salestax_educ + salestax_hwy + salestax_welfare)
stprid_c_vcov <- cluster.vcov(pols_int, df$stpr_id)
pols_int_coef <- coeftest(pols_int, vcov = stprid_c_vcov)


linearHypothesis(pols_int, c("df$ptax_ratio_L1 + ptax_educ = 0"), vcov = stprid_c_vcov)
linearHypothesis(pols_int, c("df$ptax_ratio_L1 + ptax_welfare = 0"), vcov = stprid_c_vcov)
linearHypothesis(pols_int, c("df$ptax_ratio_L1 + ptax_hwy = 0"), vcov = stprid_c_vcov)
linearHypothesis(pols_int, c("df$inctax_ratio_L1 + inctax_educ = 0"), vcov = stprid_c_vcov)
linearHypothesis(pols_int, c("df$inctax_ratio_L1 + inctax_welfare = 0"), vcov = stprid_c_vcov)
linearHypothesis(pols_int, c("df$inctax_ratio_L1 + inctax_hwy = 0"), vcov = stprid_c_vcov)
linearHypothesis(pols_int, c("df$stax_ratio_L1 + salestax_educ = 0"), vcov = stprid_c_vcov)
linearHypothesis(pols_int, c("df$stax_ratio_L1 + salestax_welfare = 0"), vcov = stprid_c_vcov)
linearHypothesis(pols_int, c("df$stax_ratio_L1 + salestax_hwy = 0"), vcov = stprid_c_vcov)

### RANKINGS ####
# unweighted aggregate tax differentials between states
df$iden <- df$cofip_sub*100000+df$cofip_nbr
df$taxdif <- df$ptax_ratio_L1 + df$inctax_ratio_L1  +df$capgntax_ratio_L1 + df$stax_ratio_L1 + df$corptax_ratio_L1  + df$wctax_ratio_L1  + df$uitax_ratio_L1
unweightedvalues <- data.frame(df$taxdif, df$cofip_sub, df$cofip_nbr)
unweightedvalues <- data.frame(df$taxdif, df$cofip_sub, df$cofip_nbr, df$iden)[order(unweightedvalues$df.taxdif, decreasing = TRUE),] 

# weighted aggregate tax differentials between states
# first I look at the starting difference
firstdat <- df[ df$year == 1999,]
firstdat <- firstdat[!duplicated(firstdat$stpr_id), ]
firstdat$start <- abs(pols_namen_r_coef[2]*firstdat$ptax_ratio_L1 + pols_namen_r_coef[3]*firstdat$inctax_ratio_L1  + pols_namen_r_coef[4]*firstdat$capgntax_ratio_L1
+ pols_namen_r_coef[5]*firstdat$stax_ratio_L1  + pols_namen_r_coef[6]*firstdat$corptax_ratio_L1  + pols_namen_r_coef[7]*firstdat$wctax_ratio_L1
+ pols_namen_r_coef[8]*firstdat$uitax_ratio_L1 + pols_namen_r_coef[9]*firstdat$minwage_L1_ratio)
firstdat <-data.frame(firstdat$cofip_sub, firstdat$cofip_nbr, firstdat$start,firstdat$stpr_id)[order(firstdat$start, decreasing = TRUE),]

# then I look at the ending difference
finaldat <- df[ df$year == 2007,]
finaldat <- finaldat[ !duplicated(finaldat$stpr_id), ]
par(mfrow=c(1,1))
hist(pols_namen_r_coef[2]*finaldat$ptax_ratio_L1 + pols_namen_r_coef[3]*finaldat$inctax_ratio_L1  + pols_namen_r_coef[4]*finaldat$capgntax_ratio_L1
     + pols_namen_r_coef[5]*finaldat$stax_ratio_L1  + pols_namen_r_coef[6]*finaldat$corptax_ratio_L1  + pols_namen_r_coef[7]*finaldat$wctax_ratio_L1
     + pols_namen_r_coef[8]*finaldat$uitax_ratio_L1 + pols_namen_r_coef[9]*finaldat$minwage_L1_ratio, breaks = 100, main = "Weighted Tax Differentials")
# want to see which areas have seen the biggest improvement.

finaldat$finish <- abs(pols_namen_r_coef[2]*finaldat$ptax_ratio_L1 + pols_namen_r_coef[3]*finaldat$inctax_ratio_L1  + pols_namen_r_coef[4]*finaldat$capgntax_ratio_L1
+ pols_namen_r_coef[5]*finaldat$stax_ratio_L1  + pols_namen_r_coef[6]*finaldat$corptax_ratio_L1  + pols_namen_r_coef[7]*finaldat$wctax_ratio_L1
+ pols_namen_r_coef[8]*finaldat$uitax_ratio_L1 + pols_namen_r_coef[9]*finaldat$minwage_L1_ratio)
finaldat <- data.frame(finaldat$cofip_sub,finaldat$cofip_nbr,finaldat$finish,finaldat$stpr_id)[order(finaldat$finish, decreasing = TRUE),]


# diff = change from first ranking to last ranking
finaldat$diff <- match(firstdat$firstdat.stpr_id,finaldat$finaldat.stpr_id)-index(finaldat$finaldat.stpr_id) # values matched, values matched against

finaldat[1:10,]  

mean_starts <- length(unique(df$stpr_id))
for (i in 1:length(unique(df$stpr_id))){
  temp_dat <- df[df$stpr_id == unique(df$stpr_id)[i],]
  mean_starts[i] <- sum(temp_dat$births_ratio[complete.cases(temp_dat$births_ratio)])/length(temp_dat[,1])
}

mean <- data.frame(mean_starts, unique(df$stpr_id))[order(mean_starts, decreasing = TRUE),]

test <- lmer(births_diff ~ ptax_ratio_L1 + inctax_ratio_L1  +capgntax_ratio_L1
              + stax_ratio_L1  + corptax_ratio_L1  + wctax_ratio_L1  + uitax_ratio_L1
              + minwage_L1_ratio  + rtw_L1_ratio  + educ_ratio_L1  + hwy_ratio_L1
              + welfare_ratio_L1+ (1|stpr_id), data = df)
