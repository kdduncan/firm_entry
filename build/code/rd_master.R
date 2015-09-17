
naics <- c("--", "11","21","22","23","31-33","42","44-45","48-49", 
           "51","52","53","54","55","56","61","62","71","72","81","95","99")

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
 master$base_diff <- master$base_sub-master$base_nbr
 master$base_ratio <- master$lnbase_sub - master$lnbase_nbr
 master$births_diff <- master$births_sub - master$births_nbr
 master$births_ratio <- master$lnbirths_sub - master$lnbirths_nbr
 master$deaths_diff <- master$deaths_sub-master$deaths_nbr
 master$deaths_ratio <- master$lndeaths_sub - master$lndeaths_nbr
 master$expansions_diff <- master$expansions_sub - master$expansions_nbr
 master$expansions_ratio <- master$lnexpan_sub-master$lnexpan_nbr
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
 
 rd_master <-  master[c("cofip_sub","cofip_nbr","statefips.x","statefips.y","year",
                        "base_diff","base_ratio","births_diff","births_ratio","deaths_diff","deaths_ratio",
                        "expansions_diff","expansions_ratio","contractions_diff","contractions_ratio", 
                        "educ_pc_L1_diff","hwy_pc_L1_diff","welfare_pc_L1_diff",
                "ptax_diff","inctax_diff","capgntax_diff","salestax_diff","corptax_diff","wctax_diff","uitax_diff",
                "hsplus_diff","realfuelpr_diff","unionmem_diff","popdensity_diff","pctmanuf_diff",
                "JAN.TEMP.Z_diff","JAN.SUN.Z_diff","JUL.TEMP.Z_diff","JUL.HUM.Z_diff","TOPOG.Z_diff","WATER.AR.Z_diff")]
 
 write.csv(border_master, file = paste("~/papers/firm_entry/build/output/",naics[m],"rd_master.csv", sep = "_"))

}