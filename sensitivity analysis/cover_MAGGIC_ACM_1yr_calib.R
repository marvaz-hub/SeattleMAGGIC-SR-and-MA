## MAGGIC ##
## Sensitivity analysis for O:E 
## Estimates for ACM and approx ACM (outc==”E6”), horizon 12m (most frequent horizon)


# install.packages("Hmisc")
# install.packages("metamisc")
# install.packages("metafor")
# install.packages("rJava")
# install.packages("XLConnect")
# 
# install.packages("devtools")
# library("devtools")
# install_version("XLConnect", version = "1.0.1", repos = "http://cran.us.r-project.org")
# 

library(Hmisc)
library(metamisc)
library(metafor)

library(rJava)
library("XLConnect")

library("shape")


path1 = "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/SeattleMAGGIC analysis files NEWCOVERCODES/"
path2 = "subscripts/"


## Data for calibration 
source(paste0(path1, path2, "cr_MAGGIC_calib_data.R"))

## Function to get pooled estimates for oe
source(paste0(path1, path2, "fn_ma_oe.R"))



#################################################################################
### create the necessary dataframes for pooling ###
#################################################################################
# 
# If using all the data
# dat <- df_ma

# 1) choose outcome ACM and approx ACM (outc=="E6")
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") # 29


# 2)  horizon
dat <- dat[which(dat$horizon==12),]
table(dat$horizon, useNA = "always") #18

### 3) 4 sensitivity analyses for MAGGIC (6 for SHFM) (+ 1 sensitivity analysis: exclude internal validation results)
# 1.	Exclude estimates with high ROB on any domain (O1)
dat1 <- dat[which(dat$anyhighROB!="O1"),]
dim(dat1) #3

# 2.	Exclude estimates with high COVER departure (indep=="E16"|outc=="E17"| cal=="E19"|cal==E17"|model_p=="E21"|model_p=="E22"|model_p=="E23")
dat2 <- dat[which(dat$indep!="E16" & dat$outc!="E17"& dat$cal!="E19"& 
                    dat$cal!="E17"& dat$model_p!="E21" & dat$model_p!="E22" &
                    dat$model_p!="E23"),]
dim(dat2) #3

# 3.	Exclude estimates with medium or high COVER departure (popul=="E2"| indep=="E5"|indep=="E16"|outc=="E6"|outc=="E7"|outc=="E17"|cal=="E8"|cal=="E19"|cal==E17"|cal=="E9"|model_f=="E12"|model_p=="E12|model_p==E13"|model_p=="E14|model_p=="E15"|model_p=="E21"|model_p=="E22"|model_p=="E23")
dat3 <- dat[which(dat$popul!="E2" & dat$indep!="E5" & dat$indep!="E16" & 
                    dat$outc!="E6" & dat$outc!="E7" & dat$outc!="E17" & 
                    dat$cal!="E8" & dat$cal!="E19" & dat$cal!="E17" & 
                    dat$cal!="E9" & dat$model_f!="E12" & dat$model_p!="E12" &
                    dat$model_p!="E13" & dat$model_p!="E14" & dat$model_p!="E15" &
                    dat$model_p!="E21" & dat$model_p!="E22" & dat$model_p!="E23"),]
dim(dat3) #0

# 4.	Exclude estimates with medium or high COVER departure but ignoring E12 (popul=="E2"| indep=="E5"|indep=="E16"|outc=="E6"|outc=="E7"|outc=="E17"|cal=="E8"|cal=="E19"|cal==E17"|cal=="E9"|model_p==E13"|model_p=="E14|model_p=="E15"|model_p=="E21"|model_p=="E22"|model_p=="E23")
dat4 <- dat[which(dat$popul!="E2" & dat$indep!="E5" & dat$indep!="E16" & 
                  dat$outc!="E6" & dat$outc!="E7" & dat$outc!="E17" & 
                  dat$cal!="E8" & dat$cal!="E19" & dat$cal!="E17" & 
                  dat$cal!="E9" & dat$model_p!="E13" & dat$model_p!="E14" & 
                  dat$model_p!="E15" & dat$model_p!="E21" & dat$model_p!="E22" &
                  dat$model_p!="E23"),]
dim(dat4) #0

# 5.	Exclude estimates with negative CI (neg_confC=="O3") (None for MAGGIC)
# 6.	Exclude outlier (outlierC=="O4") (None for MAGGIC) 

# 7. Exclude internal validation results (int_valid=="O2")
# Pocock 2013 lineno 70
dat[(dat$lineno==70 | dat$author=="Pocock"|dat$year==2013), c("author","year","lineno")]
#Pocock 2013 did not contribute with internal calibration data


################################################################################################
###########     GROUP 1 #############
################################################################################################

## ** UPDATE DATABASE AND NAME OF VARIBLE **

dat <- dat1 

# create dataframe/list with all oe estimates and recalculated 95%-CIs
oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                 citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                 data=dat, slab = study_id  )
# create a dataframe for forest plot
oe_all$n      <- dat$sample_n_model
oe_all$events <- dat$o.est
oe_all$id     <- dat$authoryear
oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
dim(oe_all_forest) ## 18,9
plot(oe_all_forest)

# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
resM_cal_ACM_1yr_cover1 <- fn_ma_oe(dat)[[2]]


################################################################################################
###########     GROUP 2  #############
################################################################################################

## ** UPDATE DATABASE AND NAME OF VARIBLE **

dat <- dat2 #high rob

# create dataframe/list with all oe estimates and recalculated 95%-CIs
oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                 citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                 data=dat, slab = study_id  )
# create a dataframe for forest plot
oe_all$n      <- dat$sample_n_model
oe_all$events <- dat$o.est
oe_all$id     <- dat$authoryear
oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
dim(oe_all_forest) ## 18,9
plot(oe_all_forest)

# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
resM_cal_ACM_1yr_cover2 <- fn_ma_oe(dat)[[2]]


################################################################################################
###########     GROUP 3 #############
################################################################################################

## ** UPDATE DATABASE AND NAME OF VARIBLE **

# dat <- dat3 #high rob
# 
# # create dataframe/list with all oe estimates and recalculated 95%-CIs
# oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
#                  citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
#                  data=dat, slab = study_id  )
# # create a dataframe for forest plot
# oe_all$n      <- dat$sample_n_model
# oe_all$events <- dat$o.est
# oe_all$id     <- dat$authoryear
# oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
# dim(oe_all_forest) ## 18,9
# plot(oe_all_forest)
# 
# # funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
# resM_cal_ACM_1yr_cover3 <- fn_ma_oe(dat)[[2]]

resM_cal_ACM_1yr_cover3 <- c(rep("-",5), 0, "-") 


################################################################################################
###########     GROUP 4 #############
################################################################################################

## ** UPDATE DATABASE AND NAME OF VARIBLE **

# dat <- dat4 
# 
# # create dataframe/list with all oe estimates and recalculated 95%-CIs
# oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
#                  citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
#                  data=dat, slab = study_id  )
# # create a dataframe for forest plot
# oe_all$n      <- dat$sample_n_model
# oe_all$events <- dat$o.est
# oe_all$id     <- dat$authoryear
# oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
# dim(oe_all_forest) ## 18,9
# plot(oe_all_forest)
# 
# # funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
# resM_cal_ACM_1yr_cover4 <- fn_ma_oe(dat)[[2]]

resM_cal_ACM_1yr_cover4 <- c(rep("-",5), 0, "-") 
