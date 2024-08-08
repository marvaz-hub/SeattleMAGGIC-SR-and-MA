## SHFM ##
## Meta-regression for O:E 


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

path1 = # INSERT PATHWAY TO DIRECTORY CONTAINING THE SUBFOLDERS

## Data for calibration 
path2 = "subscripts/"
source(paste0(path1, path2,"cr_SHFM_calib_data.R"))


#################################################################################
### create the necessary dataframes for plotting and pooling ###
#################################################################################
# 
# If using all the data
# dat <- df_ma

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always")
#  1    5 <NA> 
# 34    6    0 


# 2) horizon
dat <- dat[which(dat$horizon==12),]
table(dat$horizon, useNA = "always") #28


# ################################################################
#  oe for MA will include oe.est (original or estimated from O and E) and oe.est.frominter
# ################################################################

oe_all_log <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                     citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                     data=dat, slab = study_id, g="log(OE)")

## add information for meta-regression
oe_all_log$vi <- oe_all_log$theta.se^2
oe_all_log$year <- dat$year
# oe_all_log$noofpreds2<- dat$noofpreds2 ## change this for SHFM ((SHFM: noofpreds for Levy 2006 PRAISE version and
                                         ## noofpreds2 for Levy 2006 external validation version)
oe_all_log$noofoutcomes<- dat$o.est
oe_all_log$sd1 <- dat$sd1
# oe_all_log$horizon<- dat$horizon # this will be needed when working with all horizons

oe_all_log <- oe_all_log[which(!is.na(oe_all_log$theta.se)),]   # remove NA SE(OE)
nstudies <- nrow(oe_all_log) #28

####################################
## univariate MR
resS_cal_ACM_1yr_univmr_year <- rma(yi=theta, sei=theta.se, mods = ~ year, data=oe_all_log, method="REML", test="knha")
# resS_cal_ACM_1yr_univmr_pred <- rma(yi=theta, sei=theta.se, mods = ~ noofpreds, data=oe_all_log, method="REML", test="knha")
# resS_cal_ACM_1yr_univmr_pred <- rma(yi=theta, sei=theta.se, mods = ~ noofpreds2, data=oe_all_log, method="REML", test="knha")
resS_cal_ACM_1yr_univmr_outc <- rma(yi=theta, sei=theta.se, mods = ~ noofoutcomes, data=oe_all_log, method="REML", test="knha")
resS_cal_ACM_1yr_univmr_sdage<- rma(yi=theta, sei=theta.se, mods = ~ sd1, data=oe_all_log, method="REML", test="knha")

# resS_cal_ACM_1yr_mr <- rma(yi=theta, sei=theta.se, mods = ~ year + noofpreds + noofpreds2 + noofoutcomes, data=oe_all_log, method="REML", test="knha")

# resS_cal_ACM_1yr_mr <- rma(yi=theta, sei=theta.se, mods = ~ year + noofoutcomes, data=oe_all_log, method="REML", test="knha")


## write results using "writes in xl ACM 1yr MR results.r"
