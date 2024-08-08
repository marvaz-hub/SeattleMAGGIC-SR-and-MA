## SHFM ##
## Meta-regression for cstat and auc together
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


library(Hmisc)
library(metamisc)
library(metafor)

library(rJava)
library("XLConnect")

library("dplyr")

path1 = # INSERT PATHWAY TO DIRECTORY CONTAINING THE SUBFOLDERS

## Data for discrimination 
path2 = "subscripts/"
source(paste0(path1, path2, "cr_SHFM_disc_data.R"))


#################################################################################
### create the necessary dataframes for plotting and pooling ###
#################################################################################
# 
# If using all the data
# dat <- df_ma

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") # 45 ACM 4 out5 1 out9 

# 2) horizon
dat <- dat[which(dat$horizon==12),]
table(dat$horizon, useNA = "always") #34

#################################################################################
# create dataframe/list with all cstats and recalculated 95%-CIs
#################################################################################

cstat_all_log <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                       N = sample_n_model, O= o.est, data=dat, slab=study_id, g="log(cstat/(1-cstat))")

## add information for meta-regression
cstat_all_log$vi <- cstat_all_log$theta.se^2
cstat_all_log$year <- dat$year
cstat_all_log$noofpreds<- dat$noofpreds    ## SHFM: noofpreds for Levy 2006 PRAISE version and
cstat_all_log$noofpreds2<- dat$noofpreds2   ## noofpreds2 for Levy 2006 external validation version)
cstat_all_log$S5 <- dat$S5
cstat_all_log$noofoutcomes<- dat$o.est
cstat_all_log$sd1 <- dat$sd1
# cstat_all_log$horizon<- dat$horizon # this will be needed when working with all horizons

# remove NA SE(cstat)
cstat_all_log <- cstat_all_log[which(!is.na(cstat_all_log$theta.se)),]  

nstudies  <- nrow(cstat_all_log) #34

#inspect
# cstat_all_log

####################################

## univariate MR
resS_disc_ACM_1yr_univmr_year <- rma(yi=theta, sei=theta.se, mods = ~ year, data=cstat_all_log, method="REML", test="knha")
# resS_disc_ACM_1yr_univmr_pred <- rma(yi=theta, sei=theta.se, mods = ~ noofpreds, data=cstat_all_log, method="REML", test="knha")
# resS_disc_ACM_1yr_univmr_pred2 <- rma(yi=theta, sei=theta.se, mods = ~ noofpreds2, data=cstat_all_log, method="REML", test="knha")
resS_disc_ACM_1yr_univmr_outc <- rma(yi=theta, sei=theta.se, mods = ~ noofoutcomes, data=cstat_all_log, method="REML", test="knha")
resS_disc_ACM_1yr_univmr_sdage <- rma(yi=theta, sei=theta.se, mods = ~ sd1, data=cstat_all_log, method="REML", test="knha")

# resS_disc_ACM_1yr_mr <- rma(yi=theta, sei=theta.se, mods = ~ year + noofpreds + noofoutcomes, data=cstat_all_log, method="REML", test="knha")
# resS_disc_ACM_1yr_mr2 <- rma(yi=theta, sei=theta.se, mods = ~ year + noofpreds2 + noofoutcomes, data=cstat_all_log, method="REML", test="knha")

# resS_disc_ACM_1yr_mr <- rma(yi=theta, sei=theta.se, mods = ~ year + noofoutcomes, data=cstat_all_log, method="REML", test="knha")

## write results using "writes in xl ACM 1yr MR results.r"

 
round(1/(1+exp(-0.2392- 0.0592))-1/(1+exp(-0.2392)),2)

plot(cstat_all_log$sd1,cstat_all_log$theta)

lnpred <- 0.2392+ 0.0592*cstat_all_log$sd1
cstat <- 1/(1+exp(-lnpred))

plot(cstat_all_log$sd1,cstat,xlab = "Standard deviation of age (years)", ylab = "SHFM c-statistic")
