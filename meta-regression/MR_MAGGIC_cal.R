## MAGGIC ##
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


path1 = "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/SeattleMAGGIC analysis files NEWCOVERCODES/"


#################################################################################
### 4. Meta regression by horizon, using estimates for ACM and approx. ACM ###
#################################################################################

## Data for calibration 
path2 = "subscripts/"
source(paste0(path1, path2, "cr_MAGGIC_calib_data.R"))

# choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") # 29

table(dat$horizon, useNA = "always")
  # 12   24   36 <NA> 
  # 18    1   10    0 

table(dat$year)
# 2013 2014 2017 2018 2019 2020 2021 2022 2023 
# 1    1    2    3    3    4    6    1    8 

dat$noofoutcomes<- round(dat$o.est,0)
summary(dat$noofoutcomes)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6      57     108    1532     273      20111 

# create dataframe/list with all oe estimates and recalculated 95%-CIs
# oe for MA will include oe.est (original or estimated from O and E) and oe.est.frominter

oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                 citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                 data=dat, slab = study_id  )

# create a dataframe for forest plot to inspect
oe_all$n      <- dat$sample_n_model
oe_all$events <- round(dat$o.est,0)
oe_all$id     <- dat$authoryear
oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
dim(oe_all_forest) ## 29
plot(oe_all_forest)

# dataframe for MA
oe_all_log <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                     citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                     data=dat, slab = study_id, g="log(OE)")

## add information for meta-regression
oe_all_log$vi <- oe_all_log$theta.se^2
oe_all_log$year <- dat$year
oe_all_log$noofoutcomes<- dat$noofoutcomes
oe_all_log$horizon<- dat$horizon
oe_all_log$sd1<- dat$sd1

oe_all_log <- oe_all_log[which(!is.na(oe_all_log$theta.se)),]   # remove NA SE(OE)
nstudies <- nrow(oe_all_log) #29

## univariate MR
resM_cal_ACM_univmr_year <- rma(yi=theta, sei=theta.se, mods = ~ year, data=oe_all_log, method="REML", test="knha")
resM_cal_ACM_univmr_outc <- rma(yi=theta, sei=theta.se, mods = ~ noofoutcomes, data=oe_all_log, method="REML", test="knha")
resM_cal_ACM_univmr_hor <- rma(yi=theta, sei=theta.se, mods = ~ horizon, data=oe_all_log, method="REML", test="knha")
resM_cal_ACM_univmr_sdage <- rma(yi=theta, sei=theta.se, mods = ~ sd1, data=oe_all_log, method="REML", test="knha")

## multivariable MR
# resM_cal_ACM_mr <- rma(yi=theta, sei=theta.se, mods = ~ year + noofoutcomes + horizon, data=oe_all_log, method="REML", test="knha")



#######################################################################################
### 5. Meta regression by horizon, using estimates for ACM, approx. ACM and composites ###
#######################################################################################

path3 = "datasets/"
file = "extraMA_M.csv"

# read the data into R
# Revised files with ‘inclC’ and inclC’ =”Exclude” if not reporting estimate and
# could not estimate from other data or estimate is provided or estimated from other
# data for composite outcome but there is already an estimate included for ACM 
# (or another composite) for the same horizon.

df_extraMA <- data.frame(read.csv(paste0(path1, path3, file),sep=","))
table(df_extraMA$inclC)
# Exclude Include 
# 74      32 
df_extraMA <- df_extraMA[df_extraMA$inclC=="Include",]

## Data for calibration 
path2 = "subscripts/"
source(paste0(path1, path2, "cr_MAGGIC_calib_data.R"))

# choose outcome ACM and approx ACM (outc==”E6”) and composites when ACM not reported
dat <- df_ma[df_ma$lineno %in% df_extraMA$lineno,]
dim(dat)
table(dat$outcome, useNA = "always")
#  1    2    5 <NA> 
# 29    2    1    0 
#outcome = outcome in numerical codes 
# 1 ACM
# 2 ACM or heart Tx
# 3 ACM or urgent heart Tx
# 4 ACM or hospitalisation
# 5 ACM or heart Tx or LVAD
# 6 ACM or urgent heart Tx or LVAD
# 7 ACM or hosp for acute HF or urgent heart Tx
# 8 ACM or ICD shock
# 9 ACM or urgent heart Tx
# 10 ACM or LVAD
# 11 ACM or HF hosp

table(dat$horizon, useNA = "always")
# 12   24   36 <NA> 
# 20    1   11    0  

table(dat$year)
# 2013 2014 2017 2018 2019 2020 2021 2022 2023 
#   1    1    2    3    3    4    6    1   11 

dat$noofoutcomes<- round(dat$o.est,0)
summary(dat$noofoutcomes)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.0    41.5   107.0  1393.3   251.2 20111.0 

# create dataframe/list with all oe estimates and recalculated 95%-CIs
# oe for MA will include oe.est (original or estimated from O and E) and oe.est.frominter

oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                 citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                 data=dat, slab = study_id  )

# create a dataframe for forest plot to inspect
oe_all$n      <- dat$sample_n_model
oe_all$events <- round(dat$o.est,0)
oe_all$id     <- dat$authoryear
oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
dim(oe_all_forest) ## 29
plot(oe_all_forest)

# dataframe for MA
oe_all_log <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                     citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                     data=dat, slab = study_id, g="log(OE)")

## add information for meta-regression
oe_all_log$vi <- oe_all_log$theta.se^2
oe_all_log$year <- dat$year
oe_all_log$noofoutcomes<- dat$noofoutcomes
oe_all_log$horizon<- dat$horizon
oe_all_log$sd1<- dat$sd1

oe_all_log <- oe_all_log[which(!is.na(oe_all_log$theta.se)),]   # remove NA SE(OE)
nstudies <- nrow(oe_all_log) #32

## univariate MR
resM_cal_ACMcomp_univmr_year <- rma(yi=theta, sei=theta.se, mods = ~ year, data=oe_all_log, method="REML", test="knha")
resM_cal_ACMcomp_univmr_outc <- rma(yi=theta, sei=theta.se, mods = ~ noofoutcomes, data=oe_all_log, method="REML", test="knha")
resM_cal_ACMcomp_univmr_hor <- rma(yi=theta, sei=theta.se, mods = ~ horizon, data=oe_all_log, method="REML", test="knha")
resM_cal_ACMcomp_univmr_sdage <- rma(yi=theta, sei=theta.se, mods = ~ sd1, data=oe_all_log, method="REML", test="knha")

## multivariable MR
# resM_cal_ACMcomp_mr <- rma(yi=theta, sei=theta.se, mods = ~ year + noofoutcomes + horizon, data=oe_all_log, method="REML", test="knha")



#################################################################################
### 6.	Sensitivity analysis: meta regression by horizon, using estimates of ACM
### and approx. ACM but exclude estimates if horiz==”E18” 
#################################################################################

## Data for calibration 
path2 = "subscripts/"
source(paste0(path1, path2,"cr_MAGGIC_calib_data.R")) 

## exclude estimates if horiz==”E18” 
df_ma<- df_ma[which(df_ma$horiz!="E18" ),]

# choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always")
#  1    5 <NA> 
# 32    4    0 

table(dat$horizon, useNA = "always")
# 12   24   36   60 <NA> 
# 27    5    3    1    0 

table(dat$year)
# 2006 2009 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 
# 8    3    2    2    2    3    1    3    1    2    2    1    3    1    2 

dat$noofoutcomes<- round(dat$o.est,0)
summary(dat$noofoutcomes)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0    33.0    94.0   294.5   415.8  1727.0 

# create dataframe/list with all oe estimates and recalculated 95%-CIs
# oe for MA will include oe.est (original or estimated from O and E) and oe.est.frominter

oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                 citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                 data=dat, slab = study_id  )

# create a dataframe for forest plot to inspect
oe_all$n      <- dat$sample_n_model
oe_all$events <- dat$o.est
oe_all$id     <- dat$authoryear
oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
dim(oe_all_forest) ## 36
plot(oe_all_forest)

# dataframe for MA
oe_all_log <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                     citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                     data=dat, slab = study_id, g="log(OE)")

## add information for meta-regression
oe_all_log$vi <- oe_all_log$theta.se^2
oe_all_log$year <- dat$year
oe_all_log$noofoutcomes<- dat$noofoutcomes
oe_all_log$horizon<- dat$horizon
oe_all_log$sd1<- dat$sd1

# remove NA SE(OE)
oe_all_log <- oe_all_log[which(!is.na(oe_all_log$theta.se)),]  
nstudies <- nrow(oe_all_log) #36

## univariate MR
resM_cal_ACM_nonestHor_univmr_year <- rma(yi=theta, sei=theta.se, mods = ~ year, data=oe_all_log, method="REML", test="knha")
resM_cal_ACM_nonestHor_univmr_outc <- rma(yi=theta, sei=theta.se, mods = ~ noofoutcomes, data=oe_all_log, method="REML", test="knha")
resM_cal_ACM_nonestHor_univmr_hor <- rma(yi=theta, sei=theta.se, mods = ~ horizon, data=oe_all_log, method="REML", test="knha")
resM_cal_ACM_nonestHor_univmr_adage <- rma(yi=theta, sei=theta.se, mods = ~ sd1, data=oe_all_log, method="REML", test="knha")

## multivariable MR
# resM_cal_ACM_nonestHor_mr <- rma(yi=theta, sei=theta.se, mods = ~ year + noofoutcomes + horizon, data=oe_all_log, method="REML", test="knha")
