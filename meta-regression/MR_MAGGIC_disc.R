## MAGGIC ##
## Meta-regression for cstat and auc together

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

library("shape")


path1 = # INSERT PATHWAY TO DIRECTORY CONTAINING THE SUBFOLDERS


###############################################################################
### 4. Meta regression by horizon, using estimates for ACM and approx. ACM ###
###############################################################################

## Data for discrimination 
path2 = "subscripts/"
source(paste0(path1, path2,"cr_MAGGIC_disc_data.R"))


# choose outcome ACM and approx ACM (outc==”E6”) 
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") # 45 

table(dat$horizon, useNA = "always")
  # 12 18.2 20.4   24   26 34.8   36 37.2 43.2 51.6 52.8 58.8 <NA> 
  # 23    1    1    2    1    1   11    1    1    1    1    1    0 

table(dat$year)
# 2014 2015 2017 2018 2019 2020 2021 2022 2023 
# 1    1    3    7    3    4    8    9    9 

dat$noofoutcomes<- round(dat$o.est,0)
summary(dat$noofoutcomes)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   9      46     102     807     273   20111       4  


# create dataframe/list with all cstats and recalculated 95%-CIs

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se,
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                           N = sample_n_model, O = o.est, data=dat, slab=study_id)

# create a dataframe for forest plot to inspect
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) ## 45
plot(cstat_all_forest)

# dataframe for MA
cstat_all_log <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                       N = sample_n_model, O= o.est, data=dat, slab=study_id, g="log(cstat/(1-cstat))")

## add information for meta-regression
cstat_all_log$vi <- cstat_all_log$theta.se^2
cstat_all_log$year <- dat$year
cstat_all_log$noofoutcomes<- dat$noofoutcomes
cstat_all_log$horizon<- dat$horizon
cstat_all_log$sd1 <- dat$sd1

# remove NA SE(cstat)
cstat_all_log <- cstat_all_log[which(!is.na(cstat_all_log$theta.se)),]  

nstudies  <- nrow(cstat_all_log) #45


## univariate MR
resM_disc_ACM_univmr_year <- rma(yi=theta, sei=theta.se, mods = ~ year, data=cstat_all_log, method="REML", test="knha")
resM_disc_ACM_univmr_outc <- rma(yi=theta, sei=theta.se, mods = ~ noofoutcomes, data=cstat_all_log, method="REML", test="knha")
resM_disc_ACM_univmr_hor <- rma(yi=theta, sei=theta.se, mods = ~ horizon, data=cstat_all_log, method="REML", test="knha")
resM_disc_ACM_univmr_sdage <- rma(yi=theta, sei=theta.se, mods = ~ sd1, data=cstat_all_log, method="REML", test="knha")

## multivariable MR
# resM_disc_ACM_mr <- rma(yi=theta, sei=theta.se, mods = ~ year + noofoutcomes + horizon, data=cstat_all_log, method="REML", test="knha")

  
#######################################################################################
### 5. Meta regression by horizon, using estimates for ACM, approx. ACM and composites ###
#######################################################################################

path3 = "datasets/"
file = "extraMA_M.csv"

# read the data into R
# Revised files with ‘inclC’ and inclD’ =”Exclude” if not reporting estimate and
# could not estimate from other data or estimate is provided or estimated from other
# data for composite outcome but there is already an estimate included for ACM 
# (or another composite) for the same horizon.

df_extraMA <- data.frame(read.csv(paste0(path1, path3, file),sep=","))
table(df_extraMA$inclD)
# Exclude Include 
# 50      56 
df_extraMA <- df_extraMA[df_extraMA$inclD=="Include",]

## Data for discrimination 
path2 = "subscripts/"
source(paste0(path1, path2,"cr_MAGGIC_disc_data.R"))

# choose outcome ACM and approx ACM (outc==”E6”) and composites when ACM not reported
dat <- df_ma[df_ma$lineno %in% df_extraMA$lineno,]
dim(dat)
table(dat$outcome, useNA = "always") # 45 
#  1    2    5    6   11 <NA> 
# 45    1    1    1    8    0 
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
# 12   18 18.2 20.4   24   26   30 31.2   33 34.8   36 37.2 43.2 45.6 46.8 51.6 52.8 58.8 <NA> 
# 25    1    1    1    3    1    1    2    1    1   11    1    2    1    1    1    1    1    0 

table(dat$year)
# 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 
#    1    1    1    3    7    3    6    8   16   10 

dat$noofoutcomes<- round(dat$o.est,0)
summary(dat$noofoutcomes)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 9.00    43.25    90.50   651.88   190.00 20111.00        4 

# create dataframe/list with all cstats and recalculated 95%-CIs

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se,
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O = o.est, data=dat, slab=study_id)

# create a dataframe for forest plot to inspect
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) ## 56
plot(cstat_all_forest)

# dataframe for MA
cstat_all_log <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                       N = sample_n_model, O= o.est, data=dat, slab=study_id, g="log(cstat/(1-cstat))")

## add information for meta-regression
cstat_all_log$vi <- cstat_all_log$theta.se^2
cstat_all_log$year <- dat$year
cstat_all_log$noofoutcomes<- dat$noofoutcomes
cstat_all_log$horizon<- dat$horizon
cstat_all_log$sd1<- dat$sd1

# remove NA SE(cstat)
cstat_all_log <- cstat_all_log[which(!is.na(cstat_all_log$theta.se)),]  

nstudies  <- nrow(cstat_all_log) #56


## univariate MR
resM_disc_ACMcomp_univmr_year <- rma(yi=theta, sei=theta.se, mods = ~ year, data=cstat_all_log, method="REML", test="knha")
resM_disc_ACMcomp_univmr_outc <- rma(yi=theta, sei=theta.se, mods = ~ noofoutcomes, data=cstat_all_log, method="REML", test="knha")
resM_disc_ACMcomp_univmr_hor <- rma(yi=theta, sei=theta.se, mods = ~ horizon, data=cstat_all_log, method="REML", test="knha")
resM_disc_ACMcomp_univmr_sdage <- rma(yi=theta, sei=theta.se, mods = ~ sd1, data=cstat_all_log, method="REML", test="knha")

## multivariable MR
# resM_disc_ACMcomp_mr <- rma(yi=theta, sei=theta.se, mods = ~ year + noofoutcomes + horizon, data=cstat_all_log, method="REML", test="knha")


#################################################################################
### 6.	Sensitivity analysis: meta regression by horizon, using estimates of ACM
### and approx. ACM but exclude estimates if horiz==”E18”
#################################################################################

## Data for calibration 
path2 = "subscripts/"
source(paste0(path1, path2,"cr_MAGGIC_disc_data.R"))

## exclude estimates if horiz==”E18”
df_ma<- df_ma[which(df_ma$horiz!="E18"),]

# choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always")
#  1    
# 36    

table(dat$horizon, useNA = "always")
# 12   24   36 <NA> 
# 23    2   11    0 

table(dat$year)
# 2014 2015 2017 2018 2019 2020 2021 2022 2023 
# 1    1    2    6    3    4    8    3    8 

dat$noofoutcomes<- round(dat$o.est,0)
summary(dat$noofoutcomes)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 9.0    57.0   108.0   959.5   273.0 20111.0       3 

# create dataframe/list with all cstats and recalculated 95%-CIs

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se,
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O = o.est, data=dat, slab=study_id)

# create a dataframe for forest plot to inspect
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) 
plot(cstat_all_forest)

# dataframe for MA
cstat_all_log <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                       N = sample_n_model, O= o.est, data=dat, slab=study_id, g="log(cstat/(1-cstat))")

## add information for meta-regression
cstat_all_log$vi <- cstat_all_log$theta.se^2
cstat_all_log$year <- dat$year
cstat_all_log$noofoutcomes<- dat$noofoutcomes
cstat_all_log$horizon<- dat$horizon
cstat_all_log$sd1<- dat$sd1

# remove NA SE(cstat)
cstat_all_log <- cstat_all_log[which(!is.na(cstat_all_log$theta.se)),]  

nstudies  <- nrow(cstat_all_log)


## univariate MR
resM_disc_ACM_nonestHor_univmr_year <- rma(yi=theta, sei=theta.se, mods = ~ year, data=cstat_all_log, method="REML", test="knha")
resM_disc_ACM_nonestHor_univmr_outc <- rma(yi=theta, sei=theta.se, mods = ~ noofoutcomes, data=cstat_all_log, method="REML", test="knha")
resM_disc_ACM_nonestHor_univmr_hor <- rma(yi=theta, sei=theta.se, mods = ~ horizon, data=cstat_all_log, method="REML", test="knha")
resM_disc_ACM_nonestHor_univmr_sdage <- rma(yi=theta, sei=theta.se, mods = ~ sd1, data=cstat_all_log, method="REML", test="knha")

## multivariable MR
# resM_disc_ACM_nonestHor_mr <- rma(yi=theta, sei=theta.se, mods = ~ year + noofoutcomes + horizon, data=cstat_all_log, method="REML", test="knha")
