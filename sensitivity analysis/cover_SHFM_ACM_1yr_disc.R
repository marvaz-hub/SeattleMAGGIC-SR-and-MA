## SHFM ##
## Sensitivity analysis for cstat and auc together
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

library("dplyr")


path1 = # INSERT PATHWAY TO DIRECTORY CONTAINING THE SUBFOLDERS
path2 = "subscripts/"

## Data for discrimination 
source(paste0(path1, path2, "cr_SHFM_disc_data.R"))

## Function to get pooled estimates for oe
source(paste0(path1, path2, "fn_ma_cstat.R"))


#################################################################################
### create the necessary dataframes for pooling ###
#################################################################################
# 
# If using all the data
# dat <- df_ma

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") 

# 2) horizon
dat <- dat[which(dat$horizon==12),]
table(dat$horizon, useNA = "always") #34


# 3) 4 sensitivity analyses (E3 and E4 should be ignored in all sensitivity analyses)  (+ 1 sensitivity analysis: exclude internal validation results)
# 1. Exclude estimates with high ROB on any domain (O1)
table(dat$anyhighROB) 
# - O1 
# 2 31
dat1 <- dat[which(dat$anyhighROB!="O1"),]
dim(dat1) #2 DO NOT CALCULATE ESTIMATE

# 2.	Exclude estimates with high COVER departure (indep==”E16”|outc==”E17”| disc==”E20”|disc==E17”|model_p==”E21”|model_p==”E22”|model_p==”E23”)
dat2 <- dat[which(dat$indep!="E16" & dat$outc!="E17" & dat$disc!="E20" & 
                    dat$disc!="E17" & dat$model_p!="E21" & dat$model_p!="E22" & dat$model_p!="E23"),]

dim(dat2) #25

# 3.	Exclude estimates with medium or high COVER departure (popul==”E2”| indep==”E5”|indep==”E16”|outc==”E6”|outc==”E7”|outc==”E17”|disc==”E10”|disc==”E20”|disc==E17”|disc==”E9”|disc==”E11”|model_f==”E12”|model_p==”E12|model_p==E13”|model_p==”E14|model_p==”E15”|model_p==”E21”|model_p==”E22”|model_p==”E23”)
dat3 <- dat[which(dat$popul!="E2" &  dat$indep!="E5" & dat$indep!="E16" & dat$outc!="E6" & 
                    dat$outc!="E7" & dat$outc!="E17" & dat$disc!="E10" & dat$disc!="E20" & dat$disc!="E17" & 
                    dat$disc!="E9" & dat$disc!="E11" & dat$model_f!="E12" & dat$model_p!="E12" & dat$model_p!="E13" & 
                    dat$model_p!="E14" & dat$model_p!="E15" & dat$model_p!="E21" & dat$model_p!="E22" & dat$model_p!="E23"),]
dim(dat3) #3 

# 4.	Exclude estimates with medium or high COVER departure but ignoring E12 (popul==”E2”| indep==”E5”|indep==”E16”|outc==”E6”|outc==”E7”|outc==”E17”|disc==”E10”|disc==”E20”|disc==E17”|disc==”E9”|disc==”E11”|model_p==E13”|model_p==”E14|model_p==”E15”|model_p==”E21”|model_p==”E22”|model_p==”E23”)
dat4 <- dat[which(dat$popul!="E2" & dat$indep!="E5" & dat$indep!="E16" & 
                    dat$outc!="E6" & dat$outc!="E7" & dat$outc!="E17" & dat$disc!="E10" & 
                    dat$disc!="E20" & dat$disc!="E17" & dat$disc!="E9" & dat$disc!="E11" &
                    dat$model_p!="E13" & dat$model_p!="E14" & dat$model_p!="E15" & dat$model_p!="E21" & dat$model_p!="E22" & dat$model_p!="E23"),]
dim(dat4) #5 


# 5. Exclude internal validation results (int_valid=="O2")
#Levy 2006 PRAISE1 lineno 83 12 months outcome 5
dat5 <- dat[which(dat$int_valid!="02"),]



################################################################################################
###########     GROUP 1 #############
################################################################################################

## ** UPDATE DATABASE AND NAME OF VARIBLE **

dat <- dat1 

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O = o.est, data=dat, slab=study_id)

# create a dataframe for forest plot
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) ## 5 9
plot(cstat_all_forest)

# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
resS_disc_ACM_1yr_cover1 <- c(rep("-",5), fn_ma_cstat(dat)[[2]]$n, "-") #fn_ma_cstat(dat)[[2]])  


################################################################################################
###########     GROUP 2 #############
################################################################################################

## ** UPDATE DATABASE AND NAME OF VARIBLE **

dat <- dat2 

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O = o.est, data=dat, slab=study_id)

# create a dataframe for forest plot
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) ## 21
plot(cstat_all_forest)

# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
(resS_disc_ACM_1yr_cover2 <- fn_ma_cstat(dat)[[2]])  


################################################################################################
###########     GROUP 3 #############
################################################################################################

## ** UPDATE DATABASE AND NAME OF VARIBLE **

dat <- dat3 

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O = o.est, data=dat, slab=study_id)

# create a dataframe for forest plot
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) ## 2
plot(cstat_all_forest)

# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
resS_disc_ACM_1yr_cover3 <- fn_ma_cstat(dat)[[2]] #c(rep("-",5), fn_ma_cstat(dat)[[2]]$n, "-")  


################################################################################################
###########     GROUP 4 #############
################################################################################################

## ** UPDATE DATABASE AND NAME OF VARIBLE **

dat <- dat4 

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O = o.est, data=dat, slab=study_id)

# create a dataframe for forest plot
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) ## 2
plot(cstat_all_forest)

# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
resS_disc_ACM_1yr_cover4 <- fn_ma_cstat(dat)[[2]] #fn_ma_cstat(dat)[[2]])  


################################################################################################
###########     GROUP 5 #############
################################################################################################

## ** UPDATE DATABASE AND NAME OF VARIBLE **

dat <- dat5 

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O = o.est, data=dat, slab=study_id)

# create a dataframe for forest plot
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) ## 2
plot(cstat_all_forest)

# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
resS_disc_ACM_1yr_cover5 <- fn_ma_cstat(dat)[[2]] #fn_ma_cstat(dat)[[2]])  

resS_disc_ACM_1yr_cover5
