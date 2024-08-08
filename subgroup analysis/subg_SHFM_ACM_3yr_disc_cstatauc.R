## SHFM ##
## cstat and auc together, mortality, 36m horizons

## Subgroup analysis

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

## Function to get pooled estimates for cstat
 source(paste0(path1, path2, "fn_ma_cstat.R"))


#################################################################################
### SUBGROUP
### •	AUC vs c-stat (S4) – updated in cr file
#################################################################################

table(df_ma$S4,useNA = "always")
  # AUC c-stat   <NA> 
  # 60     35      0 


#################################################################################
### create the necessary dataframes for plotting and pooling ###
#################################################################################
# 
# If using all the data
# dat <- df_ma

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always")
  # 1    5    9 <NA> 
  # 57    3    1    0 

# 2) horizon
dat <- dat[which(dat$horizon==36),]
table(dat$horizon, useNA = "always") #8

table(dat$S4,useNA = "always") # CHANGE subgroup variable
# AUC c-stat   <NA> 
#   4     4      0 

# 3) Any split? 

dat1 <- dat[which(dat$S4=="AUC"),] 

table(dat1$S4, useNA = "always") #4


dat2 <- dat[which(dat$S4=="c-stat"),] 

table(dat2$S4, useNA = "always") #



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
dim(cstat_all_forest) 
plot(cstat_all_forest)

rma.res <- fn_ma_cstat(dat)
resS_disc_ACM_3yr_auc <- rma.res[[2]]
res.auc <- rma.res[[3]]


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
dim(cstat_all_forest)
plot(cstat_all_forest)

rma.res <- fn_ma_cstat(dat)
resS_disc_ACM_3yr_cstat <- rma.res[[2]]
res.cstat <- rma.res[[3]]


