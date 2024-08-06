## MAGGIC ##
## cstat and auc together
## Estimates for ACM and approx ACM (outc==”E6”), horizon 36m 

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

path1 = "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/SeattleMAGGIC analysis files NEWCOVERCODES/"
path2 = "subscripts/"

## Data for discrimination 
 source(paste0(path1, path2, "cr_MAGGIC_disc_data.R"))

## Function to get pooled estimates for cstat
 source(paste0(path1, path2, "fn_ma_cstat.R"))

# ## Function to add Q-test, I^2, and tau^2 estimate info
#  source(paste0(path1, path2, "fn_forestplot_mlab.r"))
# titplot <- "MAGGIC" #" 3-year ACM"
# figfile <- "Fig5d_M_3yr_ACMapprox_disc_modelv.jpg"
# pathfig <- "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/figures/"


#################################################################################
### SUBGROUP
### •	•	Model version (S5) 
#################################################################################
table(df_ma$S5,useNA = "always")
# Original  Unclear   Update     <NA> 
#   6       18          40        0 



#################################################################################
### create the necessary dataframes for plotting and pooling ###
#################################################################################
# 
# If using all the data
# dat <- df_ma

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") #45

# 2) horizon
dat <- dat[which(dat$horizon==36),]
table(dat$horizon, useNA = "always") #11

table(dat$S5,useNA = "always") # CHANGE subgroup variable
# Original  Unclear   Update     <NA> 
#      3        5       3        0 


# 3) Any split? 

dat1 <- dat[which(dat$S5=="Original"),] 

table(dat1$S5, useNA = "always") #3


dat2 <- dat[which(dat$S5=="Unclear"),] 

table(dat2$S5, useNA = "always") #5


dat3 <- dat[which(dat$S5=="Update"),] 

table(dat3$S5, useNA = "always") #3


# ################################################################################################
# ###########     Overall estimate  #############
# ################################################################################################
# 
# # here dat has all studies
# 
# cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
#                    cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
#                    N = sample_n_model, O = o.est, data=dat, slab=study_id)
# 
# # create a dataframe for forest plot
# cstat_all$n      <- dat$sample_n_model
# cstat_all$events <- round(dat$o.est,0)
# cstat_all$id     <- dat$authoryear
# cstat_all$S5     <-dat$S5 #CHANGE subgroup variable
# cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
# dim(cstat_all_forest) ## 23
# plot(cstat_all_forest)
# 
# y <- cstat_all_forest # to use as the base for the forestplot
# 
# # function fn_ma_cstat returns a list with weights,c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2"), and rma object
# rma.res <- fn_ma_cstat(dat)
# resM_disc_ACM_3yr_all <- rma.res[[2]]
# res.all <- rma.res[[3]]
# 
# # to use when comparing the subgroups
# cstat_all_log <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
#                        N = sample_n_model, O= o.est, data=dat, slab=study_id, g="log(cstat/(1-cstat))")
# cstat_all_log$S5 <- dat$S5 #CHANGE subgroup variable
# cstat_all_log <- cstat_all_log[which(!is.na(cstat_all_log$theta.se)),] 
# 

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
dim(cstat_all_forest) ## 23 9
plot(cstat_all_forest)

rma.res <- fn_ma_cstat(dat)
resM_disc_ACM_3yr_origmod <- rma.res[[2]]
res.origmod <- rma.res[[3]]


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
dim(cstat_all_forest) ## 23 9
plot(cstat_all_forest)

rma.res <- fn_ma_cstat(dat)
resM_disc_ACM_3yr_uncmod <- rma.res[[2]]
res.uncmod <- rma.res[[3]]


################################################################################################
###########     GROUP 3    #############
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
dim(cstat_all_forest) ## 23 9
plot(cstat_all_forest)

rma.res <- fn_ma_cstat(dat)
resM_disc_ACM_3yr_upmod <- rma.res[[2]]
res.upmod <- rma.res[[3]]

