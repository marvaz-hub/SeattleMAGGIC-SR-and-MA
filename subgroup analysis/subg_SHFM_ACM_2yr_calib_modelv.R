## SHFM ##
## O:E
## Estimates for ACM and approx ACM (outc==”E6”), horizon 24m (most frequent horizon)

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
 source(paste0(path1, path2, "cr_SHFM_calib_data.R"))

## Function to get pooled estimates for oe
 source(paste0(path1, path2, "fn_ma_oe.R"))

# ## Function to add Q-test, I^2, and tau^2 estimate info
#  source(paste0(path1, path2, "fn_forestplot_mlab.r"))
# titplot <- "" #"SHFM 2-year ACM"
# figfile <- "Fig3h_S_2yr_ACMapprox_cal_modelv.jpg"
# pathfig <- # INSERT THE PATH WHERE YOU WANT TO SAVE THE FIGURES
# 


#################################################################################
### SUBGROUP
### •	•	Model version (S5) 
#################################################################################
table(df_ma$S5,useNA = "always")
# Added CRT   Added LVAD Levy 2006 ext validation         Levy 2006 PRAISE                   Update 
# 3              2                       27                        9                        6 

#################################################################################
### create the necessary dataframes for plotting and pooling ###
#################################################################################
# 
# If using all the data
# dat <- df_ma


# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") 

# 2) horizon
dat <- dat[which(dat$horizon==24),]
table(dat$horizon, useNA = "always") #6

table(dat$S5,useNA = "always") # CHANGE subgroup variable
# Added LVAD Levy 2006 ext validation         Levy 2006 PRAISE                   Update                     <NA> 
#   0                       4                        2                        0                        0 


# 3) Any split? 

dat1 <- dat[which(dat$S5=="Levy 2006 ext validation"),] 
table(dat1$S5, useNA = "always") #4


# dat2 <- dat[which(dat$S5=="Levy 2006 PRAISE"),] 
# table(dat2$S5, useNA = "always") #2
# 
# 
# dat3 <- dat[which(dat$S5=="Update"),] 
# table(dat3$S5, useNA = "always") #0

# ################################################################################################
# ###########     Overall estimate  #############
# ################################################################################################
# 
# # here dat has all studies
# 
# oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
#                  citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
#                  data=dat, slab = study_id  )
# 
# # create a dataframe for forest plot
# oe_all$n      <- dat$sample_n_model
# oe_all$events <- round(dat$o.est,0)
# oe_all$id     <- dat$authoryear
# oe_all$S5     <-dat$S5 #CHANGE subgroup variable
# oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
# dim(oe_all_forest) ## 33
# plot(oe_all_forest)
# 
# y <- oe_all_forest # to use as the base for the forestplot
# 
# # function fn_ma_oe returns a list with weights,c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2"), and rma object
# rma.res <- fn_ma_oe(dat)
# resS_cal_ACM_2yr_25 <- rma.res[[2]]
# res.25 <- rma.res[[3]]
# 
# # to use when comparing the subgroups
# oe_all_log <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
#                      citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
#                      data=dat, slab = study_id, g="log(OE)")
# oe_all_log$S5 <- dat$S5 #CHANGE subgroup variable
# oe_all_log <- oe_all_log[which(!is.na(oe_all_log$theta.se)),] 
# dim(oe_all_log) 

################################################################################################
###########     GROUP 1    #############
################################################################################################

## ** UPDATE DATABASE AND NAME OF VARIBLE **

dat <- dat1 

oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                 citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                 data=dat, slab = study_id  )

# create a dataframe for forest plot
oe_all$n      <- dat$sample_n_model
oe_all$events <- round(dat$o.est,0)
oe_all$id     <- dat$authoryear
oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
dim(oe_all_forest) 
plot(oe_all_forest)


rma.res <- fn_ma_oe(dat)
resS_cal_ACM_2yr_Levy2006extval <- rma.res[[2]]
res.Levy2006extval<- rma.res[[3]]

# ################################################################################################
# ###########     GROUP 2    #############
# ################################################################################################
# 
# ## ** UPDATE DATABASE AND NAME OF VARIBLE **
# 
# dat <- dat2 
# 
# oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
#                  citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
#                  data=dat, slab = study_id  )
# 
# # create a dataframe for forest plot
# oe_all$n      <- dat$sample_n_model
# oe_all$events <- round(dat$o.est,0)
# oe_all$id     <- dat$authoryear
# oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
# dim(oe_all_forest) ## 5
# plot(oe_all_forest)
# 
# rma.res <- fn_ma_oe(dat)
# resS_cal_ACM_2yr_Levy2006PRAISE <- rma.res[[2]]
# res.Levy2006PRAISE <- rma.res[[3]]
#
resS_cal_ACM_2yr_Levy2006PRAISE <- c(rep("-",5), 2, "-")
# 
# ################################################################################################
# ###########     GROUP 3    #############
# ################################################################################################
# 
# ## ** UPDATE DATABASE AND NAME OF VARIBLE **
# 
# dat <- dat3 
# 
# oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
#                  citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
#                  data=dat, slab = study_id  )
# 
# # create a dataframe for forest plot
# oe_all$n      <- dat$sample_n_model
# oe_all$events <- round(dat$o.est,0)
# oe_all$id     <- dat$authoryear
# oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
# dim(oe_all_forest) ## 5 
# plot(oe_all_forest)
# 
# 
# rma.res <- fn_ma_oe(dat)
# resS_cal_ACM_2yr_updatemod <- rma.res[[2]]
# res.updatemod <- rma.res[[3]]
#
resS_cal_ACM_2yr_updatemod <- c(rep("-",5), 0, "-")
