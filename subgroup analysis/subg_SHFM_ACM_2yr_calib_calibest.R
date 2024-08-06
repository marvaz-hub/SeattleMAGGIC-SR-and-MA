## SHFM ##
## O:E
## Estimates for ACM and approx ACM (outc==”E6”), horizon 24m 

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

library("shape")

path1 = "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/SeattleMAGGIC analysis files NEWCOVERCODES/"
path2 = "subscripts/"

## Data for discrimination 
 source(paste0(path1, path2, "cr_SHFM_calib_data.R"))

## Function to get pooled estimates for OE
 source(paste0(path1, path2, "fn_ma_oe.R"))

# ## Function to add Q-test, I^2, and tau^2 estimate info
#  source(paste0(path1, path2, "fn_forestplot_mlab.r"))
# titplot <- "SHFM" #" 2-year ACM"
# figfile <- "Fig4g_S_2yr_ACMapprox_cal_calibest.jpg"
# pathfig <- "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/figures/"


#################################################################################
### SUBGROUP
### •	Calibration estimate O:E reported and estimated variability vs estimates based 
### on intercept of calibration curve (S3) – Maria to derive
#################################################################################
table(df_ma$S3,useNA = "always")
# Estimates based on intercept of calibration curve O and E reported, OE calculated and variability estimated 
# 7                                                         4 
# OE reported and variability estimated                                                      <NA> 
#   36  

df_ma$S3[df_ma$S3 == "O and E reported, OE calculated and variability estimated"] <- "OE reported and variability estimated"


#################################################################################
### create the necessary dataframes for plotting and pooling ###
#################################################################################
# 
# If using all the data
# dat <- df_ma

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") #40

# 2) horizon
dat <- dat[which(dat$horizon==24),]
table(dat$horizon, useNA = "always") #6

table(dat$S3,useNA = "always") # CHANGE subgroup variable 
# Estimates based on intercept of calibration curve             OE reported and variability estimated 
# 0                                                             6 


# 3) Any split? 

# dat1 <- dat[which(dat$S3=="Estimates based on intercept of calibration curve"),] 
# 
# table(dat1$S3, useNA = "always") #0
# 

dat2 <- dat[which(dat$S3=="OE reported and variability estimated"),] 

table(dat2$S3, useNA = "always") #6

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
# oe_all$S3     <-dat$S3 #CHANGE subgroup variable
# oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
# dim(oe_all_forest) 
# plot(oe_all_forest)
# 
# y <- oe_all_forest # to use as the base for the forestplot
# 
# # function fn_ma_oe returns a list with weights,c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2"), and rma object
# rma.res <- fn_ma_oe(dat)
# resS_cal_ACM_2yr_all <- rma.res[[2]]
# res.all <- rma.res[[3]]
# 
# # to use when comparing the subgroups
# oe_all_log <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
#                      citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
#                      data=dat, slab = study_id, g="log(OE)")
# oe_all_log$S3 <- dat$S3 #CHANGE subgroup variable
# oe_all_log <- oe_all_log[which(!is.na(oe_all_log$theta.se)),] 


# ################################################################################################
# ###########     GROUP 1 #############
# ################################################################################################
# 
# ## ** UPDATE DATABASE AND NAME OF VARIBLE **
# 
# dat <- dat1 
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
# dim(oe_all_forest) 
# plot(oe_all_forest)
# 
# rma.res <- fn_ma_oe(dat)
# resS_cal_ACM_2yr_oeestfrominterc <- rma.res[[2]]
# res.oeestfrominterc<- rma.res[[3]]
# 
resS_cal_ACM_2yr_oeestfrominterc <- c(rep("-",5), 0, "-")
#
################################################################################################
###########     GROUP 2 #############
################################################################################################

## ** UPDATE DATABASE AND NAME OF VARIBLE **

dat <- dat2 

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
resS_cal_ACM_2yr_oerep <- rma.res[[2]]
res.oerep <- rma.res[[3]]



