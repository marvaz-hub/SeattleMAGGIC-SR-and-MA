## SHFM ##
## Forest plot and MA for O:E 

## SECONDARY ANALYSES: 2YR, 3YR, 5YR (GT3YR out because leads to including duplicated populations)

# install.packages("Hmisc")
# install.packages("metamisc")
# install.packages("metafor")
# install.packages("rJava")
# install.packages("XLConnect")
# 
# install.packages("devtools")
# library("devtools")
# install_version("XLConnect", version = "1.0.1", repos = "http://cran.us.r-project.org")
# install.packages("shape")

library(Hmisc)
library(metamisc)
library(metafor)

library(rJava)
library("XLConnect")

library("dplyr")

library("shape")


path1 = # INSERT PATHWAY TO DIRECTORY CONTAINING THE SUBFOLDERS
path2 = "subscripts/"


#################################################################################
### 2 YR ###
#################################################################################

## Data for calibration 
source(paste0(path1, path2, "cr_SHFM_calib_data.R")) 

## Function to get pooled estimates for oe
source(paste0(path1, path2, "fn_ma_oe.R"))

## Functions to plot forest plot
source(paste0(path1, path2, "fn_forestplot_mlab.r"))
source(paste0(path1, path2, "fn_forestplot_calib.R"))
titplot <- "SHFM" #"SHFM 2-year ACM"
figfile <- "Fig4d_S_2yr_ACMapprox_calib.jpg"
pathfig <- "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/figures/"


### create the necessary dataframes for plotting and pooling ###

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") 
#  1    5 <NA> 
# 34    6    0 


# 2) horizon
dat <- dat[which(dat$horizon==24),]
table(dat$horizon, useNA = "always") #6


####     Overall estimate  
## oe for MA will include oe.est (original or estimated from O and E) and oe.est.frominter

# create dataframe/list with all oe estimates and recalculated 95%-CIs
oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                 citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                 data=dat, slab = study_id  )

# create a dataframe for forest plot
oe_all$n      <- dat$sample_n_model
oe_all$events <- round(dat$o.est,0)
oe_all$id     <- dat$authoryear
oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
dim(oe_all_forest) ## 
plot(oe_all_forest)

# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
resS_cal_ACM_2yr_all <- fn_ma_oe(dat)[[2]]


#### Forest plot

# small preparations
weights_all <- sprintf("%.1f",fn_ma_oe(dat)[[1]])             # extract weights from REML fit
weights_all <- c(weights_all)                               # add 0.0 weight for development study in plot
tau_all     <- paste0( "(Tau2=", round(fn_ma_oe(dat)[[2]]$tau2, 2), ")")   # extract tau from REML fit
estimates <- fn_ma_oe(dat)[[2]]
res <- fn_ma_oe(dat)[[3]]

jpeg(paste0(pathfig, figfile), width = 1200, height = 800)
fn_forest_calib(oe_all_forest,estimates,titplot,weights_all,tau_all,res)
dev.off()



#################################################################################
### 3 YR ###
#################################################################################

## Data for calibration 
source(paste0(path1, path2, "cr_SHFM_calib_data.R")) 

## Function to get pooled estimates for oe
source(paste0(path1, path2, "fn_ma_oe.R"))

## Functions to plot forest plot
source(paste0(path1, path2, "fn_forestplot_mlab.r"))
source(paste0(path1, path2, "fn_forestplot_calib.R"))
titplot <- "SHFM" #"SHFM 3-year ACM"
figfile <- "Fig5d_S_3yr_ACMapprox_calib.jpg"
pathfig <- "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/figures/"


### create the necessary dataframes for plotting and pooling ###

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") # 89


# 2) horizon
dat <- dat[which(dat$horizon==36),]
table(dat$horizon, useNA = "always") #3


####     Overall estimate  
## oe for MA will include oe.est (original or estimated from O and E) and oe.est.frominter

# create dataframe/list with all oe estimates and recalculated 95%-CIs
oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                 citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                 data=dat, slab = study_id  )

# create a dataframe for forest plot
oe_all$n      <- dat$sample_n_model
oe_all$events <- round(dat$o.est,0)
oe_all$id     <- dat$authoryear
oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
dim(oe_all_forest) ## 
plot(oe_all_forest)

# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
resS_cal_ACM_3yr_all <- fn_ma_oe(dat)[[2]]


#### Forest plot

# small preparations
weights_all <- sprintf("%.1f",fn_ma_oe(dat)[[1]])             # extract weights from REML fit
weights_all <- c(weights_all)                               # add 0.0 weight for development study in plot
tau_all     <- paste0( "(Tau2=", round(fn_ma_oe(dat)[[2]]$tau2, 2), ")")   # extract tau from REML fit
estimates <- fn_ma_oe(dat)[[2]]
res <- fn_ma_oe(dat)[[3]]

jpeg(paste0(pathfig, figfile), width = 1200, height = 800)
fn_forest_calib(oe_all_forest,estimates,titplot,weights_all,tau_all,res)
dev.off()



#################################################################################
### GT3 YR ###
#################################################################################

## Data for calibration 
source(paste0(path1, path2, "cr_SHFM_calib_data.R")) 

## Function to get pooled estimates for oe
source(paste0(path1, path2, "fn_ma_oe.R"))

## Functions to plot forest plot
source(paste0(path1, path2, "fn_forestplot_mlab.r"))
source(paste0(path1, path2, "fn_forestplot_calib.R"))
titplot <- "SHFM" #"SHFM GT3-year ACM"
figfile <- "Fig6d_S_GT3yr_ACMapprox_calib.jpg"
pathfig <- "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/figures/"


### create the necessary dataframes for plotting and pooling ###

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") # 40


# 2) horizon
dat <- dat[which((trunc(dat$horizon)==dat$horizon) & (dat$horizon>36)),]
table(dat$horizon, useNA = "always") #1


# ####     Overall estimate  
# ## oe for MA will include oe.est (original or estimated from O and E) and oe.est.frominter
# 
# # create dataframe/list with all oe estimates and recalculated 95%-CIs
# oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
#                  citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
#                  data=dat, slab = study_id  )
# 
# # create a dataframe for forest plot
# oe_all$n      <- dat$sample_n_model
# oe_all$events <- round(dat$o.est,0)
# oe_all$id     <- dat$authoryear
# oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
# dim(oe_all_forest) ## 
# plot(oe_all_forest)
# 
# # funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
# resS_cal_ACM_GT3yr_all <- fn_ma_oe(dat)[[2]]

resS_cal_ACM_GT3yr_all <- c(rep("-",5), 1, "-")


# #### Forest plot
# 
# # small preparations
# weights_all <- sprintf("%.1f",fn_ma_oe(dat)[[1]])             # extract weights from REML fit
# weights_all <- c(weights_all)                               # add 0.0 weight for development study in plot
# tau_all     <- paste0( "(Tau2=", round(fn_ma_oe(dat)[[2]]$tau2, 2), ")")   # extract tau from REML fit
# estimates <- fn_ma_oe(dat)[[2]]
# res <- fn_ma_oe(dat)[[3]]
# 
# jpeg(paste0(pathfig, figfile), width = 1200, height = 800)
# fn_forest_calib(oe_all_forest,estimates,titplot,weights_all,tau_all,res)
# dev.off()


#################################################################################
### 5 YR ###
#################################################################################

## Data for calibration 
source(paste0(path1, path2, "cr_SHFM_calib_data.R")) 

## Function to get pooled estimates for oe
source(paste0(path1, path2, "fn_ma_oe.R"))

## Functions to plot forest plot
source(paste0(path1, path2, "fn_forestplot_mlab.r"))
source(paste0(path1, path2, "fn_forestplot_calib.R"))
titplot <- "SHFM" #"SHFM 5-year ACM"
figfile <- "Fig7d_S_5yr_ACMapprox_calib.jpg"
pathfig <- "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/figures/"


### create the necessary dataframes for plotting and pooling ###

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") 


# 2) horizon
dat <- dat[which(dat$horizon==60),]
table(dat$horizon, useNA = "always") #1


# ####     Overall estimate  
# ## oe for MA will include oe.est (original or estimated from O and E) and oe.est.frominter
# 
# # create dataframe/list with all oe estimates and recalculated 95%-CIs
# oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
#                  citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
#                  data=dat, slab = study_id  )
# 
# # create a dataframe for forest plot
# oe_all$n      <- dat$sample_n_model
# oe_all$events <- round(dat$o.est,0)
# oe_all$id     <- dat$authoryear
# oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
# dim(oe_all_forest) ## 
# plot(oe_all_forest)
# 
# # funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
# resS_cal_ACM_5yr_all <- fn_ma_oe(dat)[[2]]

resS_cal_ACM_5yr_all <- c(rep("-",5), 1, "-")

# #### Forest plot
# 
# # small preparations
# weights_all <- sprintf("%.1f",fn_ma_oe(dat)[[1]])             # extract weights from REML fit
# weights_all <- c(weights_all)                               # add 0.0 weight for development study in plot
# tau_all     <- paste0( "(Tau2=", round(fn_ma_oe(dat)[[2]]$tau2, 2), ")")   # extract tau from REML fit
# estimates <- fn_ma_oe(dat)[[2]]
# res <- fn_ma_oe(dat)[[3]]
# 
# jpeg(paste0(pathfig, figfile), width = 1200, height = 800)
# fn_forest_calib(oe_all_forest,estimates,titplot,weights_all,tau_all,res)
# dev.off()
