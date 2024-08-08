## MAGGIC ##
## Forest plot and MA for cstat and auc together
## Estimates for ACM and approx ACM (outc==”E6”), horizon 24m (most frequent horizon)

## SECONDARY ANALYSES: 2YR, 3YR, GT3YR

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


#################################################################################
### 2 YR ###
#################################################################################

## Data for discrimination 
source(paste0(path1, path2, "cr_MAGGIC_disc_data.R"))

## Function to get pooled estimates for cstat
source(paste0(path1, path2, "fn_ma_cstat.R"))

## Functions to plot forest plot
source(paste0(path1, path2, "fn_forestplot_mlab.r"))
source(paste0(path1, path2, "fn_forestplot_disc.R"))
titplot <- "MAGGIC" #"MAGGIC 2-year ACM"
figfile <- "Fig4a_M_2yr_ACMapprox_disc.jpg"
pathfig <- "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/figures/"

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") # 45 

# 2) horizon
dat <- dat[which(dat$horizon==24),]
table(dat$horizon, useNA = "always") #2

###########     Overall estimate  #############

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

# # function fn_ma_cstat returns a list with weights,c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2"), and rma object
# resM_disc_ACM_2yr_all <- fn_ma_cstat(dat)[[2]]

resM_disc_ACM_2yr_all <- c(rep("-",5), 2, "-")

# # ########################### Forests plot ########################################
# 
# # small preparations
# weights_all <- sprintf("%.1f",fn_ma_cstat(dat)[[1]])             # extract weights from REML fit
# weights_all <- c(weights_all)                               # add 0.0 weight for development study in plot
# tau_all     <- paste0( "(Tau2=", round(fn_ma_cstat(dat)[[2]]$tau2, 2), ")")   # extract tau from REML fit
# estimates <- fn_ma_cstat(dat)[[2]]
# res <- fn_ma_cstat(dat)[[3]]
# 
# jpeg(paste0(pathfig, figfile), width = 1200, height = 800)
# fn_forest_disc(cstat_all_forest,estimates,titplot,weights_all,tau_all,res)
# dev.off()
# 




#################################################################################
### 3 YR ###
#################################################################################

## Data for discrimination 
source(paste0(path1, path2, "cr_MAGGIC_disc_data.R"))

## Function to get pooled estimates for cstat
source(paste0(path1, path2, "fn_ma_cstat.R"))

## Functions to plot forest plot
source(paste0(path1, path2, "fn_forestplot_mlab.r"))
source(paste0(path1, path2, "fn_forestplot_disc.R"))
titplot <- "MAGGIC" #"MAGGIC 3-year ACM"
figfile <- "Fig5a_M_3yr_ACMapprox_disc.jpg"
pathfig <- "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/figures/"

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") # 45 

# 2) horizon
dat <- dat[which(dat$horizon==36),]
table(dat$horizon, useNA = "always") #11

###########     Overall estimate  #############

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O = o.est, data=dat, slab=study_id)

# create a dataframe for forest plot
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) ## 11
plot(cstat_all_forest)

# function fn_ma_cstat returns a list with weights,c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2"), and rma object
resM_disc_ACM_3yr_all <- fn_ma_cstat(dat)[[2]]

# ########################### Forests plot ########################################

# small preparations
weights_all <- sprintf("%.1f",fn_ma_cstat(dat)[[1]])             # extract weights from REML fit
weights_all <- c(weights_all)                               # add 0.0 weight for development study in plot
tau_all     <- paste0( "(Tau2=", round(fn_ma_cstat(dat)[[2]]$tau2, 2), ")")   # extract tau from REML fit
estimates <- fn_ma_cstat(dat)[[2]]
res <- fn_ma_cstat(dat)[[3]]

jpeg(paste0(pathfig, figfile), width = 1200, height = 800)
fn_forest_disc(cstat_all_forest,estimates,titplot,weights_all,tau_all,res)
dev.off()




#################################################################################
### GT3 YR ###
#################################################################################

## Data for discrimination 
source(paste0(path1, path2, "cr_MAGGIC_disc_data.R"))

## Function to get pooled estimates for cstat
source(paste0(path1, path2, "fn_ma_cstat.R"))

## Functions to plot forest plot
source(paste0(path1, path2, "fn_forestplot_mlab.r"))
source(paste0(path1, path2, "fn_forestplot_disc.R"))
titplot <- "MAGGIC" #"MAGGIC GT3-year ACM"
figfile <- "Fig6a_M_GT3yr_ACMapprox_disc.jpg"
pathfig <- "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/figures/"

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") # 45 

# 2) horizon
dat <- dat[which((trunc(dat$horizon)==dat$horizon) & (dat$horizon>36)),]
table(dat$horizon, useNA = "always") #0

# ###########     Overall estimate  #############
# 
# cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
#                    cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
#                    N = sample_n_model, O = o.est, data=dat, slab=study_id)
# 
# # create a dataframe for forest plot
# cstat_all$n      <- dat$sample_n_model
# cstat_all$events <- round(dat$o.est,0)
# cstat_all$id     <- dat$authoryear
# cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
# dim(cstat_all_forest) ## 23
# plot(cstat_all_forest)
# 
# # function fn_ma_cstat returns a list with weights,c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2"), and rma object
# resM_disc_ACM_GT3yr_all <- fn_ma_cstat(dat)[[2]]

resM_disc_ACM_GT3yr_all <- c(rep("-",5), 0, "-")

# # ########################### Forests plot ########################################
# 
# # small preparations
# weights_all <- sprintf("%.1f",fn_ma_cstat(dat)[[1]])             # extract weights from REML fit
# weights_all <- c(weights_all)                               # add 0.0 weight for development study in plot
# tau_all     <- paste0( "(Tau2=", round(fn_ma_cstat(dat)[[2]]$tau2, 2), ")")   # extract tau from REML fit
# estimates <- fn_ma_cstat(dat)[[2]]
# res <- fn_ma_cstat(dat)[[3]]
# 
# jpeg(paste0(pathfig, figfile), width = 1200, height = 800)
# fn_forest_disc(cstat_all_forest,estimates,titplot,weights_all,tau_all,res)
# dev.off()
