## MAGGIC ##
## Forest plot and MA for O:E, 
## Estimates for ACM and approx ACM (outc==”E6”), horizon 12m (most frequent horizon)
## 28.03.2024

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

## Data for calibration 
source(paste0(path1, path2, "cr_MAGGIC_calib_data.R"))

## Function to get pooled estimates for oe
source(paste0(path1, path2, "fn_ma_oe.R"))

## Functions to plot forest plot
source(paste0(path1, path2, "fn_forestplot_mlab.r"))
source(paste0(path1, path2, "fn_forestplot_calib.R"))
titplot <- "MAGGIC"
figfile <- "Fig1b_M_1yr_ACMapprox_calib.png"
pathfig <- # INSERT THE PATH WHERE YOU WANT TO SAVE THE FIGURES

#################################################################################
### create the necessary dataframes for plotting and pooling ###
#################################################################################
# 
# If using all the data
# dat <- df_ma

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") # 61


# 2) horizon
dat <- dat[which(dat$horizon==12),]
table(dat$horizon, useNA = "always") #32


################################################################################################
############     Overall estimate  #############
## oe for MA will include oe.est (original or estimated from O and E) and oe.est.frominter
################################################################################################

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
resM_cal_ACM_1yr_all <- fn_ma_oe(dat)[[2]]


#################################################################################
########################### Forests plot ########################################
#################################################################################

# small preparations
weights_all <- sprintf("%.1f",fn_ma_oe(dat)[[1]])             # extract weights from REML fit
weights_all <- c(weights_all)                               # add 0.0 weight for development study in plot
tau_all     <- paste0( "(Tau2=", round(fn_ma_oe(dat)[[2]]$tau2, 2), ")")   # extract tau from REML fit
estimates <- fn_ma_oe(dat)[[2]]
res <- fn_ma_oe(dat)[[3]]

# jpeg(paste0(pathfig, figfile), width = 1200, height = 800)
# png(paste0(pathfig, figfile), width = 1100, height = 900)
fn_forest_calib(oe_all_forest,estimates,titplot,weights_all,tau_all,res)
# dev.off()

