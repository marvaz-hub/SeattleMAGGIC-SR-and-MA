## SHFM ##
## cstat and auc together
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

path1 = # INSERT PATHWAY TO DIRECTORY CONTAINING THE SUBFOLDERS
path2 = "subscripts/"

## Data for discrimination 
 source(paste0(path1, path2, "cr_SHFM_disc_data.R"))

## Function to get pooled estimates for cstat
 source(paste0(path1, path2, "fn_ma_cstat.R"))

# ## Function to add Q-test, I^2, and tau^2 estimate info
#  source(paste0(path1, path2, "fn_forestplot_mlab.r"))
# titplot <- "" #"SHFM 2-year ACM"
# figfile <- "Fig4f_S_2yr_ACMapprox_disc_HFtype.jpg"
# pathfig <- # INSERT THE PATH WHERE YOU WANT TO SAVE THE FIGURES
# 

#################################################################################
### SUBGROUP
### •	HF type chronic vs acute vs mixed/unclear (S1)
#################################################################################
table(df_ma$S1,useNA = "always")
  #       Acute Chronic   Mixed    <NA> 
  # 21       8      60       6       0  

df_ma$S1[nchar(df_ma$S1)==0] <- "Unclear/Mixed"
df_ma$S1[df_ma$S1 == "Mixed"] <- "Unclear/Mixed"

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
# 57    4    1    0 

# 2) horizon
dat <- dat[which(dat$horizon==24),]
table(dat$horizon, useNA = "always") #5

table(dat$S1,useNA = "always") # CHANGE subgroup variable 
# Acute       Chronic Unclear/Mixed          <NA> 
#   0            3             2             0 


# # 3) Any split? 
# 
# dat1 <- dat[which(dat$S1=="Acute"),] 
# 
# table(dat1$S1, useNA = "always") #0
# 
# 
dat2 <- dat[which(dat$S1=="Chronic"),]

table(dat2$S1, useNA = "always") #3
# 
# 
# dat3 <- dat[which(dat$S1=="Unclear/Mixed" | dat$S1==""),] 
# 
# table(dat3$S1, useNA = "always") #2
# 
# 
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
# cstat_all$S1     <-dat$S1 #CHANGE subgroup variable
# cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
# dim(cstat_all_forest) ## 33
# plot(cstat_all_forest)
# 
# y <- cstat_all_forest # to use as the base for the forestplot
# 
# # function fn_ma_cstat returns a list with weights,c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2"), and rma object
# rma.res <- fn_ma_cstat(dat)
# resS_disc_ACM_2yr_all <- rma.res[[2]]
# res.all <- rma.res[[3]]
# 
# # to use when comparing the subgroups
# cstat_all_log <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
#                        N = sample_n_model, O= o.est, data=dat, slab=study_id, g="log(cstat/(1-cstat))")
# cstat_all_log$S1 <- dat$S1 #CHANGE subgroup variable
# cstat_all_log <- cstat_all_log[which(!is.na(cstat_all_log$theta.se)),] 
# 
# ################################################################################################
# ###########     GROUP 1 #############
# ################################################################################################
# 
# ## ** UPDATE DATABASE AND NAME OF VARIBLE **
# 
# dat <- dat1 
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
# dim(cstat_all_forest) 
# plot(cstat_all_forest)
# 
# rma.res <- fn_ma_cstat(dat)
# resS_disc_ACM_2yr_acute <- rma.res[[2]]
# res.acute <- rma.res[[3]]
# 
resS_disc_ACM_2yr_acute <- c(rep("-",5), 0, "-") 
#
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
resS_disc_ACM_2yr_chronic <- rma.res[[2]]
res.chronic <- rma.res[[3]]
# 
# 
# ################################################################################################
# ###########     GROUP 3    #############
# ################################################################################################
# 
# ## ** UPDATE DATABASE AND NAME OF VARIBLE **
# 
# dat <- dat3 
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
# dim(cstat_all_forest) 
# plot(cstat_all_forest)
# 
# rma.res <- fn_ma_cstat(dat)
# resS_disc_ACM_2yr_uncmixhf <- rma.res[[2]]
# res.uncmixhf <- rma.res[[3]]
# 
#
resS_disc_ACM_2yr_uncmixhf <- c(rep("-",5), 2, "-")
#
# #################################################################################
# ########################### Forests plot ########################################
# #################################################################################
# 
# jpeg(paste0(pathfig, figfile), width = 1200, height = 800)
# 
# # y has cstat_all_forest with all studies
# 
# N <- y$n
# 
# temp <- rep(NA,length(N))
# for (i in 1:length(N)){
#   temp[i] <- nchar(N[i])
# }
# maxlengthN <- max(temp)
# 
# ### set up forest plot (the 'rows' argument is
# ### used to specify in which rows the outcomes will be plotted)
# #pos_weight = 1.03    # define position of weights
# pos_event  = 1.04 # define position of no. events
# pos_div    = 1.09 # define position of divide
# pos_N      = pos_div + (maxlengthN)/100 # define position of sample size
# pos_CI     = 1.68  # define position of CI and graph size
# pos_pred   = -0.5
# 
# ### set font expansion factor (as in forest() above) and use a bold font
# op <- par(cex=1.1, font=2)
# 
# 
# forest(y$theta, ci.lb=y$theta.ciub, ci.ub=y$theta.cilb,
#        slab = y$id,
#        #cex=1.1,
#        xlab = "C-statistic", 
#        xlim=(c(-0.3,pos_CI-.33)), alim=(c(0.4,1)), at=c(0.4,0.5,0.6,0.7,0.8,0.9,1), steps = 7, 
#        refline = 0.5, 
#        ilab=cbind( y$events,  y$n),
#        ilab.xpos=c( pos_event,  pos_N),
#        ilab.pos=c(2,2),
#        order=y$S1,              # CHANGE subgroup variable
#        rows=c(43:38,33:16,11:3), # CHANGE studies within groups
#        ylim=(c(-5,nrow(y)+14)),
#        main=titplot
# )
# 
# 
# ### switch to bold italic font
# par(font=4)
# 
# ### add text for the subgroups (CHANGE)
# text(-0.3, c(44,34,12), pos=4, c("Acute Heart Failure",
#                                  "Chronic Heart Failure",
#                                  "Unclear/Mixed Heart Failure"))
# 
# ### set par back to the original settings
# par(op)
# 
# ## summary polygon for overall
# addpoly(x = resS_disc_ACM_2yr_all$est, ci.lb=resS_disc_ACM_2yr_all$ci.lb, ci.ub=resS_disc_ACM_2yr_all$ci.ub,
#         row=-2, mlab=mlabfun("Overall", res.all))
# 
# ### add summary polygons for the three subgroups (CHANGE res variables and rows for the polygon)
# addpoly(x =resS_disc_ACM_2yr_acute$est, ci.lb=resS_disc_ACM_2yr_acute$ci.lb, ci.ub=resS_disc_ACM_2yr_acute$ci.ub,
#         row=36.5, mlab=mlabfun("RE Model for Subgroup", res.acute))
# addpoly(x =resS_disc_ACM_2yr_chronic$est, ci.lb=resS_disc_ACM_2yr_chronic$ci.lb, ci.ub=resS_disc_ACM_2yr_chronic$ci.ub,
#         row= 14.5, mlab=mlabfun("RE Model for Subgroup", res.chronic))
# addpoly(x =resS_disc_ACM_2yr_uncmixhf$est, ci.lb=resS_disc_ACM_2yr_uncmixhf$ci.lb, ci.ub=resS_disc_ACM_2yr_uncmixhf$ci.ub,
#         row= 1.5, mlab=mlabfun("RE Model for Subgroup", res.uncmixhf))
# 
# 
# 
# ### fit meta-regression model to test for subgroup differences (CHANGE)
# res <- rma(yi=theta, sei=theta.se, mods = ~ S1, data=cstat_all_log, method="REML", test="knha")
# 
# ### add text for the test of subgroup differences
# par(op)
# text(-0.3, -3.5, pos=4, bquote(paste("Test for Subgroup Differences: ",
#                                      Q[M], " = ", .(fmtx(res$QM, digits=2)),
#                                      ", df = ", .(res$p - 1), ", ",
#                                      .(fmtp(res$QMp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)))))
# 
# par(cex=1.1, font=2)
# text(c(-0.3, pos_event, pos_N, pos_CI-.33), nrow(y)+13 , pos = c(4,2,2,2) ,
#      c("Studies", "Events", "N", "C-statistic [95% CI]"))
# 
# dev.off()
