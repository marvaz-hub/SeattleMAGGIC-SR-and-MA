## SHFM ##
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

path1 = # INSERT PATHWAY TO DIRECTORY CONTAINING THE SUBFOLDERS
path2 = "subscripts/"

## Data for discrimination 
 source(paste0(path1, path2, "cr_SHFM_disc_data.R"))

## Function to get pooled estimates for cstat
 source(paste0(path1, path2, "fn_ma_cstat.R"))


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
# 57    3    1    0 

# 2) horizon
dat <- dat[which(dat$horizon==36),]
table(dat$horizon, useNA = "always") #8

table(dat$S1,useNA = "always") # CHANGE subgroup variable 
# Acute       Chronic Unclear/Mixed          <NA> 
#   0            5             3             0 


# 3) Any split? 

# dat1 <- dat[which(dat$S1=="Acute"),] 
# 
# table(dat1$S1, useNA = "always") #0


dat2 <- dat[which(dat$S1=="Chronic"),] 

table(dat2$S1, useNA = "always") #5


dat3 <- dat[which(dat$S1=="Unclear/Mixed" | dat$S1==""),] 

table(dat3$S1, useNA = "always") #3




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
# resS_disc_ACM_3yr_acute <- rma.res[[2]]
# res.acute <- rma.res[[3]]

resS_disc_ACM_3yr_acute <- c(rep("-",5), 0, "-")


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
resS_disc_ACM_3yr_chronic <- rma.res[[2]]
res.chronic <- rma.res[[3]]


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
dim(cstat_all_forest) 
plot(cstat_all_forest)

rma.res <- fn_ma_cstat(dat)
resS_disc_ACM_3yr_uncmixhf <- rma.res[[2]]
res.uncmixhf <- rma.res[[3]]


