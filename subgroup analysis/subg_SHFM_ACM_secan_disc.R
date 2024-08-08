## SHFM ##
## cstat and auc together

## Subgroup analysis
## 2 yr horizon

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
### •	High participants ROB vs low vs unclear (OS1)
#################################################################################
table(df_ma$OS1,useNA = "always")
# High     Low Unclear    <NA> 
#   50      33      12       0 



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
dat <- dat[which(dat$horizon==24),]
table(dat$horizon, useNA = "always") #5

table(dat$OS1,useNA = "always") # CHANGE subgroup variable 
# High     Low Unclear    <NA> 
#   2      2       1       0 

table(dat$S1,useNA = "always") # CHANGE subgroup variable 
  #     Chronic   Mixed    <NA> 
  # 1       3       1       0 

table(dat$S4,useNA = "always") # CHANGE subgroup variable 
# AUC c-stat   <NA> 
#   3      2      0 

table(dat$S5,useNA = "always") # CHANGE subgroup variable 
# Levy 2006 ext validation         Levy 2006 PRAISE                   Update                     <NA> 
#   3                        1                        1                        0 

