## SHFM ##
## O:E
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

library("shape")

path1 = "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/SeattleMAGGIC analysis files NEWCOVERCODES/"
path2 = "subscripts/"

## Data for discrimination 
 source(paste0(path1, path2, "cr_SHFM_calib_data.R"))

## Function to get pooled estimates for OE
 source(paste0(path1, path2, "fn_ma_oe.R"))



#################################################################################
### SUBGROUP
### •	High participants ROB vs low vs unclear (OS1)
#################################################################################
table(df_ma$OS1,useNA = "always")
# High     Low Unclear    <NA> 
#   29      16       2       0 


#################################################################################
### create the necessary dataframes for plotting and pooling ###
#################################################################################
# 
# If using all the data
# dat <- df_ma

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") #38

# 2) horizon
dat <- dat[which(dat$horizon==36),]
table(dat$horizon, useNA = "always") #3

table(dat$OS1,useNA = "always") # CHANGE subgroup variable 
# High     Low Unclear    <NA> 
#   2      1       0       0 


# 3) Any split? 

# dat1 <- dat[which(dat$OS1=="High"),] 
# 
# table(dat1$OS1, useNA = "always") #19
# 
# 
# dat2 <- dat[which(dat$OS1=="Low"),] 
# 
# table(dat2$OS1, useNA = "always") #6
# 
# 
# dat3 <- dat[which(dat$OS1=="Unclear"),] 
# 
# table(dat3$OS1, useNA = "always") #1 out

resS_cal_ACM_3yr_hrob <- c(rep("-",5), 2, "-")
resS_cal_ACM_3yr_lrob <- c(rep("-",5), 1, "-")
resS_cal_ACM_3yr_urob <- c(rep("-",5), 0, "-")

