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

path1 = "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/SeattleMAGGIC analysis files NEWCOVERCODES/"
path2 = "subscripts/"

## Data for discrimination 
 source(paste0(path1, path2, "cr_SHFM_calib_data.R"))

## Function to get pooled estimates for oe
 source(paste0(path1, path2, "fn_ma_oe.R"))



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
dat <- dat[which(dat$horizon==36),]
table(dat$horizon, useNA = "always") #3

table(dat$S5,useNA = "always") # CHANGE subgroup variable
# Added LVAD Levy 2006 ext validation         Levy 2006 PRAISE                   Update                     <NA> 
#   0                       2                        1                        0                        0 


# 3) Any split? 

# dat1 <- dat[which(dat$S5=="Levy 2006 ext validation"),] 
# table(dat1$S5, useNA = "always") #15
# 

# dat2 <- dat[which(dat$S5=="Levy 2006 PRAISE"),] 
# table(dat2$S5, useNA = "always") #5
# 
# 
# dat3 <- dat[which(dat$S5=="Update"),] 
# table(dat3$S5, useNA = "always") #5

resS_cal_ACM_3yr_Levy2006extval <- c(rep("-",5), 2, "-")
resS_cal_ACM_3yr_Levy2006PRAISE <- c(rep("-",5), 1, "-")
resS_cal_ACM_3yr_updatemod <- c(rep("-",5), 0, "-")

