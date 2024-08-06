## MAGGIC ##
## O:E
## Estimates for ACM and approx ACM (outc==”E6”), horizon 12m (most frequent horizon)

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
 source(paste0(path1, path2, "cr_MAGGIC_calib_data.R"))

## Function to get pooled estimates for OE
 source(paste0(path1, path2, "fn_ma_oe.R"))

## Function to add Q-test, I^2, and tau^2 estimate info
 source(paste0(path1, path2, "fn_forestplot_mlab.r"))
titplot <- "MAGGIC" #" 1-year ACM"
figfile <- "Fig3c_M_1yr_ACMapprox_cal_calibest.jpg"
pathfig <- "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/figures/"


#################################################################################
### SUBGROUP
### •	Calibration estimate O:E reported and estimated variability vs estimates based 
### on intercept of calibration curve (S3) – Maria to derive
#################################################################################
table(df_ma$S3,useNA = "always")
# Estimates based on intercept of calibration curve O and E reported, OE calculated and variability estimated 
# 14                                                         2 
# OE reported and variability estimated                                                      <NA> 
#   18  

df_ma$S3[df_ma$S3 == "O and E reported, OE calculated and variability estimated"] <- "OE reported and variability estimated"


#################################################################################
### create the necessary dataframes for plotting and pooling ###
#################################################################################
# 
# If using all the data
# dat <- df_ma

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") #29

# 2) horizon
dat <- dat[which(dat$horizon==12),]
table(dat$horizon, useNA = "always") #18

table(dat$S3,useNA = "always") # CHANGE subgroup variable 
# Estimates based on intercept of calibration curve             OE reported and variability estimated 
# 10                                                             8 


# 3) Any split? 

dat1 <- dat[which(dat$S3=="Estimates based on intercept of calibration curve"),] 

table(dat1$S3, useNA = "always") #10


dat2 <- dat[which(dat$S3=="OE reported and variability estimated"),] 

table(dat2$S3, useNA = "always") #8

################################################################################################
###########     Overall estimate  #############
################################################################################################

# here dat has all studies

oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                 citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                 data=dat, slab = study_id  )

# create a dataframe for forest plot
oe_all$n      <- dat$sample_n_model
oe_all$events <- round(dat$o.est,0)
oe_all$id     <- dat$authoryear
oe_all$S3     <-dat$S3 #CHANGE subgroup variable
oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
dim(oe_all_forest) ## 23
plot(oe_all_forest)

y <- oe_all_forest # to use as the base for the forestplot

# function fn_ma_oe returns a list with weights,c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2"), and rma object
rma.res <- fn_ma_oe(dat)
resM_cal_ACM_1yr_all <- rma.res[[2]]
res.all <- rma.res[[3]]

# to use when comparing the subgroups
oe_all_log <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                     citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                     data=dat, slab = study_id, g="log(OE)")
oe_all_log$S3 <- dat$S3 #CHANGE subgroup variable
oe_all_log <- oe_all_log[which(!is.na(oe_all_log$theta.se)),] 


################################################################################################
###########     GROUP 1 #############
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
resM_cal_ACM_1yr_oeestfrominterc <- rma.res[[2]]
res.oeestfrominterc<- rma.res[[3]]


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
resM_cal_ACM_1yr_oerep <- rma.res[[2]]
res.oerep <- rma.res[[3]]



#################################################################################
########################### Forests plot ########################################
#################################################################################

jpeg(paste0(pathfig, figfile), width = 1200, height = 800)

# y has oe_all_forest with all studies

N <- y$n

temp <- rep(NA,length(N))
for (i in 1:length(N)){
  temp[i] <- nchar(N[i])
}
maxlengthN <- max(temp)

pos_weight = 1.58 +.20   # define position of weights 
pos_event  = 1.67 +.20 # define position of no. events
pos_div    = 1.68 +.22 # define position of divide
pos_N      = pos_div + (maxlengthN+1)/100 #1.73 # define position of sample size
pos_CI     = 2.3 +.23 # define position of CI and graph size
pos_pred   = -0.5
#par(cex=0.8, font=1)  
#y lim - space for rows, rows are more narrow if ylim high
#rows = number of rows, # validations

### set font expansion factor (as in forest() above) and use a bold font
#op <- par(cex=1.1, font=2)

metafor::forest(y$theta, ci.lb=y$theta.cilb,ci.ub=y$theta.ciub,
                slab = y$id, 
                cex=1.1,
                xlab = "O:E ratio", pch = 15,
                xlim=(c(-0.3,pos_CI-.33)), alim=(c(0.2,1.8)), at=c(0.2,.4,.6,.8,1,1.2,1.4,1.6,1.8),
                refline = 1, 
                order=y$S3, # CHANGE subgroup variable
                rows=c(24:15,10:3), # CHANGE studies within groups
                ylim=(c(-5,nrow(y)+10)),
                ilab=cbind( y$events,  y$n),
                ilab.xpos=c(pos_event, pos_N),
                ilab.pos=c(2,2),
                main=titplot
)

### switch to bold italic font
par(font=4)

### add text for the subgroups (CHANGE)
text(-0.3, c(25,11), pos=4,  c("Estimates based on intercept of calibration curve",
                                 "OE reported and variability estimated"))

### set par back to the original settings
par(op)

## summary polygon for overall
addpoly(x = resM_cal_ACM_1yr_all$est, ci.lb=resM_cal_ACM_1yr_all$ci.lb, ci.ub=resM_cal_ACM_1yr_all$ci.ub,
        row=-2, mlab=mlabfun("Overall", res.all))

### add summary polygons for the three subgroups (CHANGE res variables and rows for the polygon)
addpoly(x = resM_cal_ACM_1yr_oeestfrominterc$est, ci.lb=resM_cal_ACM_1yr_oeestfrominterc$ci.lb, ci.ub=resM_cal_ACM_1yr_oeestfrominterc$ci.ub,
        row=13.5, mlab=mlabfun("RE Model for Subgroup", res.hrob))
addpoly(x = resM_cal_ACM_1yr_oerep$est, ci.lb=resM_cal_ACM_1yr_oerep$ci.lb, ci.ub=resM_cal_ACM_1yr_oerep$ci.ub,
        row= 1.5, mlab=mlabfun("RE Model for Subgroup", res.oerep))

### fit meta-regression model to test for subgroup differences (CHANGE)
res <- rma(yi=theta, sei=theta.se, mods = ~ S3, data=oe_all_log, method="REML", test="knha")

### add text for the test of subgroup differences
par(op)
text(-0.3, -3.5, pos=4, bquote(paste("Test for Subgroup Differences: ",
                                     Q[M], " = ", .(fmtx(res$QM, digits=2)),
                                     ", df = ", .(res$p - 1), ", ",
                                     .(fmtp(res$QMp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)))))

par(cex=1.1, font=2)
text(c(-0.3, pos_event, pos_N, pos_CI-.33), nrow(y)+9 , pos = c(4,2,2,2) ,
     c("Studies", "Events", "N", "O/E [95% CI]"))

dev.off()
