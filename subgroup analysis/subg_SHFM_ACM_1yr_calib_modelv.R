## SHFM ##
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

path1 = # INSERT PATHWAY TO DIRECTORY CONTAINING THE SUBFOLDERS
path2 = "subscripts/"

## Data for discrimination 
 source(paste0(path1, path2, "cr_SHFM_calib_data.R"))

## Function to get pooled estimates for oe
 source(paste0(path1, path2, "fn_ma_oe.R"))

## Function to add Q-test, I^2, and tau^2 estimate info
 source(paste0(path1, path2, "fn_forestplot_mlab.r"))
titplot <- "SHFM" #" 1-year ACM"
figfile <- "Fig3h_S_1yr_ACMapprox_cal_modelv.jpg"
pathfig <- # INSERT THE PATH WHERE YOU WANT TO SAVE THE FIGURES



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
table(dat$outcome, useNA = "always") #40

# 2) horizon
dat <- dat[which(dat$horizon==12),]
table(dat$horizon, useNA = "always") #28

table(dat$S5,useNA = "always") # CHANGE subgroup variable
# Added LVAD Levy 2006 ext validation         Levy 2006 PRAISE                   Update                     <NA> 
#   2                       16                        5                        5                        0 

dat <- dat[which(dat$S5!="Added LVAD"),]

# 3) Any split? 

dat1 <- dat[which(dat$S5=="Levy 2006 ext validation"),] 
table(dat1$S5, useNA = "always") #16


dat2 <- dat[which(dat$S5=="Levy 2006 PRAISE"),]
table(dat2$S5, useNA = "always") #5


dat3 <- dat[which(dat$S5=="Update"),]
table(dat3$S5, useNA = "always") #5

################################################################################################
###########     Overall estimate  #############
################################################################################################

# here dat has all studies
dim(dat)

oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                 citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                 data=dat, slab = study_id  )

# create a dataframe for forest plot
oe_all$n      <- dat$sample_n_model
oe_all$events <- round(dat$o.est,0)
oe_all$id     <- dat$authoryear
oe_all$S5     <-dat$S5 #CHANGE subgroup variable
oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
dim(oe_all_forest) ## 33
plot(oe_all_forest)

y <- oe_all_forest # to use as the base for the forestplot

# function fn_ma_oe returns a list with weights,c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2"), and rma object
rma.res <- fn_ma_oe(dat)
resS_cal_ACM_1yr_26 <- rma.res[[2]]
res_26 <- rma.res[[3]]

# to use when comparing the subgroups
oe_all_log <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                     citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                     data=dat, slab = study_id, g="log(OE)")
oe_all_log$S5 <- dat$S5 #CHANGE subgroup variable
oe_all_log <- oe_all_log[which(!is.na(oe_all_log$theta.se)),]
dim(oe_all_log)

################################################################################################
###########     GROUP 1    #############
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
resS_cal_ACM_1yr_Levy2006extval <- rma.res[[2]]
res.Levy2006extval<- rma.res[[3]]

################################################################################################
###########     GROUP 2    #############
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
dim(oe_all_forest) ## 5
plot(oe_all_forest)

rma.res <- fn_ma_oe(dat)
resS_cal_ACM_1yr_Levy2006PRAISE <- rma.res[[2]]
res.Levy2006PRAISE <- rma.res[[3]]

################################################################################################
###########     GROUP 3    #############
################################################################################################

## ** UPDATE DATABASE AND NAME OF VARIBLE **

dat <- dat3

oe_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                 citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                 data=dat, slab = study_id  )

# create a dataframe for forest plot
oe_all$n      <- dat$sample_n_model
oe_all$events <- round(dat$o.est,0)
oe_all$id     <- dat$authoryear
oe_all_forest <- oe_all[which(!is.na(oe_all$theta.se)),]
dim(oe_all_forest) ## 5
plot(oe_all_forest)


rma.res <- fn_ma_oe(dat)
resS_cal_ACM_1yr_updatemod <- rma.res[[2]]
res.updatemod <- rma.res[[3]]

#################################################################################
########################### Forests plot ########################################
#################################################################################

jpeg(paste0(pathfig, figfile), width = 1200, height = 900)

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
                order=y$S5, # CHANGE subgroup variable
                rows=c(36:21,16:12,7:3), # CHANGE studies within groups
                ylim=(c(-5,nrow(y)+14)),
                ilab=cbind( y$events,  y$n),
                ilab.xpos=c(pos_event, pos_N),
                ilab.pos=c(2,2),
                main=titplot
)


### switch to bold italic font
par(font=4)

### add text for the subgroups (CHANGE)
text(-0.3, c(37,17,8), pos=4, c("Levy 2006 ext validation",
                                 "Levy 2006 PRAISE",
                                 "Update"))

### set par back to the original settings
par(op)

## summary polygon for overall
addpoly(x = resS_cal_ACM_1yr_26$est, ci.lb=resS_cal_ACM_1yr_26$ci.lb, ci.ub=resS_cal_ACM_1yr_26$ci.ub,
        row=-2, mlab=mlabfun("Overall", res_26))

### add summary polygons for the three subgroups (CHANGE res variables and rows for the polygon)
addpoly(x =resS_cal_ACM_1yr_Levy2006extval$est, ci.lb=resS_cal_ACM_1yr_Levy2006extval$ci.lb, ci.ub=resS_cal_ACM_1yr_Levy2006extval$ci.ub,
        row=19.5, mlab=mlabfun("RE Model for Subgroup", res.Levy2006extval))
addpoly(x =resS_cal_ACM_1yr_Levy2006PRAISE$est, ci.lb=resS_cal_ACM_1yr_Levy2006PRAISE$ci.lb, ci.ub=resS_cal_ACM_1yr_Levy2006PRAISE$ci.ub,
        row= 10.5, mlab=mlabfun("RE Model for Subgroup", res.Levy2006PRAISE))
addpoly(x =resS_cal_ACM_1yr_updatemod$est, ci.lb=resS_cal_ACM_1yr_updatemod$ci.lb, ci.ub=resS_cal_ACM_1yr_updatemod$ci.ub,
        row= 1.5, mlab=mlabfun("RE Model for Subgroup", res.updatemod))



### fit meta-regression model to test for subgroup differences (CHANGE)
res <- rma(yi=theta, sei=theta.se, mods = ~ S5, data=oe_all_log, method="REML", test="knha")

### add text for the test of subgroup differences
par(op)
text(-0.3, -3.5, pos=4, bquote(paste("Test for Subgroup Differences: ",
                                     Q[M], " = ", .(fmtx(res$QM, digits=2)),
                                     ", df = ", .(res$p - 1), ", ",
                                     .(fmtp(res$QMp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)))))

par(cex=1.1, font=2)
text(c(-0.3, pos_event, pos_N, pos_CI-.33), nrow(y)+13 , pos = c(4,2,2,2) ,
     c("Studies", "Events", "N", "O/E [95% CI]"))

dev.off()
