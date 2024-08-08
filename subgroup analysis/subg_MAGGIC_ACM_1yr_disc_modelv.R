## MAGGIC ##
## cstat and auc together
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
 source(paste0(path1, path2, "cr_MAGGIC_disc_data.R"))

## Function to get pooled estimates for cstat
 source(paste0(path1, path2, "fn_ma_cstat.R"))

## Function to add Q-test, I^2, and tau^2 estimate info
 source(paste0(path1, path2, "fn_forestplot_mlab.r"))
titplot <- "MAGGIC" #" 1-year ACM"
figfile <- "Fig2d_M_1yr_ACMapprox_disc_modelv.jpg"
pathfig <- "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/figures/"


#################################################################################
### SUBGROUP
### •	•	Model version (S5) 
#################################################################################
table(df_ma$S5,useNA = "always")
# Original  Unclear   Update     <NA> 
#   6       18          40        0 



#################################################################################
### create the necessary dataframes for plotting and pooling ###
#################################################################################
# 
# If using all the data
# dat <- df_ma

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") #45

# 2) horizon
dat <- dat[which(dat$horizon==12),]
table(dat$horizon, useNA = "always") #23

table(dat$S5,useNA = "always") # CHANGE subgroup variable
# Original  Unclear   Update     <NA> 
#      3        9     11        0 


# 3) Any split? 

dat1 <- dat[which(dat$S5=="Original"),] 

table(dat1$S5, useNA = "always") #3


dat2 <- dat[which(dat$S5=="Unclear"),] 

table(dat2$S5, useNA = "always") #5


dat3 <- dat[which(dat$S5=="Update"),] 

table(dat3$S5, useNA = "always") #3


################################################################################################
###########     Overall estimate  #############
################################################################################################

# here dat has all studies

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se,
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O = o.est, data=dat, slab=study_id)

# create a dataframe for forest plot
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all$S5     <-dat$S5 #CHANGE subgroup variable
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) ## 23
plot(cstat_all_forest)

y <- cstat_all_forest # to use as the base for the forestplot

# function fn_ma_cstat returns a list with weights,c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2"), and rma object
rma.res <- fn_ma_cstat(dat)
resM_disc_ACM_1yr_all <- rma.res[[2]]
res.all <- rma.res[[3]]

# to use when comparing the subgroups
cstat_all_log <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                       N = sample_n_model, O= o.est, data=dat, slab=study_id, g="log(cstat/(1-cstat))")
cstat_all_log$S5 <- dat$S5 #CHANGE subgroup variable
cstat_all_log <- cstat_all_log[which(!is.na(cstat_all_log$theta.se)),]


################################################################################################
###########     GROUP 1 #############
################################################################################################


## ** UPDATE DATABASE AND NAME OF VARIBLE **

dat <- dat1 

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O = o.est, data=dat, slab=study_id)

# create a dataframe for forest plot
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) ## 23 9
plot(cstat_all_forest)

rma.res <- fn_ma_cstat(dat)
resM_disc_ACM_1yr_origmod <- rma.res[[2]]
res.origmod <- rma.res[[3]]


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
dim(cstat_all_forest) ## 23 9
plot(cstat_all_forest)

rma.res <- fn_ma_cstat(dat)
resM_disc_ACM_1yr_uncmod <- rma.res[[2]]
res.uncmod <- rma.res[[3]]


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
dim(cstat_all_forest) ## 23 9
plot(cstat_all_forest)

rma.res <- fn_ma_cstat(dat)
resM_disc_ACM_1yr_upmod <- rma.res[[2]]
res.upmod <- rma.res[[3]]



#################################################################################
########################### Forests plot ########################################
#################################################################################

 jpeg(paste0(pathfig, figfile), width = 1200, height = 800)

# y has cstat_all_forest with all studies

N <- y$n

temp <- rep(NA,length(N))
for (i in 1:length(N)){
  temp[i] <- nchar(N[i])
}
maxlengthN <- max(temp)

### set up forest plot (the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)
#pos_weight = 1.03    # define position of weights
pos_event  = 1.04 # define position of no. events
pos_div    = 1.09 # define position of divide
pos_N      = pos_div + (maxlengthN)/100 # define position of sample size
pos_CI     = 1.68  # define position of CI and graph size
pos_pred   = -0.5

### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=1.1, font=2)

forest(y$theta, ci.lb=y$theta.ciub, ci.ub=y$theta.cilb,
       slab = y$id,
       #cex=1.1,
       xlab = "C-statistic",
       xlim=(c(-0.3,pos_CI-.33)), alim=(c(0.4,1)), at=c(0.4,0.5,0.6,0.7,0.8,0.9,1), steps = 7,
       refline = 0.5,
       ilab=cbind( y$events,  y$n),
       ilab.xpos=c( pos_event,  pos_N),
       ilab.pos=c(2,2),
       order=y$S5, # CHANGE subgroup variable
       rows=c(33:31,26:18,13:3), # CHANGE studies within groups
       ylim=(c(-5,nrow(y)+14)),
       main=titplot
)


### switch to bold italic font
par(font=4)

### add text for the subgroups (CHANGE)
text(-0.3, c(34,27,14), pos=4, c("Original MAGGIC score",
                                 "Unclear MAGGIC score version",
                                 "Updated MAGGIC score"))

### set par back to the original settings
par(op)

## summary polygon for overall
addpoly(x = resM_disc_ACM_1yr_all$est, ci.lb=resM_disc_ACM_1yr_all$ci.lb, ci.ub=resM_disc_ACM_1yr_all$ci.ub,
        row=-2, mlab=mlabfun("Overall", res.all))

### add summary polygons for the three subgroups (CHANGE res variables and rows for the polygon)
addpoly(x = resM_disc_ACM_1yr_origmod$est, ci.lb=resM_disc_ACM_1yr_origmod$ci.lb, ci.ub=resM_disc_ACM_1yr_origmod$ci.ub,
        row=29.5, mlab=mlabfun("RE Model for Subgroup", res.acute))
addpoly(x = resM_disc_ACM_1yr_uncmod$est, ci.lb=resM_disc_ACM_1yr_uncmod$ci.lb, ci.ub=resM_disc_ACM_1yr_uncmod$ci.ub,
        row= 16.5, mlab=mlabfun("RE Model for Subgroup", res.chronic))
addpoly(x = resM_disc_ACM_1yr_upmod$est, ci.lb=resM_disc_ACM_1yr_upmod$ci.lb, ci.ub=resM_disc_ACM_1yr_upmod$ci.ub,
        row= 1.5, mlab=mlabfun("RE Model for Subgroup", res.uncmixhf))

### fit meta-regression model to test for subgroup differences (CHANGE)
res <- rma(yi=theta, sei=theta.se, mods = ~ S5, data=cstat_all_log, method="REML", test="knha")

par(op)
### add text for the test of subgroup differences
text(-0.3, -3.5, pos=4, bquote(paste("Test for Subgroup Differences: ",
                                     Q[M], " = ", .(fmtx(res$QM, digits=2)),
                                     ", df = ", .(res$p - 1), ", ",
                                     .(fmtp(res$QMp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)))))

par(cex=1.1, font=2)
text(c(-0.3, pos_event, pos_N, pos_CI-.33), nrow(y)+13 , pos = c(4,2,2,2) ,
     c("Studies", "Events", "N", "C-statistic [95% CI]"))

dev.off()
