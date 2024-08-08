## SHFM ##
## Forest plot and MA for cstat and auc together
## Estimates for ACM and approx ACM (outc==”E6”), horizon 12m (most frequent horizon)

## SECONDARY ANALYSES: 2YR, 3YR, 5YR (GT3YR out because leads to including duplicated populations)

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


path1 = # INSERT PATHWAY TO DIRECTORY CONTAINING THE SUBFOLDERS
path2 = "subscripts/"


#################################################################################
### 2 YR ###
#################################################################################

## Data for discrimination 
source(paste0(path1, path2, "cr_SHFM_disc_data.R"))

## Function to get pooled estimates for oe
source(paste0(path1, path2, "fn_ma_cstat.R"))

## Functions to plot forest plot
source(paste0(path1, path2, "fn_forestplot_mlab.r"))
source(paste0(path1, path2, "fn_forestplot_disc.R"))
titplot <- "SHFM" #"SHFM 2-year ACM"
figfile <- "Fig4c_S_2yr_ACMapprox_disc.jpg"
pathfig <- # INSERT THE PATH WHERE YOU WANT TO SAVE THE FIGURES


### create the necessary dataframes for plotting and pooling ###

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") 
  # 1    5    9 <NA> 
  # 57    4    1    0 

# 2) horizon
dat <- dat[which(dat$horizon==24),]
table(dat$horizon, useNA = "always") #5


# create dataframe/list with all cstats and recalculated 95%-CIs
cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                           N = sample_n_model, O= o.est, data=dat, slab=study_id)

# create a dataframe for forest plot
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) 
plot(cstat_all_forest)


# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
resS_disc_ACM_2yr_all <- round(fn_ma_cstat(dat)[[2]],2)


### Forests OS

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
### 3 YR ###
#################################################################################

## Data for discrimination 
source(paste0(path1, path2, "cr_SHFM_disc_data.R"))

## Function to get pooled estimates for oe
source(paste0(path1, path2, "fn_ma_cstat.R"))

## Functions to plot forest plot
source(paste0(path1, path2, "fn_forestplot_mlab.r"))
source(paste0(path1, path2, "fn_forestplot_disc.R"))
titplot <- "SHFM" #"SHFM 3-year ACM"
figfile <- "Fig5c_S_3yr_ACMapprox_disc.jpg"
pathfig <- # INSERT THE PATH WHERE YOU WANT TO SAVE THE FIGURES


### create the necessary dataframes for plotting and pooling ###

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") 
# 1    5    9 <NA> 
# 57   4    1    0 

# 2) horizon
dat <- dat[which(dat$horizon==36),]
table(dat$horizon, useNA = "always") #8


# create dataframe/list with all cstats and recalculated 95%-CIs
cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O= o.est, data=dat, slab=study_id)

# create a dataframe for forest plot
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest)
plot(cstat_all_forest)


# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
resS_disc_ACM_3yr_all <- round(fn_ma_cstat(dat)[[2]],2)


### Forests OS

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
### GT 3 YR ###
#################################################################################

## Data for discrimination 
source(paste0(path1, path2, "cr_SHFM_disc_data.R"))

## Function to get pooled estimates for oe
source(paste0(path1, path2, "fn_ma_cstat.R"))

## Functions to plot forest plot
source(paste0(path1, path2, "fn_forestplot_mlab.r"))
source(paste0(path1, path2, "fn_forestplot_disc.R"))
titplot <- "SHFM" #"SHFM GT3-year ACM"
figfile <- "Fig6c_S_GT3yr_ACMapprox_disc.jpg"
pathfig <- # INSERT THE PATH WHERE YOU WANT TO SAVE THE FIGURES


### create the necessary dataframes for plotting and pooling ###

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") 
# 1    5    9 <NA> 
# 57   4    1    0 

# 2) horizon
dat <- dat[which((trunc(dat$horizon)==dat$horizon) & (dat$horizon>36)),]
table(dat$horizon, useNA = "always") #9


# create dataframe/list with all cstats and recalculated 95%-CIs

# create individual study identifier
a <- sapply(strsplit(as.character(dat$author), " "), function(x) x[which.max(nchar(x))])  # longest word in author vector
b <- sapply(strsplit(as.character(dat$year), " "), function(x) x[which.max(nchar(x))])  # longest word in author vector

dat$study_id_GT3yr <- paste0(a, ",", b, " (Horizon ", dat$horizon, " months)")

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O= o.est, data=dat, slab=study_id_GT3yr)

# create a dataframe for forest plot
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$study_id_GT3yr
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) 
plot(cstat_all_forest)


# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
resS_disc_ACM_GT3yr_all <- round(fn_ma_cstat(dat)[[2]],2)


### Forests OS

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
### 5 YR (GT 3 YRS led to duplicated population included) ###
#################################################################################

## Data for discrimination 
source(paste0(path1, path2, "cr_SHFM_disc_data.R"))

## Function to get pooled estimates for oe
source(paste0(path1, path2, "fn_ma_cstat.R"))

## Functions to plot forest plot
source(paste0(path1, path2, "fn_forestplot_mlab.r"))
source(paste0(path1, path2, "fn_forestplot_disc.R"))
titplot <- "SHFM" #"SHFM 5-year ACM"
figfile <- "Fig7c_S_5yr_ACMapprox_disc.jpg"
pathfig <- # INSERT THE PATH WHERE YOU WANT TO SAVE THE FIGURES


### create the necessary dataframes for plotting and pooling ###

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") 
# 1    5    9 <NA> 
# 57    4    1    0 

# 2) horizon
dat <- dat[which(dat$horizon==60),]
table(dat$horizon, useNA = "always") #6


# create dataframe/list with all cstats and recalculated 95%-CIs

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O= o.est, data=dat, slab=study_id)

# create a dataframe for forest plot
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) 
plot(cstat_all_forest)


# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
resS_disc_ACM_5yr_all <- round(fn_ma_cstat(dat)[[2]],2)


### Forests OS

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
### 4 YR for reporting in text 
#################################################################################

## Data for discrimination 
source(paste0(path1, path2, "cr_SHFM_disc_data.R"))

## Function to get pooled estimates for oe
source(paste0(path1, path2, "fn_ma_cstat.R"))

## Functions to plot forest plot
source(paste0(path1, path2, "fn_forestplot_mlab.r"))
source(paste0(path1, path2, "fn_forestplot_disc.R"))
titplot <- "SHFM" #"SHFM 5-year ACM"
figfile <- "Fig7c_S_5yr_ACMapprox_disc.jpg"
pathfig <- # INSERT THE PATH WHERE YOU WANT TO SAVE THE FIGURES


### create the necessary dataframes for plotting and pooling ###

# 1) choose outcome ACM and approx ACM (outc==”E6”)
dat <- df_ma[which(df_ma$outcome==1 | (df_ma$outc=="E6" & df_ma$outcome!=1)),]
table(dat$outcome, useNA = "always") 
# 1    5    9 <NA> 
# 57    3    1    0 

# 2) horizon
dat <- dat[which(dat$horizon==48),]
table(dat$horizon, useNA = "always") #3


# create dataframe/list with all cstats and recalculated 95%-CIs

cstat_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O= o.est, data=dat, slab=study_id)

# create a dataframe for forest plot
cstat_all$n      <- dat$sample_n_model
cstat_all$events <- round(dat$o.est,0)
cstat_all$id     <- dat$authoryear
cstat_all_forest <- cstat_all[which(!is.na(cstat_all$theta.se)),]
dim(cstat_all_forest) 
plot(cstat_all_forest)


# funtion that returns a df with c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")
resS_disc_ACM_4yr_all <- round(fn_ma_cstat(dat)[[2]],2)


### Forests

# small preparations
weights_all <- sprintf("%.1f",fn_ma_cstat(dat)[[1]])             # extract weights from REML fit
weights_all <- c(weights_all)                               # add 0.0 weight for development study in plot
tau_all     <- paste0( "(Tau2=", round(fn_ma_cstat(dat)[[2]]$tau2, 2), ")")   # extract tau from REML fit
estimates <- fn_ma_cstat(dat)[[2]]
res <- fn_ma_cstat(dat)[[3]]

fn_forest_disc_5yS <- function(df_forest,fit1,titplot,weights_all,tau_all,rmaobject){
  
  df <- df_forest
  res <- rmaobject
  
  N <- df$n
  
  temp <- rep(NA,length(N))
  for (i in 1:length(N)){
    temp[i] <- nchar(N[i])
  }
  maxlengthN <- max(temp)
  
  pos_weight = 1.03    # define position of weights
  pos_event  = 1.10 # define position of no. events
  pos_div    = 1.11 # define position of divide
  pos_N      = pos_div + (maxlengthN)/100 # define position of sample size
  pos_CI     = 1.65  # define position of CI and graph size
  pos_pred   = -0.5
  #par(cex=0.8, font=1)
  # y lim - space for rows, rows are more narrow if ylim high
  # rows = number of rows, # validations
  
  forest(df$theta, ci.lb=df$theta.ciub, ci.ub=df$theta.cilb,
         slab = df$id,
         cex = 1.2,
         xlab = "C-statistic", 
         rows=c((nrow(df)+2):3), # CHANGE studies within groups
         ylim=(c(-3,nrow(df)+5)),
         #ylim=(c(-1,nrow(df)+3)),
         xlim=(c(0,pos_CI-.33)), alim=(c(0.4,1)), at=c(0.4,0.5,0.6,0.7,0.8,0.9,1), steps = 7, 
         #rows=c(nrow(df):1),
         refline = 0.5, 
         ilab=cbind(weights_all, df$events, rep("/", length(df$n)), df$n),
         ilab.xpos=c(pos_weight, pos_event, pos_div, pos_N),
         ilab.pos=c(2,2,2,2),
         main=titplot
  )
  #xpos is trying out
  
  # #Add pooled estimate
  par(cex = 0.75, font=2)
  addpoly(x = fit1$est, ci.lb=fit1$ci.lb, ci.ub=fit1$ci.ub, rows=1, mlab=mlabfun("Overall", res))
  # par(cex = 1.2, font=2)
  # text(0.2,0, "Overall", pos=4)
  # par(cex = 1.2.2, font=1)
  # text(0.3,0, tau_all, pos=4)
  
  #Add prediction interval
  #par(cex = 1.2, font=1)
  text(0,pos_pred-0.5, cex = 1.2, font=1, pos=4, "Estimated prediction interval")
  
  segments(x0 = fit1$pi.lb, y0 = pos_pred-0.5, x1 = fit1$pi.ub, y1=pos_pred-0.5)
  segments(x0 = fit1$pi.lb, y0 = pos_pred-0.8, x1 = fit1$pi.lb, y1=pos_pred-0.2)
  segments(x0 = fit1$pi.ub, y0 = pos_pred-0.8, x1 = fit1$pi.ub, y1=pos_pred-0.2)
  
  par(cex = 1.2, font=1)
  text(pos_CI-.33, pos_pred-0.5, pos=2, paste0("[", sprintf("%.2f",round(fit1$pi.lb,2)), ", ", sprintf("%.2f",round(fit1$pi.ub, 2)), "]"))
  
  par(cex = 1.2, font=2)
  text(c(0, pos_weight, pos_event+.005, pos_div, pos_N-0.03, pos_CI-.33), nrow(df)+4 , pos = c(4,2,2,2,2,2) ,
       c("Studies", "% Weight", "Events", "/", "N", "C-statistic [95% CI]"))
  # text(.6,nrow(df)+5.5, pos = 4,titplot,cex = 2)
}  

jpeg(paste0(pathfig, figfile), width = 1200, height = 800)
fn_forest_disc_5yS(cstat_all_forest,estimates,titplot,weights_all,tau_all,res)
dev.off()
