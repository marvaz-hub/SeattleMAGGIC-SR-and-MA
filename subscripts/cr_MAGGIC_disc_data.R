## MAGGIC ##
## Data for discrimination 


path1 = "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/SeattleMAGGIC analysis files NEWCOVERCODES/"
path2 = "subscripts/"
path3 = "datasets/"

## Raw data
file = "all4MariaM_merged FINAL.csv"

# read the data into R
maggic <- data.frame(read.csv(paste0(path1, path3, file),sep=","))

# initial working copy
df <- maggic

# table(df$cal)
# -   E9  X 
# 44  24  38 
# table(df$disc)
# -   E9 E11   X 
# 92  4   1   9 

dim(df) # 106 166

# create individual study identifier
a <- sapply(strsplit(as.character(df$author), " "), function(x) x[which.max(nchar(x))])  # longest word in author vector
b <- sapply(strsplit(as.character(df$year), " "), function(x) x[which.max(nchar(x))])  # longest word in author vector

df$study_id <- paste0(a, ",", b, " (", df$outcome, ", ", df$horizon, " months)")

df$authoryear <- paste0(a, ",", b)

# df[df$disc=="E11","authoryear"] #"McGranaghan,2021"

#################################################################################
# Estimating O and E using information in commentS58 and 69 ###
#################################################################################
source(paste0(path1, path2, "fn_oande_M.r"))

df <- fn_oande_M(df)

#################################################################################
# studies to be included in the MA
#################################################################################

inma <- which(df$NotMA==0)
df_ma <- df[inma,] 
dim(df_ma) #97 166
table(df_ma$toestD)
# Checking other data Estimates not required 
# 40                     57 

# studies to be included in MA of discrimination
df_ma <- df_ma[which(df_ma$toestD!="Exclude"),]
dim(df_ma) #97 166


# as.numeric(as.character()) # converts a factor variable into numeric
# subset(df, select="name of the variables") # to list only a subset of the variables in the dataset

#################################################################################
### •	AUC vs c-stat (S4) – Maria to update post-estimation
#################################################################################

table(df_ma$S4,useNA = "always")
  #         AUC c-stat   <NA> 
  # 40     21     36      0  


# length(which(!is.na(df_ma$extva_dis_cstat_val) & 
#        ((!is.na(df_ma$extva_dis_cstat_lci) & !is.na(df_ma$extva_dis_cstat_uci)) | !is.na(df_ma$extva_dis_cstat_se))
#             )
#       )      
# 
# length(which(df_ma$S4=="c-stat" & (is.na(df_ma$extva_dis_cstat_val) | 
#                !((!is.na(df_ma$extva_dis_cstat_lci) & !is.na(df_ma$extva_dis_cstat_uci)) | !is.na(df_ma$extva_dis_cstat_se)))
# )
# )    # Bo 2023  has cstat and auc, with ci for auc only
# df_ma[9,c("author","year","extva_dis_auc_val","extva_dis_auc_lci","extva_dis_auc_uci")]
# 
# 
# length(which(!is.na(df_ma$extva_dis_auc_val) & !is.na(df_ma$extva_dis_auc_lci) & !is.na(df_ma$extva_dis_auc_uci)
# )
# )   

df_ma$S4[!is.na(df_ma$extva_dis_cstat_val)] <- "c-stat"
df_ma$S4[is.na(df_ma$extva_dis_cstat_val) & !is.na(df_ma$extva_dis_auc_val)] <- "AUC"

table(df_ma$S4,useNA = "always")
  #       AUC c-stat   <NA> 
  # 30     26     41      0 


#################################################################################
### Preparation of dataset for c-statistic ###
### combinig cstat and auc
#################################################################################

## for the forest plot, add development study cstat ##
## Pocock did not report discrimination for MAGGIC model. Reported calibration ##
## (See calibration extraction files Fri 16Feb24) ##

## those without c-stat or auc have to be excluded
obsc <- which(!is.na(df_ma$extva_dis_cstat_val) | !is.na(df_ma$extva_dis_auc_val))
dim(df_ma) #97 166
length(obsc) #97-30 = 67

## new ma_c data
df_ma <- df_ma[obsc,]
dim(df_ma) #67 174

## data with c-stat replaced with auc when c-stat not available but auc is
rownum <- which(is.na(df_ma$extva_dis_cstat_val) & !is.na(df_ma$extva_dis_auc_val))

####### This df_ma will have cstat and auc in same column ###### 

df_ma$extva_dis_cstat_val[rownum] <- df_ma$extva_dis_auc_val[rownum]
df_ma$extva_dis_cstat_lci[rownum] <- df_ma$extva_dis_auc_lci[rownum]
df_ma$extva_dis_cstat_uci[rownum] <- df_ma$extva_dis_auc_uci[rownum]


# ####### to select subset of studies with data for MAs #######
# msubs_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
#                    cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
#                    N = sample_n_model, O = o.est, data=df_ma, slab=study_id)
# 
# msubs_all$n      <- df_ma$sample_n_model
# msubs_all$events <- round(df_ma$o.est,0)
# msubs_all$id     <- df_ma$study_id
# msubs_all <- msubs_all[which(!is.na(msubs_all$theta.se)),]
# dim(msubs_all) ## 64
# plot(msubs_all)

# ### These studies rounded up reported estimates such that the intervals look skewed
# ### Use reported CI to estimate SE (=width of CI/4) and remove CI information
# df_ma[which(df_ma$author=="Ahmad"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]
# df_ma[which(df_ma$author=="Michaels_1"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]
# df_ma[which(df_ma$author=="Szlacheta"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]

ul <- df_ma$extva_dis_cstat_uci[df_ma$author=="Ahmad"]
ll <- df_ma$extva_dis_cstat_lci[df_ma$author=="Ahmad"]
df_ma$extva_dis_cstat_se[df_ma$author=="Ahmad"] <- (ul-ll)/4
df_ma$extva_dis_cstat_uci[df_ma$author=="Ahmad"] <- NA
df_ma$extva_dis_cstat_lci[df_ma$author=="Ahmad"] <- NA
df_ma[which(df_ma$author=="Ahmad"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]


ul <- df_ma$extva_dis_cstat_uci[df_ma$author=="Michaels_1"]
ll <- df_ma$extva_dis_cstat_lci[df_ma$author=="Michaels_1"]
df_ma$extva_dis_cstat_se[df_ma$author=="Michaels_1"] <- (ul-ll)/4
df_ma$extva_dis_cstat_uci[df_ma$author=="Michaels_1"] <- NA
df_ma$extva_dis_cstat_lci[df_ma$author=="Michaels_1"] <- NA
df_ma[which(df_ma$author=="Michaels_1"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]

ul <- df_ma$extva_dis_cstat_uci[df_ma$author=="Szlacheta"]
ll <- df_ma$extva_dis_cstat_lci[df_ma$author=="Szlacheta"]
df_ma$extva_dis_cstat_se[df_ma$author=="Szlacheta"] <- (ul-ll)/4
df_ma$extva_dis_cstat_uci[df_ma$author=="Szlacheta"] <- NA
df_ma$extva_dis_cstat_lci[df_ma$author=="Szlacheta"] <- NA
df_ma[which(df_ma$author=="Szlacheta"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]


####### REPEAT AFTER PREVIOUS CHANGE                  #######
####### IF SATISFIED, ADD COVER VARIABLES             #######
####### to select subset of studies with data for MAs #######
msubs_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O = o.est, data=df_ma, slab=study_id)

msubs_all$lineno    <- df_ma$lineno
msubs_all$n         <- df_ma$sample_n_model
msubs_all$events    <- round(df_ma$o.est,0)
msubs_all$id        <- df_ma$study_id
msubs_all$author    <- df_ma$author
msubs_all$year      <- df_ma$year
msubs_all$data_name <- df_ma$data_name
msubs_all$horizon   <- df_ma$horizon
msubs_all$outcome   <- df_ma$outcome
msubs_all$popul     <- df_ma$popul
msubs_all$casemix   <- df_ma$casemix
msubs_all$indep     <- df_ma$indep
msubs_all$outc      <- df_ma$outc
msubs_all$horiz     <- df_ma$horiz
msubs_all$MAoutcome <- df_ma$MAoutcome
msubs_all$MAhorizon <- df_ma$MAhorizon
msubs_all$disc      <- df_ma$disc # CHANGE created in cr_disc files
#msubs_all$cal       <- df_ma$cal # CHANGE created in cr_calib files
msubs_all$model_f   <- df_ma$model_f
msubs_all$model_p   <- df_ma$model_p
msubs_all$OS1        <- df_ma$OS1
msubs_all$S1        <- df_ma$S1
msubs_all$S2        <- df_ma$S2
#msubs_all$S3        <- df_ma$S3 #Calibration estimate O:E reported and estimated variability vs estimates based on intercept of calibration curve (S3) # CHANGE created in cr_calib files
msubs_all$S4        <- df_ma$S4 #AUC vs c-stat (S4) # CHANGE created in cr_disc files
msubs_all$S5        <- df_ma$S5 #model version
msubs_all$extva_dis_cstat_val <- df_ma$extva_dis_cstat_val
msubs_all$extva_dis_cstat_se  <- df_ma$extva_dis_cstat_se
msubs_all$extva_dis_cstat_lci <- df_ma$extva_dis_cstat_lci
msubs_all$extva_dis_cstat_uci <- df_ma$extva_dis_cstat_uci
msubs_all$toestD     <-df_ma$toestD

##### update toestD ####
msubs_all$toestD[!is.na(msubs_all$extva_dis_cstat_val)& !is.na(msubs_all$extva_dis_cstat_lci)&!is.na(msubs_all$extva_dis_cstat_uci)]
msubs_all$toestDup[!is.na(msubs_all$extva_dis_cstat_val)& !is.na(msubs_all$extva_dis_cstat_lci)&!is.na(msubs_all$extva_dis_cstat_uci)] <- "Estimates not required"
msubs_all$toestDup[is.na(msubs_all$toestDup) & !is.na(msubs_all$theta.se)] <- "Estimates are possible"
msubs_all$toestDup[is.na(msubs_all$toestDup) & is.na(msubs_all$theta.se)] <- "Estimates not possible"

msubs_all[,c("toestD","toestDup")]

msubs_all <- msubs_all[which(!is.na(msubs_all$theta.se)),]

dim(msubs_all) ## 64
#plot(msubs_all)


#######  Sensitivity: changes to disc ####### 
# Code received:
# E9 =	Observed outcomes post-intervention compared with pre-intervention predictions 
#       and not clinically relevant or evidence of miscalibration or question about data reliability

# CHANGE
table(msubs_all$disc)
# - E11 
# 63   1 
#table(msubs_all$cal)

table(msubs_all$theta.se.source)
# Confidence Interval Newcombe (Method 4)      Standard Error 
#     51                  10                   3 
msubs_all[which(msubs_all$theta.se.source=="Newcombe (Method 4)" | msubs_all$theta.se.source=="Standard Error"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci","disc")]

# If you estimate both logit(c) and Var(logit(c)) - DOES NOT APPLY IN THIS REVIEW; THERE WAS NO DATA FOR ALL PREDICTORS 
# E20	Estimated both logit(c) and Var(logit(c)) 
# replace disc=”E20” if disc !=”E9”
# replace disc=”E17” if disc ==”E9”
# E17	Exclusion in sensitivity analysis for more than one reason 

# If you estimate just the variance
# replace disc=”E10” if disc !=”E9”
# E10	Estimated Var(logit(c)) only 
# E11 Non-standard discrimination measure
msubs_all$disc[which(msubs_all$theta.se.source=="Standard Error" & (msubs_all$disc!="E9" &  msubs_all$disc!="E11"))]<-"E10"
msubs_all$disc[which(msubs_all$theta.se.source=="Standard Error" & (msubs_all$disc=="E9" | msubs_all$disc=="E11"))]<-"E17"


msubs_all$disc[which(msubs_all$theta.se.source=="Newcombe (Method 4)" & (msubs_all$disc!="E9" &  msubs_all$disc!="E11"))]<-"E10"
msubs_all$disc[which(msubs_all$theta.se.source=="Newcombe (Method 4)" & (msubs_all$disc=="E9" | msubs_all$disc=="E11"))]<-"E17"


# If you cannot make estimates
# replace disc=”X”
# X	Exclude from meta-analysis
# NB: In file to send, studies not appearing did not have data for MA

# After you have updated the disc and cal, when you can please send me the updated COVER tables  i.e. 
# author 
# year 
# data_name 
# horizon 
# outcome 
# MAoutcome 
# MAhorizon 
# popul casemix indep outc horiz cal disc model_f model_p 
# OS1 S1 S2 S4 S5
# disc # CHANGE TO cal for calibration file
# 
# mcovertab_disc <- subset(msubs_all, select=c("lineno","author",
# "year",
# "data_name",
# "horizon",
# "outcome",
# "MAoutcome",
# "MAhorizon",
# "popul", "casemix", "indep", "outc", "horiz" ,
# # "cal",  # CHANGE un-comment for calibration file
# "disc", # CHANGE un-comment for discrimination file
# "model_f", "model_p",
# "OS1", "S1", "S2",
# # "S3", # CHANGE un-comment for calibration file
# "S4", # CHANGE un-comment for discrimination file
# "S5",
# "toestDup"))
# 
# # CHANGE NAME OF FINAL FILE
# write.csv(mcovertab_disc, "C:\\Users\\mvazquezmontes\\OneDrive - Nexus365\\Documents\\SHFM and MAGGIC\\Analysis files\\RCode\\results\\MAGGIC_covertable_disc.csv", row.names = FALSE)

df_ma <- df_ma[df_ma$lineno %in% msubs_all$lineno,]
df_ma$disc <- msubs_all$disc # CHANGE to cal for calibration
df_ma$toestDup <- msubs_all$toestDup # CHANGE to cal for calibration
# table(df_ma$disc)
# -  E9 E10 E17 
# 37  14  10   3 

