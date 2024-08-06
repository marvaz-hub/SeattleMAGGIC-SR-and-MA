## SHFM ##
## Data for discrimination 


path1 = "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/SeattleMAGGIC analysis files NEWCOVERCODES/"
path2 = "subscripts/"
path3 = "datasets/"

# Raw data
file = "all4MariaS_merged FINAL.csv"

# read the data into R
shfm <- data.frame(read.csv(paste0(path1, path3, file),sep=","))

# initial working copy
df <- shfm

# table(df$cal)
# - E9   X 
# 64  62  38 
# table(df$disc)
# -   E9 E11   X 
# 140  9   2   13 

dim(df) # 164 213

# create individual study identifier
a <- sapply(strsplit(as.character(df$author), " "), function(x) x[which.max(nchar(x))])  # longest word in author vector
b <- sapply(strsplit(as.character(df$year), " "), function(x) x[which.max(nchar(x))])  # longest word in author vector

df$study_id <- paste0(a, ",", b, " (", df$outcome, ", ", df$horizon, " months)")

df$authoryear <- paste0(a, ",", b)


#################################################################################
# Estimating O and E using information in comments68 and 69 ###
#################################################################################
source(paste0(path1, path2, "fn_oande_S.r"))

df <- fn_oande_S(df)

#################################################################################
# studies to be included in the MA
#################################################################################

inma <- which(df$NotMA==0)
df_ma <- df[inma,] 
dim(df_ma) #152 220
table(df_ma$toestD)
# Checking other data Estimates not required     Exclude 
# 78                     73                      1 

# studies to be included in MA of discrimination
df_ma <- df_ma[which(df_ma$toestD!="Exclude"),]
dim(df_ma) #151 220


# as.numeric(as.character()) # converts a factor variable into numeric
# subset(df, select="name of the variables") # to list only a subset of the variables in the dataset

#################################################################################
### •	AUC vs c-stat (S4) – Maria to update post-estimation
#################################################################################

table(df_ma$S4,useNA = "always")
  #       AUC c-stat   <NA> 
  # 78     47     26      0 
df_ma$S4[!is.na(df_ma$extva_dis_cstat_val)] <- "c-stat"
df_ma$S4[is.na(df_ma$extva_dis_cstat_val) & !is.na(df_ma$extva_dis_auc_val)] <- "AUC"

table(df_ma$S4,useNA = "always")
  #       AUC c-stat   <NA> 
  # 42     64     45      0 

#################################################################################
### Preparation of dataset for c-statistic ###
### combinig cstat and auc
#################################################################################

## those without c-stat or auc have to be excluded
obsc <- which(!is.na(df_ma$extva_dis_cstat_val) | !is.na(df_ma$extva_dis_auc_val))

## new ma_c data
df_ma <- df_ma[obsc,]
dim(df_ma) # 109 220


## ma_c data with c-stat replaced with auc when c-stat not available but auc does
rownum <- which(is.na(df_ma$extva_dis_cstat_val) & !is.na(df_ma$extva_dis_auc_val))

####### This df_ma will have cstat and auc in same column ###### 

df_ma$extva_dis_cstat_val[rownum] <- df_ma$extva_dis_auc_val[rownum]
df_ma$extva_dis_cstat_lci[rownum] <- df_ma$extva_dis_auc_lci[rownum]
df_ma$extva_dis_cstat_uci[rownum] <- df_ma$extva_dis_auc_uci[rownum]

### These studies rounded up reported estimates so that the CIs look skewed
### replacing the auc/cstat value 
# df_ma[which(df_ma$author=="Blum"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]
# 0.60+(0.71-0.60)/2
# df_ma[which(df_ma$author=="Miyagawa"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]
# 0.521+(0.777-0.521)/2
# df_ma[which(df_ma$author=="Levy_2" & df_ma$year==2006),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]
# 0.68+(0.72-0.68)/2
# 
# 

# ####### to select subset of studies with data for MAs #######
# ssubs_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se,
#                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
#                   N = sample_n_model, O = o.est, data=df_ma, slab=study_id)
# ssubs_all$n      <- df_ma$sample_n_model
# ssubs_all$events <- round(df_ma$o.est,0)
# ssubs_all$id     <- df_ma$study_id
# ssubs_all <- ssubs_all[which(!is.na(ssubs_all$theta.se)),]
# dim(ssubs_all) ## 95
# plot(ssubs_all)
# 
# ### These studies rounded up reported estimates such that the intervals look skewed
# ### Use reported CI to estimate SE (=width of CI/4) and remove CI information
# #Blum 2022
# #Dardas 2015
# #Levy_2 2006
# #Laszczynska 2014
# #May 2007
# #Rich 2018
# #Ulloa 2021
# #Vishram-Nielsen 2020
# 
# # df_ma[which(df_ma$author=="Blum"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]
# # df_ma[which(df_ma$author=="Dardas"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]
# # df_ma[which(df_ma$author=="Levy_2"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]
# # df_ma[which(df_ma$author=="Laszczynska"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]
# # df_ma[which(df_ma$author=="May"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]
# # df_ma[which(df_ma$author=="Rich"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]
# # df_ma[which(df_ma$author=="Ulloa Cerna"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]
# # df_ma[which(df_ma$author=="Vishram-Nielsen"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]

fn_seest<- function(df,author){
  ul <- df$extva_dis_cstat_uci[df$author==author]
  ll <- df$extva_dis_cstat_lci[df$author==author]
  df$extva_dis_cstat_se[df$author==author] <- (ul-ll)/4
  df$extva_dis_cstat_uci[df$author==author] <- NA
  df$extva_dis_cstat_lci[df$author==author] <- NA
  df[df$author==author,c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci")]
}

df_ma[which(df_ma$author=="Blum"),
      c("extva_dis_cstat_val","extva_dis_cstat_se",
        "extva_dis_cstat_lci","extva_dis_cstat_uci")] <- fn_seest(df_ma,author = "Blum")

df_ma[which(df_ma$author=="Dardas"),
      c("extva_dis_cstat_val","extva_dis_cstat_se",
        "extva_dis_cstat_lci","extva_dis_cstat_uci")] <- fn_seest(df_ma,author = "Dardas")

df_ma[which(df_ma$author=="Levy_2"),
      c("extva_dis_cstat_val","extva_dis_cstat_se",
        "extva_dis_cstat_lci","extva_dis_cstat_uci")] <- fn_seest(df_ma,author = "Levy_2")

df_ma[which(df_ma$author=="Laszczynska"),
      c("extva_dis_cstat_val","extva_dis_cstat_se",
        "extva_dis_cstat_lci","extva_dis_cstat_uci")] <- fn_seest(df_ma,author = "Laszczynska")

df_ma[which(df_ma$author=="May"),
      c("extva_dis_cstat_val","extva_dis_cstat_se",
        "extva_dis_cstat_lci","extva_dis_cstat_uci")] <- fn_seest(df_ma,author = "May")

df_ma[which(df_ma$author=="Rich"),
      c("extva_dis_cstat_val","extva_dis_cstat_se",
        "extva_dis_cstat_lci","extva_dis_cstat_uci")] <- fn_seest(df_ma,author = "Rich")

df_ma[which(df_ma$author=="Ulloa Cerna"),
      c("extva_dis_cstat_val","extva_dis_cstat_se",
        "extva_dis_cstat_lci","extva_dis_cstat_uci")] <- fn_seest(df_ma,author = "Ulloa Cerna")

df_ma[which(df_ma$author=="Vishram-Nielsen"),
      c("extva_dis_cstat_val","extva_dis_cstat_se",
        "extva_dis_cstat_lci","extva_dis_cstat_uci")] <- fn_seest(df_ma,author = "Vishram-Nielsen")


####### REPEAT AFTER PREVIOUS CHANGE                  #######
####### IF SATISFIED, ADD COVER VARIABLES             #######
####### to select subset of studies with data for MAs #######
ssubs_all <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, 
                   cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                   N = sample_n_model, O = o.est, data=df_ma, slab=study_id)

ssubs_all$lineno    <- df_ma$lineno
ssubs_all$n         <- df_ma$sample_n_model
ssubs_all$events    <- round(df_ma$o.est,0)
ssubs_all$id        <- df_ma$study_id
ssubs_all$author    <- df_ma$author
ssubs_all$year      <- df_ma$year
ssubs_all$data_name <- df_ma$data_name
ssubs_all$horizon   <- df_ma$horizon
ssubs_all$outcome   <- df_ma$outcome
ssubs_all$popul     <- df_ma$popul
ssubs_all$casemix   <- df_ma$casemix
ssubs_all$indep     <- df_ma$indep
ssubs_all$outc      <- df_ma$outc
ssubs_all$horiz     <- df_ma$horiz
ssubs_all$MAoutcome <- df_ma$MAoutcome
ssubs_all$MAhorizon <- df_ma$MAhorizon
ssubs_all$disc      <- df_ma$disc # CHANGE created in cr_disc files
#ssubs_all$cal       <- df_ma$cal # CHANGE created in cr_calib files
ssubs_all$model_f   <- df_ma$model_f
ssubs_all$model_p   <- df_ma$model_p
ssubs_all$OS1        <- df_ma$OS1
ssubs_all$S1        <- df_ma$S1
ssubs_all$S2        <- df_ma$S2
#ssubs_all$S3        <- df_ma$S3 #Calibration estimate O:E reported and estimated variability vs estimates based on intercept of calibration curve (S3) # CHANGE created in cr_calib files
ssubs_all$S4        <- df_ma$S4 #AUC vs c-stat (S4) # CHANGE created in cr_disc files
ssubs_all$S5        <- df_ma$S5 #model version
ssubs_all$extva_dis_cstat_val <- df_ma$extva_dis_cstat_val
ssubs_all$extva_dis_cstat_se  <- df_ma$extva_dis_cstat_se
ssubs_all$extva_dis_cstat_lci <- df_ma$extva_dis_cstat_lci
ssubs_all$extva_dis_cstat_uci <- df_ma$extva_dis_cstat_uci
ssubs_all$toestD     <-df_ma$toestD

##### update toestD ####
ssubs_all$toestD[!is.na(ssubs_all$extva_dis_cstat_val)& !is.na(ssubs_all$extva_dis_cstat_lci)&!is.na(ssubs_all$extva_dis_cstat_uci)]
ssubs_all$toestDup[!is.na(ssubs_all$extva_dis_cstat_val)& !is.na(ssubs_all$extva_dis_cstat_lci)&!is.na(ssubs_all$extva_dis_cstat_uci)] <- "Estimates not required"
ssubs_all$toestDup[is.na(ssubs_all$toestDup) & !is.na(ssubs_all$theta.se)] <- "Estimates are possible"
ssubs_all$toestDup[is.na(ssubs_all$toestDup) & is.na(ssubs_all$theta.se)] <- "Estimates not possible"

ssubs_all[,c("toestD","toestDup")]

ssubs_all <- ssubs_all[which(!is.na(ssubs_all$theta.se)),]

dim(ssubs_all) ## 95
# plot(ssubs_all)


#######  Sensitivity: changes to disc ####### 
# Code received:
# E9 =	Observed outcomes post-intervention compared with pre-intervention predictions 
#       and not clinically relevant or evidence of miscalibration or question about data reliability

# CHANGE
table(ssubs_all$disc)
# -   E9 E11 
# 87  6   2 
#table(ssubs_all$cal)

table(ssubs_all$theta.se.source)
# Confidence Interval Newcombe (Method 4)      Standard Error 
#     54                  21                   20 

# To check
# ssubs_all[which(ssubs_all$theta.se.source=="Newcombe (Method 4)" | ssubs_all$theta.se.source=="Standard Error"),c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci","disc")]

# If you estimate both logit(c) and Var(logit(c)) - DOES NOT APPLY IN THIS REVIEW; THERE WAS NO DATA FOR ALL PREDICTORS 
# E20	Estimated both logit(c) and Var(logit(c)) 
# replace disc=”E20” if disc !=”E9”
# replace disc=”E17” if disc ==”E9”
# E17	Exclusion in sensitivity analysis for more than one reason 

# If you estimate just the variance
# replace disc=”E10” if disc !=”E9”
# E10	Estimated Var(logit(c)) only 
# E11 Non-standard discrimination measure 
ssubs_all$disc[which(ssubs_all$theta.se.source=="Standard Error" & (ssubs_all$disc!="E9" &  ssubs_all$disc!="E11"))]<-"E10"
ssubs_all$disc[which(ssubs_all$theta.se.source=="Standard Error" & (ssubs_all$disc=="E9" | ssubs_all$disc=="E11"))]<-"E17"


ssubs_all$disc[which(ssubs_all$theta.se.source=="Newcombe (Method 4)" & (ssubs_all$disc!="E9" &  ssubs_all$disc!="E11"))]<-"E10"
ssubs_all$disc[which(ssubs_all$theta.se.source=="Newcombe (Method 4)" & (ssubs_all$disc=="E9" | ssubs_all$disc=="E11"))]<-"E17"

# To check
# ssubs_all[,c("extva_dis_cstat_val","extva_dis_cstat_se","extva_dis_cstat_lci","extva_dis_cstat_uci","disc","theta.se.source")]


# If you cannot make estimates
# replace disc=”X”
# X	Exclude from meta-analysis
# NB: In file to send, studies not appearing did not have data for MA


# # After you have updated the disc and cal, when you can please send me the updated COVER tables  i.e.
# # author
# # year
# # data_name
# # horizon
# # outcome
# # MAoutcome
# # MAhorizon
# # popul casemix indep outc horiz cal disc model_f model_p
# # OS1 S1 S2 S4 S5
# # disc # CHANGE TO cal for calibration file
# #
# 
# scovertab_disc <- subset(ssubs_all, select=c("lineno","author",
# "year",
# "data_name",
# "horizon",
# "outcome",
# "MAoutcome",
# "MAhorizon",
# "popul", "casemix", "indep", "outc", "horiz" ,
# # "cal",    # CHANGE un-comment for calibration file
# "disc",     # CHANGE un-comment for discrimination file
# "model_f", "model_p",
# "OS1", "S1", "S2",
# # "S3",     # CHANGE un-comment for calibration file
# "S4",       # CHANGE un-comment for discrimination file
# "S5",
# "toestDup"))
# 
# # CHANGE NAME OF FINAL FILE
# write.csv(scovertab_disc, "C:\\Users\\mvazquezmontes\\OneDrive - Nexus365\\Documents\\SHFM and MAGGIC\\Analysis files\\RCode\\results\\SHFM_covertable_disc.csv", row.names = FALSE)

df_ma <- df_ma[df_ma$lineno %in% ssubs_all$lineno,]
df_ma$disc <- ssubs_all$disc # CHANGE to cal for calibration
df_ma$toestDup <- ssubs_all$toestDup # CHANGE to cal for calibration
# table(df_ma$disc)
# -   E9 E10  E11 E17 
# 48  5   39   1    2 


# To check if Levy_5 is included
# df_ma[which(df_ma$outc=="E6"),c("author","year","data_name","outcome","outc")]
