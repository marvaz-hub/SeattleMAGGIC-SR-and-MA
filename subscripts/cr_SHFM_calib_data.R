## SHFM ##
## Data for calibration 

path1 = "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/SeattleMAGGIC analysis files NEWCOVERCODES/"
path2 = "subscripts/"
path3 = "datasets/"

# Raw data
file = "all4MariaS_merged FINAL.csv"

# read the data into R
shfm <- data.frame(read.csv(paste0(path1, path3, file),sep=","))

# initial working copy
df <- shfm
dim(df) # 164 213

# table(df$cal)
# -   E9  X 
#  64  62  38 
# table(df$disc)
# -   E9 E11   X 
# 140   9   2  13 


# create individual study identifier
a <- sapply(strsplit(as.character(df$author), " "), function(x) x[which.max(nchar(x))])  # longest word in author vector
b <- sapply(strsplit(as.character(df$year), " "), function(x) x[which.max(nchar(x))])  # longest word in author vector

df$study_id <- paste0(a, ",", b, " (", df$outcome, ", ", df$horizon, " months)")

df$authoryear <- paste0(a, ",", b)


table(df$toestC)
# Checking other data  Exclude 
# 126                  38   

# df[which(!is.na(df$OErat_v)),c("lineno","author","year","horizon","outcome","OErat_v", "toestC", "cslope_v" ,          "cslope_l"       ,     "cslope_u"       ,     "cslope_p"        ,    "cslope_SE" ,
#                                                 "cinter_v","cinter_l","cinter_u","cinter_SE")]
# nrow(df[which(!is.na(df$OErat_v)),c("lineno","author","year","horizon","outcome","OErat_v", "toestC", "cslope_v" ,          "cslope_l"       ,     "cslope_u"       ,     "cslope_p"        ,    "cslope_SE" ,
#                                     "cinter_v","cinter_l","cinter_u","cinter_SE")]
# ) #69
# df[which(is.na(df$OErat_v) & !is.na(df$cslope_v)),c("lineno","author","year","horizon","outcome","OErat_v", "cslope_v" ,          "cslope_l"       ,     "cslope_u"       ,     "cslope_p"        ,    "cslope_SE" ,
#                                                     "cinter_v","cinter_l","cinter_u","cinter_SE","toestC")]
# Cheng2021; and Freitas2017 report intercept but not its variability
# coded as Checking other data or  Exclude - I won't change this


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
dim(df_ma) # 152 220

# To explore studies to be potentially included in MA of Calibration
table(df_ma$toestC)
# Checking other data             Exclude 
# 126                             26 
# studies to be included in MA of calib
df_ma <- df_ma[which(df_ma$toestC!="Exclude"),]
dim(df_ma) #126 220

# as.numeric(as.character()) # converts a factor variable into numeric
# subset(df, select="name of the variables") # to list only a subset of the variables in the dataset

# subset variables we need for calibration (if needed)
# subset(df_ma, select=c("study_id", "lineno", "author", "year", "data_name", "horizon", "outcome", "HFtype",
#                                            "sample_n_model", "sample_e_model","cplot","ctab","cother","cslope_v","cslope_l","cslope_u","cslope_SE",
#                                            "cslope_p", "cinter_v","cinter_l","cinter_u","cinter_SE", "cinter_p","HLtest_v","HLtest_p","OErat_v",
#                                            "OErat_l","OErat_u","OErat_p","kap_v","kap_p","calib_no_det","R2_v","Brier_v","Brier_l","Brier_u","Brier_se",
#                                           "comments68","comments69"))
# 




#################################################################################
# Estimating OE from slope
#################################################################################
# variable.names(df_ma)
# 
# datcal <- subset(df_ma, select=c("lineno","author","year","year","horizon","outcome","OErat_v","oe.est",
#                                  "cplot",               "ctab" ,               "cother", 
#                                  "cslope_v" ,          "cslope_l"       ,     "cslope_u"       ,     "cslope_p"        ,    "cslope_SE" ,
#                                  "cinter_v","cinter_l","cinter_u","cinter_SE",
#                                  "sample_n_model","sample_e_model", "comments68","comments69"))
# 
# #Is it enought to condition on cplot? Yes.
# # table(datcal$cplot) #27 yes
# # table(datcal$ctab ) #2 yes (they have a cplot too)
# # table(datcal$cother ) #3 with something
# # table(datcal$cplot,datcal$ctab )
# # table(datcal$cplot,datcal$cother )
# 
# datcal[which(datcal$cplot=="Yes" & is.na(datcal$OErat_v)),]
# datcal[which(datcal$cplot=="Yes" & is.na(datcal$OErat_v)),c("lineno","author","year","horizon","outcome","OErat_v","cinter_v","cslope_v")]
# 
# datcal[which(datcal$cplot=="Yes" & is.na(datcal$OErat_v) & !is.na(datcal$cslope_v)),c("lineno","author","year","horizon","outcome","OErat_v", "cslope_v" ,          "cslope_l"       ,     "cslope_u"       ,     "cslope_p"        ,    "cslope_SE" ,
#                                                                                       "cinter_v","cinter_l","cinter_u","cinter_SE")]
# #     lineno  author year horizon outcome OErat_v cslope_v cslope_l cslope_u cslope_p cslope_SE cinter_v cinter_l cinter_u cinter_SE
# # 20     20   Cheng 2021    36.2       1      NA   0.8285       NA       NA       NA        NA  0.02104       NA       NA        NA
# # 38     41 Freitas 2017    12.0       9      NA   1.4400       NA       NA       NA      0.21 -0.01600       NA       NA        NA
# 


#################################################################################
# Estimating intercept and slope from plots
#################################################################################
# datcal[which(datcal$cplot=="Yes" & is.na(datcal$oe.est)),]
# datcal[which(datcal$cplot=="Yes" & is.na(datcal$oe.est)),c("lineno","author","year","horizon","outcome")]
#  
# lineno      author year horizon outcome OErat_v cinter_v cslope_v
# 4        4       Ahmad 2023    12.0       1      NA       NA       NA
E <-1-c(
0.259785144,
0.354000115,
0.462538059,
0.558143276,
0.661003045,
0.758851037,
0.85922445,
0.937560751)
O <- 1-c(
0.504247716,
0.572093985,
0.35613259,
0.561199517,
0.700490607,
0.740148216,
0.845600046,
0.924694663)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==4 & df_ma$author=="Ahmad" & df_ma$year==2023] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==4 & df_ma$author=="Ahmad" & df_ma$year==2023] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==4 & df_ma$author=="Ahmad" & df_ma$year==2023]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==4 & df_ma$author=="Ahmad" & df_ma$year==2023] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==4 & df_ma$author=="Ahmad" & df_ma$year==2023]  <- res1[2,1]+1.96*res1[2,2] 


# 20      20       Cheng 2021    36.2       1      NA  0.02104   0.8285
O <- c(
  0.016229508,
  0.036885246,
  0.071803279,
  0.063934426,
  0.089016393,
  0.131803279,
  0.135245902,
  0.164262295,
  0.21147541,
  0.213934426
)
E <- c(0.05686445,
       0.072713965,
       0.095012018,
       0.065345019,
       0.07922489,
       0.084731952,
       0.111426257,
       0.135197288,
       0.172836849,
       0.2820666
)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==20 & df_ma$author=="Cheng" & df_ma$year==2021] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==20 & df_ma$author=="Cheng" & df_ma$year==2021] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==20 & df_ma$author=="Cheng" & df_ma$year==2021]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==20 & df_ma$author=="Cheng" & df_ma$year==2021] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==20 & df_ma$author=="Cheng" & df_ma$year==2021]  <- res1[2,1]+1.96*res1[2,2] 


#     41     Freitas 2017    12.0       9      1.4400       NA       NA       NA      0.21 
E<- c(0.029484789,	0.016535492,	0.053203355,	0.128115511)
O <- c(0.007741935,	0.017548387,	0.073290323,	0.166193548)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==41 & df_ma$author=="Frietas" & df_ma$year==2017] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==41 & df_ma$author=="Frietas" & df_ma$year==2017] <- res1[1,2]

#replace OE with slope values (keep slope’s data in case we need it later)
df_ma$oe.est.fromslope[df_ma$lineno==41 & df_ma$author=="Frietas" & df_ma$year==2017]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==41 & df_ma$author=="Frietas" & df_ma$year==2017] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==41 & df_ma$author=="Frietas" & df_ma$year==2017]  <- res1[2,1]+1.96*res1[2,2]



#       51 Gorodeski_1 2010    12.0       6      NA       NA       NA
E<- c(0.389728097,
      0.179254783,
      0.124874119,
      0.07653575,
      0.049345418)
O <- c(0.56838565,
       0.628923767,
       0.283632287,
       0.393497758,
       0.188340807)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==51 & df_ma$author=="Gorodeski_1" & df_ma$year==2010] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==51 & df_ma$author=="Gorodeski_1" & df_ma$year==2010] <- res1[1,2]

#replace OE with slope values (keep slope’s data in case we need it later)
df_ma$oe.est.fromslope[df_ma$lineno==51 & df_ma$author=="Gorodeski_1" & df_ma$year==2010]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==51 & df_ma$author=="Gorodeski_1" & df_ma$year==2010] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==51 & df_ma$author=="Gorodeski_1" & df_ma$year==2010]  <- res1[2,1]+1.96*res1[2,2]


#       52 Gorodeski_1 2010    24.0       6      NA       NA       NA
E<- c(0.609379255,
      0.324108748,
      0.238274025,
      0.151071246,
      0.098866124)
O <- c(0.569856986,
       0.693069307,
       0.386138614,
       0.460946095,
       0.188118812)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==52 & df_ma$author=="Gorodeski_1" & df_ma$year==2010] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==52 & df_ma$author=="Gorodeski_1" & df_ma$year==2010] <- res1[1,2]

#replace OE with slope values (keep slope’s data in case we need it later)
df_ma$oe.est.fromslope[df_ma$lineno==52 & df_ma$author=="Gorodeski_1" & df_ma$year==2010]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==52 & df_ma$author=="Gorodeski_1" & df_ma$year==2010] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==52 & df_ma$author=="Gorodeski_1" & df_ma$year==2010]  <- res1[2,1]+1.96*res1[2,2]


#       53 Gorodeski_2 2010    12.0       1      NA       NA       NA
E <- c(
0.798724954,
0.43989071,
0.229508197,
0.115664845,
0.066484517)
O <- c(
0.614504035,
0.372642162,
0.339794376,
0.221022657,
0.092250255)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==53 & df_ma$author=="Gorodeski_2" & df_ma$year==2010] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==53 & df_ma$author=="Gorodeski_2" & df_ma$year==2010] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==53 & df_ma$author=="Gorodeski_2" & df_ma$year==2010]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==53 & df_ma$author=="Gorodeski_2" & df_ma$year==2010] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==53 & df_ma$author=="Gorodeski_2" & df_ma$year==2010]  <- res1[2,1]+1.96*res1[2,2] 


#       54 Gorodeski_2 2010    24.0       1      NA       NA       NA
E<- c(0.942627658,
      0.672788961,
      0.404526328,
      0.218560218,
      0.127010122)
O <- c(0.693961105,
       0.502558854,
       0.410440123,
       0.358239509,
       0.316274309)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==54 & df_ma$author=="Gorodeski_2" & df_ma$year==2010] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==54 & df_ma$author=="Gorodeski_2" & df_ma$year==2010] <- res1[1,2]

#replace OE with slope values (keep slope’s data in case we need it later)
df_ma$oe.est.fromslope[df_ma$lineno==54 & df_ma$author=="Gorodeski_2" & df_ma$year==2010]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==54 & df_ma$author=="Gorodeski_2" & df_ma$year==2010] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==54 & df_ma$author=="Gorodeski_2" & df_ma$year==2010]  <- res1[2,1]+1.96*res1[2,2]


#       55 Gorodeski_2 2010    12.0       6      NA       NA       NA
E<- c(0.797583082,
      0.438066465,
      0.229607251,
      0.114803625,
      0.064451158)
O <- c(0.6367713,
       0.433856502,
       0.471973094,
       0.225336323,
       0.141255605)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==55 & df_ma$author=="Gorodeski_2" & df_ma$year==2010] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==55 & df_ma$author=="Gorodeski_2" & df_ma$year==2010] <- res1[1,2]

#replace OE with slope values (keep slope’s data in case we need it later)
df_ma$oe.est.fromslope[df_ma$lineno==55 & df_ma$author=="Gorodeski_2" & df_ma$year==2010]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==55 & df_ma$author=="Gorodeski_2" & df_ma$year==2010] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==55 & df_ma$author=="Gorodeski_2" & df_ma$year==2010]  <- res1[2,1]+1.96*res1[2,2]


#       56 Gorodeski_2 2010    24.0       6      NA       NA       NA
E<- c(0.941915281,
      0.674744306,
      0.403483319,
      0.217514623,
      0.124451851,
      0.609379255,
      0.324108748,
      0.238274025,
      0.151071246,
      0.098866124)
O <- c(0.708470847,
       0.551155116,
       0.524752475,
       0.353135314,
       0.346534653,
       0.569856986,
       0.693069307,
       0.386138614,
       0.460946095,
       0.188118812)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==56 & df_ma$author=="Gorodeski_2" & df_ma$year==2010] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==56 & df_ma$author=="Gorodeski_2" & df_ma$year==2010] <- res1[1,2]

#replace OE with slope values (keep slope’s data in case we need it later)
df_ma$oe.est.fromslope[df_ma$lineno==56 & df_ma$author=="Gorodeski_2" & df_ma$year==2010]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==56 & df_ma$author=="Gorodeski_2" & df_ma$year==2010] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==56 & df_ma$author=="Gorodeski_2" & df_ma$year==2010]  <- res1[2,1]+1.96*res1[2,2]



#       79     Lanfear 2017    12.0       1      NA       NA       NA
E<- c(
0.358778626,
0.157251908,
0.067175573)
O <- c(
0.310546488,
0.160340477,
0.090236034)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==79 & df_ma$author=="Lanfear" & df_ma$year==2017] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==79 & df_ma$author=="Lanfear" & df_ma$year==2017] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==79 & df_ma$author=="Lanfear" & df_ma$year==2017]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==79 & df_ma$author=="Lanfear" & df_ma$year==2017] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==79 & df_ma$author=="Lanfear" & df_ma$year==2017]  <- res1[2,1]+1.96*res1[2,2]


#     112    Miyagawa 2019    60.0       1      NA       NA       NA
E<- c(0.835635462,	0.532896274,	0.329417827,	0.178025275)
O <- c(0.503708541,	0.373284628,	0.209747611,	0.137296117)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==112 & df_ma$author=="Miyagawa" & df_ma$year==2019] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==112 & df_ma$author=="Miyagawa" & df_ma$year==2019] <- res1[1,2]

#replace OE with slope values (keep slope’s data in case we need it later)
df_ma$oe.est.fromslope[df_ma$lineno==112 & df_ma$author=="Miyagawa" & df_ma$year==2019]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==112 & df_ma$author=="Miyagawa" & df_ma$year==2019] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==112 & df_ma$author=="Miyagawa" & df_ma$year==2019]  <- res1[2,1]+1.96*res1[2,2]






#     132      Regoli 2013    60.0       1      NA       NA       NA
E<- c(0.141033313,
      0.209300979,
      0.248560195,
      0.298880974,
      0.399213657,
      0.337644704,
      0.449122604,
      0.518915336,
      0.599422148,
      0.758698352)
O <- c(0.191756272,
       0.17921147,
       0.245519713,
       0.231182796,
       0.288530466,
       0.435483871,
       0.388888889,
       0.451612903,
       0.53046595,
       0.672043011)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==132 & df_ma$author=="Regoli" & df_ma$year==2013] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==132 & df_ma$author=="Regoli" & df_ma$year==2013] <- res1[1,2]

#replace OE with slope values (keep slope’s data in case we need it later)
df_ma$oe.est.fromslope[df_ma$lineno==132 & df_ma$author=="Regoli" & df_ma$year==2013]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==132 & df_ma$author=="Regoli" & df_ma$year==2013] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==132 & df_ma$author=="Regoli" & df_ma$year==2013]  <- res1[2,1]+1.96*res1[2,2]


#     131      Regoli 2013    24.0       1      NA       NA       NA

#     130      Regoli 2013    12.0       1      NA       NA       NA
O <- c(
0.015178571,
0.052380952,
0.079166667,
0.07172619,
0.098511905,
0.116369048,
0.107440476,
0.156547619,
0.262202381,
0.204166667)
E <- c(
0.027343617,
0.060332395,
0.048480969,
0.066375448,
0.090366317,
0.07848289,
0.138188044,
0.109988906,
0.168611324,
0.249048472)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==130 & df_ma$author=="Regoli" & df_ma$year==2013] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==130 & df_ma$author=="Regoli" & df_ma$year==2013] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==130 & df_ma$author=="Regoli" & df_ma$year==2013]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==130 & df_ma$author=="Regoli" & df_ma$year==2013] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==130 & df_ma$author=="Regoli" & df_ma$year==2013]  <- res1[2,1]+1.96*res1[2,2]


#     134        Rich 2018    43.2       1      NA       NA       NA
E<- c( 0.04010585,
       0.112828666,
       0.185541371,
       0.254627161,
       0.3115988,
       0.372204937,
       0.442521604,
       0.501922139,
       0.567396178,
       0.631662088,
       0.698341729,
       0.767430046,
       0.838937149,
       0.906807228,
       0.977108729)
O <- c( 0.054454282,
        0.123499632,
        0.184194231,
        0.249056623,
        0.307630676,
        0.3683,
        0.449866423,
        0.514708596,
        0.596264909,
        0.679906383,
        0.757289847,
        0.824239927,
        0.889107374,
        0.949791863,
        1.018832158)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==134 & df_ma$author=="Rich" & df_ma$year==2018] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==134 & df_ma$author=="Rich" & df_ma$year==2018] <- res1[1,2]

#replace OE with slope values (keep slope’s data in case we need it later)
df_ma$oe.est.fromslope[df_ma$lineno==134 & df_ma$author=="Rich" & df_ma$year==2018]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==134 & df_ma$author=="Rich" & df_ma$year==2018] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==134 & df_ma$author=="Rich" & df_ma$year==2018]  <- res1[2,1]+1.96*res1[2,2]


#     143   Scrutinio 2014    12.0       1      NA       NA       NA
E<-c(10.4,18.6,31.7,61.5)/100
O<-c(5.5,21.8,35.5,58.5)/100
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==143 & df_ma$author=="Scrutinio" & df_ma$year==2014] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==143 & df_ma$author=="Scrutinio" & df_ma$year==2014] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==143 & df_ma$author=="Scrutinio" & df_ma$year==2014]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==143 & df_ma$author=="Scrutinio" & df_ma$year==2014] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==143 & df_ma$author=="Scrutinio" & df_ma$year==2014]  <- res1[2,1]+1.96*res1[2,2]


#     148     Tohyama 2021    12.0       1      NA       NA       NA
E <- c(
0.009928,
0.019314067,
0.031001632,
0.042094672,
0.053818252,
0.066606833,
0.085848871,
0.110437633,
0.150737869,
0.294278552)
O <- c(
0.015911553,
0.074174453,
0.100679417,
0.115417926,
0.178981818,
0.138431197,
0.138487791,
0.240324817,
0.309266876,
0.412630231)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==148 & df_ma$author=="Tohyama" & df_ma$year==2021] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==148 & df_ma$author=="Tohyama" & df_ma$year==2021] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==148 & df_ma$author=="Tohyama" & df_ma$year==2021]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==148 & df_ma$author=="Tohyama" & df_ma$year==2021] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==148 & df_ma$author=="Tohyama" & df_ma$year==2021]  <- res1[2,1]+1.96*res1[2,2]


#     160    Williams 2018    12.0       1      NA       NA       NA
E	<- c(0.025672878,
	0.035012652,
	0.045364619,
	0.054566368,
	0.063193007,
	0.074120083,
	0.08447205,
	0.093673798,
	0.104025765,
	0.113802622,
	0.124154589,
	0.133931447,
	0.144858523,
	0.154060271,
	0.163837129,
	0.173613987,
	0.183965954,
	0.195468139,
	0.204669887,
	0.215021854,
	0.22594893,
	0.23400046,
	0.243202208,
	0.254704394,
	0.26563147,
	0.27310789,
	0.284610076,
	0.294962043,
	0.388129745)

O	<- c(0.018219462,
	0.018334484,
	0.036162871,
	0.048240166,
	0.062617897,
	0.06836899,
	0.077570738,
	0.086772487,
	0.105751093,
	0.119553715,
	0.136806993,
	0.150609616,
	0.159811364,
	0.163837129,
	0.179940189,
	0.188566828,
	0.169013112,
	0.221923165,
	0.201219232,
	0.243202208,
	0.214446745,
	0.24952841,
	0.25757994,
	0.275983437,
	0.257004831,
	0.326593053,
	0.314515758,
	0.339245457,
	0.343271222)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients) #160    Williams 2018 
df_ma$cinter_v[df_ma$lineno==160 & df_ma$author=="Williams" & df_ma$year==2018] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==160 & df_ma$author=="Williams" & df_ma$year==2018] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==160 & df_ma$author=="Williams" & df_ma$year==2018]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==160 & df_ma$author=="Williams" & df_ma$year==2018] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==160 & df_ma$author=="Williams" & df_ma$year==2018]  <- res1[2,1]+1.96*res1[2,2]

#     161      Zafrir 2012    25.0       1      NA       NA       NA
E<- c(0.688755484,
      0.481105061,
      0.400895096,
      0.298995221,
      0.158616029,
      0.193303432,
      0.264858195,
      0.107127468,
      0.032203855,
      0.071852476)
O <- c(0.769225948,
       0.566993106,
       0.38106393,
       0.384503291,
       0.305437167,
       0.197281416,
       0.165477123,
       0.138028831,
       0.112014259,
       0.061681291)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==161 & df_ma$author=="Zafrir" & df_ma$year==2012] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==161 & df_ma$author=="Zafrir" & df_ma$year==2012] <- res1[1,2]

#replace OE with slope values (keep slope’s data in case we need it later)
df_ma$oe.est.fromslope[df_ma$lineno==161 & df_ma$author=="Zafrir" & df_ma$year==2012]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==161 & df_ma$author=="Zafrir" & df_ma$year==2012] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==161 & df_ma$author=="Zafrir" & df_ma$year==2012]  <- res1[2,1]+1.96*res1[2,2]


## EXTRA DATA ##


# 162	Miyagawa	2019	OSCAR	SHFM	12	1
E<- c(0.420714566,	0.144210156,	0.073200447,	0.033916136)
O <- c(0.331986103,	0.14092864,	0.082533787,	0.028214298)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==162 & df_ma$author=="Miyagawa" & df_ma$year==2019] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==162 & df_ma$author=="Miyagawa" & df_ma$year==2019] <- res1[1,2]

#replace OE with slope values (keep slope’s data in case we need it later)
df_ma$oe.est.fromslope[df_ma$lineno==162 & df_ma$author=="Miyagawa" & df_ma$year==2019]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==162 & df_ma$author=="Miyagawa" & df_ma$year==2019] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==162 & df_ma$author=="Miyagawa" & df_ma$year==2019]  <- res1[2,1]+1.96*res1[2,2]


# 163	Miyagawa	2019	OSCAR	SHFM	36	1
E<- c(0.712751075,	0.372973414,	0.215060687,	0.111286842)
O <- c(0.420831909,	0.277609473,	0.149129368,	0.092443637)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==163 & df_ma$author=="Miyagawa" & df_ma$year==2019] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==163 & df_ma$author=="Miyagawa" & df_ma$year==2019] <- res1[1,2]

#replace OE with slope values (keep slope’s data in case we need it later)
df_ma$oe.est.fromslope[df_ma$lineno==163 & df_ma$author=="Miyagawa" & df_ma$year==2019]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==163 & df_ma$author=="Miyagawa" & df_ma$year==2019] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==163 & df_ma$author=="Miyagawa" & df_ma$year==2019]  <- res1[2,1]+1.96*res1[2,2]

# 164	Rosenbaum	2014	University of Minnesota	SHFM	60	1
E<- c(0.835635462,	0.532896274,	0.329417827,	0.178025275)
O <- c(0.503708541,	0.373284628,	0.209747611,	0.137296117)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==164 & df_ma$author=="Rosenbaum" & df_ma$year==2014] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==164 & df_ma$author=="Rosenbaum" & df_ma$year==2014] <- res1[1,2]

#replace OE with slope values (keep slope’s data in case we need it later)
df_ma$oe.est.fromslope[df_ma$lineno==164 & df_ma$author=="Rosenbaum" & df_ma$year==2014]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==164 & df_ma$author=="Rosenbaum" & df_ma$year==2014] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==164 & df_ma$author=="Rosenbaum" & df_ma$year==2014]  <- res1[2,1]+1.96*res1[2,2]


###########################################################################################################################
## oe for MA will include oe.est (original or estimated from O and E, except for Haga for now) and oe.est.fromslope
########################################################################################################################

df_ma$oe.est.slope.ma <- df_ma$oe.est 
df_ma$oe.est.slope.ma[is.na(df_ma$oe.est )] <- df_ma$oe.est.fromslope[is.na(df_ma$oe.est )] 


#to check
# subset(df_ma, select=c("lineno","author","year","OErat_v", "oe.est", "oe.est.slope.ma",
#                        "oe.est.fromslope","oell.est.fromslope","oeul.est.fromslope",
#                                               "o.est","e.est",
#                        "sample_n_model","sample_e_model"))
# 
# 
# subset(df_ma, select=c("lineno","author","year","OErat_v", "oe.est", "oe.est.slope.ma",
#                        "oe.est.fromslope"  ))

########################################################################################################################
##for ma using the intercept and not implementing Debray's formulas manually, use oe.est
########################################################################################################################


################################################################################
## The following is to use the intercept based on Debray's formulas
################################################################################

#### OE from intercept
inter <- df_ma$cinter_v
Po <- df_ma$o.est/df_ma$sample_n_model
lnoe <- log(-exp(inter)*Po+exp(inter)+Po)

df_ma$oe.est.frominter <- exp(lnoe)

var_inter <- df_ma$cinter_SE^2
varlnoe <- var_inter*((Po-1)^2*(Po^2+1)*(exp(Po+inter))^2)/(Po*(-exp(inter))+Po+exp(inter))^2
selnoe <- sqrt(varlnoe)
lllnoe <- lnoe -1.96*selnoe
ullnoe <- lnoe +1.96*selnoe

df_ma$oell.est.frominter <- exp(lllnoe)
df_ma$oeul.est.frominter <- exp(ullnoe)

########################################################################################################################
## oe for MA (using intercept) will include oe.est (original or estimated from O and E, except for Freitas2017) and oe.est.frominter
########################################################################################################################

df_ma$oe.est.inter.ma <- df_ma$oe.est
df_ma$oe.est.inter.ma[is.na(df_ma$oe.est )] <- df_ma$oe.est.frominter[is.na(df_ma$oe.est )]

#df_ma$oe.est.inter.ma[df_ma$lineno==41 & df_ma$author=="Freitas" & df_ma$year==2017] <- df_ma$oe.est.frominter[df_ma$lineno==41 & df_ma$author=="Freitas" & df_ma$year==2017]

# #to check
# subset(df_ma, select=c("lineno","author","year","OErat_v", "oe.est", "oe.est.slope.ma",
#                        "oe.est.fromslope","oell.est.fromslope","oeul.est.fromslope",
#                        "oe.est.frominter","oell.est.frominter","oeul.est.frominter",
#                        "o.est","e.est",
#                        "sample_n_model","sample_e_model"))
# 
# 
# subset(df_ma, select=c("lineno","author","year","OErat_v", "oe.est", "oe.est.slope.ma","oe.est.inter.ma",
#                        "oe.est.fromslope","oe.est.frominter","oell.est.frominter","oeul.est.frominter"  ))

# Update S3
table(df_ma$S3,useNA = "always")
#        
#   126  


## These lineno have O and E reported and variability estimated
lineno_oerepvarest <- c(10:12, 16, 24, 33, 58,62,80,83,84,88,89,91:102,114:116,122,126:128,133,136,137,155)
lineno_OandE <- c(8, 19, 36, 57)
lineno_inter <- c(4,20,41,51:56,79,112,130,132,134,143,148,160:164)

length(c(lineno_oerepvarest,lineno_OandE,lineno_inter)) #61
length(lineno_oerepvarest) #36
length(lineno_OandE) #4
length(lineno_inter) #21

df_ma[which(!is.na(df_ma$OErat_v)),c("lineno","author","year","horizon","outcome","OErat_v", "e.est" ,"o.est" , "cinter_v", "oell.est.frominter","oeul.est.frominter","S3", "toestC","cinter_SE")]
df_ma$lineno[which(!is.na(df_ma$OErat_v))]

df_ma$S3[df_ma$lineno %in% lineno_oerepvarest] <- "OE reported and variability estimated"
df_ma$S3[df_ma$lineno %in% lineno_OandE] <- "O and E reported, OE calculated and variability estimated"
df_ma$S3[df_ma$lineno %in% lineno_inter] <- "Estimates based on intercept of calibration curve" 


# subset(df_ma, select=c("e.est",
#                        "oell.est.frominter","oeul.est.frominter","S3"
# ))

table(df_ma$S3,useNA = "always")
# 68                                                          
# Estimates based on intercept of calibration curve 
# 18 
# O and E reported, OE calculated and variability estimated 
# 4 
# OE reported and variability estimated 
# 36 
# <NA> 
#   0 

# df_ma[which(nchar(df_ma$S3)==0),c("lineno","author","year","horizon","outcome","OErat_v", "e.est" ,"o.est" , "cinter_v", "oell.est.frominter","oeul.est.frominter","S3", "toestC","cinter_SE")]

#Update toestC
df_ma$toestCup <- df_ma$toestC
df_ma$toestCup[df_ma$lineno %in% c(lineno_oerepvarest,lineno_OandE,lineno_inter)] <- "Estimates are possible"
df_ma$toestCup[df_ma$lineno %nin% c(lineno_oerepvarest,lineno_OandE,lineno_inter)] <- "Estimates not possible"

# df_ma[,c("lineno","author","year","horizon","outcome","OErat_v", "e.est" ,"o.est" , "cinter_v", "oell.est.frominter","oeul.est.frominter","S3", "toestC", "toestCup","cinter_SE")]


# # ####### to select subset of studies with data for MAs #######
# ssubs_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
#                     citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
#                     data=df_ma, slab = study_id  )
# ssubs_all$n      <- df_ma$sample_n_model
# ssubs_all$events <- round(df_ma$o.est,0)
# ssubs_all$id     <- df_ma$study_id
# ssubs_all <- ssubs_all[which(!is.na(ssubs_all$theta.se)),]
# dim(ssubs_all) ## 47
# plot(ssubs_all)


####### REPEAT AFTER ANY CHANGES                      #######
####### IF SATISFIED, ADD COVER VARIABLES             #######
####### to select subset of studies with data for MAs #######
ssubs_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                    citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                    data=df_ma, slab = study_id  )
ssubs_all$lineno    <- df_ma$lineno
ssubs_all$n         <- df_ma$sample_n_model
ssubs_all$oevents    <- round(df_ma$o.est,0)
ssubs_all$eevents    <- round(df_ma$e.est,0)
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
# ssubs_all$disc      <- df_ma$disc # CHANGE created in cr_disc files
ssubs_all$cal       <- df_ma$cal # CHANGE created in cr_calib files
ssubs_all$model_f   <- df_ma$model_f
ssubs_all$model_p   <- df_ma$model_p
ssubs_all$OS1        <- df_ma$OS1
ssubs_all$S1        <- df_ma$S1
ssubs_all$S2        <- df_ma$S2
ssubs_all$S3        <- df_ma$S3 #Calibration estimate O:E reported and estimated variability vs estimates based on intercept of calibration curve (S3) # CHANGE created in cr_calib files
# ssubs_all$S4        <- df_ma$S4 #AUC vs c-stat (S4) # CHANGE created in cr_disc files
ssubs_all$S5        <- df_ma$S5 #model version
ssubs_all$oe.est.inter.ma <- df_ma$oe.est.inter.ma
ssubs_all$oell.est.frominter  <- df_ma$oell.est.frominter
ssubs_all$oeul.est.frominter <- df_ma$oeul.est.frominter
ssubs_all$OErat_v <- df_ma$OErat_v
ssubs_all$OErat_l <- df_ma$OErat_l
ssubs_all$OErat_u <- df_ma$OErat_u
ssubs_all$cinter_v <- df_ma$cinter_v
ssubs_all$cinter_SE <- df_ma$cinter_SE
ssubs_all$toestC     <- df_ma$toestC
ssubs_all$toestCup     <- df_ma$toestCup

ssubs_all <- ssubs_all[which(!is.na(ssubs_all$theta.se)),]

dim(ssubs_all) ## 47
# plot(ssubs_all)

#######  Sensitivity: changes to disc #######
# Code received:
# E9 =	Observed outcomes post-intervention compared with pre-intervention predictions
#       and not clinically relevant or evidence of miscalibration or question about data reliability

# CHANGE
# table(ssubs_all$disc)
table(ssubs_all$cal)
# - E9
# 29  18

table(ssubs_all$S3)
# 4 O and E reported, OE calculated and variability estimated

# 1.	If you estimate both the mean and its variance
# replace cal=”E19” if cal !=”E9”
# E19	Estimated both ln(O:E)) and Var(In(O:E))
ssubs_all$cal[(ssubs_all$S3=="Estimates based on intercept of calibration curve"
               | ssubs_all$S3=="O and E reported, OE calculated and variability estimated") & (ssubs_all$cal!="E9")] <- "E19"
ssubs_all$cal[(ssubs_all$S3=="Estimates based on intercept of calibration curve"
               | ssubs_all$S3=="O and E reported, OE calculated and variability estimated") & (ssubs_all$cal=="E9")] <- "E17"

#
# 2.	If you estimate just the variance
# replace cal=”E8” if cal !=”E9”
# E8	Estimated Var(In(O:E)) only
ssubs_all$cal[ssubs_all$S3=="OE reported and variability estimated"
              & (ssubs_all$cal!="E9")] <- "E8"
ssubs_all$cal[ssubs_all$S3=="OE reported and variability estimated" & (ssubs_all$cal=="E9")] <- "E17"


# 3.	If you cannot make estimates
# replace cal=”X”
# X	EXclude from meta-analysis

ssubs_all$cal[ssubs_all$toestCup=="Estimates are not possible"] <- "X"

# NB: In file to send, studies not appearing did not have data for MA

table(ssubs_all$S3)
# Estimates based on intercept of calibration curve
# 7
# O and E reported, OE calculated and variability estimated
# 4
# OE reported and variability estimated
# 36
# ssubs_all$lineno[ssubs_all$S3=="O and E reported, OE calculated and variability estimated"] #8 19 36 57
ssubs_all$S3[ssubs_all$lineno==8 | ssubs_all$lineno==19 |
               ssubs_all$lineno==36 | ssubs_all$lineno==57
             ] <- "OE reported and variability estimated"
table(ssubs_all$S3)

# After you have updated the disc and cal, when you can please send me the updated COVER tables  i.e. 
# author 
# year 
# data_name 
# horizon 
# outcome 
# MAoutcome 
# MAhorizon 
# popul casemix indep outc horiz cal disc model_f model_p 
# OS1 S1 S2 S3 S5
# cal # CHANGE TO cal for calibration file

# scovertab_cal <- subset(ssubs_all, select=c("lineno","author",
# "year",
# "data_name",
# "horizon",
# "outcome",
# "MAoutcome",
# "MAhorizon",
# "popul", "casemix", "indep", "outc", "horiz" ,
# "cal",  # CHANGE un-comment for calibration file
# # "disc", # CHANGE un-comment for discrimination file
# "model_f", "model_p",
# "OS1", "S1", "S2",
# "S3", # CHANGE un-comment for calibration file
# # "S4", # CHANGE un-comment for discrimination file
# "S5",
# "toestC", "toestCup"))
# 
# # CHANGE NAME OF FINAL FILE
# write.csv(scovertab_cal, "C:\\Users\\mvazquezmontes\\OneDrive - Nexus365\\Documents\\SHFM and MAGGIC\\Analysis files\\RCode\\results\\SHFM_covertable_cal.csv", row.names = FALSE)


df_ma <- df_ma[df_ma$lineno %in% ssubs_all$lineno,]
df_ma$cal <- ssubs_all$cal # CHANGE to cal for calibration
df_ma$toestCup <- ssubs_all$toestCup # CHANGE to cal for calibration
# table(df_ma$cal)
# E17 E19  E8 
# 18   7   22 

