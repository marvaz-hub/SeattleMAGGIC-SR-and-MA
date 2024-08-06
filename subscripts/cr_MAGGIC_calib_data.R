## MAGGIC ##
## Data for calibration 

path1 = "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/SeattleMAGGIC analysis files NEWCOVERCODES/"
path2 = "subscripts/"
path3 = "datasets/"

## Raw data 
file = "all4MariaM_merged FINAL.csv"

# read the data into R
maggic <- data.frame(read.csv(paste0(path1, path3, file),sep=","))

# initial working copy
df <- maggic
dim(df) # 106 166

# table(df$cal)
# -   E9  X 
# 44  24  38 
# table(df$disc)
# -   E9 E11   X 
# 92    4   1   9 

# create individual study identifier
a <- sapply(strsplit(as.character(df$author), " "), function(x) x[which.max(nchar(x))])  # longest word in author vector
b <- sapply(strsplit(as.character(df$year), " "), function(x) x[which.max(nchar(x))])  # longest word in author vector

df$study_id <- paste0(a, ",", b, " (", df$outcome, ", ", df$horizon, " months)")

df$authoryear <- paste0(a, ",", b)

table(df$toestC)
# Checking other data Estimates not required                Exclude 
#               66                      2                     38 

# df[which(df$toestC=="Estimates not required"),c("lineno","author","year","horizon","outcome","OErat_v", "cslope_v" ,  "cslope_l", "cslope_u", "cslope_p", "cslope_SE",
#                                                 "cinter_v","cinter_l","cinter_u","cinter_SE")]
#Freitas2017 is incorrectly classified as "Estimates not required" (S3). We needed to estimate the variability of the intercept.


df$toestC[df$toestC=="Estimates not required"] <- "Intercept reported but not its variability"

# df[which(!is.na(df$OErat_v)),c("lineno","author","year","horizon","outcome","OErat_v", "toestC", "cslope_v" ,          "cslope_l"       ,     "cslope_u"       ,     "cslope_p"        ,    "cslope_SE" ,
#                                                 "cinter_v","cinter_l","cinter_u","cinter_SE")]

# df[which(is.na(df$OErat_v) & !is.na(df$cslope_v)),c("lineno","author","year","horizon","outcome","OErat_v", "cslope_v" ,          "cslope_l"       ,     "cslope_u"       ,     "cslope_p"        ,    "cslope_SE" ,
#                                                     "cinter_v","cinter_l","cinter_u","cinter_SE","toestC")]
#same for Cheng2021; michaels_1 and _2 2020 and Xu 2022

# df$toestC[which(is.na(df$OErat_v) & !is.na(df$cslope_v))] <- "Intercept reported but not its variability"
# 
# df[df$lineno==66 | df$lineno==67,c("lineno","author","year","horizon","outcome","OErat_v", "cslope_v" ,          "cslope_l"       ,     "cslope_u"       ,     "cslope_p"        ,    "cslope_SE" ,
#                                                     "cinter_v","cinter_l","cinter_u","cinter_SE","toestC")]


#################################################################################
# Estimating O and E using information in comments68 and 69 ###
#################################################################################

source(paste0(path1, path2, "fn_oande_M.r"))

df <- fn_oande_M(df)


#################################################################################
# studies to be included in the MA
#################################################################################
inma <- which(df$NotMA==0)
df_ma <- df[inma,] 
dim(df_ma) #97 173
# To explore studies to be potentially included in MA of Calibration
table(df_ma$toestC)
# Checking other data    Exclude        Intercept reported but not its variability 
# 66                     29                                          6 

# studies to be included in MA of calib
df_ma <- df_ma[which(df_ma$toestC!="Exclude"),]
dim(df_ma) #68 173
table(df_ma$toestC)
# Checking other data         Intercept reported but not its variability 
# 66                           6 


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
# #      lineno     author year horizon outcome OErat_v cslope_v cslope_l cslope_u cslope_p cslope_SE cinter_v cinter_l cinter_u cinter_SE
# # 15      15      Cheng 2021      12       1      NA   0.4734       NA       NA       NA        NA  0.05672       NA       NA        NA
# # 41      41    Freitas 2017      12       1      NA   0.5100       NA       NA       NA      0.10 -0.01700       NA       NA        NA
# # 42      42    Freitas 2017      24       1      NA   0.7100       NA       NA       NA      0.07 -0.05000       NA       NA        NA
# # 61      61 Michaels_1 2020      12       1      NA   1.1500       NA       NA       NA        NA  0.51000       NA       NA        NA
# # 62      62 Michaels_2 2020      12       1      NA   0.9300       NA       NA       NA        NA -4.17000       NA       NA        NA
# # 104    104         Xu 2022      12       1      NA   0.9970       NA       NA       NA        NA -0.00200       NA       NA        NA
# 


#################################################################################
# Estimating intercept and slope from plots
#################################################################################
# datcal[which(datcal$cplot=="Yes" & is.na(datcal$oe.est)),]
# datcal[which(datcal$cplot=="Yes" & is.na(datcal$oe.est)),c("lineno","author","year","horizon","outcome")]
#  
# lineno     author year
# 1        1      Ahmad 2023
E <- 1-c(
  0.456027917,
  0.55727801,
  0.65198651,
  0.755598845,
  0.854732885,
  0.932421132)
O <- 1-c(
  0.50323758,
  0.694405258,
  0.687965268,
  0.783824604,
  0.85193102,
  0.942456262)

mod1 <- lm(O~E)
res1 <- summary(mod1)$coefficients
df_ma$cinter_v[df_ma$lineno==1 & df_ma$author=="Ahmad" & df_ma$year==2023] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==1 & df_ma$author=="Ahmad" & df_ma$year==2023] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==1 & df_ma$author=="Ahmad" & df_ma$year==2023] <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==1 & df_ma$author=="Ahmad" & df_ma$year==2023] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==1 & df_ma$author=="Ahmad" & df_ma$year==2023] <- res1[2,1]+1.96*res1[2,2]



# 2        2       Alba 2023    12.0       1 #no data

# 3        3       Alba 2023    12.0       5
E <- c(
  0.039790576,
  0.060034904,
  0.076788831,
  0.092146597,
  0.108900524,
  0.131239092,
  0.155671902,
  0.191972077,
  0.251308901,
  0.386038394)
O <- c(
  0.012121424,
  0.03152083,
  0.062266663,
  0.032242219,
  0.043622442,
  0.075045378,
  0.119827403,
  0.087834582,
  0.18209057,
  0.279143541)
mod1 <- lm(O~E)
res1 <- summary(mod1)$coefficients
df_ma$cinter_v[df_ma$lineno==3 & df_ma$author=="Alba" & df_ma$year==2023] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==3 & df_ma$author=="Alba" & df_ma$year==2023] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==3 & df_ma$author=="Alba" & df_ma$year==2023]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==3 & df_ma$author=="Alba" & df_ma$year==2023]  <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==3 & df_ma$author=="Alba" & df_ma$year==2023]  <- res1[2,1]+1.96*res1[2,2]



# 13      13     Canepa 2018    12.0       1 # no cplot; O and E reported in abstract; added above into oe.est

# 14      14     Canepa 2019    36.0       1
O	<- c(0.125728155,
       0.202184466,
       0.283737864,
       0.383980583,
       0.575970874)

E <- c(0.093446602,
       0.15631068,
       0.248058252,
       0.355097087,
       0.560679612)
mod1 <- lm(O~E)
res1 <- summary(mod1)$coefficients
df_ma$cinter_v[df_ma$lineno==14 & df_ma$author=="Canepa" & df_ma$year==2019] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==14 & df_ma$author=="Canepa" & df_ma$year==2019] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==14 & df_ma$author=="Canepa" & df_ma$year==2019]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==14 & df_ma$author=="Canepa" & df_ma$year==2019] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==14 & df_ma$author=="Canepa" & df_ma$year==2019]  <- res1[2,1]+1.96*res1[2,2] 


# 15      15      Cheng 2021    12.0       1 # se to be estimated from plot
O<-c(
  0,
  0.015498527,
  0.017381554,
  0.049089509,
  0.051445722,
  0.120512955,
  0.144182726,
  0.152673913,
  0.168737572,
  0.239548044,
  0.240188676)
E<- c(0.070616114,
      0.073933649,
      0.080094787,
      0.077251185,
      0.083412322,
      0.09478673,
      0.088151659,
      0.104739336,
      0.120379147,
      0.226540284,
      0.120379147)

mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==15 & df_ma$author=="Cheng" & df_ma$year==2021] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==15 & df_ma$author=="Cheng" & df_ma$year==2021] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==15 & df_ma$author=="Cheng" & df_ma$year==2021]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==15 & df_ma$author=="Cheng" & df_ma$year==2021] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==15 & df_ma$author=="Cheng" & df_ma$year==2021]  <- res1[2,1]+1.96*res1[2,2] 


# 31      31     Codina 2023    12.0       1
E <- c(
  0.09214578,
  0.128588337,
  0.154153042,
  0.182634055,
  0.257169151,
  0.330169612,
  0.205029188,
  0.228798471,
  0.290755582,
  0.408180204)
O <- c(
  0.112341772,
  0.107594937,
  0.238924051,
  0.210443038,
  0.242088608,
  0.305379747,
  0.341772152,
  0.340189873,
  0.435126582,
  0.530063291)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==31 & df_ma$author=="Codina" & df_ma$year==2023] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==31 & df_ma$author=="Codina" & df_ma$year==2023] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==31 & df_ma$author=="Codina" & df_ma$year==2023]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==31 & df_ma$author=="Codina" & df_ma$year==2023] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==31 & df_ma$author=="Codina" & df_ma$year==2023]  <- res1[2,1]+1.96*res1[2,2] 



# 41      41    Freitas 2017      12       1      NA   0.5100       NA       NA       NA      0.10 -0.01700       NA       NA        NA
E<- c(4.032258065,
      7.258064516,
      10.64516129,
      18.22580645 )/100
O <- c(0.161290323,
       3.387096774,
       3.064516129,
       7.903225806)/100
mod1 <- lm(O~E)
res1 <- summary(mod1)$coefficients
df_ma$cinter_v[df_ma$lineno==41 & df_ma$author=="Freitas" & df_ma$year==2017] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==41 & df_ma$author=="Freitas" & df_ma$year==2017] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==41 & df_ma$author=="Freitas" & df_ma$year==2017]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==41 & df_ma$author=="Freitas" & df_ma$year==2017] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==41 & df_ma$author=="Freitas" & df_ma$year==2017]  <- res1[2,1]+1.96*res1[2,2] 


# 42      42    Freitas 2017      24       1      NA   0.7100       NA       NA       NA      0.07 -0.05000       NA       NA        NA

E <- c(7.318435754,
       12.68156425,
       18.2122905,
       29.44134078 )/100
O<- c(0.166666667,
      5.166666667,
      6.833333333,
      16.16666667 )/100
mod1 <- lm(O~E)
res1 <- summary(mod1)$coefficients
df_ma$cinter_v[df_ma$lineno==42 & df_ma$author=="Freitas" & df_ma$year==2017] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==42 & df_ma$author=="Freitas" & df_ma$year==2017] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==42 & df_ma$author=="Freitas" & df_ma$year==2017]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==42 & df_ma$author=="Freitas" & df_ma$year==2017] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==42 & df_ma$author=="Freitas" & df_ma$year==2017]  <- res1[2,1]+1.96*res1[2,2] 




# 61      61 Michaels_1 2020    12.0       1 hospit
E <- c(3.8,6.1,8.9,13.3,25.4)/100
O <- c(0.6,3.1,2.9,5.0,21.0)/100
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==61 & df_ma$author=="Michaels_1" & df_ma$year==2020] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==61 & df_ma$author=="Michaels_1" & df_ma$year==2020] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==61 & df_ma$author=="Michaels_1" & df_ma$year==2020]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==61 & df_ma$author=="Michaels_1" & df_ma$year==2020] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==61 & df_ma$author=="Michaels_1" & df_ma$year==2020]  <- res1[2,1]+1.96*res1[2,2] 



# 62      62 Michaels_2 2020    12.0       1 amb
E <- c(10.1,17.9,26.1,33.9,47)/100
O <- c(11.3,22.2,31.4,38.5,54.9)/100
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==62 & df_ma$author=="Michaels_2" & df_ma$year==2020] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==62 & df_ma$author=="Michaels_2" & df_ma$year==2020] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==62 & df_ma$author=="Michaels_2" & df_ma$year==2020]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==62 & df_ma$author=="Michaels_2" & df_ma$year==2020] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==62 & df_ma$author=="Michaels_2" & df_ma$year==2020]  <- res1[2,1]+1.96*res1[2,2] 


# 64      64    Navarta 2018    12.0       1
E <- c(4.1,8.9,12.9,18.2,25.9,41.7)/100
O <- c(8.5,7.3,8.9,21.7,19.5,44.8)/100
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==64 & df_ma$author=="Navarta" & df_ma$year==2018] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==64 & df_ma$author=="Navarta" & df_ma$year==2018] <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==64 & df_ma$author=="Navarta" & df_ma$year==2018]  <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==64 & df_ma$author=="Navarta" & df_ma$year==2018] <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==64 & df_ma$author=="Navarta" & df_ma$year==2018]  <- res1[2,1]+1.96*res1[2,2] 



# 70      70     Pocock 2013    36.0       1
E	<- c(0.108418891,
       0.188911704,
       0.27761807,
       0.392607803,
       0.524024641,
       0.708008214)
O <- c(0.087063655,
       0.17412731,
       0.272689938,
       0.425462012,
       0.553593429,
       0.716221766)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==70 & df_ma$author=="Pocock" & df_ma$year==2013] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==70 & df_ma$author=="Pocock" & df_ma$year==2013]  <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==70 & df_ma$author=="Pocock" & df_ma$year==2013]   <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==70 & df_ma$author=="Pocock" & df_ma$year==2013]  <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==70 & df_ma$author=="Pocock" & df_ma$year==2013]   <- res1[2,1]+1.96*res1[2,2] 


# 74      74       Rich 2018    43.2       1
E <- c(
  0.060159834,
  0.108825416,
  0.171473753,
  0.236679109,
  0.296764946,
  0.355579125,
  0.409284747,
  0.475778205,
  0.541010968,
  0.593494264,
  0.651077895,
  0.706118209,
  0.761166746,
  0.805965282)
O <- c(
  0.02360515,
  0.128755365,
  0.182403433,
  0.238197425,
  0.285407725,
  0.336909871,
  0.388412017,
  0.4527897,
  0.530042918,
  0.624463519,
  0.712446352,
  0.809012876,
  0.912017167,
  0.989270386)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==74 & df_ma$author=="Rich" & df_ma$year==2018] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==74 & df_ma$author=="Rich" & df_ma$year==2018]  <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==74 & df_ma$author=="Rich" & df_ma$year==2018]   <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==74 & df_ma$author=="Rich" & df_ma$year==2018]  <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==74 & df_ma$author=="Rich" & df_ma$year==2018]   <- res1[2,1]+1.96*res1[2,2] 

# 82      82     Sawano 2018    12.0       1
E <- c(0.056283784,
       0.091756757,
       0.133378378,
       0.247364865,
       0.174527027)
O <- c(0.028274007,
       0.052665805,
       0.067761305,
       0.168789257,
       0.091167522)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==82 & df_ma$author=="Sawano" & df_ma$year==2018] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==82 & df_ma$author=="Sawano" & df_ma$year==2018]  <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==82 & df_ma$author=="Sawano" & df_ma$year==2018]   <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==82 & df_ma$author=="Sawano" & df_ma$year==2018]  <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==82 & df_ma$author=="Sawano" & df_ma$year==2018]   <- res1[2,1]+1.96*res1[2,2] 



# 104    104         Xu 2022    12.0       1
E <- c(0.001092896,
       0.005464481,
       0.013114754,
       0.019672131,
       0.038251366,
       0.027322404,
       0.05136612,
       0.069945355,
       0.109289617,
       0.319125683)
O <- c(-0.002197802,
       0.030769231,
       0.027472527,
       0.043956044,
       0.042307692,
       0.020879121,
       0.019230769,
       0.058791209,
       0.050549451,
       0.340659341)
mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==104 & df_ma$author=="Xu" & df_ma$year==2022] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==104 & df_ma$author=="Xu" & df_ma$year==2022]  <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==104 & df_ma$author=="Xu" & df_ma$year==2022]   <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==104 & df_ma$author=="Xu" & df_ma$year==2022]  <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==104 & df_ma$author=="Xu" & df_ma$year==2022]   <- res1[2,1]+1.96*res1[2,2] 


# Tohyama 2021 ACM 12m Is this acceptable as calibration? It is acceptable. They are very likely plotting the means wihtin centiles or similar grouping.
E <- c(
  0.037223323,
  0.069711786,
  0.099738952,
  0.12779026,
  0.152062716,
  0.193233682,
  0.239222577,
  0.286255759,
  0.41368722)
O <- c(
  0.022753906,
  0.027148437,
  0.069140625,
  0.148242187,
  0.123339844,
  0.176074219,
  0.261523438,
  0.2703125,
  0.388476562)

mod1 <- lm(O~E)
(res1 <- summary(mod1)$coefficients)
df_ma$cinter_v[df_ma$lineno==93 & df_ma$author=="Tohyama" & df_ma$year==2021] <- res1[1,1]
df_ma$cinter_SE[df_ma$lineno==93 & df_ma$author=="Tohyama" & df_ma$year==2021]  <- res1[1,2]

#replace OE with slope values
df_ma$oe.est.fromslope[df_ma$lineno==93 & df_ma$author=="Tohyama" & df_ma$year==2021]   <- res1[2,1]
df_ma$oell.est.fromslope[df_ma$lineno==93 & df_ma$author=="Tohyama" & df_ma$year==2021]  <- res1[2,1]-1.96*res1[2,2]
df_ma$oeul.est.fromslope[df_ma$lineno==93 & df_ma$author=="Tohyama" & df_ma$year==2021]   <- res1[2,1]+1.96*res1[2,2] 


###########################################################################################################################
## oe for MA will include oe.est (original or estimated from O and E, except for Barge-Caballero 2019) and oe.est.fromslope
########################################################################################################################

df_ma$oe.est.slope.ma <- df_ma$oe.est 
df_ma$oe.est.slope.ma[is.na(df_ma$oe.est )] <- df_ma$oe.est.fromslope[is.na(df_ma$oe.est )] 

df_ma$oe.est.slope.ma[df_ma$lineno==5 & df_ma$author=="Barge-Caballero" & df_ma$year==2019]  <- 0.7673136 #slope calculated above

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
## oe for MA (using intercept) will include oe.est (original or estimated from O and E, except for Freitas2017) and oe.est.fromint
########################################################################################################################

df_ma$oe.est.inter.ma <- df_ma$oe.est 
df_ma$oe.est.inter.ma[is.na(df_ma$oe.est )] <- df_ma$oe.est.frominter[is.na(df_ma$oe.est )] 

# df_ma$oe.est.inter.ma[df_ma$lineno==41 & df_ma$author=="Freitas" & df_ma$year==2017] <- df_ma$oe.est.frominter[df_ma$lineno==41 & df_ma$author=="Freitas" & df_ma$year==2017]
# df_ma$oe.est.inter.ma[df_ma$lineno==42 & df_ma$author=="Freitas" & df_ma$year==2017] <- df_ma$oe.est.frominter[df_ma$lineno==42 & df_ma$author=="Freitas" & df_ma$year==2017]


# #to check
# subset(df_ma, select=c("lineno","author","year","OErat_v", "oe.est", "oe.est.slope.ma",
#                        "oe.est.fromslope","oell.est.fromslope","oeul.est.fromslope",
#                        "oe.est.frominter","oell.est.frominter","oeul.est.frominter",
#                        "o.est","e.est",
#                        "sample_n_model","sample_e_model"))
# 
# 

# Update S3
table(df_ma$S3,useNA = "always")
#        
#   68   

## These lineno have O and E reported and variability estimated
lineno_oerepvarest <- c(5 ,  6,   7,   8,   9,  29,  30,  35,  56,  58,  66,  67,  81,  91, 100, 101, 102, 103)
lineno_OandE <- c(13,39,40,106)
lineno_inter <- c(1,3,14,15,31,41,42,61,62,64,70,74,82,104,93)

# length(c(lineno_oerepvarest,lineno_OandE,lineno_inter)) #37
# length(lineno_oerepvarest) #18
# length(lineno_OandE) #4
# length(lineno_inter) #15
# 
# df_ma[which(!is.na(df_ma$OErat_v)),c("lineno","author","year","horizon","outcome","OErat_v", "e.est" ,"o.est" , "cinter_v", "oell.est.frominter","oeul.est.frominter","S3", "toestC","cinter_SE")]
# df_ma$lineno[which(!is.na(df_ma$OErat_v))]

df_ma$S3[df_ma$lineno %in% lineno_oerepvarest] <- "OE reported and variability estimated"
df_ma$S3[df_ma$lineno %in% lineno_OandE] <- "O and E reported, OE calculated and variability estimated"
df_ma$S3[df_ma$lineno %in% lineno_inter] <- "Estimates based on intercept of calibration curve" 

# subset(df_ma, select=c("e.est",
#                        "oell.est.frominter","oeul.est.frominter","S3"
# ))

table(df_ma$S3,useNA = "always")
#                                                           Estimates based on intercept of calibration curve          O and E reported, OE calculated and variability estimated 
# 31                                                         15                                                         4 
# OE reported and variability estimated                                                      <NA> 
#   18                                                                                        0 

# df_ma[which(nchar(df_ma$S3)==0),c("lineno","author","year","horizon","outcome","OErat_v", "e.est" ,"o.est" , "cinter_v", "oell.est.frominter","oeul.est.frominter","S3", "toestC","cinter_SE")]

#Update toestC
df_ma$toestCup <- df_ma$toestC
df_ma$toestCup[df_ma$lineno %in% c(lineno_oerepvarest,lineno_OandE,lineno_inter)] <- "Estimates are possible"
df_ma$toestCup[df_ma$lineno %nin% c(lineno_oerepvarest,lineno_OandE,lineno_inter)] <- "Estimates not possible"
#df_ma$toestCup[df_ma$toestC!="Checking other data"] <- df_ma$toestC[df_ma$toestC!="Checking other data"]
#df_ma$toestCup[df_ma$lineno==72 | df_ma$lineno==73] <- "OE reported, variability estimate not possible"

# df_ma[,c("lineno","author","year","horizon","outcome","OErat_v", "e.est" ,"o.est" , "cinter_v", "oell.est.frominter","oeul.est.frominter","S3", "toestC", "toestCup","cinter_SE")]


# # ####### to select subset of studies with data for MAs #######
# msubs_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
#                     citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
#                     data=df_ma, slab = study_id  )
# msubs_all$n      <- df_ma$sample_n_model
# msubs_all$events <- round(df_ma$o.est,0)
# msubs_all$id     <- df_ma$study_id
# msubs_all <- msubs_all[which(!is.na(msubs_all$theta.se)),]
# dim(msubs_all) ## 34
# plot(msubs_all)


####### REPEAT AFTER ANY CHANGES                      #######
####### IF SATISFIED, ADD COVER VARIABLES             #######
####### to select subset of studies with data for MAs #######
msubs_all <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                    citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                    data=df_ma, slab = study_id  )
msubs_all$lineno    <- df_ma$lineno
msubs_all$n         <- df_ma$sample_n_model
msubs_all$oevents    <- round(df_ma$o.est,0)
msubs_all$eevents    <- round(df_ma$e.est,0)
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
# msubs_all$disc      <- df_ma$disc # CHANGE created in cr_disc files
msubs_all$cal       <- df_ma$cal # CHANGE created in cr_calib files
msubs_all$model_f   <- df_ma$model_f
msubs_all$model_p   <- df_ma$model_p
msubs_all$OS1        <- df_ma$OS1
msubs_all$S1        <- df_ma$S1
msubs_all$S2        <- df_ma$S2
# msubs_all$S4        <- df_ma$S4 #AUC vs c-stat (S4) # CHANGE created in cr_disc files
msubs_all$S3        <- df_ma$S3 #Calibration estimate O:E reported and estimated variability vs estimates based on intercept of calibration curve (S3) # CHANGE created in cr_calib files
msubs_all$S5        <- df_ma$S5 #model version
msubs_all$oe.est.inter.ma <- df_ma$oe.est.inter.ma
msubs_all$oell.est.frominter  <- df_ma$oell.est.frominter
msubs_all$oeul.est.frominter <- df_ma$oeul.est.frominter
msubs_all$OErat_v <- df_ma$OErat_v
msubs_all$OErat_l <- df_ma$OErat_l
msubs_all$OErat_u <- df_ma$OErat_u
msubs_all$cinter_v <- df_ma$cinter_v
msubs_all$cinter_SE <- df_ma$cinter_SE
msubs_all$toestC     <- df_ma$toestC
msubs_all$toestCup     <- df_ma$toestCup

msubs_all <- msubs_all[which(!is.na(msubs_all$theta.se)),]

dim(msubs_all) ## 34
# plot(msubs_all)

#msubs_all$lineno[msubs_all$author=="Pavlovskaya"] #66 67

#######  Sensitivity: changes to disc ####### 
# Code received:
# E9 =	Observed outcomes post-intervention compared with pre-intervention predictions 
#       and not clinically relevant or evidence of miscalibration or question about data reliability

# CHANGE
# table(msubs_all$disc)
table(msubs_all$cal)
# - E9 
# 17  17 

table(msubs_all$S3)

# 1.	If you estimate both the mean and its variance 
# replace cal=”E19” if cal !=”E9”
# E19	Estimated both ln(O:E)) and Var(In(O:E)) 
msubs_all$cal[(msubs_all$S3=="Estimates based on intercept of calibration curve"
               | msubs_all$S3=="O and E reported, OE calculated and variability estimated") & (msubs_all$cal!="E9")] <- "E19"
msubs_all$cal[(msubs_all$S3=="Estimates based on intercept of calibration curve"
               | msubs_all$S3=="O and E reported, OE calculated and variability estimated") & (msubs_all$cal=="E9")] <- "E17"

# 
# 2.	If you estimate just the variance
# replace cal=”E8” if cal !=”E9”
# E8	Estimated Var(In(O:E)) only 
msubs_all$cal[msubs_all$S3=="OE reported and variability estimated"
                & (msubs_all$cal!="E9")] <- "E8"
msubs_all$cal[msubs_all$S3=="OE reported and variability estimated" & (msubs_all$cal=="E9")] <- "E17"


# 3.	If you cannot make estimates
# replace cal=”X”
# X	EXclude from meta-analysis

msubs_all$cal[msubs_all$toestCup=="Estimates are not possible"] <- "X"

# NB: In file to send, studies not appearing did not have data for MA

table(msubs_all$S3)
# Estimates based on intercept of calibration curve O and E reported, OE calculated and variability estimated 
# 14                                                         2 
# OE reported and variability estimated 
# 18 
# msubs_all$lineno[msubs_all$S3=="O and E reported, OE calculated and variability estimated"] #13 106
msubs_all$S3[msubs_all$lineno==13 | msubs_all$lineno==106] <- "OE reported and variability estimated"
table(msubs_all$S3)

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
# 
# mcovertab_cal <- subset(msubs_all, select=c("lineno","author",
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
# # "S4", # CHANGE un-comment for discrimination file
# "S3", # CHANGE un-comment for calibration file
# "S5",
# "toestC", "toestCup"))
# 
# # CHANGE NAME OF FINAL FILE
# write.csv(mcovertab_cal, "C:\\Users\\mvazquezmontes\\OneDrive - Nexus365\\Documents\\SHFM and MAGGIC\\Analysis files\\RCode\\results\\MAGGIC_covertable_cal.csv", row.names = FALSE)

df_ma <- df_ma[df_ma$lineno %in% msubs_all$lineno,]
df_ma$cal <- msubs_all$cal # CHANGE to cal for calibration
df_ma$toestCup <- msubs_all$toestCup # CHANGE to cal for calibration
# table(df_ma$cal)
# E17 E19  E8 
# 17   8   9 

