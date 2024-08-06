fn_oande_M <- function(df_ma){

#################################################################################
# Estimating O and E using information in comments68 and 69 ###
#################################################################################


datcal <- subset(df_ma, select=c("OErat_v", "OErat_l","OErat_u", "OErat_p",
                                 "cinter_v","cinter_l","cinter_u","cinter_SE", "sample_n_model","sample_e_model", "comments68","comments69"))
datcal <- subset(df_ma, select=c("lineno", "comments68","comments69"))
datcal[which(nchar(datcal$comments68)>0),]

df_ma$o.perc[df_ma$lineno==5 & df_ma$author=="Barge-Caballero" & df_ma$year==2019] <-6.2/100
df_ma$e.perc[df_ma$lineno==5 & df_ma$author=="Barge-Caballero" & df_ma$year==2019] <-10.9/100

df_ma$o.perc[df_ma$lineno==6 & df_ma$author=="Barge-Caballero" & df_ma$year==2019] <-16.7/100
df_ma$e.perc[df_ma$lineno==6 & df_ma$author=="Barge-Caballero" & df_ma$year==2019] <-27.7/100

df_ma$o.perc[df_ma$lineno==7] <- 0.2 
df_ma$e.perc[df_ma$lineno==7] <- 0.206


df_ma$o.perc[df_ma$lineno==8] <- 0.33
df_ma$e.perc[df_ma$lineno==8] <- 0.425


df_ma$o.perc[df_ma$lineno==9] <- 2.21/100
df_ma$e.perc[df_ma$lineno==9] <- (2.21/100)/df_ma$OErat_v[df_ma$lineno==9]


df_ma$o.perc[df_ma$lineno==29] <-  9.5/100
df_ma$e.perc[df_ma$lineno==29] <- 15/100


df_ma$o.perc[df_ma$lineno==30] <-  21.9/100
df_ma$e.perc[df_ma$lineno==30] <- 36.8/100


df_ma$o.perc[df_ma$lineno==35] <-  14.7/100
df_ma$e.perc[df_ma$lineno==35] <- 11.5/100


df_ma$o.perc[df_ma$lineno==39] <-  35/100
df_ma$e.perc[df_ma$lineno==39] <- 43/100


df_ma$o.perc[df_ma$lineno==40] <-  29/100
df_ma$e.perc[df_ma$lineno==40] <- 37/100


df_ma$o.perc[df_ma$lineno==56] <-  8.6/100 
df_ma$e.perc[df_ma$lineno==56] <- 12.2/100


df_ma$o.perc[df_ma$lineno==58] <-  0.31 
df_ma$e.perc[df_ma$lineno==58] <- 0.2728


df_ma$o.perc[df_ma$lineno==66] <-  4/100 
df_ma$e.perc[df_ma$lineno==66] <- 9/100


df_ma$o.perc[df_ma$lineno==67] <-  20.3/100 
df_ma$e.perc[df_ma$lineno==67] <- 22.6/100


df_ma$o.perc[df_ma$lineno==81] <-   39.4/100
df_ma$e.perc[df_ma$lineno==81] <-   36.4/100


df_ma$o.perc[df_ma$lineno==91] <-   30.1/100
df_ma$e.perc[df_ma$lineno==91] <-   26.1/100


df_ma$o.perc[df_ma$lineno==100] <- 11/100  
df_ma$e.perc[df_ma$lineno==100] <- 12/100


df_ma$o.perc[df_ma$lineno==101] <- 24/100  
df_ma$e.perc[df_ma$lineno==101] <- 28/100


df_ma$o.perc[df_ma$lineno==102] <- 15/100  
df_ma$e.perc[df_ma$lineno==102] <- 12/100


df_ma$o.perc[df_ma$lineno==103] <- 29/100  
df_ma$e.perc[df_ma$lineno==103] <- 28/100


lineno_OandE <- c(5,6,7,8,9,29,30,35,39,40,56,58,66,67,81,91,100,101,102,103)
k <- length(lineno_OandE)
for(i in 1:k){
  df_ma$oe.est[df_ma$lineno==lineno_OandE[i]] <-  df_ma$o.perc[df_ma$lineno==lineno_OandE[i]]/df_ma$e.perc[df_ma$lineno==lineno_OandE[i]]
  df_ma$o.est[df_ma$lineno==lineno_OandE[i]] <-  df_ma$o.perc[df_ma$lineno==lineno_OandE[i]] * df_ma$sample_n_model[df_ma$lineno==lineno_OandE[i]]
  df_ma$e.est[df_ma$lineno==lineno_OandE[i]] <-  df_ma$e.perc[df_ma$lineno==lineno_OandE[i]] * df_ma$sample_n_model[df_ma$lineno==lineno_OandE[i]]
}

df_ma$e.est[df_ma$lineno==lineno_OandE[i]]

df_ma$o.est[df_ma$lineno==106] <- 33.5  
df_ma$e.est[df_ma$lineno==106] <- 29.5


# 13      13     Canepa 2018    12.0       1
df_ma$o.est[df_ma$lineno==13 & df_ma$author=="Canepa" & df_ma$year==2018] <- 508
df_ma$e.est[df_ma$lineno==13 & df_ma$author=="Canepa" & df_ma$year==2018] <- 672.6505
df_ma$oe.est[df_ma$lineno==13 & df_ma$author=="Canepa" & df_ma$year==2018] <- 508/672.6505

# add 13 and 106 to the list with O and E
lineno_OandE <- c(5,6,7,8,9,13,29,30,35,39,40,56,58,66,67,81,91,100,101,102,103,106)

# copy number of observed values in o.est for those that did not have separated information on O and E 
df_ma$o.est[is.na(df_ma$o.est)] <-  df_ma$sample_e_model[is.na(df_ma$o.est)]

# copy OErat_v in oe.est for those that did not have separated information on O and E 
df_ma$oe.est[is.na(df_ma$oe.est)] <-  df_ma$OErat_v[is.na(df_ma$oe.est)]

#to check
# subset(df_ma, select=c("lineno","author","year","OErat_v", "oe.est","o.est","e.est",
#                         "sample_n_model","sample_e_model"))

df_ma
}