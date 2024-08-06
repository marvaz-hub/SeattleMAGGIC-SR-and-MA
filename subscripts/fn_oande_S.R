fn_oande_S <- function(df_ma){

  #########################################################################################################
  # Estimating O and E using information in comments68 and 69 and data in "For Maria SHFM calib3.docx" ### 
  #########################################################################################################

  datcal <- subset(df_ma, select=c("lineno", "author", "year", "data_name", "horizon", "outcome", "OErat_v", "OErat_l","OErat_u", "OErat_p",
                                   "cinter_v","cinter_l","cinter_u","cinter_SE", "sample_n_model","sample_e_model", "comments68","comments69"))
  
  
  length(datcal[which(nchar(datcal$comments68)>0 | nchar(datcal$comments69)>0),"lineno"]) #103 studies have comments
  
  datcal <- datcal[which(nchar(datcal$comments68)>0 | nchar(datcal$comments69)>0),] 
  dim(datcal) #103, 18
  
  #write.csv(datcal, "shfmcal_comments.csv", row.names = FALSE)
  
  datcal[,c("lineno")]
  
  datcal[datcal$lineno==5,c("comments68")] #Figure, no data for 1 year but maybe for overall f-up
  #datcal[datcal$lineno==5,c("author", "year","outcome","horizon")]
  #datcal[datcal$author=="Allen",c("lineno","author", "year","outcome","horizon")]
  
  
  datcal[datcal$lineno==10,c("comments69")] #"Reported obs and pred in Fig1A"
  datcal[datcal$lineno==10,c("author", "year","outcome","horizon")]
  df_ma$o.perc[df_ma$lineno==10] <- 7.67/100 
  df_ma$e.perc[df_ma$lineno==10] <- 8.14/100
  
  df_ma$o.perc[df_ma$lineno==11 & df_ma$author=="Assmus" & df_ma$year==2016] <- 0.156406
  df_ma$e.perc[df_ma$lineno==11 & df_ma$author=="Assmus" & df_ma$year==2016] <- 0.12312812
  
  df_ma$o.perc[df_ma$lineno==12 & df_ma$author=="Assmus" & df_ma$year==2016] <- 0.215058
  df_ma$e.perc[df_ma$lineno==12 & df_ma$author=="Assmus" & df_ma$year==2016] <- 0.187188
  
    datcal[datcal$lineno==16,c("comments69")] # paper Blum 2022     **  EXCLUDE in SA **
  datcal[datcal$lineno==16,c("author", "year","outcome","horizon")]
  df_ma$o.perc[df_ma$lineno==16] <- 82/535 #to have percentage of observed values
  df_ma$e.perc[df_ma$lineno==16] <- .132 #86.8/100 #average predicted prob 86.8% +/- 24.9.
  
  datcal[datcal$lineno==24,c("comments69")] 
  df_ma$o.perc[df_ma$lineno==24] <- 9.5/100 
  df_ma$e.perc[df_ma$lineno==24] <- 6/100
  
  datcal[datcal$lineno==33,c("comments69")] 
  df_ma$o.perc[df_ma$lineno==33] <- 4.2/100 
  df_ma$e.perc[df_ma$lineno==33] <- 9/100
  
 
  # datcal[datcal$lineno==53,c("comments69")] #plot Gorodeski_2 2010  ** EXTRACTED BELOW **
  # datcal[datcal$lineno==53,c("author", "year","outcome","horizon")]
  # datcal[datcal$author=="Gorodeski_2",c("lineno","author", "year","outcome","horizon","sample_n_model","sample_e_model")]
  
  
  datcal[datcal$lineno==58,c("comments69")]  
  df_ma$o.perc[df_ma$lineno==58] <- 1.9/100 
  df_ma$e.perc[df_ma$lineno==58] <- 7/100
  
  # datcal[datcal$lineno==60,c("comments68")]  #nothing
  
  datcal[datcal$lineno==62,c("comments69")]  
  df_ma$o.perc[df_ma$lineno==62] <- 8.6/100 
  df_ma$e.perc[df_ma$lineno==62] <- 9.2/100
  
  # datcal[datcal$lineno==74,c("comments68")] #out  
  
  # datcal[datcal$lineno==76,c("comments69")] #nothing  
  
  datcal[datcal$lineno==80,c("comments69")]  
  df_ma$o.perc[df_ma$lineno==80] <- 12.2/100 
  df_ma$e.perc[df_ma$lineno==80] <- 10.4/100
  
  df_ma$o.perc[df_ma$lineno==83 & df_ma$author=="Levy" & df_ma$year==2006] <- 0.257
  df_ma$e.perc[df_ma$lineno==83 & df_ma$author=="Levy" & df_ma$year==2006] <- 0.266
  
  df_ma$o.perc[df_ma$lineno==84 & df_ma$author=="Levy" & df_ma$year==2006] <- 0.440
  df_ma$e.perc[df_ma$lineno==84 & df_ma$author=="Levy" & df_ma$year==2006] <- 0.433
  
  
  datcal[datcal$lineno==88,c("comments68")] #paper Levy_1 2006
  datcal[datcal$lineno==88,c("author", "year","outcome","horizon")]
  datcal[datcal$author=="Levy_1",c("lineno","author", "year","outcome","horizon","sample_n_model","sample_e_model")]
  df_ma$o.perc[df_ma$lineno==88] <- 1-88.5/100 
  df_ma$e.perc[df_ma$lineno==88] <- 1-90.5/100
  
  
  df_ma$o.perc[df_ma$lineno==89 & df_ma$author=="Levy_1" & df_ma$year==2006] <- 0.200
  df_ma$e.perc[df_ma$lineno==89 & df_ma$author=="Levy_1" & df_ma$year==2006] <- 0.176
  
  
  datcal[datcal$lineno==91,c("comments68")]  #paper
  datcal[datcal$lineno==91,c("author", "year","outcome","horizon")]
  datcal[datcal$author=="Levy_2",c("lineno","author", "year","outcome","horizon","data_name","OErat_v","sample_n_model","sample_e_model")]
  df_ma$o.perc[df_ma$lineno==91] <- 1-91.0/100 
  df_ma$e.perc[df_ma$lineno==91] <- 1-90.9/100
  
  df_ma$o.perc[df_ma$lineno==92 & df_ma$author=="Levy_2" & df_ma$year==2006] <- 0.184
  df_ma$e.perc[df_ma$lineno==92 & df_ma$author=="Levy_2" & df_ma$year==2006] <- 0.167
  
  df_ma$o.perc[df_ma$lineno==93 & df_ma$author=="Levy_2" & df_ma$year==2006] <- 0.283
  df_ma$e.perc[df_ma$lineno==93 & df_ma$author=="Levy_2" & df_ma$year==2006] <- 0.232
  
  
  datcal[datcal$lineno==94,c("comments69")]  
  df_ma$o.perc[df_ma$lineno==94] <- 72/100 
  df_ma$e.perc[df_ma$lineno==94] <- 70/100
  
  datcal[datcal$lineno==95,c("comments68")]  #paper
  datcal[datcal$lineno==95,c("author", "year","outcome","horizon")]
  datcal[datcal$author=="Levy_3",c("lineno","author", "year","outcome","horizon","data_name","OErat_v","sample_n_model","sample_e_model")]
  df_ma$o.perc[df_ma$lineno==95 & df_ma$author=="Levy_3" & df_ma$year==2006] <- 1-86.7/100
  df_ma$e.perc[df_ma$lineno==95 & df_ma$author=="Levy_3" & df_ma$year==2006] <- 1-89.6/100
  
  
  datcal[datcal$lineno==96,c("comments69")]  
  df_ma$o.perc[df_ma$lineno==96] <- 48/100 
  df_ma$e.perc[df_ma$lineno==96] <- 51/100
  
  df_ma$o.perc[df_ma$lineno==97 & df_ma$author=="Levy_4" & df_ma$year==2006] <- 0.135
  df_ma$e.perc[df_ma$lineno==97 & df_ma$author=="Levy_4" & df_ma$year==2006] <- 0.135
  
  df_ma$o.perc[df_ma$lineno==98 & df_ma$author=="Levy_4" & df_ma$year==2006] <- 0.203
  df_ma$e.perc[df_ma$lineno==98 & df_ma$author=="Levy_4" & df_ma$year==2006] <- 0.235
  
  df_ma$o.perc[df_ma$lineno==99 & df_ma$author=="Levy_4" & df_ma$year==2006] <-0.282
  df_ma$e.perc[df_ma$lineno==99 & df_ma$author=="Levy_4" & df_ma$year==2006] <- 0.314
  
  df_ma$o.perc[df_ma$lineno==100 & df_ma$author=="Levy_5" & df_ma$year==2006] <- 0.167
  df_ma$e.perc[df_ma$lineno==100 & df_ma$author=="Levy_5" & df_ma$year==2006] <- 0.162
  
  df_ma$o.perc[df_ma$lineno==101 & df_ma$author=="Levy_5" & df_ma$year==2006] <- 0.346
  df_ma$e.perc[df_ma$lineno==101 & df_ma$author=="Levy_5" & df_ma$year==2006] <- 0.277
  
  
  datcal[datcal$lineno==102,c("comments68")]  
  df_ma$o.perc[df_ma$lineno==102] <- 18/100 
  df_ma$e.perc[df_ma$lineno==102] <- 9/100
  
  #     114    Nakayama 2011    12.0       5      NA       NA       NA
  df_ma$o.perc[df_ma$lineno==114] <- .053 
  df_ma$e.perc[df_ma$lineno==114] <- .068
  
  #     115    Nakayama 2011    24.0       5      NA       NA       NA
  df_ma$o.perc[df_ma$lineno==115] <- 0.113
  df_ma$e.perc[df_ma$lineno==115] <- 0.129
  
  #     116    Nakayama 2011    36.0       5      NA       NA       NA
  df_ma$o.perc[df_ma$lineno==116] <- 0.187970013
  df_ma$e.perc[df_ma$lineno==116] <- 0.200454738

  
  # datcal[datcal$lineno==117,c("comments69")]  #nothing
  
  # datcal[datcal$lineno==119,c("comments68")]  #for reporting only
  
  datcal[datcal$lineno==122,c("comments68")]  #paper Pamboukian 2012
  datcal[datcal$lineno==122,c("author", "year","outcome","horizon")]
  datcal[datcal$author=="Pamboukian",c("lineno","author", "year","outcome","horizon","data_name","OErat_v","sample_n_model","sample_e_model")]
  df_ma$o.perc[df_ma$lineno==122] <- (1-.60)
  df_ma$e.perc[df_ma$lineno==122] <- (1-.47)
  
  #     126    Perrotta 2012    12.0   36 and 60 months
  df_ma$o.perc[df_ma$lineno==126] <- 0.104 
  df_ma$e.perc[df_ma$lineno==126] <- 0.105
  
  #     127    Perrotta 2012    24.0       9      NA       NA       NA
  df_ma$o.perc[df_ma$lineno==127] <- 0.190 
  df_ma$e.perc[df_ma$lineno==127] <- 0.202
  
  #     128    Perrotta 2012    60.0       9      NA       NA       NA
  df_ma$o.perc[df_ma$lineno==128] <- 0.410
  df_ma$e.perc[df_ma$lineno==128] <- 0.376
  
  
  datcal[datcal$lineno==133,c("comments68")]
  df_ma$o.perc[df_ma$lineno==133] <- 8/100 
  df_ma$e.perc[df_ma$lineno==133] <- 12/100
  
  datcal[datcal$lineno==136,c("comments68")]
  df_ma$o.perc[df_ma$lineno==136] <- 19/100 
  df_ma$e.perc[df_ma$lineno==136] <- 14.5/100
  
  datcal[datcal$lineno==137,c("comments68")]
  df_ma$o.perc[df_ma$lineno==137] <- 1-74/100 
  df_ma$e.perc[df_ma$lineno==137] <- 1-54/100
  
  # datcal[datcal$lineno==143,c("comments69")] #nothing
  
  
  # datcal[datcal$lineno==148,c("comments69")] #paper Tohyama 2021 ** EXTRACTED IN CR FILE **
  # datcal[datcal$lineno==148,c("author", "year","outcome","horizon")]
  # datcal[datcal$author=="Tohyama",c("lineno","author", "year","outcome","horizon","data_name","OErat_v","sample_n_model","sample_e_model")]
  
  
  # datcal[datcal$lineno==149,c("comments69")] #out
  
  
  datcal[datcal$lineno==155,c("comments69")]
  df_ma$o.perc[df_ma$lineno==155] <- 11/100 
  df_ma$e.perc[df_ma$lineno==155] <- 11/100
  
  # datcal[datcal$lineno==160,c("comments69")] #nothing
  
  #This list of studies is updated below
  lineno_OandE <- c(10:12, 16, 24, 33, 58,62,80,83,84,88,89,91:102,114:116,122,126:128,133,136,137,155)
   
  k <- length(lineno_OandE)
  for(i in 1:k){
    df_ma$oe.est[df_ma$lineno==lineno_OandE[i]] <-  df_ma$o.perc[df_ma$lineno==lineno_OandE[i]]/df_ma$e.perc[df_ma$lineno==lineno_OandE[i]]
    df_ma$o.est[df_ma$lineno==lineno_OandE[i]] <-  df_ma$o.perc[df_ma$lineno==lineno_OandE[i]] * df_ma$sample_n_model[df_ma$lineno==lineno_OandE[i]]
    df_ma$e.est[df_ma$lineno==lineno_OandE[i]] <-  df_ma$e.perc[df_ma$lineno==lineno_OandE[i]] * df_ma$sample_n_model[df_ma$lineno==lineno_OandE[i]]
    
  }
  
  # 8
  datcal[datcal$lineno==8,c("comments68")]
  df_ma$o.est[df_ma$lineno==8] <- 337
  df_ma$e.est[df_ma$lineno==8] <- 258
  df_ma$oe.est[df_ma$lineno==8] <- 337/258
  
  # 19      19      Canepa 2018    12.0       1      NA       NA       NA
  df_ma$o.est[df_ma$lineno==19 & df_ma$author=="Canepa" & df_ma$year==2018] <- 508
  df_ma$e.est[df_ma$lineno==19 & df_ma$author=="Canepa" & df_ma$year==2018] <- 392.633
  df_ma$oe.est[df_ma$lineno==19 & df_ma$author=="Canepa" & df_ma$year==2018] <- 508/392.633
  
  # 36
  datcal[datcal$lineno==36,c("comments68")] #paper Estep 2015    ** EXCLUDE in SA **
  datcal[datcal$lineno==36,c("author", "year","outcome","horizon")]
  datcal[datcal$author=="Estep",c("lineno","author", "year","outcome","horizon","sample_n_model","sample_e_model")]
  df_ma$o.est[df_ma$lineno==36] <- 17 
  df_ma$e.est[df_ma$lineno==36] <- 78 #median predicted number of outcomes 
  df_ma$oe.est[df_ma$lineno==36] <- 17/78
  
  datcal[datcal$lineno==57,c("comments69")] # Haga2012, OE = 7.17 outlier.
  df_ma$o.est[df_ma$lineno==57] <- 43
  df_ma$e.est[df_ma$lineno==57] <- 6
  df_ma$oe.est[df_ma$lineno==57] <- 43/6
  
  ##### add 8, 19, 36, 57 (57 will be exclude in SA, Haga2012 O/E 7.17) to the list with O and E
  lineno_OandE <- sort(c(8, 19, 36, 57, lineno_OandE))
  length(lineno_OandE)
  
  # copy number of observed values in o.est for those that did not have separated information on O and E 
  df_ma$o.est[is.na(df_ma$o.est)] <-  df_ma$sample_e_model[is.na(df_ma$o.est)]
  
  
  # copy OErat_v in oe.est for those that did not have separated information on O and E 
  df_ma$oe.est[is.na(df_ma$oe.est)] <-  df_ma$OErat_v[is.na(df_ma$oe.est)]
  
  # #to check
  # subset(df_ma, select=c("lineno","author","year","outcome","horizon","OErat_v", "oe.est","o.est","e.est",
  #                        "sample_n_model","sample_e_model"))

  
df_ma
}