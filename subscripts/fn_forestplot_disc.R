fn_forest_disc <- function(df_forest,fit1,titplot,weights_all,tau_all,rmaobject){
  
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
  par(cex = 1.2, font=2)
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
