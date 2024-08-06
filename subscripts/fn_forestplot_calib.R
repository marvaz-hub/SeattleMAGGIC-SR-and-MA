
fn_forest_calib <- function(df_forest,fit1,titplot,weights_all,tau_all,rmaobject){
    
  df <- df_forest
  res <- rmaobject
  
  N <- df$n
  
  temp <- rep(NA,length(N))
  for (i in 1:length(N)){
    temp[i] <- nchar(N[i])
  }
  maxlengthN <- max(temp)
  
  pos_weight = 1.58 +.20 +.15  # define position of weights 
  pos_event  = 1.67 +.20 +.04 +.2 # define position of no. events
  pos_div    = 1.68 +.20 +.05 +.2# define position of divide
  pos_N      = pos_div + (maxlengthN)/100 +.07 #1.73 # define position of sample size
  pos_CI     = 2.3 +.18 + .55 # define position of CI and graph size
  pos_pred   = -0.5
  #par(cex=0.8, font=1)  
  #y lim - space for rows, rows are more narrow if ylim high
  #rows = number of rows, # validations
  
metafor::forest(df$theta, ci.lb=df$theta.cilb,ci.ub=df$theta.ciub,
                slab = df$id, 
                cex = 1.2,
                rows=c((nrow(df)+2):3), # CHANGE studies within groups
                ylim=(c(-3,nrow(df)+5)),
                xlab = "O:E ratio", pch = 15,
                xlim=(c(-.8,pos_CI-.33)), alim=(c(0.2,1.8)), at=c(0.2,.4,.6,.8,1,1.2,1.4,1.6,1.8),
                refline = 1, 
                ilab=cbind(weights_all, df$events, rep("/", length(df$n)), df$n),
                ilab.xpos=c(pos_weight, pos_event, pos_div, pos_N),
                ilab.pos=c(2,2,2,2),
                                main=titplot
               
                )
#xpos is trying out

#Add pooled estimate
#par(cex = 1.2, font=2)
addpoly(x = fit1$est, ci.lb=fit1$ci.lb, ci.ub=fit1$ci.ub, rows=1, mlab=mlabfun("Overall", res))


#Add prediction interval
text(-0.8,pos_pred-0.5, cex=1.2, font=1, "Estimated prediction interval", pos=4)



if(fit1$pi.ub>1.8){
  Arrows(x0 = fit1$pi.lb, y0 = pos_pred-0.5, x1 = 1.78, y1=pos_pred-0.5,
         lwd=1,
         arr.type="triangle", arr.length= .5 , arr.width = .3, segment = TRUE	)
  segments(x0 = fit1$pi.lb, y0 = pos_pred-0.8, x1 = fit1$pi.lb, y1=pos_pred-0.2)
  } else {
    segments(x0 = fit1$pi.lb, y0 = pos_pred-0.5, x1 = fit1$pi.ub, y1=pos_pred-0.5)
    segments(x0 = fit1$pi.lb, y0 = pos_pred-0.8, x1 = fit1$pi.lb, y1=pos_pred-0.2)
    segments(x0 = fit1$pi.ub, y0 = pos_pred-0.8, x1 = fit1$pi.ub, y1=pos_pred-0.2)
  }

par(cex = 1.2, font=1)
text(pos_CI-.33, pos_pred-0.5, pos=2, paste0("[", sprintf("%.2f",round(fit1$pi.lb,2)), ", ", sprintf("%.2f",round(fit1$pi.ub, 2)), "]"))

par(cex = 1.2, font=2)
text(c(-0.8, pos_weight, pos_event+.03, pos_div+.03, pos_N-0.05, pos_CI-.33), nrow(df)+4 , pos = c(4,2,2,2,2,2) ,
     c("Studies", "% Weight", "Events", "/", "N", "O/E [95% CI]"))
#text(.8,nrow(df)+5.5, pos = 4,titplot,cex = 2)
}









