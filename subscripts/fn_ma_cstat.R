fn_ma_cstat <- function(dat){
 
  # calculate the confidence intervals
  cstat_all_log <- ccalc(cstat= extva_dis_cstat_val, cstat.se= extva_dis_cstat_se, cstat.cilb=extva_dis_cstat_lci, cstat.ciub = extva_dis_cstat_uci,
                         N = sample_n_model, O= o.est, data=dat, slab=study_id, g="log(cstat/(1-cstat))")
  cstat_all_log <- cstat_all_log[which(!is.na(cstat_all_log$theta.se)),]   # remove NA SE(cstat)
  nstudies  <- nrow(cstat_all_log) 
  
  #NB: ccalc used g="log(cstat/(1-cstat))" because cstat takes values between 0 and 1 (think of AUC)

  ####################################
  # 2.2) Pool the result
 
  # use a function from Thomas' package, which was before:
  #1/(1+exp(-(c(fitc.REML.os$ci.lb, fitc.REML.os$beta, fitc.REML.os$ci.ub)))) # pooled estimate transformed (inverse logit)
  
  # there are two different packages, and they have a few different options for information they offer
  fit_REML_all_1 <- rma(yi=theta, sei=theta.se, data=cstat_all_log, method="REML", test="knha")
  # provides results on the logit scale, recalculate like this:
  pooled <- 1/(1+exp(-(c(fit_REML_all_1$beta, fit_REML_all_1$ci.lb,  fit_REML_all_1$ci.ub))))

  # Predictions and prediction intervals
  pred <- predict(fit_REML_all_1)
  predint <- 1/(1+exp(-(c(pred$pi.lb,pred$pi.ub))))
  
   ## To have all transformed results in one df
  fit1 <- matrix(c(pooled,predint,nstudies,fit_REML_all_1$tau2),1,7,
                 dimnames = list("Pooled results",c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")))
  fit1 <- data.frame(fit1)
  
  list(weights(fit_REML_all_1),fit1, fit_REML_all_1)
}


