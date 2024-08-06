fn_ma_oe <- function(dat){ 

# ####################################
# 2.1) dataframe for pooled estimate

# df_val  <- df[1:nrow(df),]                          # remove development study?
# calculate the confidence intervals 

oe_all_log <- oecalc(OE=oe.est.inter.ma, OE.cilb=oell.est.frominter, OE.ciub=oeul.est.frominter,
                     citl=cinter_v,citl.se=cinter_SE, N=sample_n_model, E=e.est, O=o.est,
                     data=dat, slab = study_id, g="log(OE)")
oe_all_log <- oe_all_log[which(!is.na(oe_all_log$theta.se)),]   # remove NA SE(OE)

nstudies <- nrow(oe_all_log) 

####################################
# 2.2) Pool the result

# there are two different packages, and they have a few different options for information they offer
fit_REML_all_1 <- rma(yi=theta, sei=theta.se, data=oe_all_log, method="REML", test="knha")
# provides results on the log scale, recalculate like this:
pooled <- exp(c(fit_REML_all_1$beta, fit_REML_all_1$ci.lb,  fit_REML_all_1$ci.ub))


# Predictions and prediction intervals
pred <- predict(fit_REML_all_1)
predint <- exp(c(pred$pi.lb,pred$pi.ub))

## To have all transformed results in one df
fit1 <- matrix(c(pooled,predint,nstudies,fit_REML_all_1$tau2),1,7,
               dimnames = list("Pooled results",c("est","ci.lb","ci.ub","pi.lb","pi.ub","n","tau2")))
fit1 <- data.frame(fit1)

list(weights(fit_REML_all_1),fit1,fit_REML_all_1)

}