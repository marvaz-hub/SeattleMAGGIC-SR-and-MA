##### write to excel
## Needs Java installed

###individual results worked out somewhere else

path1 = # INSERT PATHWAY TO DIRECTORY CONTAINING THE SUBFOLDERS
path2 = "subscripts/"
path3 = "datasets/"
path4 = "primary analysis/"
path5 = "secondary analysis/"
path6 = "subgroup analysis/"
path7 = "sensitivity analysis/"
path8 = "meta-regression/"


 source(paste0(path1, path8, "MR _MAGGIC_ACM_1yr_disc.r", echo = TRUE))
 source(paste0(path1, path8, "MR _MAGGIC_ACM_1yr_cal.r", echo = TRUE))
 source(paste0(path1, path8, "MR _SHFM_ACM_1yr_disc.r", echo = TRUE))
 source(paste0(path1, path8, "MR _SHFM_ACM_1yr_cal.r", echo = TRUE))

 source(paste0(path1, path8, "MR _MAGGIC_disc.r", echo = TRUE))
 source(paste0(path1, path8, "MR _MAGGIC_cal.r", echo = TRUE))
 source(paste0(path1, path8, "MR _SHFM_disc.r", echo = TRUE))
 source(paste0(path1, path8, "MR _SHFM_cal.r", echo = TRUE))

resultspath = # INSERT THE PATH WHERE YOU WANT TO SAVE THE TABLES WITH THE OUTPUTS
setwd(resultspath)
file.copy("mrtemplate.xlsx","mrresults.xlsx", overwrite=TRUE) # Load workbook 
wb <- loadWorkbook("mrresults.xlsx")

setStyleAction(wb, XLC$"STYLE_ACTION.PREDEFINED")

#################################################################################
### Meta regression for 1yr outcomes, using estimates for ACM and approx. ACM ###
#################################################################################

################################
##  MAGGIC discrimination
################################
x <- resM_disc_ACM_1yr_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_1yr",
               startRow = 5, startCol = 2,
               header = FALSE)
x <- resM_disc_ACM_1yr_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_1yr",
               startRow = 6, startCol = 2,
               header = FALSE)

x <- resM_disc_ACM_1yr_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACM_1yr",
               startRow = 5, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACM_1yr",
               startRow = 6, startCol = 4,
               header = FALSE)


################################
##  SHFM discrimination
################################
x <- resS_disc_ACM_1yr_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_1yr",
               startRow = 9, startCol = 2,
               header = FALSE)
x <- resS_disc_ACM_1yr_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_1yr",
               startRow = 10, startCol = 2,
               header = FALSE)

x <- resS_disc_ACM_1yr_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACM_1yr",
               startRow = 9, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACM_1yr",
               startRow = 10, startCol = 4,
               header = FALSE)


################################
##  MAGGIC calibration
################################
x <- resM_cal_ACM_1yr_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_1yr",
               startRow = 5, startCol = 6,
               header = FALSE)
x <- resM_cal_ACM_1yr_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_1yr",
               startRow = 6, startCol = 6,
               header = FALSE)

x <- resM_cal_ACM_1yr_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACM_1yr",
               startRow = 5, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACM_1yr",
               startRow = 6, startCol = 8,
               header = FALSE)


################################
##  SHFM calibration
################################
x <- resS_cal_ACM_1yr_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_1yr",
               startRow = 9, startCol = 6,
               header = FALSE)
x <- resS_cal_ACM_1yr_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_1yr",
               startRow = 10, startCol = 6,
               header = FALSE)

x <- resS_cal_ACM_1yr_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACM_1yr",
               startRow = 9, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACM_1yr",
               startRow = 10, startCol = 8,
               header = FALSE)




#################################################################################
### 4. Meta regression by horizon, using estimates for ACM and approx. ACM ###
#################################################################################

################################
##  MAGGIC discrimination
################################
x <- resM_disc_ACM_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                    sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM",
               startRow = 5, startCol = 2,
               header = FALSE)
x <- resM_disc_ACM_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                    sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM",
               startRow = 6, startCol = 2,
               header = FALSE)
x <- resM_disc_ACM_univmr_hor
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM",
               startRow = 7, startCol = 2,
               header = FALSE)



x <- resM_disc_ACM_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                    sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACM",
               startRow = 5, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                    sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACM",
               startRow = 6, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[4],4)), " (", sprintf("%.4f",round(x$ci.lb[[4]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[4]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[4]], 3)))),
               sheet = "ACM",
               startRow = 7, startCol = 4,
               header = FALSE)


################################
##  SHFM discrimination
################################
x <- resS_disc_ACM_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM",
               startRow = 9, startCol = 2,
               header = FALSE)
x <- resS_disc_ACM_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM",
               startRow = 10, startCol = 2,
               header = FALSE)
x <- resS_disc_ACM_univmr_hor
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM",
               startRow = 11, startCol = 2,
               header = FALSE)

x <- resS_disc_ACM_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACM",
               startRow = 9, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACM",
               startRow = 10, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[4],4)), " (", sprintf("%.4f",round(x$ci.lb[[4]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[4]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[4]], 3)))),
               sheet = "ACM",
               startRow = 11, startCol = 4,
               header = FALSE)


################################
##  MAGGIC calibration
################################
x <- resM_cal_ACM_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM",
               startRow = 5, startCol = 6,
               header = FALSE)
x <- resM_cal_ACM_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM",
               startRow = 6, startCol = 6,
               header = FALSE)
x <- resM_cal_ACM_univmr_hor
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM",
               startRow = 7, startCol = 6,
               header = FALSE)

x <- resM_cal_ACM_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACM",
               startRow = 5, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACM",
               startRow = 6, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[4],4)), " (", sprintf("%.4f",round(x$ci.lb[[4]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[4]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[4]], 3)))),
               sheet = "ACM",
               startRow = 7, startCol = 8,
               header = FALSE)

################################
##  SHFM calibration
################################
x <- resS_cal_ACM_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM",
               startRow = 9, startCol = 6,
               header = FALSE)
x <- resS_cal_ACM_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM",
               startRow = 10, startCol = 6,
               header = FALSE)
x <- resS_cal_ACM_univmr_hor
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM",
               startRow = 11, startCol = 6,
               header = FALSE)

x <- resS_cal_ACM_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACM",
               startRow = 9, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACM",
               startRow = 10, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[4],4)), " (", sprintf("%.4f",round(x$ci.lb[[4]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[4]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[4]], 3)))),
               sheet = "ACM",
               startRow = 11, startCol = 8,
               header = FALSE)

##########################################################################################
### 5. Meta regression by horizon, using estimates for ACM, approx. ACM and composites ###
##########################################################################################

################################
##  MAGGIC discrimination
################################
x <- resM_disc_ACMcomp_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACMcomp",
               startRow = 5, startCol = 2,
               header = FALSE)
x <- resM_disc_ACMcomp_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACMcomp",
               startRow = 6, startCol = 2,
               header = FALSE)
x <- resM_disc_ACMcomp_univmr_hor
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACMcomp",
               startRow = 7, startCol = 2,
               header = FALSE)



x <- resM_disc_ACMcomp_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACMcomp",
               startRow = 5, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACMcomp",
               startRow = 6, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[4],4)), " (", sprintf("%.4f",round(x$ci.lb[[4]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[4]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[4]], 3)))),
               sheet = "ACMcomp",
               startRow = 7, startCol = 4,
               header = FALSE)


################################
##  SHFM discrimination
################################
x <- resS_disc_ACMcomp_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACMcomp",
               startRow = 9, startCol = 2,
               header = FALSE)
x <- resS_disc_ACMcomp_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACMcomp",
               startRow = 10, startCol = 2,
               header = FALSE)
x <- resS_disc_ACMcomp_univmr_hor
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACMcomp",
               startRow = 11, startCol = 2,
               header = FALSE)

x <- resS_disc_ACMcomp_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACMcomp",
               startRow = 9, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACMcomp",
               startRow = 10, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[4],4)), " (", sprintf("%.4f",round(x$ci.lb[[4]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[4]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[4]], 3)))),
               sheet = "ACMcomp",
               startRow = 11, startCol = 4,
               header = FALSE)


################################
##  MAGGIC calibration
################################
x <- resM_cal_ACMcomp_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACMcomp",
               startRow = 5, startCol = 6,
               header = FALSE)
x <- resM_cal_ACMcomp_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACMcomp",
               startRow = 6, startCol = 6,
               header = FALSE)
x <- resM_cal_ACMcomp_univmr_hor
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACMcomp",
               startRow = 7, startCol = 6,
               header = FALSE)

x <- resM_cal_ACMcomp_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACMcomp",
               startRow = 5, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACMcomp",
               startRow = 6, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[4],4)), " (", sprintf("%.4f",round(x$ci.lb[[4]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[4]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[4]], 3)))),
               sheet = "ACMcomp",
               startRow = 7, startCol = 8,
               header = FALSE)

################################
##  SHFM calibration
################################
x <- resS_cal_ACMcomp_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACMcomp",
               startRow = 9, startCol = 6,
               header = FALSE)
x <- resS_cal_ACMcomp_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACMcomp",
               startRow = 10, startCol = 6,
               header = FALSE)
x <- resS_cal_ACMcomp_univmr_hor
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACMcomp",
               startRow = 11, startCol = 6,
               header = FALSE)

x <- resS_cal_ACMcomp_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACMcomp",
               startRow = 9, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACMcomp",
               startRow = 10, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[4],4)), " (", sprintf("%.4f",round(x$ci.lb[[4]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[4]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[4]], 3)))),
               sheet = "ACMcomp",
               startRow = 11, startCol = 8,
               header = FALSE)


#################################################################################
### 6.	Sensitivity analysis: meta regression by horizon, using estimates of ACM
### and approx. ACM but exclude estimates if horiz==”E19” or horiz==“E20”
#################################################################################

################################
##  MAGGIC discrimination
################################
x <- resM_disc_ACM_nonestHor_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 5, startCol = 2,
               header = FALSE)
x <- resM_disc_ACM_nonestHor_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 6, startCol = 2,
               header = FALSE)
x <- resM_disc_ACM_nonestHor_univmr_hor
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 7, startCol = 2,
               header = FALSE)



x <- resM_disc_ACM_nonestHor_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 5, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 6, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[4],4)), " (", sprintf("%.4f",round(x$ci.lb[[4]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[4]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[4]], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 7, startCol = 4,
               header = FALSE)


################################
##  SHFM discrimination
################################
x <- resS_disc_ACM_nonestHor_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 9, startCol = 2,
               header = FALSE)
x <- resS_disc_ACM_nonestHor_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 10, startCol = 2,
               header = FALSE)
x <- resS_disc_ACM_nonestHor_univmr_hor
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 11, startCol = 2,
               header = FALSE)

x <- resS_disc_ACM_nonestHor_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 9, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 10, startCol = 4,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[4],4)), " (", sprintf("%.4f",round(x$ci.lb[[4]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[4]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[4]], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 11, startCol = 4,
               header = FALSE)


################################
##  MAGGIC calibration
################################
x <- resM_cal_ACM_nonestHor_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 5, startCol = 6,
               header = FALSE)
x <- resM_cal_ACM_nonestHor_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 6, startCol = 6,
               header = FALSE)
x <- resM_cal_ACM_nonestHor_univmr_hor
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 7, startCol = 6,
               header = FALSE)

x <- resM_cal_ACM_nonestHor_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 5, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 6, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[4],4)), " (", sprintf("%.4f",round(x$ci.lb[[4]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[4]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[4]], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 7, startCol = 8,
               header = FALSE)

################################
##  SHFM calibration
################################
x <- resS_cal_ACM_nonestHor_univmr_year
writeWorksheet(wb,t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                      sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 9, startCol = 6,
               header = FALSE)
x <- resS_cal_ACM_nonestHor_univmr_outc
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 10, startCol = 6,
               header = FALSE)
x <- resS_cal_ACM_nonestHor_univmr_hor
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[2],4)), ", ", sprintf("%.4f",round(x$ci.ub[2], 4)), ")"),
                       sprintf("%.3f",round(x$pval[2], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 11, startCol = 6,
               header = FALSE)

x <- resS_cal_ACM_nonestHor_mr
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[2],4)), " (", sprintf("%.4f",round(x$ci.lb[[2]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[2]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[2]], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 9, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[3],4)), " (", sprintf("%.4f",round(x$ci.lb[[3]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[3]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[3]], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 10, startCol = 8,
               header = FALSE)
writeWorksheet(wb, t(c(paste0(sprintf("%.4f",round(x$beta[4],4)), " (", sprintf("%.4f",round(x$ci.lb[[4]],4)), ", ", sprintf("%.4f",round(x$ci.ub[[4]], 4)), ")"),
                       sprintf("%.3f",round(x$pval[[4]], 3)))),
               sheet = "ACM_nonestHor",
               startRow = 11, startCol = 8,
               header = FALSE)


# Save workbook (this actually writes the file to disk)
saveWorkbook(wb)
