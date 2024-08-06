##### write to excel
## Needs Java installed

###individual results worked out somewhere else

path1 = "C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/SeattleMAGGIC analysis files NEWCOVERCODES/"
path2 = "subscripts/"
path3 = "datasets/"
path4 = "primary analysis/"
path5 = "secondary analysis/"
path6 = "subgroup analysis/"
path7 = "sensitivity analysis/"
path8 = "meta-regression/"

source(paste0(path1, path4, "overall_MAGGIC_ACMapprox_secan_disc.r", echo = TRUE))
source(paste0(path1, path4, "overall_SHFM_ACMapprox_secan_disc.r", echo = TRUE))

source(paste0(path1, path4, "overall_MAGGIC_ACMapprox_secan_calib.r", echo = TRUE))
source(paste0(path1, path4, "overall_SHFM_ACMapprox_secan_calib.r", echo = TRUE))

source(paste0(path1, path6, "subg_SHFM_ACM_2yr_disc_ROB.r", echo = TRUE))
source(paste0(path1, path6, "subg_SHFM_ACM_2yr_disc_HFtype.r", echo = TRUE))
source(paste0(path1, path6, "subg_SHFM_ACM_2yr_disc_cstatauc.r", echo = TRUE))
source(paste0(path1, path6, "subg_SHFM_ACM_2yr_disc_modelv.r", echo = TRUE))

source(paste0(path1, path6, "subg_SHFM_ACM_2yr_calib_ROB.r", echo = TRUE))
source(paste0(path1, path6, "subg_SHFM_ACM_2yr_calib_HFtype.r", echo = TRUE))
source(paste0(path1, path6, "subg_SHFM_ACM_2yr_calib_calibest.r", echo = TRUE))
source(paste0(path1, path6, "subg_SHFM_ACM_2yr_calib_modelv.r", echo = TRUE))

 
setwd("C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/results/")
file.copy("tabletemplate.xlsx","results.xlsx", overwrite=TRUE) # Load workbook
wb <- loadWorkbook("results.xlsx")

setStyleAction(wb, XLC$"STYLE_ACTION.PREDEFINED")

################################
## Row labels MAGGIC
################################
writeWorksheet(wb, "MAGGIC", sheet = "ACM_2yr", startRow = 7,
               startCol = 1, header = FALSE)

writeWorksheet(wb, "Secondary analysis", sheet = "ACM_2yr", startRow = 8,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "All studies", sheet = "ACM_2yr", startRow = 8,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "RoB Participants", sheet = "ACM_2yr", startRow = 9,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "High", sheet = "ACM_2yr", startRow = 9,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Low", sheet = "ACM_2yr", startRow = 10,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear", sheet = "ACM_2yr", startRow = 11,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Type of heart failure", sheet = "ACM_2yr", startRow = 12,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "Chronic", sheet = "ACM_2yr", startRow = 12,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Acute", sheet = "ACM_2yr", startRow = 13,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear/mixed", sheet = "ACM_2yr", startRow = 14,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Model version", sheet = "ACM_2yr", startRow = 15,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "Original", sheet = "ACM_2yr", startRow = 15,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Updated", sheet = "ACM_2yr", startRow = 16,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear", sheet = "ACM_2yr", startRow = 17,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Discrimination measure", sheet = "ACM_2yr", startRow = 18,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "C-statistic", sheet = "ACM_2yr", startRow = 18,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "AUC", sheet = "ACM_2yr", startRow = 19,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Calibration measure", sheet = "ACM_2yr", startRow = 20,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "OE reported and variability estimated", sheet = "ACM_2yr", startRow = 20,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Estimates based on intercept of calibration curve", sheet = "ACM_2yr", startRow = 21,
               startCol = 3, header = FALSE)


################################
## Row labels SHFM
################################
writeWorksheet(wb, "SHFM", sheet = "ACM_2yr", startRow = 26,
               startCol = 1, header = FALSE)

writeWorksheet(wb, "Secondary analysis", sheet = "ACM_2yr", startRow = 27,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "All studies", sheet = "ACM_2yr", startRow = 27,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "RoB Participants", sheet = "ACM_2yr", startRow = 28,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "High", sheet = "ACM_2yr", startRow = 28,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Low", sheet = "ACM_2yr", startRow = 29,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear", sheet = "ACM_2yr", startRow = 30,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Type of heart failure", sheet = "ACM_2yr", startRow = 31,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "Chronic", sheet = "ACM_2yr", startRow = 31,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Acute", sheet = "ACM_2yr", startRow = 32,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear/mixed", sheet = "ACM_2yr", startRow = 33,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Model version", sheet = "ACM_2yr", startRow = 34,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "Levy 2006 ext validation", sheet = "ACM_2yr", startRow = 34,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Levy 2006 PRAISE", sheet = "ACM_2yr", startRow = 35,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Update", sheet = "ACM_2yr", startRow = 36,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Discrimination measure", sheet = "ACM_2yr", startRow = 37,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "C-statistic", sheet = "ACM_2yr", startRow = 37,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "AUC", sheet = "ACM_2yr", startRow = 38,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Calibration measure", sheet = "ACM_2yr", startRow = 39,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "OE reported and variability estimated", sheet = "ACM_2yr", startRow = 39,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Estimates based on intercept of calibration curve", sheet = "ACM_2yr", startRow = 40,
               startCol = 3, header = FALSE)




################################
## Discrimination SHFM   ##########
################################
writeWorksheet(wb, round(resS_disc_ACM_2yr_all,2), sheet = "ACM_2yr", startRow = 27,
               startCol = 4, header = FALSE)

# SHFM ROB
writeWorksheet(wb, t(resS_disc_ACM_2yr_hrob), sheet = "ACM_2yr", startRow = 28,
               startCol = 4, header = FALSE)
writeWorksheet(wb, t(resS_disc_ACM_2yr_lrob), sheet = "ACM_2yr", startRow = 29,
               startCol = 4, header = FALSE)
writeWorksheet(wb, t(resS_disc_ACM_2yr_urob), sheet = "ACM_2yr", startRow = 30,
               startCol = 4, header = FALSE)

# SHFM HFtype
writeWorksheet(wb, round(resS_disc_ACM_2yr_chronic,2), sheet = "ACM_2yr", startRow = 31,
               startCol = 4, header = FALSE)
writeWorksheet(wb, t(resS_disc_ACM_2yr_acute), sheet = "ACM_2yr", startRow = 32,
               startCol = 4, header = FALSE)
writeWorksheet(wb, t(resS_disc_ACM_2yr_uncmixhf), sheet = "ACM_2yr", startRow = 33,
               startCol = 4, header = FALSE)

#SHFM model version
writeWorksheet(wb, round(resS_disc_ACM_2yr_Levy2006extval,2), sheet = "ACM_2yr", startRow = 34,
               startCol = 4, header = FALSE)
writeWorksheet(wb, t(resS_disc_ACM_2yr_Levy2006PRAISE), sheet = "ACM_2yr", startRow = 35,
               startCol = 4, header = FALSE)
writeWorksheet(wb, t(resS_disc_ACM_2yr_updatemod), sheet = "ACM_2yr", startRow = 36,
               startCol = 4, header = FALSE)

#SHFM disc parameter
writeWorksheet(wb, t(resS_disc_ACM_2yr_cstat), sheet = "ACM_2yr", startRow = 37,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_2yr_auc,2), sheet = "ACM_2yr", startRow = 38,
               startCol = 4, header = FALSE)

#SHFM calib parameter - not relevant for summary of discrimination rows 39 and 40







################################
## Calibration SHFM ##########
################################
writeWorksheet(wb, round(resS_cal_ACM_2yr_all,2), sheet = "ACM_2yr", startRow = 27,
               startCol = 11, header = FALSE)

# SHFM ROB
writeWorksheet(wb, round(resS_cal_ACM_2yr_hrob,2), sheet = "ACM_2yr", startRow = 28,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resS_cal_ACM_2yr_lrob,2), sheet = "ACM_2yr", startRow = 29,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_2yr_urob), sheet = "ACM_2yr", startRow = 30,
               startCol = 11, header = FALSE)


# SHFM HFtype
writeWorksheet(wb, round(resS_cal_ACM_2yr_chronic,2), sheet = "ACM_2yr", startRow = 31,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_2yr_acute), sheet = "ACM_2yr", startRow = 32,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_2yr_uncmixhf), sheet = "ACM_2yr", startRow = 33,
               startCol = 11, header = FALSE)

#SHFM model version
writeWorksheet(wb, round(resS_cal_ACM_2yr_Levy2006extval,2), sheet = "ACM_2yr", startRow = 34,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_2yr_Levy2006PRAISE), sheet = "ACM_2yr", startRow = 35,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_2yr_updatemod), sheet = "ACM_2yr", startRow = 36,
               startCol = 11, header = FALSE)

#SHFM disc parameter - not relevant for summary of Calibration rows 37 and 38

#SHFM cal parameter
writeWorksheet(wb, round(resS_cal_ACM_2yr_oerep,2), sheet = "ACM_2yr", startRow = 39,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_2yr_oeestfrominterc), sheet = "ACM_2yr", startRow = 40,
               startCol = 11, header = FALSE)



# Save workbook (this actually writes the file to disk)
saveWorkbook(wb)

