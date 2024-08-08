##### write to excel
## Needs Java installed

###individual results worked out somewhere else

# path1 = # INSERT PATHWAY TO DIRECTORY CONTAINING THE SUBFOLDERS
# path2 = "subscripts/"
# path3 = "datasets/"
# path4 = "primary analysis/"
# path5 = "secondary analysis/"
# path6 = "subgroup analysis/"
# path7 = "sensitivity analysis/"
# path8 = "meta-regression/"

# 
# source(paste0(path1, path4, "overal_MAGGIC_ACMapprox_secan_disc.r", echo = TRUE))
# source(paste0(path1, path4, "overal_SHFM_ACMapprox_secan_disc.r", echo = TRUE))
# 
# source(paste0(path1, path4, "overal_MAGGIC_ACMapprox_secan_calib.r", echo = TRUE))
# source(paste0(path1, path4, "overal_SHFM_ACMapprox_secan_calib.r", echo = TRUE))
# 
# source(paste0(path1, path6, "subg_MAGGIC_ACM_3yr_disc_ROB.r", echo = TRUE))
# source(paste0(path1, path6, "subg_MAGGIC_ACM_3yr_disc_HFtype.r", echo = TRUE))
# source(paste0(path1, path6, "subg_MAGGIC_ACM_3yr_disc_cstatauc.r", echo = TRUE))
# source(paste0(path1, path6, "subg_MAGGIC_ACM_3yr_disc_modelv.r", echo = TRUE))
# 
# source(paste0(path1, path6, "subg_MAGGIC_ACM_3yr_calib_ROB.r", echo = TRUE))
# source(paste0(path1, path6, "subg_MAGGIC_ACM_3yr_calib_HFtype.r", echo = TRUE))
# source(paste0(path1, path6, "subg_MAGGIC_ACM_3yr_calib_calibest.r", echo = TRUE))
# source(paste0(path1, path6, "subg_MAGGIC_ACM_3yr_calib_modelv.r", echo = TRUE))
# 
# source(paste0(path1, path6, "subg_SHFM_ACM_3yr_disc_ROB.r", echo = TRUE))
# source(paste0(path1, path6, "subg_SHFM_ACM_3yr_disc_HFtype.r", echo = TRUE))
# source(paste0(path1, path6, "subg_SHFM_ACM_3yr_disc_cstatauc.r", echo = TRUE))
# source(paste0(path1, path6, "subg_SHFM_ACM_3yr_disc_modelv.r", echo = TRUE))
# 
# source(paste0(path1, path6, "subg_SHFM_ACM_3yr_calib_ROB.r", echo = TRUE))
# source(paste0(path1, path6, "subg_SHFM_ACM_3yr_calib_HFtype.r", echo = TRUE))
# source(paste0(path1, path6, "subg_SHFM_ACM_3yr_calib_calibest.r", echo = TRUE))
# source(paste0(path1, path6, "subg_SHFM_ACM_3yr_calib_modelv.r", echo = TRUE))

 
resultspath = # INSERT THE PATH WHERE YOU WANT TO SAVE THE TABLES WITH THE OUTPUTS
setwd(resultspath)
file.copy("tabletemplate.xlsx","results.xlsx", overwrite=TRUE) # Load workbook
wb <- loadWorkbook("results.xlsx")

setStyleAction(wb, XLC$"STYLE_ACTION.PREDEFINED")

################################
## Row labels MAGGIC
################################
writeWorksheet(wb, "MAGGIC", sheet = "ACM_3yr", startRow = 7,
               startCol = 1, header = FALSE)

writeWorksheet(wb, "Secondary analysis", sheet = "ACM_3yr", startRow = 8,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "All studies", sheet = "ACM_3yr", startRow = 8,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "RoB Participants", sheet = "ACM_3yr", startRow = 9,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "High", sheet = "ACM_3yr", startRow = 9,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Low", sheet = "ACM_3yr", startRow = 10,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear", sheet = "ACM_3yr", startRow = 11,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Type of heart failure", sheet = "ACM_3yr", startRow = 12,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "Chronic", sheet = "ACM_3yr", startRow = 12,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Acute", sheet = "ACM_3yr", startRow = 13,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear/mixed", sheet = "ACM_3yr", startRow = 14,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Model version", sheet = "ACM_3yr", startRow = 15,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "Original", sheet = "ACM_3yr", startRow = 15,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Updated", sheet = "ACM_3yr", startRow = 16,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear", sheet = "ACM_3yr", startRow = 17,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Discrimination measure", sheet = "ACM_3yr", startRow = 18,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "C-statistic", sheet = "ACM_3yr", startRow = 18,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "AUC", sheet = "ACM_3yr", startRow = 19,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Calibration measure", sheet = "ACM_3yr", startRow = 20,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "OE reported and variability estimated", sheet = "ACM_3yr", startRow = 20,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Estimates based on intercept of calibration curve", sheet = "ACM_3yr", startRow = 21,
               startCol = 3, header = FALSE)


################################
## Row labels SHFM
################################
writeWorksheet(wb, "SHFM", sheet = "ACM_3yr", startRow = 26,
               startCol = 1, header = FALSE)

writeWorksheet(wb, "Secondary analysis", sheet = "ACM_3yr", startRow = 27,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "All studies", sheet = "ACM_3yr", startRow = 27,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "RoB Participants", sheet = "ACM_3yr", startRow = 28,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "High", sheet = "ACM_3yr", startRow = 28,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Low", sheet = "ACM_3yr", startRow = 29,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear", sheet = "ACM_3yr", startRow = 30,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Type of heart failure", sheet = "ACM_3yr", startRow = 31,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "Chronic", sheet = "ACM_3yr", startRow = 31,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Acute", sheet = "ACM_3yr", startRow = 32,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear/mixed", sheet = "ACM_3yr", startRow = 33,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Model version", sheet = "ACM_3yr", startRow = 34,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "Levy 2006 ext validation", sheet = "ACM_3yr", startRow = 34,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Levy 2006 PRAISE", sheet = "ACM_3yr", startRow = 35,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Update", sheet = "ACM_3yr", startRow = 36,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Discrimination measure", sheet = "ACM_3yr", startRow = 37,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "C-statistic", sheet = "ACM_3yr", startRow = 37,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "AUC", sheet = "ACM_3yr", startRow = 38,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Calibration measure", sheet = "ACM_3yr", startRow = 39,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "OE reported and variability estimated", sheet = "ACM_3yr", startRow = 39,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Estimates based on intercept of calibration curve", sheet = "ACM_3yr", startRow = 40,
               startCol = 3, header = FALSE)



################################
## Discrimination MAGGIC ##########
################################
writeWorksheet(wb, round(resM_disc_ACM_3yr_all,2), sheet = "ACM_3yr", startRow = 8,
               startCol = 4, header = FALSE)

# MAGGIC ROB
writeWorksheet(wb, round(resM_disc_ACM_3yr_hrob,2), sheet = "ACM_3yr", startRow = 9,
              startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_3yr_lrob,2), sheet = "ACM_3yr", startRow = 10,
              startCol = 4, header = FALSE)
writeWorksheet(wb, t(resM_disc_ACM_3yr_urob), sheet = "ACM_3yr", startRow = 11,
              startCol = 4, header = FALSE)


# MAGGIC HFtype
writeWorksheet(wb, round(resM_disc_ACM_3yr_chronic,2), sheet = "ACM_3yr", startRow = 12,
              startCol = 4, header = FALSE)
writeWorksheet(wb, t(resM_disc_ACM_3yr_acute), sheet = "ACM_3yr", startRow = 13,
              startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_3yr_uncmixhf,2), sheet = "ACM_3yr", startRow = 14,
              startCol = 4, header = FALSE)

#MAGGIC model version
writeWorksheet(wb, round(resM_disc_ACM_3yr_origmod,2), sheet = "ACM_3yr", startRow = 15,
              startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_3yr_upmod,2), sheet = "ACM_3yr", startRow = 16,
              startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_3yr_uncmod,2), sheet = "ACM_3yr", startRow = 17,
              startCol = 4, header = FALSE)

#MAGGIC disc parameter
writeWorksheet(wb, round(resM_disc_ACM_3yr_cstat,2), sheet = "ACM_3yr", startRow = 18,
              startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_3yr_auc,2), sheet = "ACM_3yr", startRow = 19,
              startCol = 4, header = FALSE)

#MAGGIC calib parameter - not relevant for summary of discrimination rows 20 and 21



################################
## Discrimination SHFM   ##########
################################
writeWorksheet(wb, round(resS_disc_ACM_3yr_all,2), sheet = "ACM_3yr", startRow = 27,
               startCol = 4, header = FALSE)

# SHFM ROB
writeWorksheet(wb, round(resS_disc_ACM_3yr_hrob,2), sheet = "ACM_3yr", startRow = 28,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_3yr_lrob,2), sheet = "ACM_3yr", startRow = 29,
               startCol = 4, header = FALSE)
writeWorksheet(wb, t(resS_disc_ACM_3yr_urob), sheet = "ACM_3yr", startRow = 30,
               startCol = 4, header = FALSE)

# SHFM HFtype
writeWorksheet(wb, round(resS_disc_ACM_3yr_chronic,2), sheet = "ACM_3yr", startRow = 31,
               startCol = 4, header = FALSE)
writeWorksheet(wb, t(resS_disc_ACM_3yr_acute), sheet = "ACM_3yr", startRow = 32,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_3yr_uncmixhf,2), sheet = "ACM_3yr", startRow = 33,
               startCol = 4, header = FALSE)

#SHFM model version
writeWorksheet(wb, round(resS_disc_ACM_3yr_Levy2006extval,2), sheet = "ACM_3yr", startRow = 34,
               startCol = 4, header = FALSE)
writeWorksheet(wb, t(resS_disc_ACM_3yr_Levy2006PRAISE), sheet = "ACM_3yr", startRow = 35,
               startCol = 4, header = FALSE)
writeWorksheet(wb, t(resS_disc_ACM_3yr_updatemod), sheet = "ACM_3yr", startRow = 36,
               startCol = 4, header = FALSE)

#SHFM disc parameter
writeWorksheet(wb, round(resS_disc_ACM_3yr_cstat,2), sheet = "ACM_3yr", startRow = 37,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_3yr_auc,2), sheet = "ACM_3yr", startRow = 38,
               startCol = 4, header = FALSE)

#SHFM calib parameter - not relevant for summary of discrimination rows 39 and 40





################################
## Calibration MAGGIC ##########
################################
writeWorksheet(wb, round(resM_cal_ACM_3yr_all,2), sheet = "ACM_3yr", startRow = 8,
               startCol = 11, header = FALSE)

# MAGGIC ROB
writeWorksheet(wb, round(resM_cal_ACM_3yr_hrob,2), sheet = "ACM_3yr", startRow = 9,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resM_cal_ACM_3yr_lrob,2), sheet = "ACM_3yr", startRow = 10,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resM_cal_ACM_3yr_urob), sheet = "ACM_3yr", startRow = 11,
               startCol = 11, header = FALSE)


# MAGGIC HFtype
writeWorksheet(wb, round(resM_cal_ACM_3yr_chronic,2), sheet = "ACM_3yr", startRow = 12,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resM_cal_ACM_3yr_acute), sheet = "ACM_3yr", startRow = 13,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resM_cal_ACM_3yr_uncmixhf,2), sheet = "ACM_3yr", startRow = 14,
               startCol = 11, header = FALSE)

#MAGGIC model version
writeWorksheet(wb, round(resM_cal_ACM_3yr_origmod,2), sheet = "ACM_3yr", startRow = 15,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resM_cal_ACM_3yr_upmod), sheet = "ACM_3yr", startRow = 16,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resM_cal_ACM_3yr_uncmod,2), sheet = "ACM_3yr", startRow = 17,
               startCol = 11, header = FALSE)

#MAGGIC cal parameter - not relevant for summary of Calibration rows 18 and 19

#MAGGIC cal parameter
writeWorksheet(wb, round(resM_cal_ACM_3yr_oerep,2), sheet = "ACM_3yr", startRow = 20,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resM_cal_ACM_3yr_oeestfrominterc), sheet = "ACM_3yr", startRow = 21,
               startCol = 11, header = FALSE)



################################
## Calibration SHFM ##########
################################
writeWorksheet(wb, round(resS_cal_ACM_3yr_all,2), sheet = "ACM_3yr", startRow = 27,
               startCol = 11, header = FALSE)

# SHFM ROB
writeWorksheet(wb, t(resS_cal_ACM_3yr_hrob), sheet = "ACM_3yr", startRow = 28,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_3yr_lrob), sheet = "ACM_3yr", startRow = 29,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_3yr_urob), sheet = "ACM_3yr", startRow = 30,
               startCol = 11, header = FALSE)


# SHFM HFtype
writeWorksheet(wb, t(resS_cal_ACM_3yr_chronic), sheet = "ACM_3yr", startRow = 31,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_3yr_acute), sheet = "ACM_3yr", startRow = 32,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_3yr_uncmixhf), sheet = "ACM_3yr", startRow = 33,
               startCol = 11, header = FALSE)

#SHFM model version
writeWorksheet(wb, t(resS_cal_ACM_3yr_Levy2006extval), sheet = "ACM_3yr", startRow = 34,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_3yr_Levy2006PRAISE), sheet = "ACM_3yr", startRow = 35,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_3yr_updatemod), sheet = "ACM_3yr", startRow = 36,
               startCol = 11, header = FALSE)

#SHFM disc parameter - not relevant for summary of Calibration rows 37 and 38

#SHFM cal parameter
writeWorksheet(wb, round(resS_cal_ACM_3yr_oerep,2), sheet = "ACM_3yr", startRow = 39,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_3yr_oeestfrominterc), sheet = "ACM_3yr", startRow = 40,
               startCol = 11, header = FALSE)



# Save workbook (this actually writes the file to disk)
saveWorkbook(wb)

