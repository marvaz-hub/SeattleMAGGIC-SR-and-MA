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


source(paste0(path1, path4, "overall_MAGGIC_ACMapprox_1yr_disc.r", echo = TRUE))
source(paste0(path1, path4, "overall_SHFM_ACMapprox_1yr_disc.r", echo = TRUE))

source(paste0(path1, path4, "overall_MAGGIC_ACMapprox_1yr_calib.r", echo = TRUE))
source(paste0(path1, path4, "overall_SHFM_ACMapprox_1yr_calib.r", echo = TRUE))

source(paste0(path1, path6, "subg_MAGGIC_ACM_1yr_disc_ROB.r", echo = TRUE))
source(paste0(path1, path6, "subg_MAGGIC_ACM_1yr_disc_HFtype.r", echo = TRUE))
source(paste0(path1, path6, "subg_MAGGIC_ACM_1yr_disc_cstatauc.r", echo = TRUE))
source(paste0(path1, path6, "subg_MAGGIC_ACM_1yr_disc_modelv.r", echo = TRUE))

source(paste0(path1, path6, "subg_MAGGIC_ACM_1yr_calib_ROB.r", echo = TRUE))
source(paste0(path1, path6, "subg_MAGGIC_ACM_1yr_calib_HFtype.r", echo = TRUE))
source(paste0(path1, path6, "subg_MAGGIC_ACM_1yr_calib_calibest.r", echo = TRUE))
source(paste0(path1, path6, "subg_MAGGIC_ACM_1yr_calib_modelv.r", echo = TRUE))

source(paste0(path1, path6, "subg_SHFM_ACM_1yr_disc_ROB.r", echo = TRUE))
source(paste0(path1, path6, "subg_SHFM_ACM_1yr_disc_HFtype.r", echo = TRUE))
source(paste0(path1, path6, "subg_SHFM_ACM_1yr_disc_cstatauc.r", echo = TRUE))
source(paste0(path1, path6, "subg_SHFM_ACM_1yr_disc_modelv.r", echo = TRUE))

source(paste0(path1, path6, "subg_SHFM_ACM_1yr_calib_ROB.r", echo = TRUE))
source(paste0(path1, path6, "subg_SHFM_ACM_1yr_calib_HFtype.r", echo = TRUE))
source(paste0(path1, path6, "subg_SHFM_ACM_1yr_calib_calibest.r", echo = TRUE))
source(paste0(path1, path6, "subg_SHFM_ACM_1yr_calib_modelv.r", echo = TRUE))


source(paste0(path1, path7, "cover_MAGGIC_ACM_1yr_disc.r", echo = TRUE))
source(paste0(path1, path7, "cover_MAGGIC_ACM_1yr_calib.r", echo = TRUE))
source(paste0(path1, path7, "cover_SHFM_ACM_1yr_disc.r", echo = TRUE))
source(paste0(path1, path7, "cover_SHFM_ACM_1yr_calib.r", echo = TRUE))
 
setwd("C:/Users/mvazquezmontes/OneDrive - Nexus365/Documents/SHFM and MAGGIC/Analysis files/RCode/results/")
file.copy("tabletemplate.xlsx","results.xlsx", overwrite=TRUE) # Load workbook 
wb <- loadWorkbook("results.xlsx")

setStyleAction(wb, XLC$"STYLE_ACTION.PREDEFINED")

################################
## Row labels MAGGIC
################################
writeWorksheet(wb, "MAGGIC", sheet = "ACM_1yr", startRow = 7,
               startCol = 1, header = FALSE)

writeWorksheet(wb, "Primary analysis", sheet = "ACM_1yr", startRow = 8,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "All studies", sheet = "ACM_1yr", startRow = 8,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "RoB Participants", sheet = "ACM_1yr", startRow = 9,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "High", sheet = "ACM_1yr", startRow = 9,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Low", sheet = "ACM_1yr", startRow = 10,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear", sheet = "ACM_1yr", startRow = 11,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Type of heart failure", sheet = "ACM_1yr", startRow = 12,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "Chronic", sheet = "ACM_1yr", startRow = 12,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Acute", sheet = "ACM_1yr", startRow = 13,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear/mixed", sheet = "ACM_1yr", startRow = 14,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Model version", sheet = "ACM_1yr", startRow = 15,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "Original", sheet = "ACM_1yr", startRow = 15,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Updated", sheet = "ACM_1yr", startRow = 16,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear", sheet = "ACM_1yr", startRow = 17,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Discrimination measure", sheet = "ACM_1yr", startRow = 18,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "C-statistic", sheet = "ACM_1yr", startRow = 18,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "AUC", sheet = "ACM_1yr", startRow = 19,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Calibration measure", sheet = "ACM_1yr", startRow = 20,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "OE reported and variability estimated", sheet = "ACM_1yr", startRow = 20,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Estimates based on intercept of calibration curve", sheet = "ACM_1yr", startRow = 21,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Sensitivity analysis", sheet = "ACM_1yr", startRow = 22,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "High RoB on any domain", sheet = "ACM_1yr", startRow = 22,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "High COVER departure", sheet = "ACM_1yr", startRow = 23,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Medium or high COVER departure", sheet = "ACM_1yr", startRow = 24,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Medium or high COVER departure (ignoring E13)", sheet = "ACM_1yr", startRow = 25,
               startCol = 3, header = FALSE)


################################
## Row labels SHFM
################################
writeWorksheet(wb, "SHFM", sheet = "ACM_1yr", startRow = 26,
               startCol = 1, header = FALSE)

writeWorksheet(wb, "Primary analysis", sheet = "ACM_1yr", startRow = 27,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "All studies", sheet = "ACM_1yr", startRow = 27,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "RoB Participants", sheet = "ACM_1yr", startRow = 28,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "High", sheet = "ACM_1yr", startRow = 28,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Low", sheet = "ACM_1yr", startRow = 29,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear", sheet = "ACM_1yr", startRow = 30,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Type of heart failure", sheet = "ACM_1yr", startRow = 31,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "Chronic", sheet = "ACM_1yr", startRow = 31,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Acute", sheet = "ACM_1yr", startRow = 32,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Unclear/mixed", sheet = "ACM_1yr", startRow = 33,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Model version", sheet = "ACM_1yr", startRow = 34,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "Levy 2006 ext validation", sheet = "ACM_1yr", startRow = 34,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Levy 2006 PRAISE", sheet = "ACM_1yr", startRow = 35,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Update", sheet = "ACM_1yr", startRow = 36,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Discrimination measure", sheet = "ACM_1yr", startRow = 37,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "C-statistic", sheet = "ACM_1yr", startRow = 37,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "AUC", sheet = "ACM_1yr", startRow = 38,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Calibration measure", sheet = "ACM_1yr", startRow = 39,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "OE reported and variability estimated", sheet = "ACM_1yr", startRow = 39,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Estimates based on intercept of calibration curve", sheet = "ACM_1yr", startRow = 40,
               startCol = 3, header = FALSE)

writeWorksheet(wb, "Sensitivity analysis excluding:", sheet = "ACM_1yr", startRow = 41,
               startCol = 2, header = FALSE)
writeWorksheet(wb, "High RoB on any domain", sheet = "ACM_1yr", startRow = 41,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "High COVER departure", sheet = "ACM_1yr", startRow = 42,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Medium or high COVER departure", sheet = "ACM_1yr", startRow = 43,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Medium or high COVER departure (ignoring E13)", sheet = "ACM_1yr", startRow = 44,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Estimates with negative CI", sheet = "ACM_1yr", startRow = 45,
               startCol = 3, header = FALSE)
writeWorksheet(wb, "Outlier (Haga 2012)", sheet = "ACM_1yr", startRow = 46,
               startCol = 3, header = FALSE)

################################
## Discrimination MAGGIC ##########
################################
writeWorksheet(wb, round(resM_disc_ACM_1yr_all,2), sheet = "ACM_1yr", startRow = 8,
               startCol = 4, header = FALSE)

# MAGGIC ROB
writeWorksheet(wb, round(resM_disc_ACM_1yr_hrob,2), sheet = "ACM_1yr", startRow = 9,
              startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_1yr_lrob,2), sheet = "ACM_1yr", startRow = 10,
              startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_1yr_urob,2), sheet = "ACM_1yr", startRow = 11, 
              startCol = 4, header = FALSE)

 
# MAGGIC HFtype
writeWorksheet(wb, round(resM_disc_ACM_1yr_chronic,2), sheet = "ACM_1yr", startRow = 12,
              startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_1yr_acute,2), sheet = "ACM_1yr", startRow = 13,
              startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_1yr_uncmixhf,2), sheet = "ACM_1yr", startRow = 14,
              startCol = 4, header = FALSE)

#MAGGIC model version
writeWorksheet(wb, round(resM_disc_ACM_1yr_origmod,2), sheet = "ACM_1yr", startRow = 15,
              startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_1yr_upmod,2), sheet = "ACM_1yr", startRow = 16,
              startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_1yr_uncmod,2), sheet = "ACM_1yr", startRow = 17,
              startCol = 4, header = FALSE)

#MAGGIC disc parameter
writeWorksheet(wb, round(resM_disc_ACM_1yr_cstat,2), sheet = "ACM_1yr", startRow = 18,
              startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_1yr_auc,2), sheet = "ACM_1yr", startRow = 19,
              startCol = 4, header = FALSE)

#MAGGIC calib parameter - not relevant for summary of discrimination rows 20 and 21

#MAGGIC sensitivity analysis
writeWorksheet(wb, round(resM_disc_ACM_1yr_cover1,2), sheet = "ACM_1yr", startRow = 22,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_1yr_cover2,2), sheet = "ACM_1yr", startRow = 23,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_1yr_cover3,2), sheet = "ACM_1yr", startRow = 24,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resM_disc_ACM_1yr_cover4,2), sheet = "ACM_1yr", startRow = 25,
               startCol = 4, header = FALSE)


################################
## Discrimination SHFM   ##########
################################
writeWorksheet(wb, round(resS_disc_ACM_1yr_all,2), sheet = "ACM_1yr", startRow = 27,
               startCol = 4, header = FALSE)

# SHFM ROB
writeWorksheet(wb, round(resS_disc_ACM_1yr_hrob,2), sheet = "ACM_1yr", startRow = 28,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_1yr_lrob,2), sheet = "ACM_1yr", startRow = 29,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_1yr_urob,2), sheet = "ACM_1yr", startRow = 30, 
               startCol = 4, header = FALSE)

# SHFM HFtype
writeWorksheet(wb, round(resS_disc_ACM_1yr_chronic,2), sheet = "ACM_1yr", startRow = 31,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_1yr_acute,2), sheet = "ACM_1yr", startRow = 32,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_1yr_uncmixhf,2), sheet = "ACM_1yr", startRow = 33,
               startCol = 4, header = FALSE)

#SHFM model version
writeWorksheet(wb, round(resS_disc_ACM_1yr_Levy2006extval,2), sheet = "ACM_1yr", startRow = 34,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_1yr_Levy2006PRAISE,2), sheet = "ACM_1yr", startRow = 35,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_1yr_updatemod,2), sheet = "ACM_1yr", startRow = 36,
               startCol = 4, header = FALSE)

#SHFM disc parameter
writeWorksheet(wb, round(resS_disc_ACM_1yr_cstat,2), sheet = "ACM_1yr", startRow = 37,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_1yr_auc,2), sheet = "ACM_1yr", startRow = 38,
               startCol = 4, header = FALSE)

#SHFM calib parameter - not relevant for summary of discrimination rows 39 and 40

#SHFM sensitivity analysis
writeWorksheet(wb, t(resS_disc_ACM_1yr_cover1), sheet = "ACM_1yr", startRow = 41,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_1yr_cover2,2), sheet = "ACM_1yr", startRow = 42,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_1yr_cover3,2), sheet = "ACM_1yr", startRow = 43,
               startCol = 4, header = FALSE)
writeWorksheet(wb, round(resS_disc_ACM_1yr_cover4,2), sheet = "ACM_1yr", startRow = 44,
               startCol = 4, header = FALSE)



################################
## Calibration MAGGIC ##########
################################
writeWorksheet(wb, round(resM_cal_ACM_1yr_all,2), sheet = "ACM_1yr", startRow = 8,
               startCol = 11, header = FALSE)

# MAGGIC ROB
writeWorksheet(wb, round(resM_cal_ACM_1yr_hrob,2), sheet = "ACM_1yr", startRow = 9,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resM_cal_ACM_1yr_lrob,2), sheet = "ACM_1yr", startRow = 10,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resM_cal_ACM_1yr_urob), sheet = "ACM_1yr", startRow = 11, 
               startCol = 11, header = FALSE)


# MAGGIC HFtype
writeWorksheet(wb, round(resM_cal_ACM_1yr_chronic,2), sheet = "ACM_1yr", startRow = 12,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resM_cal_ACM_1yr_acute,2), sheet = "ACM_1yr", startRow = 13,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resM_cal_ACM_1yr_uncmixhf,2), sheet = "ACM_1yr", startRow = 14,
               startCol = 11, header = FALSE)

#MAGGIC model version
writeWorksheet(wb, round(resM_cal_ACM_1yr_origmod,2), sheet = "ACM_1yr", startRow = 15,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resM_cal_ACM_1yr_upmod,2), sheet = "ACM_1yr", startRow = 16,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resM_cal_ACM_1yr_uncmod,2), sheet = "ACM_1yr", startRow = 17,
               startCol = 11, header = FALSE)

#MAGGIC cal parameter - not relevant for summary of Calibration rows 18 and 19

#MAGGIC cal parameter
writeWorksheet(wb, round(resM_cal_ACM_1yr_oerep,2), sheet = "ACM_1yr", startRow = 20,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resM_cal_ACM_1yr_oeestfrominterc,2), sheet = "ACM_1yr", startRow = 21,
               startCol = 11, header = FALSE)


#MAGGIC sensitivity analysis
writeWorksheet(wb, round(resM_cal_ACM_1yr_cover1,2), sheet = "ACM_1yr", startRow = 22,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resM_cal_ACM_1yr_cover2,2), sheet = "ACM_1yr", startRow = 23,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resM_cal_ACM_1yr_cover3), sheet = "ACM_1yr", startRow = 24,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resM_cal_ACM_1yr_cover4), sheet = "ACM_1yr", startRow = 25,
               startCol = 11, header = FALSE)

################################
## Calibration SHFM ##########
################################
writeWorksheet(wb, round(resS_cal_ACM_1yr_all,2), sheet = "ACM_1yr", startRow = 27,
               startCol = 11, header = FALSE)

# SHFM ROB
writeWorksheet(wb, round(resS_cal_ACM_1yr_hrob,2), sheet = "ACM_1yr", startRow = 28,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resS_cal_ACM_1yr_lrob,2), sheet = "ACM_1yr", startRow = 29,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_1yr_urob), sheet = "ACM_1yr", startRow = 30, 
               startCol = 11, header = FALSE)


# SHFM HFtype
writeWorksheet(wb, round(resS_cal_ACM_1yr_chronic,2), sheet = "ACM_1yr", startRow = 31,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resS_cal_ACM_1yr_acute,2), sheet = "ACM_1yr", startRow = 32,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resS_cal_ACM_1yr_uncmixhf,2), sheet = "ACM_1yr", startRow = 33,
               startCol = 11, header = FALSE)

#SHFM model version
writeWorksheet(wb, round(resS_cal_ACM_1yr_Levy2006extval,2), sheet = "ACM_1yr", startRow = 34,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resS_cal_ACM_1yr_Levy2006PRAISE,2), sheet = "ACM_1yr", startRow = 35,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resS_cal_ACM_1yr_updatemod,2), sheet = "ACM_1yr", startRow = 36,
               startCol = 11, header = FALSE)

#SHFM disc parameter - not relevant for summary of Calibration rows 37 and 38

#SHFM cal parameter
writeWorksheet(wb, round(resS_cal_ACM_1yr_oerep,2), sheet = "ACM_1yr", startRow = 39,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resS_cal_ACM_1yr_oeestfrominterc,2), sheet = "ACM_1yr", startRow = 40,
               startCol = 11, header = FALSE)


#SHFM sensitivity analysis
writeWorksheet(wb, t(resS_cal_ACM_1yr_cover1), sheet = "ACM_1yr", startRow = 41,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resS_cal_ACM_1yr_cover2,2), sheet = "ACM_1yr", startRow = 42,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_1yr_cover3), sheet = "ACM_1yr", startRow = 43,
               startCol = 11, header = FALSE)
writeWorksheet(wb, t(resS_cal_ACM_1yr_cover4), sheet = "ACM_1yr", startRow = 44,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resS_cal_ACM_1yr_cover5,2), sheet = "ACM_1yr", startRow = 45,
               startCol = 11, header = FALSE)
writeWorksheet(wb, round(resS_cal_ACM_1yr_cover6,2), sheet = "ACM_1yr", startRow = 46,
               startCol = 11, header = FALSE)

# Save workbook (this actually writes the file to disk)
saveWorkbook(wb)

