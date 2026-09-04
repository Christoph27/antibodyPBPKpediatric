##########################################################################
# Title:      reading urtoxazumab PK data                          
#                                    
# Description: reading scanned PK data and returning data table (long format) with dose normalized concentration (µg/ml/(mg/kg))
#
#          standard columns:
#              "ID":    identifier for profile 
#              "time_days": time [days]
#              "conc_ugml": concentration [µg/ml]
#              "concDoseNorm1mgkg": dose normalized concentration
#              "group": text describing the data
#              "Ab": antibody name
#
#           further possible columns if needed:
#              sd_ugml: standard deviation [µg/ml]
#              sdDoseNorm1mgkg: dose normalized standard deviation  
#              dose_mgkg: dose [mg/kg] 
#              dose_mg:   dose [mg]
#              age_yrs 
#              weight_kg 
#              dosingCycle: integer for dosing cycle 1: first dose, 2 second dose etc  
#              litID: literature identifier
#              ... further case specific columns
#
#########################################################################

getUrtoxazumabPK <- function() {
  library(readxl)
  library(data.table)
  library(here)

  expDataFile <- here("Data", "rawData", "urtoxazumab", "Lopez2010_Urtoxazumab.xlsx")

  expDataAdult <- readxl::read_excel(expDataFile, sheet = "adultPK", col_names = TRUE)
  expDataPediatric <- readxl::read_excel(expDataFile, sheet = "pediatricPK", col_names = TRUE)

  setDT(expDataAdult)
  setDT(expDataPediatric)
  
  colnames(expDataAdult) <- make.names(colnames(expDataAdult), unique = TRUE)
  colnames(expDataPediatric) <- make.names(colnames(expDataPediatric), unique = TRUE)

  expDataAdult <- expDataAdult[, .(time..days., conc...µg.ml., SD..µg.ml., Dose..mg.kg.)]
  expDataPediatric <- expDataPediatric[, .(time..days., conc...µg.ml., SD..µg.ml., Dose..mg.kg.)]
 
  setnames(expDataAdult,
           old = c("time..days.", "conc...µg.ml.", "SD..µg.ml.", "Dose..mg.kg."),
           new = c("time_days", "conc_ugml", "sd_ugml", "dose_mgkg"))
  setnames(expDataPediatric,
           old = c("time..days.", "conc...µg.ml.", "SD..µg.ml.", "Dose..mg.kg."),
           new = c("time_days", "conc_ugml", "sd_ugml", "dose_mgkg"))

  expDataAdult[, group := fcase(
    dose_mgkg == 0.1, "adult (0.1 mg/kg)",
    dose_mgkg == 0.3, "adult (0.3 mg/kg)",
    dose_mgkg == 1, "adult (1 mg/kg)",
    dose_mgkg == 3, "adult (3 mg/kg)"
  )]

  expDataPediatric[, group := fcase(
    dose_mgkg == 1, "2.9 +/- 2.4 yrs; incl.cr.: 1-15 yrs (1 mg/kg)",
    dose_mgkg == 3, "2.8 +/- 2.4 yrs; incl.cr.: 1-15 yrs (3 mg/kg)"
  )]

  expData <- rbindlist(list(expDataAdult, expDataPediatric), use.names = TRUE, fill = TRUE)

  expData[, concDoseNorm1mgkg := conc_ugml / dose_mgkg]
  expData[, sdDoseNorm1mgkg := sd_ugml / dose_mgkg]
  expData[, litID := "Lopez2010"]
  expData[, Ab := "urtoxazumab"]
  expData[, dosingCycle := 1]
  # adding profile ID. Here different group entry is different profile 
  expData[, ID := match(group, unique(group))]

  
  return(expData)

}
