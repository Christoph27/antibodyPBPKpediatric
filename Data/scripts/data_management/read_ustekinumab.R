##########################################################################
# Title:      reading ustekinumab PK data                          
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

getUstekinumabPK <- function() {
  library(readxl)
  library(data.table)
  library(here)

  expDataFile <- here("Data", "rawData", "ustekinumab", "Rosh2021_ustekinumab.xlsx")

  expData <- readxl::read_excel(expDataFile, sheet = "Data", col_names = TRUE)
  
  setDT(expData)

  colnames(expData) <- make.names(colnames(expData), unique = TRUE)
  
  expData[, time_days := time..week.*7]

  
  
  setnames(expData,
           old = c("concentration..µg.ml.", "dose..mg.kg.", "dose..mg.", "BW..kg."),
           new = c("conc_ugml",             "dose_mgkg",   "dose_mg", "weight_kg"))
  
  

  expData[, concDoseNorm1mgkg := conc_ugml / dose_mgkg]
  expData[!is.na(dose_mg), concDoseNorm1mgkg := conc_ugml / (dose_mg/weight_kg) ]
  expData[, Ab := "ustekinumab"]
  expData[, dosingCycle := 1]
  # adding profile ID. Here different group entry is different profile 
  expData[, ID := match(group, unique(group))]

  
  return(expData)

}
