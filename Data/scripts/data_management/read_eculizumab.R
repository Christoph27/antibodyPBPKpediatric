##########################################################################
# Title:      reading eculizumab PK data                          
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




library(readxl)
library(data.table)
library(here)

getEculizumabPK <- function() {
  
  
  # Jodele2014 pediatric patient sheets
  expDataFile_Jodele2014 <- here("Data", "rawData", "eculicumab", "Jodele2014_Eculizumab.xlsx")
  sheets <- c("Pat_1","Pat_2","Pat_3","Pat_4","Pat_5","Pat_6")
  
  pat_list <- lapply(seq_along(sheets), function(i) {
    s <- sheets[i]
    if (!file.exists(expDataFile_Jodele2014)) return(NULL)
    dt <- as.data.table(readxl::read_excel(expDataFile_Jodele2014, sheet = s, range = cell_cols("A:B"), col_names = TRUE))
    setnames(dt, names(dt), make.names(names(dt), unique = TRUE))
    dt[, ID := i]
    dt
  })
  
  pat_list <- Filter(Negate(is.null), pat_list)
  pediJodele2014_all <- if (length(pat_list)) rbindlist(pat_list, use.names = TRUE, fill = TRUE) else data.table()

  # add metadata 
  #To Do: add further dosages (they change)!!!! For now we use only first administration
  setnames(pediJodele2014_all, old = c("time..days.", "conc...µg.ml."), new = c("time_days", "conc_ugml"))
  pediJodele2014_all <- pediJodele2014_all[time_days <= 7] # restrict to first administration
  
  pediJodele2014_all[ID==1, age_yrs := 4.9]
  pediJodele2014_all[ID==1, weight_kg := 17]
  pediJodele2014_all[ID==1, doseFirst_mg := 600]
  
  pediJodele2014_all[ID==2, age_yrs := 5.1]
  pediJodele2014_all[ID==2, weight_kg := 18]
  pediJodele2014_all[ID==2, doseFirst_mg := 600]
  
  pediJodele2014_all[ID==3, age_yrs := 2.4]
  pediJodele2014_all[ID==3, weight_kg := 9]
  pediJodele2014_all[ID==3, doseFirst_mg := 600]
  
  pediJodele2014_all[ID==4, age_yrs := 4.3]
  pediJodele2014_all[ID==4, weight_kg := 17]
  pediJodele2014_all[ID==4, doseFirst_mg := 600]
  
  pediJodele2014_all[ID==5, age_yrs := 7.2]
  pediJodele2014_all[ID==5, weight_kg := 26.5]
  pediJodele2014_all[ID==5, doseFirst_mg := 600]
  
  pediJodele2014_all[ID==6, age_yrs := 10.9]
  pediJodele2014_all[ID==6, weight_kg := 52]
  pediJodele2014_all[ID==6, doseFirst_mg := 900]
  
  #dose normalization 
  pediJodele2014_all[, concDoseNorm1mgkg := conc_ugml / (doseFirst_mg / weight_kg)]
  pediJodele2014_all[, Ab := "eculizumab"]
  pediJodele2014_all[, litID := "Jodele2014"]
  pediJodele2014_all[, group := paste(age_yrs, "yrs")]

    

  # healthy adult Cho2020
  expDataFile_Cho2020 <- here("Data", "rawData", "eculicumab", "Chow2020_Eculizumab.xlsx")

  adultCho2020 <- as.data.table(readxl::read_excel(expDataFile_Cho2020, sheet = "Fig1a", range = cell_cols("A:E"), col_names = TRUE))
  colnames(adultCho2020) <- make.names(colnames(adultCho2020), unique = TRUE) 
  adultCho2020 <- adultCho2020[ , .(time..h., Eculizumab.EU..conc...µg.ml., Error.Arithm..Eculizumab.EU..µg.ml.)]
  setnames(adultCho2020, old=c("time..h.", "Eculizumab.EU..conc...µg.ml.", "Error.Arithm..Eculizumab.EU..µg.ml."), new=c("time_h", "conc_ugml", "sd_ugml"))

  
  adultCho2020[, time_days := time_h/24]
  adultCho2020[, dose_mg := 300]
  adultCho2020[, weight_kg := 71.76]
  adultCho2020[, concDoseNorm1mgkg := conc_ugml / (dose_mg/weight_kg)]
  adultCho2020[, sdDoseNorm1mgkg := sd_ugml / (dose_mg/weight_kg)]
  adultCho2020[, group := "healthy adults"]
  adultCho2020[, litID := "Cho2020"]



  # combine and return list
  return(list(pediatricJodele2014 = pediJodele2014_all, adultCho2020 = adultCho2020))
}
  
  
 
  