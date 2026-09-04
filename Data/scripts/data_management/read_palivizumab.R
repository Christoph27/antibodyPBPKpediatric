##########################################################################
# Title:      reading palivizumab PK data                          
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



getPalivizumabPK <- function() {
  library(readxl)
  library(data.table)
  library(here)
  

  # helper to get column by pattern
  pick_col <- function(dt, pattern) {
    v <- grep(pattern, names(dt), ignore.case = TRUE, value = TRUE)
    if (length(v)) return(v[1])
    return(NA_character_)
  }

  # ---- Adult (Boeckh2001) ----
  adult_f <- here("Data", "rawData", "palivizumab", "Boeckh2001_palivizumab_Adults.xlsx")
  if (!file.exists(adult_f)) stop("Missing file: ", adult_f)
  adult <- as.data.table(readxl::read_excel(adult_f, sheet = "Tabelle1", range = cell_cols("A:B"), col_names = TRUE))
  setnames(adult, names(adult), make.names(names(adult), unique = TRUE))
  tcol <- pick_col(adult, "time")
  ccol <- pick_col(adult, "conc")
  adult[, time_days := if (!is.na(tcol)) get(tcol)/24 else NA_real_]
  adult[, concDoseNorm1mgkg := if (!is.na(ccol)) get(ccol)/15 else NA_real_]
  adult[, sdDoseNorm1mgkg := NA_real_]
  adult[, group := "adult"]
  adult[, Ab := "palivizumab"]
  setnames(adult, old="conc..µg.ml.", new="conc_ugml")
  adult[, sd_ugml := NA]
  adult <- adult[, .(time_days, conc_ugml, sd_ugml, concDoseNorm1mgkg, sdDoseNorm1mgkg, Ab, group)]
  adult[, dosingCycle := 1]
  adult[, litID := "Boeckh2001"]

  # ---- Subramanian1998 (pediatric 10 & 15 mg/kg) ----
  sub_f <- here("Data", "rawData", "palivizumab", "Subramanian1998_Palivizumab_Ped.xlsx")
  sub_dt <- as.data.table(readxl::read_excel(sub_f, sheet = "Palivizumab_Subramanian1998", col_names = TRUE))
  setnames(sub_dt, names(sub_dt), make.names(names(sub_dt), unique = TRUE))
  tcol <- pick_col(sub_dt, "time")
  conc15 <- pick_col(sub_dt, "15mg|X15|15mgkg")
  conc10 <- pick_col(sub_dt, "10mg|X10|10mgkg")
  sd15 <- pick_col(sub_dt, "Error.Arithm.*15|Error.*15|Error.*15mg")
  sd10 <- pick_col(sub_dt, "Error.Arithm.*10|Error.*10|Error.*10mg")

  sub15 <- data.table(time_days = if (!is.na(tcol)) as.numeric(sub_dt[[tcol]]) else NA_real_,
                      conc_ug_ml = if (!is.na(conc15)) as.numeric(sub_dt[[conc15]]) else NA_real_,
                      sd_ug_ml = if (!is.na(sd15)) as.numeric(sub_dt[[sd15]]) else NA_real_)
  sub10 <- data.table(time_days = if (!is.na(tcol)) as.numeric(sub_dt[[tcol]]) else NA_real_,
                      conc_ug_ml = if (!is.na(conc10)) as.numeric(sub_dt[[conc10]]) else NA_real_,
                      sd_ug_ml = if (!is.na(sd10)) as.numeric(sub_dt[[sd10]]) else NA_real_)

  sub15[, concDoseNorm1mgkg := conc_ug_ml / 15]
  sub15[, sdDoseNorm1mgkg := sd_ug_ml / 15]
  sub15[, group := "0.68 yrs (Subramanian1998, 15 mg/kg)"]
  sub15[, Ab := "palivizumab"]

  sub10[, concDoseNorm1mgkg := conc_ug_ml / 10]
  sub10[, sdDoseNorm1mgkg := sd_ug_ml / 10]
  sub10[, group := "0.62 yrs (Subramanian1998, 10 mg/kg)"]
  sub10[, Ab := "palivizumab"]

  dosingTimes <- seq(from = 0, to = 3 * 30, by = 30)
  sub15[, dosingCycle := findInterval(time_days, dosingTimes, left.open = TRUE)]
  sub10[, dosingCycle := findInterval(time_days, dosingTimes, left.open = TRUE)]
  
  setnames(sub15, old=c("conc_ug_ml","sd_ug_ml"), new=c("conc_ugml","sd_ugml"))
  setnames(sub10, old=c("conc_ug_ml","sd_ug_ml"), new=c("conc_ugml","sd_ugml"))

  sub_all <- rbindlist(list(sub15, sub10), use.names = TRUE, fill = TRUE)
  
  sub_all[, litID := "Subramanian1998"]
  
  
  # ---- SaezLlorens2004 (pediatric 15 mg/kg) ----
  saez_f <- here("Data", "rawData", "palivizumab", "SaezLlorens2004_Palivizumab_Ped.xlsx")
  saez <- as.data.table(readxl::read_excel(saez_f, sheet = "Palivizumab_SaezLlorens2004", col_names = TRUE))
  setnames(saez, names(saez), make.names(names(saez), unique = TRUE))
  tcol <- pick_col(saez, "time")
  conc_col <- pick_col(saez, "15mg|X15|15mgkg|Concentration")
  sd_col <- pick_col(saez, "Error.Arithm|Error")
  saez_dt <- data.table(time_days = if (!is.na(tcol)) as.numeric(saez[[tcol]]) else NA_real_,
                       conc_ug_ml = if (!is.na(conc_col)) as.numeric(saez[[conc_col]]) else NA_real_,
                       sd_ug_ml = if (!is.na(sd_col)) as.numeric(saez[[sd_col]]) else NA_real_)
  saez_dt[, concDoseNorm1mgkg := conc_ug_ml / 15]
  saez_dt[, sdDoseNorm1mgkg := sd_ug_ml / 15]
  saez_dt[, group := "0.43 yrs (SaezLlorens2004, 15 mg/kg)"]
  saez_dt[, Ab := "palivizumab"]
  saez_dt[, dosingCycle := 1]
  saez_dt[, litID := "SaezLlorens2004"]
  setnames(saez_dt, old=c("conc_ug_ml","sd_ug_ml"), new=c("conc_ugml","sd_ugml"))

  # ---- combine ----
  combinedData <- rbindlist(list(adult, sub_all, saez_dt), use.names = TRUE, fill = TRUE)

  # adding profile ID. Here different group entry is different profile 
  combinedData[, ID := match(group, unique(group))]
  
  return(combinedData)
}
    

