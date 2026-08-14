##########################################################################
# Title:      reading pagibaximab PK data                          
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

getPagibaximabPK <- function() {
  
  
  library(readxl)
  library(data.table)
  
  
  
  read_rawData <- function(path, sheet, range) {
    dt <- as.data.table(readxl::read_excel(path, sheet = sheet, range = range, col_names = TRUE))
    setnames(dt, names(dt), make.names(names(dt), unique = TRUE))
    return(dt)
  }
  
  build_profile <- function(dt, time_col, conc_col, sd_col, dose, group_label, Ab, profile_id) {
    if (grepl(".h.", colnames(dt)[[time_col]], fixed = TRUE) ) {
      timeConv = 1/24
    } else if (grepl(".days.", colnames(dt)[[time_col]], fixed = TRUE)) {
      timeConv = 1
    }  
    else {
      stop("unit for time conversion?")
    }
    
    if (!(grepl(".ug.ml.", colnames(dt)[[conc_col]], fixed = TRUE) || grepl(".µg.ml.", colnames(dt)[[conc_col]], fixed = TRUE))) {
      stop("conc unit conversion?")
    } 
    
    if (!is.nan(sd_col)) {
      if (!(grepl(".ug.ml.", colnames(dt)[[sd_col]], fixed = TRUE) || grepl(".µg.ml.", colnames(dt)[[sd_col]], fixed = TRUE))) {
        stop("sd unit conversion?")
      }
    }
    
    profile <- data.table(
      ID = profile_id,
      time_days = as.numeric(dt[[time_col]]) * timeConv,
      conc_ugml = as.numeric(dt[[conc_col]]),
      sd_ugml = if (is.nan(sd_col)) rep(NaN, nrow(dt)) else as.numeric(dt[[sd_col]]),
      Dose_mgkg = dose,
      group = group_label,
      Ab = Ab
    )
    profile[, concDoseNorm1mgkg := conc_ugml / Dose_mgkg]
    profile[, sdDoseNorm1mgkg := sd_ugml / Dose_mgkg]
   
    # add baseline corrected concentration  
    profile[, conc_adjusted_ugml := NaN]
    if (any(profile$time_days < 0)) {
      baseline <- profile[time_days < 0, conc_ugml[1]]
      profile[, conc_adjusted_ugml := conc_ugml - baseline]
    } 
    
    profile[, concDoseNorm1mgkg_adjusted := conc_adjusted_ugml / Dose_mgkg]
    
    return(profile)
  }
  
  #====Weissman2009 - adults
  expDataFileAdult <- file.path("..", "..", "rawData", "pagibaximab", "Weisman2009_pagibaximab_adults.xlsx", fsep = .Platform$file.sep)
  expDataAdult_raw <- read_rawData(expDataFileAdult, sheet = "Fig1", range = cell_cols("A:G"))
  
  
  expDataAdult_3 <- build_profile(expDataAdult_raw, time_col=1, conc_col=4, sd_col=5, dose=3, "adult; 3 mg/kg", "pagibaximab", 1)
  expDataAdult_10 <- build_profile(expDataAdult_raw, time_col=1, conc_col=6, sd_col=7, dose=10, "adult; 10 mg/kg", "pagibaximab", 2)
  
  expDataAdult_all <- rbindlist(
    list(expDataAdult_3, expDataAdult_10),
    use.names = TRUE,
    fill = TRUE
  )
  
  # adult: single dosing; baseline (time < 0 -> cycle 0)
  expDataAdult_all[time_days>=0, dosingCycle := 1]
  expDataAdult_all[time_days<0, dosingCycle := 0]
  
  expDataAdult_all[, litID := "Weisman2009_adult"]
  
  
  
  #====Weissman2009 - pediatric
  expDataFilePed2009 <- file.path("..", "..", "rawData", "pagibaximab", "Weisman2009_pagibaximab.xlsx", fsep = .Platform$file.sep)
  expDataPed2009_raw <- read_rawData(expDataFilePed2009, sheet = "Fig1", range = cell_cols("A:K"))
  
  
  expDataPed2009_90 <- build_profile(expDataPed2009_raw, time_col=1, conc_col=4, sd_col=5, dose=90, "0.01 yrs (3-7 days), premature newborn; 90 mg/kg", "pagibaximab", 3)
  expDataPed2009_60 <- build_profile(expDataPed2009_raw, time_col=1, conc_col=6, sd_col=7, dose=60, "0.01 yrs (3-7 days), premature newborn; 60 mg/kg", "pagibaximab", 4)
  expDataPed2009_30 <- build_profile(expDataPed2009_raw, time_col=1, conc_col=8, sd_col=9, dose=30, "0.01 yrs (3-7 days), premature newborn; 30 mg/kg", "pagibaximab", 5)
  expDataPed2009_10 <- build_profile(expDataPed2009_raw, time_col=1, conc_col=10, sd_col=11, dose=10, "0.01 yrs (3-7 days), premature newborn; 10 mg/kg", "pagibaximab", 6)
  
  
  expDataPed2009_all <- rbindlist(
    list(expDataPed2009_90, expDataPed2009_60, expDataPed2009_30, expDataPed2009_10),
    use.names = TRUE,
    fill = TRUE
  )
  
    # add dosing cycle (two applications)
    dosingTimes <- c(0, 14)
    expDataPed2009_all[, dosingCycle := findInterval(time_days, dosingTimes, left.open = TRUE)]
    
    expDataPed2009_all[, litID := "Weisman2009"]
    
    
    #====Weissman2011 - pediatric
    expDataFilePed2011 <- file.path("..", "..", "rawData", "pagibaximab", "Weisman2011_pagibaximab.xlsx", fsep = .Platform$file.sep)
    expDataPed2011_raw <- read_rawData(expDataFilePed2011, sheet = "Fig1", range = cell_cols("A:E"))
    
    
    expDataPed2011_90 <- build_profile(expDataPed2011_raw, time_col=1, conc_col=4, sd_col=NaN, dose=90, "0.01 yrs (2-5 days), premature newborn; 90 mg/kg", "pagibaximab", 7)
    expDataPed2011_60 <- build_profile(expDataPed2011_raw, time_col=1, conc_col=5, sd_col=NaN, dose=60, "0.01 yrs (2-5 days), premature newborn; 60 mg/kg", "pagibaximab", 8)
    
    
    expDataPed2011_all <- rbindlist(
      list(expDataPed2011_90, expDataPed2011_60),
      use.names = TRUE,
      fill = TRUE
    )
    
    # add dosing cycle (two applications)
    dosingTimes <- c(0, 7, 14)
    expDataPed2011_all[, dosingCycle := findInterval(time_days, dosingTimes, left.open = TRUE)]
    
    
    expDataPed2011_all[, litID := "Weisman2011"]
  
  # === combine all
  
  
  allData <- rbindlist(
    list(expDataAdult_all, expDataPed2009_all, expDataPed2011_all),
    use.names = TRUE,
    fill = TRUE
  )
  
  return(allData)
}


