##########################################################################
# Title:      reading mepolizumab PK data                          
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

getMepolizumabPK <- function() {
  
  
  
  # healthy adult Smith2011 / Fig1
  expDataFile_Smith2011_250mg <- here("Data", "rawData", "mepolizumab", "Smith2011_mepolizumab_Fig1.xlsx")
  
  adultSmith2011_IV_250mg <- as.data.table(readxl::read_excel(expDataFile_Smith2011_250mg, sheet = "Fig1_log_IV", range = cell_cols("A:E"), col_names = TRUE))
  colnames(adultSmith2011_IV_250mg) <- make.names(colnames(adultSmith2011_IV_250mg), unique = TRUE) 
  adultSmith2011_IV_250mg <- adultSmith2011_IV_250mg[ , .(time..days., IV..conc...µg.ml.)]
  setnames(adultSmith2011_IV_250mg, old=c("time..days.", "IV..conc...µg.ml."), new=c("time_days", "conc_ugml"))
  

  adultSmith2011_IV_250mg[, dose_mg := 250]
  adultSmith2011_IV_250mg[, weight_kg := 66] # unknown, assuming 66 kg. Mean of default ICRP/PK/Sim male and female is 66.5 kg. 66 kg is rather to high if dose normalized profile is compared to the profiles from mg/kg dosing cf. data_analysis/plot_mepolizumab.R
  adultSmith2011_IV_250mg[, concDoseNorm1mgkg := conc_ugml / (dose_mg/weight_kg)]
  adultSmith2011_IV_250mg[, group := "healthy adults, IV, BW assumed 66 kg"]
  adultSmith2011_IV_250mg[, litID := "Smith2011"]
  adultSmith2011_IV_250mg[, Ab := "mepolizumab"]
  
  timeIncreasing <- adultSmith2011_IV_250mg[, all(time_days > shift(time_days, type = "lag"), na.rm = TRUE)] # check is time is strictly increasing (needed for OSP import)
  
  if (!timeIncreasing) {
    stop("time not increasing")
  } 
  
  
  ### SC
  adultSmith2011_SC_250mg <- as.data.table(readxl::read_excel(expDataFile_Smith2011_250mg, sheet = "Fig1_log_SC_thigh", range = cell_cols("A:E"), col_names = TRUE))
  colnames(adultSmith2011_SC_250mg) <- make.names(colnames(adultSmith2011_SC_250mg), unique = TRUE) 
  adultSmith2011_SC_250mg <- adultSmith2011_SC_250mg[ , .(time_scanned..days....4, SC_thigh..conc...µg.ml.)]
  setnames(adultSmith2011_SC_250mg, old=c("time_scanned..days....4", "SC_thigh..conc...µg.ml."), new=c("time_days", "conc_ugml"))
  
  
  adultSmith2011_SC_250mg[, dose_mg := 250]
  adultSmith2011_SC_250mg[, weight_kg := 66] # unknown, see comment for IV above
  adultSmith2011_SC_250mg[, concDoseNorm1mgkg := conc_ugml / (dose_mg/weight_kg)]
  adultSmith2011_SC_250mg[, group := "healthy adults, SC thigh, BW assumed 66 kg"]
  adultSmith2011_SC_250mg[, litID := "Smith2011"]
  adultSmith2011_SC_250mg[, Ab := "mepolizumab"]
  
 
  # first and second scanned data point have the same time. Delete the first one
  adultSmith2011_SC_250mg <- adultSmith2011_SC_250mg[-1, ]
  
  
  
  timeIncreasing <- adultSmith2011_SC_250mg[, all(time_days > shift(time_days, type = "lag"), na.rm = TRUE)] # check is time is strictly increasing (needed for OSP import)
  
  if (!timeIncreasing) {
    stop("time not increasing")
  } 
  
  
  
  
  #==== Smith2011, Fig2
  
  
  build_profile <- function(dt, time_col, conc_col, sd_col, dose, group_label, Ab, litID, profile_id) {
    
    profile <- data.table(
    #  ID = profile_id, # for now, no ID
      time_days = as.numeric(dt[[time_col]]), # all time column units are days
      conc_ugml = as.numeric(dt[[conc_col]]),
      sd_ugml = if (is.nan(sd_col)) rep(NaN, nrow(dt)) else as.numeric(dt[[sd_col]]),
      Dose_mgkg = dose,
      group = group_label,
      Ab = Ab,
      litID = litID
    )
    
    profile[sd_ugml == 0, sd_ugml := NaN]
    
    
    profile[, concDoseNorm1mgkg := conc_ugml / Dose_mgkg]
    profile[, sdDoseNorm1mgkg := sd_ugml / Dose_mgkg]
    
    profile <- na.omit(profile, cols="time_days")
    
    
    return(profile)
  }
  
  

  expDataFileAdultFig2 <- here("Data", "rawData", "mepolizumab", "Smith2011_mepolizumab_Fig2.xlsx")
  adultSmith2011Fig2 <- as.data.table(readxl::read_excel(expDataFileAdultFig2, sheet = "Fig2", range =cell_cols("A:O"), col_names = TRUE))
  setnames(adultSmith2011Fig2, names(adultSmith2011Fig2), make.names(names(adultSmith2011Fig2), unique = TRUE))
  
  
  
  adultSmith2011Fig2_10mgkg <-   build_profile(adultSmith2011Fig2, time_col=4, conc_col=5, sd_col=6, dose=10, "adults (asthma); 10 mg/kg", "mepolizumab", litID="Smith2011", 1)
  adultSmith2011Fig2_2p5mgkg <-  build_profile(adultSmith2011Fig2, time_col=7, conc_col=8, sd_col=9, dose=2.5, "adults (asthma); 2.5 mg/kg", "mepolizumab", litID="Smith2011", 2)
  adultSmith2011Fig2_0p5mgkg <-  build_profile(adultSmith2011Fig2, time_col=10, conc_col=11, sd_col=12, dose=0.5, "adults (asthma); 0.5 mg/kg", "mepolizumab", litID="Smith2011", 3)
  adultSmith2011Fig2_0p05mgkg <- build_profile(adultSmith2011Fig2, time_col=13, conc_col=14, sd_col=15, dose=0.05, "adults (asthma); 0.05 mg/kg", "mepolizumab", litID="Smith2011", 4)
  
  # Also this infusion is 30 min, cf. also Leckie200, DOI: 10.1016/S0140-6736(00)03496-6, cited in Smith2011
  # replace first negative time point (<Cmax) with 15 min & second negative time point with 30 min (Cmax)
  
  correctNegativeTime <- function(dt) {
    
    # Check the sign of the first two values
    neg1 <- dt[1, time_days] < 0                    
    neg2 <- dt[2, time_days] < 0   
    
    if (neg1 && neg2) {
      # a) first two are negative (10mg/kg, 2.5mg/kg & 0.5 mg/kg)
      dt[1, time_days := 15/60/24]
      dt[2, time_days := 30/60/24]
    } else if (neg1) {
      # b) only the first is negative -> 1st becomes 15
      dt[1, time_days := 15/60/24]
    }
    return(dt)
  }
  
  adultSmith2011Fig2_10mgkg  <-  correctNegativeTime(adultSmith2011Fig2_10mgkg)
  adultSmith2011Fig2_2p5mgkg <-  correctNegativeTime(adultSmith2011Fig2_2p5mgkg)
  adultSmith2011Fig2_0p5mgkg <-  correctNegativeTime(adultSmith2011Fig2_0p5mgkg)
  adultSmith2011Fig2_0p05mgkg <- correctNegativeTime(adultSmith2011Fig2_0p05mgkg)
  
  adultSmith2011Fig2_all <- rbindlist(
    list(adultSmith2011Fig2_10mgkg,  adultSmith2011Fig2_2p5mgkg,adultSmith2011Fig2_0p5mgkg, adultSmith2011Fig2_0p05mgkg),
    use.names = TRUE,
    fill = TRUE
  )
  
  
  
  #======= 
  
  
  # Gupta2019 pediatric patient  
  
  expDataFile_Gupta2019_100mg <- here("Data", "rawData", "mepolizumab", "Gupta2019_mepolizumab_children_100mg.xlsx")
  
  dataGupta2019_100mg <- as.data.table(readxl::read_excel(expDataFile_Gupta2019_100mg, sheet = "Fig2_100mg", range = cell_cols("A:E"), col_names = TRUE))
  colnames(dataGupta2019_100mg) <- make.names(colnames(dataGupta2019_100mg), unique = TRUE) 
  dataGupta2019_100mg <- dataGupta2019_100mg[ , .(time..days....4, X100mg..conc...µg.ml.)]
  setnames(dataGupta2019_100mg, old=c("time..days....4", "X100mg..conc...µg.ml."), new=c("time_days", "conc_ugml"))
  
  
  dataGupta2019_100mg[, dose_mg := 100]
  dataGupta2019_100mg[, weight_kg := 49.5] # 
  dataGupta2019_100mg[, concDoseNorm1mgkg := conc_ugml / (dose_mg/weight_kg)]
  dataGupta2019_100mg[, group := "ped., SC "]
  dataGupta2019_100mg[, litID := "Gupta2019"]
  dataGupta2019_100mg[, Ab := "mepolizumab"]
  
  
  # scan has some same time points. Different individuals can however not be distinguished in the plot. Add a small number to the non-unique times in order to be able to import to OSP-suite
  dataGupta2019_100mg[, timeOriginal_days := time_days] # save for debug
  sn = 1e-4 # small number
  dataGupta2019_100mg[, time_days := time_days + (seq_len(.N) - 1) * sn, by = time_days]
  
  # order per time
  setorder(dataGupta2019_100mg, time_days)
  
  #check if time now is strictly increasing
   timeIncreasing <- dataGupta2019_100mg[, all(time_days > shift(time_days, type = "lag"), na.rm = TRUE)] # check is time is strictly increasing (needed for OSP import)
  
  if (!timeIncreasing) {
    stop("time not increasing")
  } 
  
   dataGupta2019_100mg[, timeOriginal_days:=NULL]

    

   
  
 


  # combine and return list
  return(list(adultSmith2011Fig2 = adultSmith2011Fig2_all, adultSmith2011_IV_250mg = adultSmith2011_IV_250mg, adultSmith2011_SC_250mg = adultSmith2011_SC_250mg, dataGupta2019_100mg=dataGupta2019_100mg))
}
  
  
 
  