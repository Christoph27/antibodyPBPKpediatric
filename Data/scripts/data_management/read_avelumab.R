##########################################################################
# Title:      reading avelumab PK data                          
#                                    
# Description: reading scanned PK data and returning data table with dose normalized concentration (µg/ml/(mg/kg))
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
#
#


getAvelumabPK <- function() {
  library(readxl)
  library(data.table)
  library(here)

  # adult data (Heery2017)
  adult_file <- here("Data", "rawData", "avelumab", "Heery2017_avelumab.xlsx")
  
  read_Heery2017_sheet <- function(sheet_name, profile_start) {
    adult <- as.data.table(readxl::read_excel(adult_file, sheet = sheet_name, col_names = TRUE))
    setnames(adult, names(adult), make.names(names(adult), unique = TRUE))

    time_col <- grep("^Time", names(adult), ignore.case = TRUE, value = TRUE)[1]
    dose_columns <- grep("^X?[0-9]+mg.*conc", names(adult), ignore.case = TRUE, value = TRUE)
    doses <- as.numeric(sub("^X?([0-9]+)mg.*", "\\1", dose_columns))

    dose_data <- lapply(seq_along(dose_columns), function(index) {
      dose <- doses[index]
      conc_col <- dose_columns[index]
      sd_col <- grep(paste0("^Error.*", dose, "mg|^SD.*", dose, "mg"),
                     names(adult), ignore.case = TRUE, value = TRUE)[1]

      result <- data.table(
        ID = profile_start + index - 1L,
        time_days = as.numeric(adult[[time_col]]) / 24,
        conc_ugml = as.numeric(adult[[conc_col]]),
        sd_ugml = if (!is.na(sd_col)) as.numeric(adult[[sd_col]]) else NA_real_,
        concDoseNorm1mgkg = as.numeric(adult[[conc_col]]) / dose,
        sdDoseNorm1mgkg = if (!is.na(sd_col)) as.numeric(adult[[sd_col]]) / dose else NA_real_,
        dose_mgkg = dose,
        age_yrs = "Adult",
        group = paste0("adult (", dose, " mg/kg, ", sheet_name, ")"),
        Ab = "avelumab",
        litID = "Heery2017"
      )
      
      
      return(result)
    })

    rbindlist(dose_data, use.names = TRUE, fill = TRUE)
  }

  adult_log_data <- read_Heery2017_sheet("Fig2D_log", 1L)
  adult_linear_data <- read_Heery2017_sheet("Fig2C_lin", max(adult_log_data$ID) + 1L)
  
  
  # === pediatric data
  #  Vugmeyster2022 supplementary Fig2a (10 mg/kg) 
  
  # file name 
  expDataFile_Vugmeyster2022_Fig2a = here("Data", "rawData", "avelumab", "Vugmeyster2022_avelumab_SUPPL_Fig2a.xlsx")
  
  #read
  data_Vugmeyster2022_Fig2a = readxl::read_excel(expDataFile_Vugmeyster2022_Fig2a, sheet = "DataLong", col_names=TRUE)
  
  colnames(data_Vugmeyster2022_Fig2a) = make.names(colnames(data_Vugmeyster2022_Fig2a), unique = TRUE)
  setDT(data_Vugmeyster2022_Fig2a)
  data_Vugmeyster2022_Fig2a[, time_days := time..weeks....adjusted.to.q2w.scheme * 7]
  
  setnames(data_Vugmeyster2022_Fig2a, c("conc...µg.ml.","age..yrs.", "weight..kg."), c("conc_ugml","age_yrs", "weight_kg"))
  
  data_Vugmeyster2022_Fig2a[, concDoseNorm1mgkg := conc_ugml / 10]
  data_Vugmeyster2022_Fig2a[, dose_mgkg := 10]
  data_Vugmeyster2022_Fig2a[, group := "Fig2a, individ., 10 mg/kg"]
  data_Vugmeyster2022_Fig2a[, Ab := "avelumab"]
  data_Vugmeyster2022_Fig2a[, litID := "Vugmeyster2022"]
  
  # add dosing cycle (Q2W scheme). 1: first cycle, 2: second cycle etc
  maxTime <- 7 * 2 * 9
  dosingTimes <- seq(from = 0, to = maxTime, by = 2 * 7)
  if (max( data_Vugmeyster2022_Fig2a$time_days, na.rm = TRUE) > maxTime) stop("Increase maxTime for dosing cycle")
  data_Vugmeyster2022_Fig2a[, dosingCycle := findInterval(time_days, dosingTimes, left.open = TRUE)]
  
  
  data_Vugmeyster2022_Fig2a <-  data_Vugmeyster2022_Fig2a[, c("ID", "time_days", "conc_ugml", "concDoseNorm1mgkg", "dose_mgkg", "age_yrs", "weight_kg", "group", "dosingCycle", "Ab", "litID")]
  
  
  
  # Vugmeyster2022 supplementary Fig 5 (10 & 20 mg/kg)
  fig5_file <- here("Data", "rawData", "avelumab", "Vugmeyster2022_avelumab_SUPPL_Fig5.xlsx")
  
  sheets <- readxl::excel_sheets(fig5_file)
  df_list <- lapply(seq_along(sheets), function(sheet_id) {
    s <- sheets[[sheet_id]]
    tmp <- as.data.table(readxl::read_excel(fig5_file, sheet = s, col_names = TRUE))
    # expect first column = time, fourth = conc_ugml
    tmp <- tmp[, .(ID = sheet_id, time_days = as.numeric(tmp[[1]]), conc_ugml = as.numeric(tmp[[4]]))]
    tmp[, age_yrs := as.numeric(sub("p", ".", sub("y.*", "", s)))]
    tmp[, weight_kg := as.numeric( sub("p", ".", sub(".*y", "", sub("kg.*", "", s) ) ) )]
    tmp[, dose_mgkg := as.numeric(sub("p", ".", sub(".*kg", "", sub("mgkg.*", "", s))))]
  })
  data_Vugmeyster2022_Fig5 <- rbindlist(df_list, use.names = TRUE, fill = TRUE)
  
  data_Vugmeyster2022_Fig5[, concDoseNorm1mgkg := conc_ugml / dose_mgkg]
  data_Vugmeyster2022_Fig5[, sdDoseNorm1mgkg := NA_real_]
  data_Vugmeyster2022_Fig5[, group := paste0(age_yrs, " yrs (ind.; ", dose_mgkg, " mg/kg)")]
  data_Vugmeyster2022_Fig5[, Ab := "avelumab"]
  data_Vugmeyster2022_Fig5[, litID := "Vugmeyster2022"]
  
  
  # return

  return(list(Heery2017_adult_log = adult_log_data, 
              Heery2017_adult_lin = adult_linear_data, 
              Vugmeyster2022_Fig2a = data_Vugmeyster2022_Fig2a, 
              Vugmeyster2022_Fig5 =data_Vugmeyster2022_Fig5 ))
  
}


  
  
  