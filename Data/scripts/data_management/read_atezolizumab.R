##########################################################################
# Title: Read Atezolizumab experimental data 
# Purpose: provide a standard dose-normalized data.table for downstream use


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


#########################################################################

getAtezolizumabPK <- function() {
  library(readxl)
  library(data.table)
  
  
  # === Geoerger2020
  
  file_Geoerger2020 <- file.path("..", "..", "rawData", "atezolizumab", "Geoerger2020_Atezolizumab.xlsx", fsep = .Platform$file.sep)
  
  dt_Geoerger2020 <- as.data.table(readxl::read_excel(file_Geoerger2020, sheet = "PlasmaConc_scan", col_names = TRUE))
  setnames(dt_Geoerger2020, names(dt_Geoerger2020), make.names(names(dt_Geoerger2020), unique = TRUE))
  
  time_col <- "time..days....1"
  conc_cols <- names(dt_Geoerger2020)[grepl(".conc.", names(dt_Geoerger2020), ignore.case = TRUE)]
  sd_cols <- names(dt_Geoerger2020)[grepl("Error.", names(dt_Geoerger2020), ignore.case = TRUE)]
  
  
  group_lookup <- c("older18" = "18 yrs or older", "12to17" = "12 to 17 yrs", "2to11" = "2 to 11 yrs", "less2" = "younger 2 yrs")
  dt_Geoerger2020 <- rbindlist(lapply(seq_along(conc_cols), function(i) {
    conc_name <- conc_cols[[i]]
    conc_key <- tolower(conc_name)
    group <- if (grepl("older18", conc_key)) {
      group_lookup[["older18"]]
    } else if (grepl("12to17", conc_key)) {
      group_lookup[["12to17"]]
    } else if (grepl("2to11", conc_key)) {
      group_lookup[["2to11"]]
    } else if (grepl("less2", conc_key)) {
      group_lookup[["less2"]]
    } else {
      stop(sprintf("Unknown Geoerger2020 group in column '%s'", conc_name))
    }
    
    sd_col <- if (i <= length(sd_cols)) sd_cols[[i]] else NA_character_
    data.table(
      time_days = as.numeric(dt_Geoerger2020[[time_col]]),
      conc_ugml = as.numeric(dt_Geoerger2020[[conc_name]]),
      sd_ugml = if (!is.na(sd_col)) as.numeric(dt_Geoerger2020[[sd_col]]) else NA_real_,
      group = group
    )
  }), use.names = TRUE, fill = TRUE)
  
  # ensure dose and metadata fields exist
  dt_Geoerger2020[, dose_mgkg := NaN]
  dt_Geoerger2020[, dose_mg := NaN]
  dt_Geoerger2020[, age_min_yrs := NaN]
  dt_Geoerger2020[, age_max_yrs := NaN]
  dt_Geoerger2020[, age_median_yrs := NaN]
  dt_Geoerger2020[, weight_kg_typical := NaN]
  
  # apply known group-level metadata from Geoerger2020
  dt_Geoerger2020[group == "18 yrs or older", `:=`(age_min_yrs = 18,
                                                   age_max_yrs = 29,
                                                   age_median_yrs = 22, # taken from Shemesh2019
                                                   weight_kg_typical = 61)] # taken from Shemesh2019
  dt_Geoerger2020[group == "12 to 17 yrs", `:=`(age_min_yrs = 12,
                                                age_max_yrs = 17,
                                                age_median_yrs = 15, # taken from Shemesh2019
                                                weight_kg_typical = 51.1)] # taken from Shemesh2019
  dt_Geoerger2020[group == "2 to 11 yrs", `:=`(age_min_yrs = 2,
                                               age_max_yrs = 11,
                                               age_median_yrs = 7, # taken from Shemesh2019
                                               weight_kg_typical = 22.5)] # taken from Shemesh2019
  dt_Geoerger2020[group == "younger 2 yrs", `:=`(age_min_yrs = 0.6,
                                                 age_max_yrs = 1.5,
                                                 age_median_yrs = 1, # taken from Shemesh2019
                                                 weight_kg_typical = 9.1)] # taken from Shemesh2019
  
  # set dose per group (adults: absolute dose; children: mg/kg)
  dt_Geoerger2020[group == "18 yrs or older", dose_mg := 1200]
  dt_Geoerger2020[group != "18 yrs or older", dose_mgkg := 15]
  
  # compute dose-normalized concentrations
  dt_Geoerger2020[group == "18 yrs or older", `:=`(concDoseNorm1mgkg = conc_ugml / (dose_mg / weight_kg_typical),
                                                   sdDoseNorm1mgkg = sd_ugml / (dose_mg / weight_kg_typical))]
  dt_Geoerger2020[group != "18 yrs or older", `:=`(concDoseNorm1mgkg = conc_ugml / dose_mgkg,
                                                   sdDoseNorm1mgkg = sd_ugml / dose_mgkg)]
  
  # Is the dose per BW calculated from absolute dose with typical body weight?
  dt_Geoerger2020[, DosePerBWCalculated := TRUE]
  dt_Geoerger2020[group != "18 yrs or older", DosePerBWCalculated := FALSE]
  
  dt_Geoerger2020[, litID := "Geoerger2020"]
  dt_Geoerger2020[, Ab := "atezolizumab"]
  
  # add dosing cycle (Q3W scheme). 1: first cycle, 2: second cycle etc
  maxTime <- 7*3*9
  dosingTimes <- seq(from = 0, to = maxTime, by = 3*7)
  if (max(dt_Geoerger2020$time_days, na.rm = TRUE) > maxTime) stop("Increase maxTime for dosing cycle")
  dt_Geoerger2020[, dosingCycle := findInterval(time_days, dosingTimes, left.open = TRUE)]
  
  # adding profile ID. Here different group entry is different profile 
  dt_Geoerger2020[, ID := match(group, unique(group))]
  
  #remove NA columns
  dt_Geoerger2020 <- dt_Geoerger2020[ !is.na(conc_ugml) , ]
  
  
  #==== Herbst2014 (adults)
  
  file_Herbst2014 <- file.path("..", "..", "rawData", "atezolizumab", "Herbst2014_atezolizumab_adults.xlsx", fsep = .Platform$file.sep)
  
  dt_Herbts2014 <- as.data.table(readxl::read_excel(file_Herbst2014, sheet = "FigS1_scan", col_names = TRUE))
  setnames(dt_Herbts2014, names(dt_Herbts2014), make.names(names(dt_Herbts2014), unique = TRUE))
  
  time_col <- "time..days."
  conc_cols <- names(dt_Herbts2014)[grepl("concentration.", names(dt_Herbts2014), ignore.case = TRUE)]
  sd_cols <- names(dt_Herbts2014)[grepl("Error.", names(dt_Herbts2014), ignore.case = TRUE)]
  
  if (length(conc_cols) < 1L || is.na(time_col)) {
    stop("Unexpected column layout in Herbst2014_atezolizumab_adults.xlsx: scan sheet does not contain usable time and concentration columns.")
  }
  
  dt_Herbts2014 <- rbindlist(lapply(seq_along(conc_cols), function(i) {
    dose_text <- regmatches(conc_cols[[i]], regexpr("[0-9]+(?:p[0-9]+)?", conc_cols[[i]], ignore.case = TRUE))
    dose_mgkg <- as.numeric(gsub("p", ".", dose_text))
    sd_col <- if (i <= length(sd_cols)) sd_cols[[i]] else NA_character_
    data.table(
      time_days = as.numeric(dt_Herbts2014[[time_col]]),
      conc_ugml = as.numeric(dt_Herbts2014[[conc_cols[[i]]]]),
      sd_ugml = if (!is.na(sd_col)) as.numeric(dt_Herbts2014[[sd_col]]) else NA_real_,
      dose_mgkg = dose_mgkg,
      group = paste0("adult; ", dose_mgkg, " mg/kg")
    )
  }), use.names = TRUE, fill = TRUE)
  
  # adding profile ID. Here different dose entry is different profile 
  dt_Herbts2014[, ID := match(dose_mgkg, unique(dose_mgkg))]
  
  dt_Herbts2014[, concDoseNorm1mgkg := conc_ugml/dose_mgkg]
  dt_Herbts2014[, sdDoseNorm1mgkg := sd_ugml/dose_mgkg]
  
  dt_Herbts2014[, litID := "Herbst2014"]
  dt_Herbts2014[, Ab := "atezolizumab"]
  
  #remove NA columns
  dt_Herbts2014 <- dt_Herbts2014[ !is.na(conc_ugml) , ]
  
  return(list(dataGeoerger2020=dt_Geoerger2020, dataHerbst2014=dt_Herbts2014))
}
