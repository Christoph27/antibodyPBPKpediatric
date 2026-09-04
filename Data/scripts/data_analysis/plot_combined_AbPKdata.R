##########################################################################
# Title: Combine data from different antibodies and plot                           
# 


#########################################################################




library(ggplot2) 
library(tlf)
library(data.table)
library(patchwork)


source(file.path("..", "data_management", "read_atezolizumab.R", fsep = .Platform$file.sep) )
source(file.path("..", "data_management", "read_avelumab.R", fsep = .Platform$file.sep) )
source(file.path("..", "data_management", "read_pagibaximab.R", fsep = .Platform$file.sep) )
source(file.path("..", "data_management", "read_palivizumab.R", fsep = .Platform$file.sep) )
source(file.path("..", "data_management", "read_urtoxazumab.R", fsep = .Platform$file.sep) )
source(file.path("..", "data_management", "read_ustekinumab.R", fsep = .Platform$file.sep) )




# === read atezolizumab

atezolizumabPK <- getAtezolizumabPK()

atezolizumabGoerger2020 <- atezolizumabPK$dataGeoerger2020
atezolizumabGoerger2020[, group := paste(group, litID, sep=", ")]
# minimal data for plotting PK after first dose
atezolizumabGoerger2020_minCyc1 <- atezolizumabGoerger2020[dosingCycle==1, ]
atezolizumabGoerger2020_minCyc1 <- atezolizumabGoerger2020_minCyc1[, c("ID", "time_days", "concDoseNorm1mgkg", "sdDoseNorm1mgkg", "Ab", "group")]


atezolizumabHerbst2014 <- atezolizumabPK$dataHerbst2014
atezolizumabHerbst2014[, group := paste(group, litID, sep=", ")]
#only high dose
atezolizumabHerbst2014_min <- atezolizumabHerbst2014[dose_mgkg >=15, ]
atezolizumabHerbst2014_min <- atezolizumabHerbst2014_min[, c("ID", "time_days", "concDoseNorm1mgkg", "sdDoseNorm1mgkg", "Ab", "group")]

atezolizumabPlot <- rbind(atezolizumabHerbst2014_min, atezolizumabGoerger2020_minCyc1)


# === read urtoxazumab

urtoxazumabPK <- getUrtoxazumabPK()

# plot only high dose adult data (same dose as for children)
urtoxazumabPK_highAdultDose <- urtoxazumabPK[!group %in% c("adult (0.1 mg/kg)", "adult (0.3 mg/kg)")] 

# minimal data for plotting PK 
urtoxazumabPK_min <-urtoxazumabPK_highAdultDose[, c("ID", "time_days", "concDoseNorm1mgkg", "sdDoseNorm1mgkg", "Ab", "group")]




# === read avelumab

avelumabPK = getAvelumabPK()

#use adult log & children Fig5 data for now (cf. to "plot_avelumab.R" for a comparison of scanned data from different figures)
# use 20 mg data for now
avelumabPK_adult <- avelumabPK$Heery2017_adult_log
avelumabPK_ped <- avelumabPK$Vugmeyster2022_Fig5

avelumabPK_adult_min <- avelumabPK_adult[dose_mgkg==20, c("ID", "time_days", "concDoseNorm1mgkg", "sdDoseNorm1mgkg", "Ab", "group")]
avelumabPK_ped_min <- avelumabPK_ped[dose_mgkg==20, c("ID", "time_days", "concDoseNorm1mgkg", "sdDoseNorm1mgkg", "Ab", "group")]

avelumabPK_all_min <- rbind(avelumabPK_adult_min, avelumabPK_ped_min)


# read palivizumab

palivizumabPK  <-  getPalivizumabPK()
palivizumabPK_min <- palivizumabPK[palivizumabPK$dosingCycle==1, c("ID", "time_days", "concDoseNorm1mgkg", "sdDoseNorm1mgkg", "Ab", "group") ]


# read ustekinumab

ustekinumabPK  <-  getUstekinumabPK()
ustekinumabPK[, sdDoseNorm1mgkg := NaN]
ustekinumabPK_plot <- ustekinumabPK[, c("ID", "time_days", "concDoseNorm1mgkg", "sdDoseNorm1mgkg", "Ab", "group") ]


# read pagibaximab 
# to do (cf. plot_pagibaximab.R)



# =============== COMBINE ALL DATA

#add to all data
allDataCycle1 <-rbind(atezolizumabPlot, urtoxazumabPK_min, avelumabPK_all_min, palivizumabPK_min, ustekinumabPK_plot)

# add an unique profile ID for the combined data
allDataCycle1[, IDu := rleid(ID, group)]


allDataCycle1$ID
allDataCycle1$group
intersect(allDataCycle1$ID, allDataCycle1$group)



# ===== Plotting ====



plotBreaks = 10^seq(-10, 10, by=1 )
plotMinorbreaks = rep(1:9, 21)*(10^rep(-10:10, each=9))

# color and (legend) order for different groups according to age (adult: black & color gradient for children)

extract_group_age <- function(group) {
  if (grepl("to", group, ignore.case = TRUE)) {  # mean age in case of a range with "to" e.g. "12 to 17"
    parts <- unlist(strsplit(group, "to", fixed = TRUE))
    lower <- as.numeric(gsub("[^0-9\\.]", "", head(parts, 1)))
    upper <- as.numeric(gsub("[^0-9\\.]", "", tail(parts, 1)))
    age <- mean(c(lower, upper))
  } else { # all other cases (no "to"): use the first captured number
    age <- as.numeric(sub(".*?(\\d+\\.?\\d*).*", "\\1", group))
  }
  if (is.na(age)) {
    warning(sprintf("Could not extract an age from '%s'; defaulting to -10.", group))
    age <- -10
  }
  return(age)
}

all_groups <- unique(allDataCycle1$group)

adult_groups <- all_groups[grepl("adult|older", all_groups, ignore.case = TRUE)]
child_groups <- setdiff(all_groups, adult_groups)


adult_group_order <- c(
  "adult (20 mg/kg, Fig2D_log)",
  "adult; 20 mg/kg, Herbst2014",
  "adult; 15 mg/kg, Herbst2014",
  "adult (20 mg/kg)",
  "adult (15 mg/kg)",
  "adult (3 mg/kg)",
  "adult (1 mg/kg)",
  "adult",
  "18 older"
)


adult_groups_sorted <- unique(c(adult_group_order[adult_group_order %in% adult_groups], setdiff(adult_groups, adult_group_order)))
child_groups_sorted <- child_groups[order(-vapply(child_groups, extract_group_age, numeric(1)))]


group_order <- c(adult_groups_sorted, child_groups_sorted)

adult_colors <- setNames(rep("black", length(adult_groups_sorted)), adult_groups_sorted)
#child_palette <- grDevices::colorRampPalette(c("#6A00A8FF", "#FCCE25FF"))(length(child_groups_sorted))
child_palette <- grDevices::colorRampPalette(c("#1C86EE", "#00FF00"))(length(child_groups_sorted))
child_colors <- setNames(child_palette, child_groups_sorted)

group_colors <- c(adult_colors, child_colors)


# convert "group" to a factor with levels defined in "group_order":
allDataCycle1$group <- factor(allDataCycle1$group, levels = group_order)


# shape definitions for different antibodies
ab_shapes <- c(
  "atezolizumab" = 16, # 16: circle (solid, small)
  "avelumab" = 17, # 17: triangle (up, solid)
  "palivizumab" = 15, # 15: square (solid)
  "ustekinumab" = 12, # 
  "urtoxazumab" = 18) # 18 diamond







#  plot PK data  plot all in one

plotHL <-  ggplot(data = allDataCycle1, aes(x=time_days, y=concDoseNorm1mgkg, color=group, group=IDu)) +
  geom_pointrange(aes( ymin=concDoseNorm1mgkg-sdDoseNorm1mgkg, ymax=concDoseNorm1mgkg+sdDoseNorm1mgkg, shape=Ab)) +
 # geom_line() +
 # geom_line(linetype="dashed") +
   labs(x = "time [days]", y = "dose normaliced conc. [µg/ml/(mg/kg)]", color = "group") +
  scale_y_continuous( trans = 'log10', breaks = plotBreaks, minor_breaks = plotMinorbreaks) + 
  scale_color_manual(values = group_colors, breaks = group_order) +   # limits within scale_y_continuous loses lines
  scale_shape_manual(values = ab_shapes) +
  coord_cartesian(ylim = c(0.1, 30))  # just clips the display
  addWatermark(plotHL, "preliminary data\n preliminary analysis", size = 20, alpha =0.4)
  
  
  
  # ======== plot facets with legend for each facet
  
  facets <- unique(allDataCycle1$Ab)
  
  # Create a list of plots
  plot_list <- lapply(facets, function(facet_val) {
    # Subset data for this specific facet
    subset_data <- allDataCycle1[allDataCycle1$Ab == facet_val, ]
    
    # Create the plot
    p <- ggplot(data = subset_data, aes(x=time_days, y=concDoseNorm1mgkg, color=group, group=IDu)) +
      geom_pointrange(aes(ymin=concDoseNorm1mgkg-sdDoseNorm1mgkg, ymax=concDoseNorm1mgkg+sdDoseNorm1mgkg)) +
      geom_line( linetype="dashed") +
      labs(x = "time [days]", y = "dose normalized conc. [µg/ml/(mg/kg)]", color = paste("Group (", facet_val, ")")) +
      scale_y_continuous(trans = 'log10', breaks = plotBreaks, minor_breaks = plotMinorbreaks) +
      scale_color_manual(values = group_colors, breaks = group_order) +
      coord_cartesian(ylim = c(0.1, 30)) +
      ggtitle(facet_val) +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, face = "bold")) # Ensure legend is visible for each
    
    
    addWatermark(p, "preliminary data\n preliminary analysis", size = 20, alpha = 0.4)
  })
  
  # Combine the plots (e.g., in a 2-column layout)
  final_plot <- wrap_plots(plot_list, ncol = 2)
  
  # Print the final result
  print(final_plot)
  
  # =======
  
 # save.image("plot_combined_AbPKdata_DATA.RData")
  
  
  