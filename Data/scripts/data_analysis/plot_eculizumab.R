##########################################################################
# Plot eculizumab PK                                




#########################################################################

library(ggplot2)
library(tlf)



plotBreaks = 10^seq(-10, 10, by=1 )
plotMinorbreaks = rep(1:9, 21)*(10^rep(-10:10, each=9))


source(file.path("..", "data_management", "read_eculizumab.R", fsep = .Platform$file.sep) )

eculizumabPK <- getEculizumabPK()
expData_Cho2020 <- eculizumabPK$adultCho2020
expData_Jodele2014 <- eculizumabPK$pediatricJodele2014

#Plot healthy adults + pediatric patients

# Define colors using ggplot2's default palette and manually set "Adult" to black
age_levels = unique(expData_Jodele2014$group)
default_colors = scales::hue_pal()(length(age_levels))
names(default_colors) = age_levels
colorsPlotting = c("healthy adults" = "black", default_colors)


# error bar plot. For large error bars, plot only upper one (ymin=y)
plotHL = ggplot() +
  geom_point(data = expData_Cho2020, aes(x=time_days, y=concDoseNorm1mgkg, color = group), shape=19, size=2 ) +
  geom_errorbar(data = expData_Cho2020, aes(
    x=time_days, 
    ymin=ifelse(concDoseNorm1mgkg - sdDoseNorm1mgkg < 0, concDoseNorm1mgkg, concDoseNorm1mgkg - sdDoseNorm1mgkg), 
    ymax=concDoseNorm1mgkg + sdDoseNorm1mgkg), 
    width=.2, position=position_dodge(0.05)) +
  
  geom_point(data = expData_Jodele2014, aes(x=time_days, y=concDoseNorm1mgkg, color = as.factor(group)), shape=19  ) +
  
  labs(x = "time [days]", y = "dose normalized conc. [µg/ml/(mg/kg)]" ) +
  scale_y_continuous( trans = 'log10', breaks = plotBreaks, minor_breaks = plotMinorbreaks, limits = c(1e-1,30)) +
  scale_color_manual(name = "Data", values = colorsPlotting) 
addWatermark(plotHL, "preliminary data\n preliminary analysis", size = 26, alpha =0.5)



# to do: add adult aHUS patient (ID2) from Gatault2015 ?? (after discontinuation though ..., i.e. not first dose)

