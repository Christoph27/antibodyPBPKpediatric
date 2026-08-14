##########################################################################
# Title: plot and compare scanned experimental avelumab data                                
                                          






#########################################################################




library(tlf)
library(ggplot2) 
library(scales)
library(viridis)
library(data.table)



source(file.path("..", "data_management", "read_avelumab.R", fsep = .Platform$file.sep) )
  
  

plotBreaks = 10^seq(-10, 10, by=1 )
plotMinorbreaks = rep(1:9, 21)*(10^rep(-10:10, each=9))

avelumabData <-  getAvelumabPK() 

Heery2017_adult_log <- avelumabData$Heery2017_adult_log
Heery2017_adult_lin <- avelumabData$Heery2017_adult_lin
Vugmeyster2022_Fig2a <- avelumabData$Vugmeyster2022_Fig2a
Vugmeyster2022_Fig5 <- avelumabData$Vugmeyster2022_Fig5


# test plot comparison of pediatric plots of supplementary Fig2a & supplementary Fig5

plot25 = ggplot() +
  geom_point(data = Vugmeyster2022_Fig5[dose_mgkg==10, ], aes(x=time_days, y=conc_ugml, color=as.factor(weight_kg), shape="Fig.5")) +
  geom_point(data = Vugmeyster2022_Fig2a[dosingCycle==1, ], aes(x=time_days, y=conc_ugml, color=as.factor(weight_kg), shape="Fig.2a")) +
  labs(x = "time [days]", y = "conc. [µg/ml]", shape = "Data source", title="pediatric data, Fig2a vs. Fig5a") +
  scale_shape_manual(values = c("Fig.5" = 17, "Fig.2a" = 19)) +
  scale_y_continuous( trans = 'log10', breaks = plotBreaks, minor_breaks = plotMinorbreaks) 
  addWatermark(plot25, "preliminary data\n preliminary analysis", size = 20)
  
  

  
  # test plot comparison of lin vs log for adult data 
  plotHL = ggplot() +
    geom_point(data = Heery2017_adult_lin, aes(x=time_days, y=conc_ugml, color=as.factor(dose_mgkg), shape="linear scale")) +  
    geom_point(data = Heery2017_adult_log[dose_mgkg %in% c(10,20)], aes(x=time_days, y=conc_ugml, color=as.factor(dose_mgkg), shape="log scale")) +  
    labs(x = "time [days]", y = "conc. [µg/ml]", shape = "Data scale", title="adult data, scan from log (Fig. 2D) vs lin scale (Fig. 2C)" ) +
    scale_shape_manual(values = c("linear scale" = 17, "log scale" = 19)) + # 17: triangle, 19: circle
    scale_y_continuous( trans = 'log10', breaks = plotBreaks, minor_breaks = plotMinorbreaks) 
  addWatermark(plotHL, "preliminary data\n preliminary analysis", size = 20)
  
  
  
  
  
  # === plot ped. vs adult =======
  
 
  
  # Define colors using ggplot2's default palette and manually set "Adult" to black
  age_levels = unique(Vugmeyster2022_Fig5$age_yrs)
  default_colors = scales::hue_pal()(length(age_levels))
  names(default_colors) = age_levels
  colorsPlotting = c("Adult" = "black", default_colors)
  
  # error bar plot. For large error bars, plot only upper one (ymin=y)
  plotHL = ggplot() +
    geom_point(data = Heery2017_adult_log[dose_mgkg %in% c(10,20)], aes(x=time_days, y=concDoseNorm1mgkg, color = age_yrs, shape = factor(dose_mgkg)) ) +
    geom_errorbar(data = Heery2017_adult_log[dose_mgkg %in% c(10,20)], aes(
      x=time_days, 
      ymin=ifelse(concDoseNorm1mgkg - sdDoseNorm1mgkg < 0, concDoseNorm1mgkg, concDoseNorm1mgkg - sdDoseNorm1mgkg), 
      ymax=concDoseNorm1mgkg + sdDoseNorm1mgkg), 
      width=.2, position=position_dodge(0.05)) +
    
    geom_point(data = Vugmeyster2022_Fig5, aes(x=time_days, y=concDoseNorm1mgkg, color = as.factor(age_yrs), shape = factor(dose_mgkg))  ) +
    
    labs(x = "time [days]", y = "conc. dose normalized [µg/ml/mg/kg]" ) +
    scale_y_continuous( trans = 'log10', breaks = plotBreaks, minor_breaks = plotMinorbreaks, limits = c(1e-1,60)) +
    scale_color_manual(name = "Age [years]", values = colorsPlotting) 
  addWatermark(plotHL, "preliminary data\n preliminary analysis", size = 26, alpha =0.5)
  
  
  
  
  
  
  # 20 mg   --- TEST COLORS   ==== 1 =====
  
  
  age_levels = sort(unique(Vugmeyster2022_Fig5$age_yrs))
  
  # Define color palette
  child_colors <- colorRampPalette(c("green", "blue"))(length(age_levels))
  names(child_colors) = age_levels
  colorsPlotting = c("Adult" = "black", child_colors)
  
  
  
  # error bar plot. For large error bars, plot only upper one (ymin=y)
  plotHL = ggplot() +
    geom_point(data = Heery2017_adult_log[dose_mgkg ==20], aes(x=time_days, y=conc_ugml, color = age_yrs), shape=15 ) +
    geom_errorbar(data = Heery2017_adult_log[dose_mgkg ==20], aes(
      x=time_days, 
      ymin=ifelse(conc_ugml - sd_ugml < 0, conc_ugml, conc_ugml - sd_ugml), 
      ymax=conc_ugml + sd_ugml), 
      width=.2, position=position_dodge(0.05)) +
   
    geom_point(data = Vugmeyster2022_Fig5[dose_mgkg==20, ], aes(x=time_days, y=conc_ugml, color = as.factor(age_yrs)), shape=19  ) +
 
    labs(x = "time [days]", y = "conc. [µg/ml]", title="dose: 20 mg" ) +
    scale_y_continuous( trans = 'log10', breaks = plotBreaks, minor_breaks = plotMinorbreaks, limits = c(1e1,1000)) +
    scale_color_manual(name = "Age [years]", values =  colorsPlotting, breaks = c(as.character(age_levels), "Adult"), labels = c(as.character(age_levels), "Adult")) 
  addWatermark(plotHL, "preliminary data\n preliminary analysis", size = 26, alpha =0.5)
  
  
  
  