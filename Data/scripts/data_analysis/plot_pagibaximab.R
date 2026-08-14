##########################################################################
# Title: plot experimental pagibaximab PK data for originally scanned PK and baseline subtracted concentrations                                
                                          




#########################################################################



library(tlf)
library(ggplot2) 
library(scales)
library(viridis)
library(data.table)




source(file.path("..", "data_management", "read_pagibaximab.R", fsep = .Platform$file.sep) )  
  

plotBreaks = 10^seq(-10, 10, by=1 )
plotMinorbreaks = rep(1:9, 21)*(10^rep(-10:10, each=9))

pagibaximabData <-  getPagibaximabPK() 

pagibaximabData_Weisman2009 <- pagibaximabData[grepl("Weisman2009", litID), ]  # Data with baseline
pagibaximabData_Weisman2011 <- pagibaximabData[grepl("Weisman2011", litID), ]  # Data without baseline


#plot baseline adjusted concentrations for Weisman2009
plotDat = ggplot() +
  geom_point(data = pagibaximabData_Weisman2009[dosingCycle %in% c(0,1),], aes(x=time_days, y=concDoseNorm1mgkg_adjusted, color=litID, shape=factor(Dose_mgkg))) +
  geom_point(data = pagibaximabData_Weisman2011[dosingCycle %in% c(0,1),], aes(x=time_days, y=concDoseNorm1mgkg, color=litID, shape=factor(Dose_mgkg))) +
  labs(x = "time [days]", y = "conc. dose norm. [µg/ml/mg/kg]", title = "Weisman2009: baseline adjused concetrations (Cyc. 1)") +
  scale_y_continuous( trans = 'log10', breaks = plotBreaks, minor_breaks = plotMinorbreaks, limits = c(0.06,40)) 
addWatermark(plotDat, "preliminary data\n preliminary analysis", size = 20)

#plot measured concentrations for Weisman2009 incl. endogenous Ab
plotDat = ggplot() +
  geom_point(data = pagibaximabData_Weisman2009[dosingCycle %in% c(0,1),], aes(x=time_days, y=concDoseNorm1mgkg, color=litID, shape=factor(Dose_mgkg))) +
  geom_point(data = pagibaximabData_Weisman2011[dosingCycle %in% c(0,1),], aes(x=time_days, y=concDoseNorm1mgkg, color=litID, shape=factor(Dose_mgkg))) +
  labs(x = "time [days]", y = "conc. dose norm. [µg/ml/mg/kg]", title = "Weisman2009: measured concetrations (Cyc. 1)") +
  scale_y_continuous( trans = 'log10', breaks = plotBreaks, minor_breaks = plotMinorbreaks, limits = c(0.06,40)) 
addWatermark(plotDat, "preliminary data\n preliminary analysis", size = 20)

  
  

  