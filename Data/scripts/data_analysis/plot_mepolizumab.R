##########################################################################
# Title: plot experimental mepolizumab PK data for originally scanned PK and baseline subtracted concentrations                                
                                          




#########################################################################



library(tlf)
library(ggplot2) 
library(scales)
library(viridis)
library(data.table)




source(file.path("..", "data_management", "read_mepolizumab.R", fsep = .Platform$file.sep) )  
  

plotBreaks = 10^seq(-10, 10, by=1 )
plotMinorbreaks = rep(1:9, 21)*(10^rep(-10:10, each=9))

mepolizumabData <-  getMepolizumabPK()


dataIV_Fig2 <- mepolizumabData$adultSmith2011Fig2
dataIV_Fig1 <- mepolizumabData$adultSmith2011_IV_250mg



# compare all IV data

#plot baseline adjusted concentrations for Weisman2009
plotDat = ggplot() +
  geom_point(data = dataIV_Fig2, aes(x=time_days, y=concDoseNorm1mgkg, color=group)) +
  geom_point(data = dataIV_Fig1, aes(x=time_days, y=concDoseNorm1mgkg, color=group)) +
  labs(x = "time [days]", y = "conc. dose norm. [µg/ml/mg/kg]", title = "IV data") +
  scale_y_continuous( trans = 'log10', breaks = plotBreaks, minor_breaks = plotMinorbreaks, limits = c(0.1,100)) 
addWatermark(plotDat, "preliminary data\n preliminary analysis", size = 20, alpha=0.3)


  