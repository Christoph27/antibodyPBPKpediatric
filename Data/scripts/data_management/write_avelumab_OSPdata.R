##########################################################################
# Title: write avelumab data for import into OSP suite                               
                                          






#########################################################################




library(data.table)




source(file.path("..", "data_management", "read_avelumab.R", fsep = .Platform$file.sep) )
  
 
avelumabData <-  getAvelumabPK() 

Heery2017_adult_log <- avelumabData$Heery2017_adult_log
Heery2017_adult_lin <- avelumabData$Heery2017_adult_lin
Vugmeyster2022_Fig2a <- avelumabData$Vugmeyster2022_Fig2a
Vugmeyster2022_Fig5 <- avelumabData$Vugmeyster2022_Fig5



# write adult data (using log scan, cf. ../data_analysis/plot_avelumab.R for comparison of data from different plots)
write.csv(x=Heery2017_adult_log,
          file=file.path("..", "..", "final_MS_input_data", "avelumab", "Heery2017_avelumab_adult.csv", fsep = .Platform$file.sep),
          row.names = FALSE
          )

