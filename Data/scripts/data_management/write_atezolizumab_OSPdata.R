##########################################################################
# Title: write atezolizumab data for import into OSP suite                               
                                          



#########################################################################




library(data.table)




source(file.path(".", "read_atezolizumab.R", fsep = .Platform$file.sep) )
  
 
PKdata <-  getAtezolizumabPK() 

Georger2020 <- PKdata$dataGeoerger2020
Herbst2014 <- PKdata$dataHerbst2014



# write adult data 
write.csv(x=Herbst2014,
          file=file.path("..", "..", "final_MS_input_data", "atezolizumab", "Herbst2014_atezolizumab_adult.csv", fsep = .Platform$file.sep),
          row.names = FALSE
          )

