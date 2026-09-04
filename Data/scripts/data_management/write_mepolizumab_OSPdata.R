##########################################################################
# Title: write mepolizumab data for import into OSP suite                               
                                          






#########################################################################




library(data.table)




source(file.path(".", "read_mepolizumab.R", fsep = .Platform$file.sep) )
  
 
PKdata <-  getMepolizumabPK()


allDataTable = rbindlist(PKdata, use.names=TRUE, fill=TRUE)




# write adult data 
write.csv(x=allDataTable,
          file=file.path("..", "..", "final_MS_input_data", "mepolizumab", "mepolizumab_PKdata.csv", fsep = .Platform$file.sep),
          row.names = FALSE
          )

