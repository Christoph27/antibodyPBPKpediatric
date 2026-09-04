##########################################################################
# Title: write palivizumab data for import into OSP suite                               
                                          






#########################################################################




library(data.table)




source(file.path(".", "read_palivizumab.R", fsep = .Platform$file.sep) )
  
 
PKdata <-  getPalivizumabPK() 



# write adult data 
write.csv(x=PKdata,
          file=file.path("..", "..", "final_MS_input_data", "palivizumab", "palivizumab_PKdata.csv", fsep = .Platform$file.sep),
          row.names = FALSE
          )

