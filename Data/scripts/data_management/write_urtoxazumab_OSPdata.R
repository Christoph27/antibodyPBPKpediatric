##########################################################################
# Title: write urtoxazumab data for import into OSP suite                               
                                          






#########################################################################




library(data.table)




source(file.path(".", "read_urtoxazumab.R", fsep = .Platform$file.sep) )
  
 
PKdata <-  getUrtoxazumabPK() 




# write adult data 
write.csv(x=PKdata,
          file=file.path("..", "..", "final_MS_input_data", "urtoxazumab", "urtoxazumab_PKdata.csv", fsep = .Platform$file.sep),
          row.names = FALSE
          )

