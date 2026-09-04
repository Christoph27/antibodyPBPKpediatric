##########################################################################
# Title: write pagibaximab data for import into OSP suite                               
                                          






#########################################################################




library(data.table)




source(file.path(".", "read_pagibaximab.R", fsep = .Platform$file.sep) )
  
 
PKdata <-  getPagibaximabPK() 

# generate concentration_used_ugml column for concentrations to be written: 
# use baseline corrected concentration for Weisman2009 and reported concentration for Weisman2011 (no baseline values reported) 

PKdata[grepl("Weisman2009", litID), concentration_used_ugml:= conc_adjusted_ugml]
PKdata[grepl("Weisman2011", litID), concentration_used_ugml:= conc_ugml]


# discard times < 0 and write only one concentration column
PKdata[, c("conc_ugml","sd_ugml", "concDoseNorm1mgkg", "sdDoseNorm1mgkg", "conc_adjusted_ugml", "concDoseNorm1mgkg_adjusted"):=NULL]
PKdata <- PKdata[time_days > 0]



# write adult data 
write.csv(x=PKdata,
          file=file.path("..", "..", "final_MS_input_data", "pagibaximab", "pagibaximab_PKdata.csv", fsep = .Platform$file.sep),
          row.names = FALSE
          )

