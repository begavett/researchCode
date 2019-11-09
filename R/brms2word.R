brms2word <- function(brmsObject = NULL, outfile = "Table1", decimalpoints = 2){
  library(officer)
  library(dplyr)
  library(magrittr)
  
  output <- data.frame(summary(brmsObject)$fixed)
  output <- bind_cols(data.frame(Variable = rownames(output)), round(output, decimalpoints))
  
  read_docx() %>%  # a new, empty document
    body_add_table(output, style = "table_template") %>% 
    print(target=paste0(outfile, ".docx"))
}