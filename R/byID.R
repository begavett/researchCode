byID <- function(dataset, ID, column, func){
  XXXXX <- dataset[,which(names(dataset) == column)]
  YYYYY <- dataset[,which(names(dataset) == ID)]
  temp <- unlist(tapply(XXXXX, YYYYY, func))
}