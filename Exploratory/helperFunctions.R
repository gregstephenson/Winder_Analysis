combineFactorsByRegularExpression <- function(list, oldFactors, newFactors){
  filter <- list
  returnList <- list
  returnList[1:length(list)] <- "Other"
  
  for(i in seq(1:length(list))){
    cF <- grep(oldFactors[i], filter, ignore.case = TRUE, value = FALSE)
    filter[cF] <- newFactors[i]
    returnList[cF] <- newFactors[i]
  }

  return(returnList)  
}