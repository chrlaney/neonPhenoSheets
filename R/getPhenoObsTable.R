#getPhenoObsTable.R

#' Get the section of the csv file that comprises the Phenology Observations.
#' 
#' @param df The imported dataframe.
#' @return A dataframe of phenological observations from the csv file.
getPhenoObsTable <- function(df){
  obsrow <- which(grepl("Phenophase Observations:", df[,1]))
  startrow <- obsrow + 1
  str <- as.character(df[obsrow,1])
  str <- sub("[[:punct:]]", "", str)
  str <- as.numeric(sub(pattern = "Phenophase Observations: ", replacement = "", x = str))
  str <- gsub("[[:space:]]", "", str) 
  numobs <- as.numeric(str)
  endrow <- startrow + numobs
  phenodf <- data.frame(df[startrow:endrow, c(2,8:10)], row.names = NULL)
  colnames(phenodf) <- phenodf[1,]
  phenodf <- phenodf[2:nrow(phenodf), ]
  return(phenodf)
}