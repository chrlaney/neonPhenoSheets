#getPlantSamplesTable.R

#' Get the section of the csv file that comprises the Plant Samples data.
#' 
#' @param df The imported dataframe.
#' @return A dataframe of plant sample observations from the csv file.
#' @export 
getPlantSamplesTable <- function(df){
  obsrow <- which(grepl("Plant Samples:", df[,1]))
  startrow <- obsrow + 1
  str <- as.character(df[obsrow,1])
  str <- sub("[[:punct:]]", "", str)
  str <- as.numeric(sub(pattern = "Plant Samples: ", replacement = "", x = str))
  str <- gsub("[[:space:]]", "", str) 
  numobs <- as.numeric(str)
  endrow <- startrow + numobs
  phenodf <- data.frame(df[startrow:endrow, c(2,8:11,18)], row.names = NULL)
  colnames(phenodf) <- phenodf[1,]
  phenodf <- phenodf[2:nrow(phenodf), ]
  return(phenodf)
}