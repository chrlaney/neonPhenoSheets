#mergeTables.R

#' Merge the two dataframes that were retrieved from the csv file.
#' 
#' @param plantsamples The dataframe of plant samples.
#' @param phenoobs The dataframe of phenology observations.
#' @return A dataframe of merged data from the csv file.
#' 
mergeTables <- function(plantsamples,phenoobs){
  df <- merge(plantsamples, phenoobs, by = "Tag ID", all = TRUE)
  countlevels <- c("< 3","3-10","11-100","101-1,000","1,001-10,000","> 10,000")
  df$"Tag ID"  <- substr(df$"Tag ID", 19,24)
  df$"From Transect" <- paste0(substr(df$"Direction From Transect", 1, 1), 
                               df$"Ninety Degree Distance")
  df$"Phenophase Status" <- substr(df$"Phenophase Status", 1, 1)
  df$"Phenophase Intensity"[(df$"Phenophase Intensity" %in% countlevels)] <- "\\#"
  df$"Phenophase Intensity"[!(df$"Phenophase Intensity" %in% countlevels)] <- "\\%"
  df$"Remarks" <- character(nrow(df))
  rownames(df, do.NULL = TRUE)
  df <- df[,c(5,1:2,10,7:9,6,11)]
  names(df) <- c("Taxon ID","Tag ID", "Trans. Meter","From Trans.",
                 "Phenophase Desc.","Pheno. Status (y/n/m/?)",
                 "Pheno. Intensity","Drop","Remarks")
  return(df)
}