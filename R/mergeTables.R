#mergeTables.R

#' Merge the three dataframes that were retrieved from the csv file.
#' 
#' @param plantsamples The dataframe of plant samples.
#' @param phenoobs The dataframe of phenology observations.
#' @param anntable The dataframe of annual observations.
#' @return A dataframe of merged data from the csv file.
#' 
mergeTables <- function(plantsamples,phenoobs,anntable){
  df <- merge(plantsamples, phenoobs, by = "Tag ID", all = TRUE)
  df$"Tag ID"  <- substr(df$"Tag ID", 19,24)
  df$"Direction From Transect" <- substr(df$"Direction From Transect", 1, 1) 
  df$"Phenophase Status" <- substr(df$"Phenophase Status", 1, 1)
  df$"Phenophase Intensity"[(df$"Phenophase Intensity" %in% c("< 3","3-10","11-100","101-1,000","1,001-10,000","> 10,000"))] <- "\\#"
  df$"Phenophase Intensity"[!(df$"Phenophase Intensity" %in% c("< 3","3-10","11-100","101-1,000","1,001-10,000","> 10,000"))] <- "\\%"
  df$"Remarks" <- character(nrow(df))
  rownames(df, do.NULL = TRUE)
  df <- df[,c(5,1:4,7:9,6,10)]
  names(df) <- c("Spp. Code","Tag ID","Trans. Meter","Dir. from Trans. (R/L)","90 Deg. Dist. (m)",
                 "Phenophase Description","Pheno. \nStatus (Y/N)","Pheno. \nIntensity (\\%/\\#)","Drop Plant","Remarks")
  return(df)
}