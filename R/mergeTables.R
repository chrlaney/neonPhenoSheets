#mergeTables.R

#' Merge the two dataframes that were retrieved from the csv file.
#' 
#' @param plantsamples The dataframe of plant samples.
#' @param phenoobs The dataframe of phenology observations.
#' @return A dataframe of merged data from the csv file.
#' @export
#' 
mergeTables <- function(plantsamples,phenoobs){
  df <- merge(plantsamples, phenoobs, by = "Tag ID", all = TRUE)
  df <- df[which(df$"Drop Plant"=="active"),]
  df$"Drop Plant" <- rep("", nrow(df))
  df$"Tag ID"  <- substr(df$"Tag ID", 19,24)
  df$"From Transect" <- paste0(substr(df$"Direction From Transect", 1, 1), 
                               df$"Ninety Degree Distance")
  df$"Phenophase Status" <- substr(df$"Phenophase Status", 1, 1)
  df$"Phenophase Intensity" <- sub(df$"Phenophase Intensity", pattern = "%", replacement = "")
  
  countlevels <- c("< 3","3 to 10","11 to 100","101 to 1000","1001 to 10,000","> 10000")
  numbers <- which(df$"Phenophase Intensity" %in% countlevels)
  percents <- which(!df$"Phenophase Intensity" %in% countlevels)              
  
  df$"Prev. Pheno. Status Intensity" <- character(nrow(df))
  df$"Prev. Pheno. Status Intensity"[numbers] <- paste(
    df$"Phenophase Status"[numbers], " ", df$"Phenophase Intensity"[numbers], 
    "\\#", sep = "")
  df$"Prev. Pheno. Status Intensity"[percents] <- paste(
    df$"Phenophase Status"[percents], " ", df$"Phenophase Intensity"[percents], 
    "\\%", sep = "")  
  
  df$"Current Status Intensity" <- character(nrow(df))
  df$"Remarks" <- character(nrow(df))
  rownames(df, do.NULL = TRUE)
  df <- df[,c(5,1:2,10,7,11,12,6,13)]
  names(df) <- c("Taxon ID","Tag ID", "Trans. Meter","From Trans.",
                 "Phenophase Desc.", "Past Status Intensity",
                 "Current Status Intensity","Drop","Remarks")
  return(df)
}