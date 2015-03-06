#getPlot.R

#' Get the plot
#' 
#' @param file The imported csv file.
#' @return The plot from the csv file.
getPlot <- function(file) {
  line <- as.character(file[4,1])
  line <- sub("[[:punct:]]", "", line)
  str <- sub(pattern = "Plot Code: ", replacement = "", x = line)
  return(str)
}