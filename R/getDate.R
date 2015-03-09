#getDate.R

#' Get the date.
#' 
#' @param file The imported csv file.
#' @return The date from the csv file.
#' @export
getDate <- function(file) {
  line <- as.character(file[1,1])
  line <- sub("[[:punct:]]", "", line)
  str <- sub(pattern = "Date: ", replacement = "", x = line)
  return(str)
}
