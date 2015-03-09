#getProtocol.R

#' Get the protocol code
#' 
#' @param file The imported csv file.
#' @return The protocol code from the csv file.
#' @export
getProtocol <- function(file) {
  line <- as.character(file[5,1])
  line <- sub("[[:punct:]]", "", line)
  str <- sub(pattern = "Protocol Code: ", replacement = "", x = line)
  return(str)
}