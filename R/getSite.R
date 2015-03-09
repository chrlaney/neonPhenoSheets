#getSite.R

#' Get the site
#' 
#' @param file The imported csv file.
#' @return The site from the csv file.
#' @export
getSite <- function(file) {
  line <- as.character(file[3,1])
  line <- sub("[[:punct:]]", "", line)
  str <- sub(pattern = "Site Code: ", replacement = "", x = line)
  return(str)
}