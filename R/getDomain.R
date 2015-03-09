#getDomain.R

#' Get the domain.
#' 
#' @param file The imported csv file.
#' @return The domain from the csv file.
#' @export
getDomain <- function(file) {
  line <- as.character(file[2,1])
  line <- sub("[[:punct:]]", "", line)
  str <- sub(pattern = "Domain Code: ", replacement = "", x = line)
  return(str)
}
