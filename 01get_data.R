#' Get User Input Data File
#'
#' Prompts the user to input a CSV file and reads the data.
#' 
#' The file must be in the form of a contingency table with names of categories
#' in the first row and first column
#'
#' @return A data frame read from the specified file.
#' @examples
#' data <- 01_get_data()
get_data <- function() {
   file_path <- readline(prompt = "Enter the path to your CSV file: ")
   if (file.exists(file_path)) {
      data <- read.csv(file_path,header = TRUE, row.names=1)
      return(data)
   } else {
      stop("File does not exist. Please check the path and try again.")
   }
}

