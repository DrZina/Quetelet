#' @export
#' Process a Table and Add Totals
#'
#' This function processes a data table by setting the first row as column headers
#' and the first column as row names, then adds a column of row totals and a row of column totals.
#'
#' @param mytable A data frame or matrix where the first row and column contain headers.
#' @return A modified data frame with row and column totals.
#' @examples
#' # Assuming `mytable` is a data frame with numeric values starting at row 2, col 2
#' processed_table <- add_totals(mytable)
add_totals <- function(mytable) {
   # Check if the input is a data frame or matrix
   if (!is.data.frame(mytable) && !is.matrix(mytable)) {
      stop("Input must be a data frame or matrix.")
   }

   # Set first row as column headers
   colnames(mytable) <- as.character(mytable[1, ])
   mytable <- mytable[-1, , drop = FALSE] # Remove the first row

   # Set first column as row headers
   rownames(mytable) <- mytable[, 1]
   mytable <- mytable[, -1, drop = FALSE] # Remove the first column

   # Convert remaining elements to numeric
   mytable <- as.data.frame(lapply(mytable, as.numeric), row.names = rownames(mytable))

   # Add a column for row totals
   mytable$RowTotal <- rowSums(mytable, na.rm = TRUE)

   # Add a row for column totals
   column_totals <- colSums(mytable, na.rm = TRUE)
   mytable <- rbind(mytable, ColumnTotal = column_totals)

   return(mytable)
}
