
#' insert a row at a specific position of a data frame
#'
#' @param existingDF A data frame to add the row to
#' @param newrow The row of data to add
#' @param r The row number to insert the row to.
#'
#' @export
# http://stackoverflow.com/questions/11561856/add-new-row-to-dataframe
# r is the position to insert the new row
# the user needs to make sure the newrow is consistent with the df

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}
