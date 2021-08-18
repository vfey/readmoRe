# Determine the number of lines in a (large) text file without importing it
# 
# Author: vidal
###############################################################################

#' Determine the number of lines in a (large) text file without importing it.
#' 
#' @param file \code{character of length 1}. File path.
#' @param n \code{integer}. Maximum number of lines to read when determining the 'skip' value. Passed to 'get.skip'.
#' @param pattern \code{character}. Search pattern to find a certain row in 'file'. Passed to 'get.skip'.
#' @param incl.header \code{logical}. Should the file header be included in the count? Length of the header will be determined by 'get.skip'
#'     and the 'pattern' argument. Defaults to 'FALSE'.
#' @return An integer value.

#' @export
get.nlines <-
  function(file, n=1, pattern=NULL, incl.header=FALSE)
  {
    f.nrow <- system2("wc", c("-l", file, " | awk '{print $1}'"), stdout = TRUE)
    if (!incl.header) {
      f.head <- get.skip(file, n=n, pattern=pattern)
      f.nrow <- as.integer(f.nrow)-f.head
    }
    return(f.nrow)
  }
