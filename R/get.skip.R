# TODO: Determine number of lines to skip when readin text files
# 
# Author: vidal
###############################################################################


#' Determine Number of Rows to be Skipped in Text Files
#' @description \command{get.skip} attempts to determine the number of rows that could be skipped when reading text files.
#' @param file (\code{character}). The file name.
#' @param n (\code{integer}). The number of lines to be read by \code{readLines()}.
#' @param pattern (\code{character}). A search pattern like, e.g., a column  name that is used to find a particular
#'     line in the file to determine the \code{skip} value.
#' @return The \code{skip} value. If no value is determined 0 (zero) is returned.
#' @seealso \command{\link[base]{readLines}}
#' @keywords utilities
#' @export
get.skip <-
        function(file, n=1, pattern=NULL)
{
        ## initial checks
        if (!file.exists(file)) stop("File not found!")
        
        ## read file header
        if (!is.null(pattern) && n==1) n <- -1
        h <- readLines(file, n, warn=FALSE)
        
        ## search pattern
		if (is.null(pattern)) pattern <- ""
        pat <- grep(pattern, h)
        if (length(pat) > 1) {
                warning(paste("Pattern found more that once [", length(pat), "times ]. First one is used..."))
                pat <- pat[1]
        }
        
        ## determine skip
        skip <- pat-1
        
        ## return
        if (length(skip) == 1) return(skip)
        else return(0)
}
