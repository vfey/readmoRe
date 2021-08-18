# Determine field delimiter in text files
# 
# Author: vidal
###############################################################################

#' Determine field delimiter in text files
#' @param file \code{character}. Path name of a text file.
#' @param n \code{integer}. Number of lines to be read by \code{readLines}. Defaults to 1.
#' @param pattern \code{character}. Search pattern to find a specific line for determining the delimiter.
#' @return If successful, the filed delimiter. If more than on of the possible delimiters is found, an error is returned.
#' @seealso \command{\link[base]{readLines}}
#' @keywords utilities
#' @export
get.sep <-
        function(file, n=1, pattern)
{
        ## initial checks
        if (!file.exists(file)) stop("File not found!")
        
        ## read file header
        if (!missing(pattern)) n <- -1
        h <- readLines(file, n, warn=FALSE)
        
        ## search pattern
        pat <- grep(pattern, h)
        if (length(pat) > 1) {
                warning(paste("Pattern found more that once [", length(pat), "times ]. First one is used..."))
                pat <- pat[1]
        }
        
        ## determine delimiter
        ve <- c(",", ";", "\t")
        se <- which(lapply(ve, function(x) grep(x, h[pat])) == 1)
        sep <- ve[se]
        
        ## return
        if (length(sep) == 1) return(sep)
        else if (length(sep) > 1) stop("More than one delimiter found. Try a different row or pattern.")
        else return(NULL)
}
