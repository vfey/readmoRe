# TODO: Determine field delimiter in text files
# 
# Author: vidal
###############################################################################


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
