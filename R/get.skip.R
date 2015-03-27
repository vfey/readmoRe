# TODO: Determine number of lines to skip when readin text files
# 
# Author: vidal
###############################################################################


get.skip <-
        function(file, n=1, pattern)
{
        ## initial checks
        if (!file.exists(file)) stop("File not found!")
        
        ## read file header
        if (!missing(pattern) && n==1) n <- -1
        h <- readLines(file, n, warn=FALSE)
        
        ## search pattern
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
