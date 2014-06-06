# TODO: handle newline characters in data frames: remove at end of line or replace by space within line
# 
# Author: vidal
###############################################################################


rm.newline.chars <-
                function(x, verbose = TRUE)
{
        
        nx <- names(x)
        nle <- try(apply(x, 2, function(z) length(grep("\n[[:blank:]]{0,}$", z)) > 0), silent = TRUE)
        if (is(nle, "try-error")) return(x)
        if (any(nle)) {
                if (verbose) cat(paste("Found newline characters at end of line in column(s) _", names(x)[nle], "_.", sep=""), "Removing from:\n")
                x <- lapply(1 : ncol(x), function(z) {
                                        if (verbose) {
                                                if (names(x)[z] %in% names(x)[nle]) cat("  ", names(x)[z], "\n")
                                        }
                                        if (is.character(x[[z]])) x[[z]] <- sub("\n[[:blank:]]{0,}$", "", x[[z]])
                                        x[[z]]
                                })
                x <- as.data.frame(x, stringsAsFactors=FALSE)
                names(x) <- nx
                if (verbose) cat("...done\n\n")
        }
        nl <- try(apply(x, 2, function(z) length(grep("\n", z)) > 0), silent = TRUE)
        if (is(nl, "try-error")) return(x)
        if (any(nl)) {
                if (verbose) cat(paste("Found newline characters within line in column(s) _", names(x)[nle], "_.", sep=""), "Removing from:\n")
                x <- lapply(1 : ncol(x), function(z) {
                                        if (verbose) {
                                                if (names(x)[z] %in% names(x)[nl]) cat("  ", names(x)[z], "\n")
                                        }
                                        if (is.character(x[[z]])) x[[z]] <- sub("\n", " ", x[[z]])
                                        x[[z]]
                                })
                x <- as.data.frame(x, stringsAsFactors=FALSE)
                names(x) <- nx
                if (verbose) cat("...done\n\n")
        }
        return(x)
        
}

