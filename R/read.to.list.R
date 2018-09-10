#' @title Utilities for data import
#' @docType package
#' @name readR
#' @description A collection of utilities for reading and importing data into R by performing (usually small) manipulations of
#'     data structures such as data frames, matrices and list and automatically determining import parameters.
#' @author Vidal Fey <vidal.fey@geneviatechnologies.com>
#' @details \tabular{ll}{
#' Package: \tab readR\cr
#' Type: \tab Package\cr
#' Initial version: \tab 0.1-0\cr
#' Created: \tab 2011-01-07\cr
#' License: \tab Internal use only\cr
#' LazyLoad: \tab yes\cr
#' }
#' The main function of the package is \code{read.to.list} which reads a number of different file formats into a list of data objects
#'     such as data frames, depending on the source file.
#' @keywords package
#' @import R.utils
#' @importFrom methods is
#' @importFrom utils packageVersion read.delim
#' @importFrom gdata read.xls
NULL
#'
#' Read various input file formats into a list of data frames. Wrapper function for 'read2list' to automate
#'     reading further and avoid errors due to missing folders or files.
#' @description \command{read.to.list} is meant to act as a universal reading function as it attempts to read
#'     a number of different file formats into a list of data frames.
#' @param dat \code{character}. File path.
#' @param type \code{character}. File extension to be read: one of ".txt", ".tsv", ".csv", ".vcf", ".gtf", ".gff", ".xls", ".xlsx", ".xdr", ".RData".
#' @param folder \code{character}. Folder where the file is found.
#' @param nsheets \code{integer}. Number of sheets to be read if file is of type ".xls" or ".xlsx". All sheets starting from 1 up to the
#'     given number in the respective data file will be read. If more than one file is read this must be be an integer vector with the
#'     numbers of sheets in exactly the same order as the files. 
#' @param sheet \code{integer}. Sheet(s) to be read if file is of type ".xls" or ".xlsx". One sheet defined by the given integer in the
#'     respective data file will be read. If more than one file is read this must be be an integer vector with the sheet numbers in exactly
#'     the same order as the files.
#' @param skip \code{integer}. Number of lines to skip from the top of the file.
#' @param sep \code{character}. Field delimiter passed to 'read.delim' when reading text files.
#' @param lines \code{lines}. Should the file be read line by line into a character vector by readLines()?
#' @param dec \code{character}. The decimal separator for numbers.
#' @param ... Additional arguments passed to 'read2list()'.
#' @param verbose \code{logical}. Should verbose output be printed?
#' @param x.verbose \code{logical}. Should extended verbose output be printed?
#' @return A list of data frames.
#' @seealso \code{\link[base]{readLines}}
#' @seealso \code{\link[utils]{read.delim}}
#' @seealso \code{\link[gdata]{read.xls}}
#' @seealso \code{\link[base]{load}}
#' @seealso \code{\link[R.utils]{loadObject}}
#' @keywords utilities
#' @export
read.to.list <-
        function (dat, type, folder, nsheets = 1, sheet = NULL, skip = 0, sep = NULL, lines = FALSE, dec = NULL, ..., verbose = TRUE,
                  x.verbose = FALSE)
{
        
        ## initial checks
        if (!is.character(dat)) stop("'dat' must be a character vector!")
        
		if (verbose) cat("@ MULTI DATA LOADER\n")
		
        ## define valid file extensions
        ext <- c(".txt", ".tsv", ".csv", ".vcf", ".gtf", ".gff", ".xls", ".xlsx", ".xdr", ".RData") ## index of valid file extensions
        
        ## look for certain file types; if set, 'dat' will be ignored and set to "all"
        if (!missing(type)) {
                dat <- "all"
                if (type %in% ext) ext <- type
                else stop("File type not supported!")
        }
        
        ## read all files of the supported types in the given directory
        #### WARNING: This option should be used only if the user is certain regarding the contents of the files in that directory!
        if (length(dat) == 1 && dat == "all") {
                if (missing(folder)) stop("Folder name missing!")
                fls <- dir(path = folder, full.names=TRUE, recursive=TRUE)
                ext.all <- sub(".+(\\.[a-z]{3,5}$)", "\\1", basename(fls)) ## extract file extensions
                valid <- ext.all %in% ext
                if (!any(valid)) stop("No valid files found in directory.")
                fln <- fls[valid] ## generate vector of paths of valid files 
        } else {  ## read certain files
                if (missing(folder)) {
                        ## check for existence of files if folder name is missing
                        ## (that is, if 'dat' is a (vector of) file path(s) or the working directory is presumed to contain the file(s))
                        dat <- suppressWarnings(normalizePath(dat))
                        if (any(file.exists(dat))) fln <- dat[which(file.exists(dat))] ## generate (vector of) paths of existing files
                        else stop("File not found! Provide complete file path in 'dat' or use the 'folder' argument to point to the right directory.")
                } else {
                        fln <- normalizePath(file.path(folder, dat)) ## generate file path(s)
                        if (all(!file.exists(fln))) stop("File not found!") ## check if files are existing
                        else fln <- fln[which(file.exists(fln))] ## generate (vector of) paths of existing files
                }
        }
        
        ## read data into list object
        read2list(fln, nsheets=nsheets, sheet=sheet, skip=skip, sep=sep, lines=lines, dec=dec, ..., verbose = verbose, x.verbose = x.verbose)
        
}
