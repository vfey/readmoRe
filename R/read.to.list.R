#' @title Utilities for data import
#' @name readmoRe
#' @description A collection of utilities for reading and importing data into R by performing (usually small) manipulations of
#'     data structures such as data frames, matrices and list and automatically determining import parameters.
#' @author Vidal Fey <vidal.fey@gmail.com>
#' @details \tabular{ll}{
#' Package: \tab readmoRe\cr
#' Type: \tab Package\cr
#' Initial version: \tab 0.1-0\cr
#' Created: \tab 2011-01-07\cr
#' License: \tab GPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#' The main function of the package is \code{read.to.list} which reads a number of different file formats into a list of data objects
#'     such as data frames, depending on the source file.
#' @keywords package
#' "_PACKAGE"
#' @import R.utils
#' @import xml2
#' @import readxl
#' @import plyr
#' @importFrom methods is
#' @importFrom utils packageVersion read.delim
NULL
#'
#' Read various input file formats into a list of data frames. Wrapper function for 'read2list' to automate
#'     reading further and avoid errors due to missing folders or files.
#' @description \command{read.to.list} is meant to act as a universal reading function as it attempts to read
#'     a number of different file formats into a list of data frames.
#' @param dat \code{character}. File path.
#' @param type \code{character}. File extension to be read: one of ".txt", ".tsv", ".csv", ".vcf", ".gtf",
#'     ".gff", ".xls", ".xlsx", ".xdr", ".RData", ".rds", ".rda", ".xml". See details.
#' @param folder \code{character}. Folder where the file is found.
#' @param nsheets \code{integer}. Number of sheets to be read if file is of type ".xls" or ".xlsx". All sheets starting from 1 up to the
#'     given number in the respective data file will be read. If more than one file is read this must be be an integer vector with the
#'     numbers of sheets in exactly the same order as the files. 
#' @param sheet \code{integer} or \code{list}. Sheet(s) to be read if file is of type ".xls" or ".xlsx". The sheets defined by the given integer in the
#'     respective data file will be read. If more than one file is read this must be be a list with the sheet number(s) in exactly
#'     the same order as the files. If there are many files and only one sheet vector the same sheet(s) wiöl be read from
#'     all files.
#' @param keep.tibble \code{logical}. Should the data from Excel files read with \code{readxl::read_excel} be coerce to 
#'     \code{data.frames} or kept in the original \code{tibble} format? Defaults to FALSE, i.e., a \code{data.frame}
#'     is returned.
#' @param skip \code{integer}. Number of lines to skip from the top of the file.
#' @param sep \code{character}. Field delimiter passed to 'read.delim' when reading text files.
#' @param lines \code{lines}. Should the file be read line by line into a character vector by readLines()?
#' @param dec \code{character}. The decimal separator for numbers.
#' @param ... Additional arguments passed to functions.
#' @param verbose \code{logical}. Should verbose output be printed?
#' @param x.verbose \code{logical}. Should extended verbose output be printed?
#' @details Excel files (file extension .xls or .xlsx) will be read by \code{readxl::read_excel}. A test is attempted
#'     to determine whether the input file is genuinely derived from Excel or only named like an nExcel file. If the latter,
#'     it will be attempted to read it as text file.
#'     Text files are read as tables or by line if \code{lines} is \code{TRUE}.
#'     For text files, field delimiters and decimal separators are determined automatically if not provided.
#'     Files with the extensions .txt", ".tsv", ".csv", ".gtf" and ".gff" are treated and read as text files.
#'     VCF files are also treated as text files but can noly be read in full (incl. header) if read by line. Otherwise,
#'     if \code{skip} is \code{0}, the line with the column names will be determined automatically and the file read
#'     as delimited text file.
#'     XML files are read by \code{xml2::read_xml}.
#'     ".RData" files are loaded and assigned a name.
#'     ".rds" and ".rda" files are read by \code{readRDS}.
#'     ".xdr" files are read by \code{R.utils::loadObject}.
#' @return A list of tibbles/data frames.
#' @seealso \code{\link[base]{readLines}}
#' @seealso \code{\link[utils]{read.delim}}
#' @seealso \code{\link[readxl]{read_excel}}
#' @seealso \code{\link[base]{load}}
#' @seealso \code{\link[R.utils]{loadObject}}
#' @seealso \code{\link[base]{readRDS}}
#' @seealso \code{\link[readmoRe]{read2list}}
#' @keywords utilities
#' @examples 
#' # The function readxl::read_excel is used internally to read Excel files.
#' # The example uses their example data.
#' readxl_datasets <- readxl::readxl_example("datasets.xlsx")
#' # A randomly generated data frame was saved to a tab-separated text file
#' # and two different R object files.
#' tsv_datasets <- dir(system.file("extdata", package = "readmoRe"), full.names = TRUE)
#' # All example data are read into a list. From the Excel file, the first
#' # sheet is read.
#' dat <- read.to.list(c(readxl_datasets, tsv_datasets))
#' # All example data are read into a list. From the Excel file, the first
#' # 3 sheets are read.
#' dat <- read.to.list(c(readxl_datasets, tsv_datasets), nsheets=3)
#' # All example data are read into a list. From the Excel file, sheets 1 and
#' # 3 are read.
#' dat <- read.to.list(c(readxl_datasets, tsv_datasets), sheet=c(1, 3))
#' # From two Excel files, different sheets are read: 1 and 3 from the first
#' # file and 2 and 3 from the second.
#' # (For simplicity, the same example file is used.)
#' dat <- read.to.list(c(readxl_datasets, readxl_datasets), sheet=list(c(1, 3), c(2, 3)))
#' @export
read.to.list <-
  function (dat, type, folder, nsheets = 1, sheet = NULL, keep.tibble = FALSE, skip = 0, sep = NULL,
            lines = FALSE, dec = NULL, ..., verbose = TRUE, x.verbose = FALSE)
  {
    
    ## define valid file extensions
    ext <- c(".txt", ".tsv", ".csv", ".vcf", ".gtf", ".gff", ".xls", ".xlsx", ".xdr", ".rdata", ".rds", ".rda", ".xml") ## index of valid file extensions
    
    ## look for certain file types; if set, 'dat' will be ignored and set to "all"
    if (!missing(type)) {
      dat <- "all"
      if (type %in% ext) ext <- type
      else stop("File type not supported!")
    }
    
    ## initial checks
    if (!is.character(dat)) stop("'dat' must be of type character!")
    
    if (verbose) cat("@ MULTI DATA LOADER\n")
    
    ## read all files of the supported types in the given directory
    #### WARNING: This option should be used only if the user is certain regarding the contents of the files in that directory!
    if (length(dat) == 1 && dat == "all") {
      if (missing(folder)) stop("Folder name missing!")
      if (verbose) cat("  Requested reading of all files of type", sQuote(type), "in directory", sQuote(folder), "\n")
      fls <- dir(path = folder, full.names=TRUE, recursive=TRUE)
      ext.all <- sub(".+(\\.[a-z]{3,5}$)", "\\1", tolower(basename(fls))) ## extract file extensions
      valid <- ext.all %in% ext
      if (!any(valid)) stop("No valid files found in directory.")
      fln <- fls[valid] ## generate vector of paths of valid files
      if (verbose) cat("  ~~> Reading", length(fln), "files...\n")
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
    read2list(fln, nsheets=nsheets, sheet=sheet, keep.tibble=keep.tibble, skip=skip, sep=sep, lines=lines,
              dec=dec, ..., verbose = verbose, x.verbose = x.verbose)
    
  }
