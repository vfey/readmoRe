#' Read various input file formats into a list of data frames
#' @description
#' \command{read2list} is meant to act as a universal reading function as it attempts to read
#' a number of different file formats into a list of data frames.
#' @param dat \code{character}. File path.
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
#' @seealso \code{\link[utils]{read.delim}}
#' @seealso \code{\link[readxl]{read_excel}}
#' @seealso \code{\link[readmoRe]{read.to.list}}
#' @keywords utilities
#' @export
read2list <-
  function (dat, nsheets = 1, sheet = NULL, keep.tibble = FALSE, skip = 0, sep = NULL,
            lines = FALSE, dec = NULL, ..., verbose = TRUE, x.verbose = FALSE)
  {
    
    if (verbose) message("@ VERSATILE FILE READER v.", as.character(packageVersion("readmoRe")))
    if (!is.character(dat)) stop("'dat' must be a character vector")
    ext.all <- sub(".+(\\.[a-z]{3,5}$)", "\\1", tolower(dat))
    if (x.verbose) message(  "Detected file extension(s)", sQuote(ext.all))
    val.ext <- c(".txt", ".tsv", ".csv", ".vcf", ".gtf", ".gff", ".xls", ".xlsx", ".xdr", ".rdata", ".rds", ".rda", ".xml")
    valid <- ext.all %in% val.ext
    if (any(!valid)) {
      if (all(!valid)) {
        stop("File type(s) not valid.")
      } else {
        message("NOTE:", sQuote(basename(dat[!valid])), "do(es) not exist or are (is) not of (a) valid file type(s)!")
        dat <- dat[valid]
      }
    }
    nx <- grep(".xls", ext.all)
    if (!is.null(sheet)) {
      nsheets <- NULL
      if (!is.list(sheet)) {
        sheets <- list(sheet)
      } else {
        sheets <- sheet
      }
      if (length(nx) > 0) {
        if (length(nx) > 1 && length(sheets) == 1) {
          sheets <- rep(sheets, length(nx))
        } else if (length(nx) != length(sheets)) {
          stop("Length of 'sheet' and number of Excel files must match if length of 'sheet' is greater than one.")
        }
      }
    } else {
      nsheets <- as.list(nsheets)
      if (length(nx) > 0) {
        if (length(nx) > 1 && length(nsheets) == 1) {
          nsheets <- rep(nsheets, length(nx))
        } else if (length(nx) != length(nsheets)) {
          stop("Length of 'nsheets' and number of Excel files must match if length of 'nsheets' is greater than one.")
        }
      }
    }
    dT <- dl <- dx <- ds <- dR <- dxml <- NULL
    dfl <- plyr::llply(1 : length(dat), function(fl) {
      x <- dat[fl]
      ext <- sub(".+(\\.[a-z]{3,5}$)", "\\1", tolower(x))
      if (ext %in% c(".txt", ".tsv", ".csv", ".gtf", ".gff")) {
        if (verbose) message("Reading text file ", basename(x), "...", appendLF = FALSE)
        if (is.null(sep)) {
          ve <- c(",", ";", "\t")
          l <- readLines(x, n = 1+skip)
          l <- l[length(l)]
          se <- which(plyr::llply(ve, function(y) grep(y, l)) == 1)
          sep <- ve[se]
          if (length(sep) > 1) {
            if ("\t" %in% sep) {
              sep <- "\t"
            } else {
              stop("Found more than 1 possible field delimiter. Please set 'sep' manually.")
            }
          }
        }
        if (is.null(dec)) {
          dsep <- c(",", ".")
          dsep.regex <- c(paste("^[[:digit:]]{1,}", dsep[1], "[[:digit:]]{1,}$", sep=""), paste("^[[:digit:]]{1,}\\", dsep[2], "[[:digit:]]{1,}$", sep=""))
          l <- readLines(x, n = 2+skip)
          l <- l[length(l)]
          lspl <- strsplit(l, sep)[[1]]
          ds <- unique(unlist(plyr::llply(lspl, function(z) which(plyr::llply(dsep.regex, function(y) grep(y, z)) == 1))))
          decsep <- dsep[ds]
        }
        if (lines) {
          dT <- readLines(x, ...)
        } else {
          dT <- read.delim(x, dec = decsep,
                           sep = sep, comment.char = "", skip=skip, ...)
          dT <- rm.empty.cols(dT)
        }
        dT <- list(dT)
        if (verbose) message("done")
        if (x.verbose && is.null(dec)) message(paste0(" (Decimal separator detected: ", sQuote(decsep), ")"))
      }
      if (ext == ".vcf") {
        if (verbose) message("Reading variant call file ", basename(x), "...", appendLF = FALSE)
        sep <- get.sep(x, pattern="#CHROM")
        if (is.null(dec)) {
          decsep <- ifelse(sep == ";", ",", ".")
        }
        if (skip == 0) skip <- get.skip(x, pattern="#CHROM")
        if (lines) {
          dT <- readLines(x, ...)
        } else {
          dT <- read.delim(x, header = TRUE, dec = decsep,
                           sep = sep, comment.char = "", skip=skip, ...)
          dT <- rm.empty.cols(dT)
        }
        dT <- list(dT)
        if (verbose) message("done")
        if (x.verbose && is.null(dec)) message(paste0(" (Decimal separator detected: ", sQuote(decsep), ")"))
      }
      if (length(grep(".xls", ext)) > 0) {
        isXls <- TRUE
        if (x.verbose) message("  Testing for genuine Excel format...", appendLF = FALSE)
        test.xls <- readxl::format_from_signature(x)
        if (is.na(test.xls)) {
          if (x.verbose) message("Failed!")
          isXls <- FALSE
        } else {
          if (x.verbose) message("Success!")
        }
        if (!isXls) {
          if (verbose) message("Attempting to read ", basename(x), " as text file...")
          if (is.null(sep)) {
            ve <- c(",", ";", "\t")
            l <- readLines(x, n = 1+skip)
            l <- l[length(l)]
            se <- which(plyr::llply(ve, function(y) grep(y, l)) == 1)
            sep <- ve[se]
            if (length(sep) > 1) {
              if ("\t" %in% sep) {
                sep <- "\t"
              } else {
                stop("Found more than 1 possible field delimiter. Please set 'sep' manually.")
              }
            }
          }
          if (is.null(dec)) {
            dsep <- c(",", ".")
            dsep.regex <- c(paste("^[[:digit:]]{1,}", dsep[1], "[[:digit:]]{1,}$", sep=""), paste("^[[:digit:]]{1,}\\", dsep[2], "[[:digit:]]{1,}$", sep=""))
            l <- readLines(x, n = 2+skip)
            l <- l[length(l)]
            lspl <- strsplit(l, sep)[[1]]
            ds <- unique(unlist(plyr::llply(lspl, function(z) which(plyr::llply(dsep.regex, function(y) grep(y, z)) == 1))))
            decsep <- dsep[ds]
          }
          read.test <- try(read.delim(x, dec = decsep,
                                      sep = sep, comment.char = "", skip=skip, nrows=10, ...))
          if (is(read.test, "try-error")) {
            if (verbose) message(paste0("Unable to read file ", x, ". Skipping..."))
            dT <- NULL
          } else {
            if (verbose) message(paste0("Reading text file (named as Excel file) ", basename(x), "..."), appendLF = FALSE)
            if (lines) {
              dT <- readLines(x, ...)
            } else {
              dT <- read.delim(x, dec = decsep,
                               sep = sep, comment.char = "", skip=skip, ...)
              dT <- rm.empty.cols(dT)
            }
            dT <- list(dT)
            if (verbose) message("done")
            if (x.verbose && is.null(dec)) message(paste0(" (Decimal separator detected: ", sQuote(decsep), ")"))
          }
        } else {
          xl <- match(fl, nx)
          if (verbose) message("Reading Excel file ", basename(x), "...")
          if (skip != 0) {
            skip <- unlist(skip)[1]
            warning(paste("The same 'skip' value will be used for all sheets:", skip, ""), call. = FALSE)
          }
          if (!is.null(sheet)) {
            if (verbose) message(" Reading sheet(s) number ", paste(sheets[[xl]], collapse = ","), "...")
            dl <- plyr::llply(sheets[[xl]], function(z) {
              xls <- rm.empty.cols(suppressMessages(readxl::read_excel(x, sheet = z, skip=skip)))
              if (verbose) message("  Sheet ", z, " read")
              if (is.null(xls[[1]])) {
                warning("Error while reading", x, "Returning", xls)
              } else {
                if (keep.tibble) {
                  xls
                } else {
                  as.data.frame(xls, stringsAsFactors = FALSE)
                }
              }
            })
            names(dl) <- paste("sheet", sheets[[xl]])
          } else {
            nsheets <- 1:nsheets[[xl]]
            if (verbose) message(" Reading ", length(nsheets), " sheet(s)...")
            dl <- plyr::llply(nsheets, function(z) {
              if (verbose) message("  Sheet ", z, "...")
              xls <- rm.empty.cols(suppressMessages(readxl::read_excel(x, sheet = z, skip=skip)))
              if (verbose) message("done")
              if (is.null(xls[[1]])) {
                warning("Error while reading", x, "Returning", xls)
              } else {
                if (keep.tibble) {
                  xls
                } else {
                  as.data.frame(xls, stringsAsFactors = FALSE)
                }
              }
            })
            names(dl) <- paste("sheet", nsheets)
          }
        }
      }
      if (ext == ".xdr") {
        if (verbose) message("Reading object image file ", basename(x), "...", appendLF = FALSE)
        dx <- loadObject(x)
        dx.l <- list()
        dx.l[[1]] <- dx
        dx <- dx.l
        if (verbose) message("done")
      }
      if (ext %in% c(".rds", ".rda")) {
        if (verbose) message("Reading object image file ", basename(x), "...", appendLF = FALSE)
        ds <- readRDS(x)
        ds.l <- list()
        ds.l[[1]] <- ds
        ds <- ds.l
        if (verbose) message("done")
      }
      if (ext == ".rdata") {
        if (verbose) message("Reading object image file ", basename(x), "...", appendLF = FALSE)
        l <- ls()
        load(x)
        nl <- ls()
        if (length(nl) == length(l) + 2) {
          dR <- get(nl[- c(which(nl == "l"), match(l, nl))])
          dR.l <- list()
          dR.l[[1]] <- dR
          dR <- dR.l
        } else {
          stop("Image file must contain only one (1) object!")
        }
        if ("dl" %in% ls()) rm("dl")
        if ("dx" %in% ls()) rm("dx")
        if ("dT" %in% ls()) rm("dT")
        if (verbose) message("done")
      }
      if (ext == ".xml") {
        if (verbose) message("Reading XML file ", basename(x), "...", appendLF = FALSE)
        dxml <- xml2::read_xml(x)
        dxml <- list(dxml)
        if (verbose) message("done")
      }
      c(dT, dl, dx, dR, dxml)
    })
    names(dfl) <- basename(dat)
    if (any(is.null(dfl))) {
      dfl <- dfl[-which(is.null(dfl))]
    }
    do.call("c", dfl)
    
  }
