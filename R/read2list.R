read2list <-
function (dat, sheets = 1, skip = 0, ..., verbose = TRUE)
{

  sheets <- as.list(sheets)
  if (!is.character(dat)) stop("'dat' must be a character vector")
  ext.all <- sub(".+(\\.[a-z]{3,4}$)", "\\1", dat)
  val.ext <- c(".txt", ".tsv", ".csv", ".xls", ".xlsx", ".xdr", ".RData")
  valid <- ext.all %in% val.ext
  if (any(!valid)) {
      if (all(!valid)) { stop("File type(s) not valid.") }
      else { dat <- dat[valid] }
  }
  nx <- grep(".xls", ext.all)
  if (length(nx) > 0) {
      if (length(nx) > 1 && length(sheets) == 1) {
          sheets <- rep(sheets, length(nx))
      } else if (length(nx) != length(sheets)) {
          stop("Length of 'nsheets' and number of Excel files must match if length of 'sheets' is greater than one.")
      }
  }
  dT <- dl <- dx <- dR <- NULL
  dfl <- lapply(1 : length(dat), function(fl) {
      x <- dat[fl]
      ext <- sub(".+(\\.[a-z]{3,4}$)", "\\1", x)
      if (ext %in% c(".txt", ".tsv", ".csv")) {
          if (verbose) cat(paste("Reading text file ", x, "...", sep = ""))
          ve <- c(",", ";", "\t")
          l <- readLines(x, n = 1+skip)
          l <- l[length(l)]
          se <- which(lapply(ve, function(y) grep(y, l)) == 1)
          sep <- ve[se]
          dec <- ifelse(sep == ";", ",", ".")
          dT <- read.table(x, header = TRUE, dec = dec,
              sep = sep, comment.char = "", skip=skip, ...)
          dT <- rm.empty.cols(dT)
          dT <- list(dT)
          if (verbose) cat("done\n")
      }
      if (length(grep(".xls", ext)) > 0) {
          xl <- match(fl, nx)
          if (verbose) cat(paste("Reading Excel file ", x, "...\n", sep = ""))
          if (skip != 0) {
                  skip <- unlist(skip)[1]
                  warning(paste("The same 'skip' value will be used for all sheets:", skip), call. = FALSE)
          }
          if (verbose) cat(paste(" Reading ", length(sheets[[xl]]), " sheet(s)...\n", sep = ""))
          dl <- lapply(sheets[[xl]], function(z) {
              if (verbose) cat(paste("  Sheet ", z, "...", sep = ""))
              xls <- rm.empty.cols(read.xls(x, sheet = z, skip=skip, ...))
              if (verbose) cat("done\n")
              if (is.null(xls[[1]])) warning("Error while reading", x,
                  "Returning", xls)
              xls
              })
          names(dl) <- paste("sheet", sheets[[xl]])
      }
      if (ext == ".xdr") {
          if (verbose) cat(paste("Reading object image file ", x, "...", sep = ""))
          dx <- loadObject(x)
          if (!is.list(dx)) {
              dx <- list(dx)
          }
          if (verbose) cat("done\n")
      }
      if (ext == ".RData") {
          if (verbose) cat(paste("Reading object image file ", x, "...", sep = ""))
          l <- ls()
          load(x)
          nl <- ls()
          if (length(nl) == length(l) + 2) {
              dR <- get(nl[- c(which(nl == "l"), match(l, nl))])
              if (!is.list(dR)) {
                  dR <- list(dR)
              }
          } else {
              stop("Image file must contain only one (1) object!")
          }
          if (verbose) cat("done\n")
      }
      c(dT, dl, dx, dR)
      })
  names(dfl) <- basename(dat)
  if (any(is.null(dfl))) {
      dfl <- dfl[-which(is.null(dfl))]
  }
  do.call("c", dfl)

}
