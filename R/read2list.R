read2list <-
		function (dat, nsheets = 1, sheet = NULL, skip = 0, sep = NULL, lines = FALSE, ..., verbose = TRUE)
{
	
	if (verbose) cat("@ VERSATILE FILE READER v.", as.character(packageVersion("genRal")), "\n")
	if (!is.character(dat)) stop("'dat' must be a character vector")
	ext.all <- sub(".+(\\.[a-z]{3,4}$)", "\\1", dat)
	val.ext <- c(".txt", ".tsv", ".csv", ".vcf", ".gtf", ".gff", ".xls", ".xlsx", ".xdr", ".RData")
	valid <- ext.all %in% val.ext
	if (any(!valid)) {
		if (all(!valid)) { stop("File type(s) not valid.") }
		else { dat <- dat[valid] }
	}
	nx <- grep(".xls", ext.all)
	if (!is.null(sheet)) {
		nsheets <- NULL
		sheets <- as.list(sheet)
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
	dT <- dl <- dx <- dR <- NULL
	dfl <- lapply(1 : length(dat), function(fl) {
				x <- dat[fl]
				ext <- sub(".+(\\.[a-z]{3,4}$)", "\\1", x)
				if (ext %in% c(".txt", ".tsv", ".csv", ".gtf", ".gff")) {
					if (verbose) cat(paste("Reading text file ", basename(x), "...", sep = ""))
					if (is.null(sep)) {
						ve <- c(",", ";", "\t")
						l <- readLines(x, n = 1+skip)
						l <- l[length(l)]
						se <- which(lapply(ve, function(y) grep(y, l)) == 1)
						sep <- ve[se]
						if (length(sep) > 1) {
							if ("\t" %in% sep) {
								sep <- "\t"
							} else {
								stop("Found more than 1 possible field delimiter. Please set 'sep' manually.")
							}
						}
					}
					dsep <- c(",", ".")
					dsep.regex <- c(paste("^[[:digit:]]{1,}", dsep[1], "[[:digit:]]{1,}$", sep=""), paste("^[[:digit:]]{1,}\\", dsep[2], "[[:digit:]]{1,}$", sep=""))
					l <- readLines(x, n = 2+skip)
					l <- l[length(l)]
					lspl <- strsplit(l, sep)[[1]]
					ds <- unique(unlist(lapply(lspl, function(z) which(lapply(dsep.regex, function(y) grep(y, z)) == 1))))
					dec <- dsep[ds]
					if (lines) {
						dT <- readLines(x)
					} else {
						dT <- read.delim(x, dec = dec,
								sep = sep, comment.char = "", skip=skip, ...)
						dT <- rm.empty.cols(dT)
					}
					dT <- list(dT)
					if (verbose) cat("done\n")
				}
				if (ext == ".vcf") {
					if (verbose) cat(paste("Reading variant call file ", basename(x), "...", sep=""))
					sep <- get.sep(x, pattern="#CHROM")
					dec <- ifelse(sep == ";", ",", ".")
					if (skip == 0) skip <- get.skip(x, pattern="#CHROM")
					dT <- read.delim(x, header = TRUE, dec = dec,
							sep = sep, comment.char = "", skip=skip, ...)
					dT <- rm.empty.cols(dT)
					dT <- list(dT)
					if (verbose) cat("done\n")
				}
				if (length(grep(".xls", ext)) > 0) {
					xl <- match(fl, nx)
					if (verbose) cat(paste("Reading Excel file ", basename(x), "...\n", sep = ""))
					if (skip != 0) {
						skip <- unlist(skip)[1]
						warning(paste("The same 'skip' value will be used for all sheets:", skip), call. = FALSE)
					}
					if (!is.null(sheet)) {
						if (verbose) cat(paste(" Reading sheet number ", sheets[[xl]], "...\n", sep = ""))
						dl <- lapply(sheets[[xl]], function(z) {
									if (verbose) cat(paste("  Sheet ", z, "...", sep = ""))
									xls <- rm.empty.cols(gdata::read.xls(x, sheet = z, skip=skip, ...))
									if (verbose) cat("done\n")
									if (is.null(xls[[1]])) warning("Error while reading", x,
												"Returning", xls)
									xls
								})
						names(dl) <- paste("sheet", sheets[[xl]])
					} else {
						nsheets <- 1:nsheets[[xl]]
						if (verbose) cat(paste(" Reading ", length(nsheets), " sheet(s)...\n", sep = ""))
						dl <- lapply(nsheets, function(z) {
									if (verbose) cat(paste("  Sheet ", z, "...", sep = ""))
									xls <- rm.empty.cols(gdata::read.xls(x, sheet = z, skip=skip, ...))
									if (verbose) cat("done\n")
									if (is.null(xls[[1]])) warning("Error while reading", x,
												"Returning", xls)
									xls
								})
						names(dl) <- paste("sheet", nsheets)
					}
				}
				if (ext == ".xdr") {
					if (verbose) cat(paste("Reading object image file ", basename(x), "...", sep = ""))
					dx <- loadObject(x)
					if (!is.list(dx)) {
						dx <- list(dx)
					}
					if (verbose) cat("done\n")
				}
				if (ext == ".RData") {
					if (verbose) cat(paste("Reading object image file ", basename(x), "...", sep = ""))
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
