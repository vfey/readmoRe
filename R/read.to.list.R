read.to.list <-
        function (dat, type, folder, nsheets = 1, sheet = NULL, skip = 0, sep = NULL, lines = FALSE, ..., verbose = TRUE)
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
                ext.all <- sub(".+(\\.[a-z]{3,4}$)", "\\1", basename(fls)) ## extract file extensions
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
        read2list(fln, nsheets=nsheets, sheet=sheet, skip=skip, lines=lines, ..., verbose = verbose)
        
}
