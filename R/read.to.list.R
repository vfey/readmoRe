read.to.list <-
        function (dat, type, folder, sheets = 1, skip = 0, ..., verbose = TRUE)
{
        
        ## initial checks
        if (!is.character(dat)) stop("'dat' must be a character vector!")
        
        ## define valid file extensions
        ext <- c(".txt", ".tsv", ".csv", ".xls", ".xlsx", ".xdr", ".RData") ## index of valid file extensions
        
        ## look for certain file types; if set, 'dat' will be ignored and set to "all"
        if (!missing(type)) {
                dat <- "all"
                if (type %in% ext) ext <- type
                else stop("File type not supported!")
        }
        
        ## read all files of the supported types in the given directory
        #### WARNING: This option should be used only if the user is certain regarding the contents of the files in that directory!
        if (dat == "all") {
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
                        if (any(file.exists(dat))) fln <- dat[which(file.exits(dat))] ## generate (vector of) paths of existing files
                        else stop("File not found! Provide complete file path in 'dat' or use the 'folder' argument to point to the right directory.")
                } else {
                        fln <- normalizePath(file.path(folder, dat)) ## generate file path(s)
                        if (all(!file.exists(fln))) stop("File not found!") ## check if files are existing
                        else fln <- fln[which(file.exists(fln))] ## generate (vector of) paths of existing files
                }
        }
        
        ## read data into list object
        read2list(fln, sheets=sheets, skip=skip, ..., verbose = verbose)
        
}
