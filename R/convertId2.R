convertId2 <-
function (id, species = "Human")
{
    if (species == "Human") {
      ensg2eg.env <- org.Hs.egENSEMBL2EG
      sym.env <- org.Hs.egSYMBOL
      sym2eg.env <- org.Hs.egSYMBOL2EG
      ensg.env <- org.Hs.egENSEMBL
      ensg <- "ENSG"
    }
    if (species == "Mouse") {
      ensg2eg.env <- org.Mm.egENSEMBL2EG
      sym.env <- org.Mm.egSYMBOL
      sym2eg.env <- org.Mm.egSYMBOL2EG
      ensg.env <- org.Mm.egENSEMBL
      ensg <- "ENSMU"
    }
    if (length(id) == 1) {
        if (length(grep(ensg, id)) > 0) {
            if (AnnotationDbi::exists(id, envir = ensg2eg.env)) {
                entrez <- get(id, envir = ensg2eg.env)
                if (length(entrez) > 1) {
                  return(NA)
                }
                else {
                  if (AnnotationDbi::exists(entrez, envir = sym.env)) {
                    return(paste(get(entrez, envir = sym.env),
                      collapse = " /// "))
                  }
                }
            }
            else {
                return(NA)
            }
        }
        else {
            if (AnnotationDbi::exists(id, envir = sym2eg.env)) {
                entrez <- get(id, envir = sym2eg.env)
                if (length(entrez) > 1) {
                  return(NA)
                }
                else {
                  if (AnnotationDbi::exists(entrez, envir = ensg.env)) {
                    return(paste(get(entrez, envir = ensg.env),
                      collapse = " /// "))
                  }
                }
            }
            else {
                return(NA)
            }
        }
    }
    else {
        if (length(grep(ensg, id[1])) > 0) {
            entrez <- mget(id, envir = ensg2eg.env, ifnotfound = NA)
            entrez <- sapply(entrez, function(x) {
                if (length(x) > 1 || is.na(x)) {
                  "---"
                }
                else {
                  x
                }
            })
            hugo <- mget(entrez, envir = sym.env, ifnotfound = NA)
            hugo <- sapply(hugo, function(x) {
                if (length(x) > 1) {
                  paste(x, collapse = " /// ")
                }
                else {
                  x
                }
            })
            names(hugo) <- id
            return(hugo)
        }
        else {
            entrez <- mget(id, envir = sym2eg.env, ifnotfound = NA)
            entrez <- sapply(entrez, function(x) {
                if (length(x) > 1 || is.na(x)) {
                  "---"
                }
                else {
                  x
                }
            })
            ensg <- mget(entrez, envir = ensg.env, ifnotfound = NA)
            ensg <- sapply(ensg, function(x) {
                if (length(x) > 1) {
                  paste(x, collapse = " /// ")
                }
                else {
                  x
                }
            })
            names(ensg) <- id
            return(ensg)
        }
    }
}

