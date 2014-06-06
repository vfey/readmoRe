rm.empty.cols <-
function(x, na.only = FALSE)
{
  
  allna <- apply(x, 2, function(z) all(is.na(z)))
  if (!na.only) X <- sapply(colnames(x), function(z) length(grep("^X", z))) > 0
  else X <- !logical(ncol(x))
  nac <- which(apply(data.frame(allna, X), 1, all))
  if (length(nac) > 0) { x <- x[, -nac] }
  return(x)

}
