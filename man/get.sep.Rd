\name{get.sep}
\alias{get.sep}
\title{
  Determine field delimiter in text files
}
\description{
  \command{get.sep} attempts to determine if the filed delimiter in an input file is one of comma, semicolon or tab.
}
\usage{
  get.sep(file, n=1, pattern)
}
\arguments{
  \item{file}{
  character, the file name
}
  \item{n}{
  integer, the number of lines to be read by 'readLines()'
}
  \item{pattern}{
  character, a search pattern like, e.g., a column  name
}
}
\value{
  If successful, the filed delimiter. If more than on of the possible delimiters is found, an error is returned.
}
\author{
  Vidal Fey \email{vidal.fey@utu.fi}
\seealso{
  \command{\link[base]{readLines}}
}
\keyword{ utilities }
