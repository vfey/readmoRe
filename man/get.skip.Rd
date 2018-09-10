\name{get.skip}
\alias{get.skip}
\title{
  Determine number of rows to be skipped in text files
}
\description{
  \command{get.skip} attempts to determine the number of rows that could be skipped when reading text files.
}
\usage{
  get.skip(file, n=1, pattern=NULL)
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
  The 'skip' value. If no value is determined 0 (zero) is returned.
}
\author{
  Vidal Fey \email{vidal.fey@geneviatechnologies.com}
}
\seealso{
  \command{\link[base]{readLines}}
}
\keyword{ utilities }
