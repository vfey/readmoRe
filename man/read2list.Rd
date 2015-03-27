\name{read2list}
\alias{read2list}
\title{
  Read various input file formats into a list of data frames.
}
\description{
  \command{read2list} is meant to act as a universal reading function as it attempts to read
  a number of different file formats into a list of data frames.
}
\usage{
  read2list(dat, sheets = 1, skip = 0, sep = NULL, lines = FALSE, ..., verbose = TRUE)
}
\arguments{
  \item{dat}{
  character, the file name
}
  \item{sheets}{
  integer, number of sheets to be read if file is of type ".xls" or ".xlsx"
}
  \item{skip}{
  integer, number of lines to skip from the top of the file
}
  \item{sep}{
  character, field delimiter passed to 'read.delim' when reading text files
}
  \item{lines}{
  logical, should the file be read line by line into a character vector by readLines()?
}
  \item{...}{
  additional arguments passed to 'read.table()'
}
  \item{verbose}{
  logical, should verbose output be printed?
}
}
\value{
  A list of data frames.
}
\author{
  Vidal Fey \email{vidal.fey@utu.fi}
}
\keyword{ utilities }
