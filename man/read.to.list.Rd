\name{read.to.list}
\alias{read.to.list}
\title{
  Read various input file formats into a list of data frames. Wrapper function for 'read2list' to automate
  reading further and avoid errors due to missing folders or files.
}
\description{
  \command{read.to.list} is meant to act as a universal reading function as it attempts to read
  a number of different file formats into a list of data frames.
}
\usage{
  read.to.list(dat, type, folder, sheets = 1, skip = 0, ..., verbose = TRUE)
}
\arguments{
  \item{dat}{
  character, the file name
}
  \item{type}{
  character, file extension to be read: one of ".txt", ".tsv", ".csv", ".vcf", ".xls", ".xlsx", ".xdr", ".RData"
}
  \item{folder}{
  character, folder where the file is found
}
  \item{sheets}{
  integer, number of sheets to be read if file is of type ".xls" or ".xlsx"
}
  \item{skip}{
  integer, number of lines to skip from the top of the file
}
  \item{...}{
  additional arguments passed to 'read2list()'
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
