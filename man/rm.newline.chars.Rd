\name{rm.newline.chars}
\alias{rm.newline.chars}
\title{
  Remove 'newline' characters from imported Excel sheets
}
\description{
  \command{rm.newline.chars} removes \sQuote{newline} characters (\code{\\n}) from any column of a data frame
}
\usage{
  rm.newline.chars(x, verbose = TRUE)
}
\arguments{
  \item{x}{
  a data frame resulting from an imported Excel sheet by means of \command{read.xls}
}
  \item{verbose}{
  logical, should verbose output be printed, defaults to \code{TRUE}
}
}
\details{
  \sQuote{Newline} characters in data frame rows are read verbatim and will cause rows in output
  text files to be distributed across two ore more lines. Such characters, entered accidentally or
  deliberately in the source Excel file, should be avoided. This function removes all \sQuote{newline}
  characters found at the end of a line or replaces them when found within the line text. 
}
\value{
  A data frame.
}
\author{
  Vidal Fey \email{vidal.fey@geneviatechnologies.com}
}
\keyword{ utilities }
