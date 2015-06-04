\name{write.delim}
\alias{write.delim}

\title{Data output}
\description{
  Writes a Tab Delimited Table text table to disk.  
}
\usage{
  write.delim(x, file = "", quote = FALSE, sep = "\t", row.names = FALSE,...)
}

\arguments{
  \item{x}{the object to be written, preferably a matrix or data frame. If not, it is attempted to coerce x to a data frame.}
  \item{file}{either a character string naming a file or a connection open for writing. "" indicates output to the console.}
  \item{quote}{a logical value (TRUE or FALSE) or a numeric vector. If TRUE, any character or factor columns will be surrounded by double quotes. If a numeric vector, its elements are taken as the indices of columns to quote. In both cases, row and column names are quoted if they are written. If FALSE, nothing is quoted.}
  \item{sep}{the field separator string. Values within each row of x are separated by this string.}
  \item{row.names}{either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.}
  \item{\dots}{further arguments passed to \code{write.table}}  
}

\details{
  This function is a wrapper over \code{write.table} with defaults to write a nice tab delimited text file. 
}

\author{Alan Bush}
\seealso{ \code{\link{write.table}}}
\examples{
if(require(RcellData)){
  data(ACL394)
  agg<-aggregateBy(X,.(pos),select="f.tot.y",subset=t.frame==0,FUN=mean)
  if(interactive()) write.delim(agg,"myTable.txt")
}
}
\keyword{data}
