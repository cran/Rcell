\name{plotMisc}
\alias{summarise_by_group}

\title{Miscellaneous support functions for plotting}
\description{
Utility functions for plotting
}
\usage{
summarise_by_group(data, summaryFun, ...)
}
\arguments{
  \item{data}{a data.frame with a 'group', 'x' and 'y' columns}
  \item{summaryFun}{summary function}
  \item{\dots}{further arguments for summaryFun}	
}
\value{
	an aggregated data.frame
}
\author{Alan Bush}
\examples{
  if(require(Hmisc)){
    df<-data.frame(group=rep(1:5,each=3),a=1:15,b=15:1)
    summarise_by_group(df,smean.cl.normal)
  }
}
\keyword{manip}