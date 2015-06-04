\name{flatten}
\alias{flatten}
\alias{flatten.data.frame}

\title{Flatten a Data Frame}
\description{
  converts matrix elements of data frames into columns 
}
\usage{

  flatten(df,...)

  \method{flatten}{data.frame}(df,...)
}

\arguments{
  \item{df}{data.frame to be flattened}
  \item{\dots}{further arguments passed to or used by methods}  
}

\details{
  this function is useful to flatten data frames obtained by aggregate when using smean.cl.normal and other functions from Hmisc.
 }

\value{
   a data frame
}
\author{Alan Bush}
\seealso{ \code{\link{with}}}
\examples{
if(require(Hmisc)&require(RcellData)){
  #load example dataset
  data(ACL394)
  agg<-aggregate(X,f.tot.y~pos,subset=t.frame==0,FUN=smean.cl.normal) 
	str(agg)
	agg<-flatten(agg)
	str(agg)
}
}
\keyword{data}
