\name{zoom}
\alias{zoom}
\alias{xzoom}
\alias{yzoom}
\title{ Zoom in a ggplot Object }
\description{
  Sets the plotting region and axis breaks for a ggplot object
}
\usage{

zoom(xzoom=c(NA,NA),yzoom=c(NA,NA),nx.breaks=n.breaks,ny.breaks=n.breaks
  ,n.breaks=7)
xzoom(xzoom=c(NA,NA),nx.breaks=7)
yzoom(yzoom=c(NA,NA),ny.breaks=7)

}
\arguments{
  \item{xzoom}{numeric vector of length 2 specifying the range of the x axis }
  \item{yzoom}{numeric vector of length 2 specifying the range of the y axis }  
  \item{nx.breaks}{number of breaks for the x axis}
  \item{ny.breaks}{number of breaks for the y axis}
  \item{n.breaks}{number of breaks for both axis, if not specified by nx.breaks or ny.breaks}
}

\details{

xzoom and yzoom are convenient functions to specify only one of the limits

}
\value{
  a layer to be added to a ggplot object, that specifies the plotting region after the statistical transformations have been done. 
}
\note{A zoom function exists in Hmisc package. Use Rcell::zoom if both package namespaces are loaded.}
\author{ Alan Bush}
\seealso{\code{\link{cplot}},\code{\link{limits}}}
\examples{
#load example dataset
data(ACL394)

#zoom in the y axis
cplotmeans(X,f.tot.y~t.frame,color=pos) + zoom(y=c(0,7e6))
}
\keyword{aplot}