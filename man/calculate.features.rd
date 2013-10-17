\name{calculate.features}
\alias{calculate.features}

\title{Calculate Features}
\description{
Calculate additional features from the cell mask and fluorescence channels 
}
\usage{
calculate.features(X
    ,subset=NULL,channel=NULL,features="all",is.12bits.img=TRUE,...)
}
\arguments{
  \item{X}{cell.data object}
  \item{subset}{condition defining images (pos and t.frame) to use to calculate the features. Use variables in X$images}
  \item{channel}{character vector indicating the channel(s) to be used to calculate fluorescence features. See details.}	
  \item{features}{character vector indicating which features to calculate. options are 'all', 'basic', 'geom', 'haralick'}
  \item{is.12bits.img}{boolean indicating if the 'channel' images are 12 bits images}	
  \item{\dots}{further arguments to \code{\link{computeFeatures}}}	
}
\details{
  \code{calculate.features} calculates additional features based on the cell mask. This function should be used after creating the masks with \code{\link{create.cellID.mask}}. 'shape' and 'moment' features are always calculated. 'geom' features can be calculated, but this can take some time. 
  'haralick' and 'basic' features require one or more channels to be specify by the \code{channel} argument. The first letter of the image name is used as a posfix for the channel specific variables. 
  For more details on the features refer to the documentation of \code{\link{computeFeatures}}
}
\value{
  returns a cell.data object, with new variables.
}
\author{Alan Bush, Rocio Espada}
\seealso{\code{\link{computeFeatures}}, \code{\link{create.mask}}}
\examples{
\dontrun{
  X<-create.cellID.mask(X)
  X<-calculate.features(X)
}
}
\keyword{manip}