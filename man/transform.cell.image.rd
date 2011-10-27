\name{transform.cell.image.rd}
\alias{cnormalize}

\title{Transform Cell Image}

\description{funcionts that transforms a cell image object before plotting}
\usage{

cnormalize(X=NULL,normalize.group=c("channel"),...)

}

\arguments{
  \item{X}{cell.image object to transform}
  \item{normalize.group}{character vector indicating with variables should be used to group the images for normalization}
  \item{\dots}{further arguments for methods}
}
\details{

  These functions are called from \code{\link{cimage}} to transform the images in some way before plotting. 

  \code{cnormalize} applies a normalization to the images that to enhance contrast. The normalization groups (defined by \code{normalize.group}) are applied the same normalization, so the intensities can be compared within a group.
  
  if \code{X} is NULL, the funcion returns a character indicating with variables of the dataset it requires.
      
 }
\value{
  The transformed cell.image object
}
\author{ Alan Bush }
\seealso{\code{\link{cimage}}}
\examples{

#load example dataset
data(ACL394)
  
#correcting the path to the images
#normally you won't need to do this
X$images$path<-factor(system.file('img', package='Rcell'))

#select N=3 cells images from each pos (group), 
#from the first t.frame and pos 1,8,15,22,29.
ci<-get.cell.image(X,subset=match(pos,c(1,8,15,22,29),nomatch=0)>0&t.frame==11,
	group=.(pos),N=3,channel=c('BF','YFP'))

#display a cell image without normalization
if(interactive()) display(tile(combine(ci))) 

ci<-cnormalize(ci) #apply normalization
if(interactive()) display(tile(combine(ci))) #display again

}
\keyword{manip}
\keyword{methods}

