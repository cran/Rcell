\name{write.cell.image}
\alias{write.cell.image}

\title{Write a Cell Image}
\description{
  Writes a cell image object to disk. 
}
\usage{
  write.cell.image(x, file = "",...)
}

\arguments{
  \item{x}{the cell.image object, as returned by \code{get.cell.image}, to be saved}
  \item{file}{filename or filename with path to save the image}
  \item{\dots}{further arguments passed to \code{writeImage}}  
}

\details{
  This function is a wrapper over \code{writeImage}. It combines images of the cell.image object in a stack, saves it to disk, and saves de image description database as a tab delimimited file, with the name given in \code{file} argument.  
}

\author{Alan Bush}
\seealso{ \code{writeImage}}
\examples{
\dontrun{
  #load example dataset
  library(RcellData)
  data(ACL394filtered)
  
  #select N=3 cells images from each pos (group), 
  #from the first t.frame and pos 1,8,15,22,29.
  ci<-get.cell.image(X,subset=match(pos,c(1,8,15,22,29),nomatch=0)>0&t.frame==11,
    group=.(pos),N=3,channel=c('BF.out','YFP'))
    
  write.cell.image(ci,"Example-cell-image.tif")  

}
}
\keyword{data}
