\name{read.cell.image}
\alias{read.cell.image}

\title{Reads a Cell Image}
\description{
  Reads a cell image object from disk. 
}
\usage{
  read.cell.image(file,...)
}

\arguments{
  \item{file}{filename or filename with path to the saved image}
  \item{\dots}{further arguments passed to \code{readImage}}  
}

\details{
  This function is a wrapper over \code{readImage}. It reads a image saved by \code{write.cell.image}, with the image descripcion database. 
}

\author{Alan Bush}
\seealso{ \code{readImage}}
\examples{
\dontrun{
  #load example dataset
  library(RcellData)
  data(ACL394filtered)
  
  ci<-get.cell.image(X,subset=match(pos,c(1,8,15,22,29),nomatch=0)>0&t.frame==11,
    group=.(pos),N=3,channel=c('BF.out','YFP'))
    
  write.cell.image(ci,"Example-cell-image.tif")  
  
  ci2<-read.cell.image("Example-cell-image.tif")  

}
}
\keyword{data}
