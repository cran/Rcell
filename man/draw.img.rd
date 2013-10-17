\name{draw.img}
\alias{drawCross}
\alias{drawLine}
\alias{drawText}
 
\title{Draw on a Image}

\description{funcionts for modifying EBImage images}
\usage{

drawCross(img, x, y, size=2, col=0.75, z=1)

drawLine(img, x1, y1, x2, y2, col=0.75, z = 1)

drawText(img,labels,x=NULL,y=NULL,adj=c(0,0),reuseLabels=TRUE,col=NULL)

}

\arguments{
  \item{img}{EBImage Image to modify} 
  \item{x}{vector of x positions to draw}
  \item{y}{vector of y positions to draw}
  \item{x1}{vector of x1 positions to draw}
  \item{y1}{vector of y1 positions to draw}
  \item{x2}{vector of x2 positions to draw}
  \item{y2}{vector of y2 positions to draw}
  \item{labels}{character vector of labels}
  \item{adj}{one or two values in [0, 1] which specify the x (and optionally y) adjustment of the labels. On most devices values outside that interval will also work.}
  \item{col}{color of the object}
  \item{size}{size of the cross}
  \item{reuseLabels}{boolean indicating if labels created in previous calls should be reused}
  \item{z}{image layer in which to draw}
}
\details{

  \code{drawCross}, \code{drawLine} and \code{drawLabel} draw on EBImage images, at the specified x y positions.  
      
 }
\value{
  a EBImage image
}
\author{ Alan Bush }
\seealso{\code{\link{cimage}}}
\examples{

if(require(EBImage,quietly=TRUE)&interactive()){ 

  data(ACL394filtered)
  img<-show.img(X,pos=1,channel="BF",cross=FALSE)
  p1<-X[[pos==1&t.frame==0,c("?pos","cellID")]]
  display(drawCross(img,p1$xpos,p1$ypos,col=0))
  display(drawText(img,p1$cellID,p1$xpos,p1$ypos,col=0))
  display(drawLine(img,p1$xpos[1],p1$ypos[1],p1$xpos[2],p1$ypos[2]))

}
}
\keyword{manip}
\keyword{methods}

