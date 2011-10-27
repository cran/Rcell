\name{update.n.tot}
\alias{update.n.tot}
\title{ Calculate Total Number of Frames for Each Cell}
\description{
updates n.tot, the total amounts of frames in which a given cell appears
}
\usage{
update.n.tot(object, QC.filter = TRUE,...)
}

\arguments{
  \item{object}{cell.data object }
  \item{QC.filter}{a boolean value indicating if the quality control filter should be applied}
  \item{\dots}{futher arguments for methods}	
}
\value{
  returns a cell.data object, with updated values for n.tot
}
\author{Alan Bush}
\seealso{ \code{\link{load.cellID.data}},\code{\link{select.cells}}}
\examples{
#load example dataset
data(ACL394)

#update n.tot variable
X<-update.n.tot(X)

#this command is equivalent to
X<-transform.by(X,.(ucid), n.tot=length(t.frame))

}
\keyword{manip}