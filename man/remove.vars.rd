\name{remove.vars}
\alias{remove.vars}

\title{Remove Variables from a Cell Data Object}

\description{
  Returns a cell.data object, with the specified variables removed
}
\usage{
remove.vars(X,select,exclude=NULL)
}

\arguments{
  \item{X}{ cell.data object }
  \item{select}{character vector defining variables names to be removed in the returned cell.data}
  \item{exclude}{character vector defining variable names to be kept (not removed). This argument is somewhat counterintuitive (see details).}
}


\details{
 It defines variables to be excluded from the selected ones to be removed.

 \code{remove.vars} is used to eliminate variables one is not interested in. This significantly reduces the size of the cell.data object and therefore the size of the working environment when saved (to a .RData). It also reduced the chance of memory issues. 
 In the call to \code{remove.vars} select defines which variables are to be removed. You can use wildchars. For example to remove all nuclear variables use select="*nucl*". The exclude argument defines variables to be excluded from the selected ones to be deleted. For example if you want to remove all nuclear vars, except f.nucl.y use select="*nucl*", exclude="f.nucl.y".  
 }
\value{
  a cell.data object with the specified variables removed
}
\author{ Alan Bush }
\seealso{ \code{\link{subset}}, \code{\link{summary.cell.data}} }
\examples{
if(require(RcellData)){

  #load example dataset
  data(ACL394)
  
  #remove a variable
  X<-remove.vars(X,select="f.vacuole.y")
  
  #remove all background variables
  X<-remove.vars(X,select="*bg*")
  
  #remove all nuclear variables, except for f.nucl.y
  X<-remove.vars(X,select="*nucl*",exclude="f.nucl.y")
  
  summary(X)
}
}
\keyword{manip}

