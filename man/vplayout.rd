\name{vplayout}
\alias{vplayout}

\title{Viewport functions}
\description{
Multiple viewports per page
}
\usage{
vplayout(x, y)
}
\arguments{
  \item{x}{x index of grid to use to print the ggplot2 figure}
  \item{y}{y index of grid to use to print the ggplot2 figure}
}
\details{
	See documentation in package 'grid' for more details. 
}
\author{Alan Bush}
\seealso{ \code{\link{transform.cell.data}}}
\examples{

if(require(RcellData)){
  #put several figures in a page
  data(ACL394)
  grid.newpage() #create a new plot
  pushViewport(viewport(layout = grid.layout(1, 2))) #define the grid for the plots
  print(cplot(X,f.tot.y~pos), vp = vplayout(1, 1))
  print(cplot(X,f.tot.y~a.tot,color=pos), vp = vplayout(1, 2))
}
}
\keyword{manip}