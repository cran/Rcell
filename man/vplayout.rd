\name{vplayout}
\alias{vplayout}
\alias{grid.layout}
\alias{grid.newpage}
\alias{pushViewport}
\alias{viewport}

\title{Viewport functions}
\description{
Multiple viewports per page
}
\usage{
grid.layout(nrow = 1, ncol = 1,
        widths = unit(rep(1, ncol), "null"),
        heights = unit(rep(1, nrow), "null"),
        default.units = "null", respect = FALSE,
        just="centre")
grid.newpage(recording = TRUE)
pushViewport(..., recording=TRUE)
vplayout(x, y)
viewport(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
         width = unit(1, "npc"), height = unit(1, "npc"),
         default.units = "npc", just = "centre",
         gp = gpar(), clip = "inherit",
         xscale = c(0, 1), yscale = c(0, 1),
         angle = 0,
         layout = NULL,
         layout.pos.row = NULL, layout.pos.col = NULL,
         name = NULL)
}
\arguments{
  \item{nrow}{An integer describing the number of rows in the layout.}
  \item{ncol}{An integer describing the number of columns in the layout.}
  \item{widths}{A numeric vector or unit object describing the widths of the columns in the layout.}	
  \item{heights}{A numeric vector or unit object describing the heights of the rows in the layout}	
  \item{default.units}{A string indicating the default units to use if widths or heights are only given as numeric vectors.}	
  \item{respect}{A logical value or a numeric matrix. If a logical, this indicates whether row heights and column widths should respect each other. If a matrix, non-zero values indicate that the corresponding row and column should be respected (see examples below).}	
  \item{just}{A string or numeric vector specifying how the layout should be justified if it is not the same size as its parent viewport. If there are two values, the first value specifies horizontal justification and the second value specifies vertical justification. Possible string values are: "left", "right", "centre", "center", "bottom", and "top". For numeric values, 0 means left alignment and 1 means right alignment. NOTE that in this context, "left", for example, means align the left edge of the left-most layout column with the left edge of the parent viewport.}	
  \item{recording}{A logical value to indicate whether the new-page operation should be saved onto the Grid display list.}
  \item{\dots}{One or more objects of class "viewport".}	
  \item{x}{x index of grid to use to print the ggplot2 figure}
  \item{y}{y index of grid to use to print the ggplot2 figure}
  \item{width}{A numeric vector or unit object specifying width.}	
  \item{height}{A numeric vector or unit object specifying height.}	
  \item{gp}{An object of class gpar, typically the output from a call to the function gpar. This is basically a list of graphical parameter settings}
  \item{clip}{One of "on", "inherit", or "off", indicating whether to clip to the extent of this viewport, inherit the clipping region from the parent viewport, or turn clipping off altogether. For back-compatibility, a logical value of TRUE corresponds to "on" and FALSE corresponds to "inherit".}
  \item{xscale}{A numeric vector of length two indicating the minimum and maximum on the x-scale}
  \item{yscale}{A numeric vector of length two indicating the minimum and maximum on the y-scale.}
  \item{angle}{A numeric value indicating the angle of rotation of the viewport. Positive values indicate the amount of rotation, in degrees, anticlockwise from the positive x-axis.}
  \item{layout}{A Grid layout object which splits the viewport into subregions.}
  \item{layout.pos.row}{A numeric vector giving the rows occupied by this viewport in its parent's layout.}
  \item{layout.pos.col}{A numeric vector giving the columns occupied by this viewport in its parent's layout.}
  \item{name}{A character value to uniquely identify the viewport once it has been pushed onto the viewport tree.}
}
\details{
	See documentation in package 'grid' for more details. 
}
\author{Alan Bush}
\seealso{ \code{\link{transform.cell.data}}}
\examples{

#put several figures in a page
data(ACL394)
grid.newpage() #create a new plot
pushViewport(viewport(layout = grid.layout(1, 2))) #define the grid for the plots
print(cplot(X,f.tot.y~pos), vp = vplayout(1, 1))
print(cplot(X,f.tot.y~a.tot,color=pos), vp = vplayout(1, 2))

}
\keyword{manip}