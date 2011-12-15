\name{ggplot2.themes}
\alias{ggplot2.themes}
\alias{ggplot2.theme}
\alias{ggplot.themes}
\alias{ggplot.theme}
\alias{theme_minimal}
\alias{theme_minimal_cb}
\alias{theme_minimal_cb_L}
\alias{theme_minimal_light}
\alias{theme_fullframe}
\alias{theme_black}
\alias{theme_complete_bw}
\alias{theme_invisible}

\title{ggplot2 themes}
\description{
Themes for ggplot2 graphics
}

\usage{
theme_minimal(base_size = 12, base_family = "")
theme_minimal_cb(base_size = 12, base_family = "")
theme_minimal_cb_L(base_size = 12, base_family = "")
theme_minimal_light(base_size = 12, base_family = "")
theme_fullframe(base_size = 12)
theme_black(base_size = 12)
theme_complete_bw(base_size = 12)
theme_invisible(base_size = 12)
}
\arguments{
  \item{base_size}{base size of font for the theme}
  \item{base_family}{base family of font for the theme}
}

\details{

  I found these functions posted at \url{https://github.com/hadley/ggplot2/wiki/Themes} and \url{http://sape.inf.usi.ch/quick-reference/ggplot2/themes}. I included them here for convenience. 

  These functions provide more themes for ggplot2 graphics. They work just as \code{\link{theme_grey}} and \code{\link{theme_bw}}
}
\value{
  A list with theme elements
}
\examples{

#creating example datset
mdf <- data.frame(x <- seq(0, 10), y=rnorm(x), 
                   f=factor(rep(letters[1:2], each=3, length=length(x))))
#base plot
p <- qplot(x, y, data=mdf, colour=f, geom=c("line", "point")) 

#compare themes
p + theme_grey() + opts(title="theme_grey()")
p + theme_bw() + opts(title="theme_bw()")
p + theme_minimal() + opts(title="theme_minimal()")
p + theme_minimal_cb() + opts(title="theme_minimal_cb()")
p + theme_minimal_cb_L() + opts(title="theme_minimal_cb_L()")
p + theme_minimal_light() + opts(title="theme_minimal_light()")
p + theme_fullframe() + opts(title="theme_fullframe()")
p + theme_black() + opts(title="theme_black()",plot.title = theme_text(colour = "white",size=14))
p + theme_complete_bw() + opts(title="theme_complete_bw()")
p + theme_invisible() + opts(title="theme_invisible()")

}
\keyword{manip}