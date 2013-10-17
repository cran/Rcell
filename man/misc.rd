\name{misc}
\alias{misc}
\alias{paste_data_error}
\alias{paste_parameter}
\alias{paste_intercept_slope}
\alias{paste_EC50_n}

\title{Miscellaneous Functions}
\description{
Miscellaneous functions to do stuff in less lines
}
\usage{
paste_data_error(data,error,error.signif=1,plotmath=FALSE)
paste_parameter(fit,param,error.signif=1)
paste_intercept_slope(fit,error.signif=1)
paste_EC50_n(fit,leading.str="",error.signif=2)
}
\arguments{
  \item{data}{a numeric vector of values }
  \item{error}{a numeric vector of errors for \code{data} values}
  \item{error.signif}{number of significant digits for the error}	
  \item{plotmath}{if TRUE the +- character for plotmath is used instead of the default}	
  \item{fit}{an object of class 'lm' or 'nlm'}	
  \item{param}{character name of the parameter from \code{fit} to paste}	
  \item{leading.str}{string to paste before the data and error}	
}
\details{
	the \code{paste_} functions are used to paste a value and its error (or uncetainty) in resonable way.  
}
\value{
	a character vector with the data and error
}
\author{Alan Bush}
\seealso{ \code{\link{transform.cell.data}}}
\examples{
paste_data_error(1.0,0.01)
}
\keyword{manip}