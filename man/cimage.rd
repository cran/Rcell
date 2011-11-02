\name{cimage}
\alias{cimage}
\alias{cimage.cell.image}
\alias{cimage.cell.data}
\alias{cimage.default}


\title{Plot cell images}

\description{Arranges cells images in a plot}
\usage{

cimage(X,...)

\method{cimage}{cell.data}(X,formula=NULL,facets=NULL,time.var=c('time','t.frame')
  ,select=NULL,exclude=NULL,normalize.group='channel',...)

\method{cimage}{cell.image}(X,formula=NULL,facets=NULL,scales='fixed'
  ,nx=NULL,ny=NULL,facets.nx=NULL,facets.ny=NULL
  ,bg.col='gray',border=1,facets.border=1
  ,font.size=14,font.col='black',display=interactive(),...)

\method{cimage}{default}(X,...)

}

\arguments{
  \item{X}{cell.data or cell.image object to plot}
  \item{formula}{formula of the form 'var1+var2~var3' specifying how the images are to be ordered. See details.}
  \item{facets}{formula of the form 'var1+var2~var3' specifying how to facet the plot. See details.}
  \item{time.var}{variables that indicate time and should be excluded from the grouping variables. See \code{\link{get.cell.image}}}
  \item{select}{character vector defining further variables that are required for the plot}
  \item{exclude}{character vector defining variable names to be excluded}
  \item{normalize.group}{variable names that define groups of images that should be normalized together}
  \item{scales}{either 'fixed' or 'free' axis for each facet}
  \item{nx}{number of columns of images within each facet. Used with \code{formula} '~var1' or 'var1~.'}
  \item{ny}{number of rows of images within each facet. Used with \code{formulas} '~var1' or 'var1~.'}
  \item{facets.nx}{number of columns of facets. Used with \code{facets} '~var1' or 'var1~.'}
  \item{facets.ny}{number of rows of facets. Used with \code{facets} '~var1' or 'var1~.'}
  \item{bg.col}{The background color of the plot}
  \item{border}{the width in pixels of the border between images}
  \item{facets.border}{the width in pixels of the border between facets}
  \item{font.size}{The size of the font to use, in pixels}
  \item{font.col}{The color of the font to use}
  \item{display}{boolean indicating if the created image should be displayed}
  \item{\dots}{further arguments for methods. \code{cimage} calls \code{\link{get.cell.image}}, so all the arguments of this function are available.}
}
\details{

  Read the cimage vignette for a tutorial on how to use this function: vignette('cimage')

  \code{cimage} is a generic method that returns a 'Image' object, from EBImage package.
  
  If \code{cimage}'s first argument is a cell.data object, it first calls \code{\link{get.cell.image}} and then the \code{cimage} method for cell.image objects. This function arranges the images of single cell according to the \code{formula} and \code{facets} arguments, and adds appropriated axis to the image. 

 For example, formula=channel~t.frame, will arrange different channels as rows and t.frame as columns. You can use several variables per term, for example formula=channel~pos+t.frame will arrange the columns first by position, and within each position by t.frame. The variable to the right varies faster than the one to the left. 
 If only the right term of the formula is defined, as in formula=~t.frame, the images are 'wrapped' around, attempting to create a square plot. \code{nx} and \code{ny} can be used to define the number of columns or rows respectively. The special argument \dots can be used to indicate the samples within a group, for example formula=...~t.frame.
 The \code{facets} argument works in a similar way. 
      
 }
\value{
  The function returns an invisible 'Image' object of the EBImage package. Use display to render the image or writeImage to save it.
}
\author{ Alan Bush }
\seealso{EBImage,display}
\examples{

#load example dataset
data(ACL394filtered)
  
#display timecourse strip of cell 5 of pos 29, channels BF and YFP
if(interactive()) cimage(X,channel~t.frame,subset=pos==29&cellID==5,channel=c('BF','YFP'))

#display 7 cells (default value for N) of pos 29
if(interactive()) cimage(X,...+channel~t.frame,subset=pos==29,channel=c('BF','YFP'))

#display 3 cells from each pos in a different facet
if(interactive()) cimage(X,channel~...,facets=~pos,channel=c('BF.out','YFP'),N=3,
    subset=t.frame==11&match(pos,c(1,8,15,22,29),nomatch=0)>0)

#select one BF and many YFP images
if(interactive()) cimage(X,...~channel+t.frame,subset=pos==29,N=3,
	channel.subset=channel=='YFP'|(channel=='BF.out'&t.frame==11))

}
\keyword{manip}
\keyword{methods}

