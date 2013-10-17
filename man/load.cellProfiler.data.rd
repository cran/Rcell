\name{load.cellProfiler.data}
\alias{load.cellProfiler.data}
\title{Load CellProfiler Data}
\description{
\code{load.cellProfiler.data} loads a CellProfiler dataset. This functions reads the 
 matlab file (e.g. 'DefaultOUT.mat') created by CellProfiler, and returns a cell.data object. 
                                                                                              }
\usage{
load.cellProfiler.data(filename="DefaultOUT.mat",path=getwd(),input.path="../Input"
	,rm.str.from.channel.name=c(".14bit",".14.bit"),return.list=FALSE
	,cellTable="FilteredCells",nucleiTable="FilteredNuclei2"
	,cytoplasmTable="FilteredCytoplasm",out.nuc.channel=""
	,out.nuc.postfix="--Overlays.tiff",out.nuc.offset.x=0,out.nuc.offset.y=0
	,out.cyt.channel="",out.cyt.postfix="--Overlays.tiff",out.cyt.offset.x=0
	,out.cyt.offset.y=0)
}
     
\arguments{
   \item{filename}{character with the name of the *.mat file to be loaded}
   \item{path}{character containing path where the *.mat file is located}
   \item{input.path}{character containing the path, relative to \code{path}, where to find the input images. This is used only if the images are not found in the path indicated by the *.mat file, or the \code{path}}
   \item{rm.str.from.channel.name}{character vector with strings to be removed from the channel names}
   \item{return.list}{boolean. If TRUE a list containing the informatio of the *.mat file as loaded is returned.}
   \item{cellTable}{character with the name of the Table of the *.mat file that contains the cells data}
   \item{nucleiTable}{character with the name of the Table of the *.mat file that contains the nuclei data}
   \item{cytoplasmTable}{character with the name of the Table of the *.mat file that contains the cytoplasm data} 
   \item{out.nuc.channel}{character specifying the channel used as basename for the output image with the nuclear outlines. Note that this is determined by the filename of the output file, and not the actual image.}
   \item{out.nuc.postfix}{character specifying the postfix of the output image for the nuclear outlines. This should include the extension '.tiff'.}
   \item{out.nuc.offset.x}{integer indicating the x offset of the cells relative to the image. This is used when the output are image layouts.}
   \item{out.nuc.offset.y}{idem for y}
   \item{out.cyt.channel}{character specifying the channel used as basename for the output image with the cytoplasm outlines. Note that this is determined by the filename of the output file, and not the actual image.}
   \item{out.cyt.postfix}{character specifying the postfix of the output image for the cytoplasm outlines. This should include the extension '.tiff'.}
   \item{out.cyt.offset.x}{integer indicating the x offset of the cells relative to the image. This is used when the output are image layouts.}
   \item{out.cyt.offset.y}{idem for y}
}

\details{

  This function uses the \code{readMat} function from the R.matlab package to load the *.mat file to R. This is loaded as a complex list, that is first simplified to a easier to use format. This simplified list can be retrieved with the \code{return.list} argument. After loading the data, it is restructured to a cell.data object. The variables pos, cellID, t.frame, ucid, xpos, ypos are created. Other variables are renamed to a more compact form. Nuclear variables start with 'nuc.' and cytoplasmic variables with 'cyt.'. The channel is specified by a short postfix to each channel specific variable name. 
  
  The function tries to locate the input and output images. It first looks fot the images in the directory specified by the *.mat file. If it doesn't find them there it checks in specified \code{path}. For input images it also checks in the \code{input.path} folder. 
  
  The names of the output images are not specified in the *.mat file, so thay have to be specified in the function's arguments. Thats what the \code{out.*} arguments are for. 
}

\value{
  a cell.data object
}
\author{ Alan Bush }
\seealso{
   \code{\link{load.cellID.data}}
}
\examples{
\dontrun{
setwd(".")  #set the working directory to the folder with your images
X<-load.cellProfiler.data()  #load the dataset to R
}
}
\keyword{IO}
\keyword{manip}
