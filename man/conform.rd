\name{conform}
\alias{conform}

\title{Conform a Data Frame}
\description{
  conforms the structure of a data.frame to that of an other
}
\usage{
  conform(df,to)
}

\arguments{
  \item{df}{data.frame to be conformed}
  \item{to}{data.frame to use as template (columns order)}  
}

\details{
  this function is useful do \code{rbind} between data frames that have different columns, or columns in diffent order. 
 }

\value{
   a data frame conformed to the template
}
\author{Alan Bush}
\examples{
  
  #creating example data frames
  df1<-data.frame(a=1:4,b=5:8)
  df2<-data.frame(b=9:14)
  df3<-data.frame(b=9:14,a=20:25)
            
  #using conform          
  conform(df2,to=df1)
  conform(df3,to=df1)
  
  #using conform with rbind
  rbind(df1,conform(df2,to=df1))
  rbind(df1,conform(df3,to=df1))
                           
}
\keyword{data}
