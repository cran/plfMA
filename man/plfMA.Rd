\name{plfMA}
\alias{plfMA}
\title{
A function to launch the plfMA GUI.
}
\description{
A function to launch the plfMA GUI.
}
\usage{
plfMA(h,...)
}
\arguments{
	\item{h}{Data matrix, data frame, RGList, MAList or Empty}
	\item{\dots}{Other arguments for h object or Empty}
}
\details{
Once the package is loaded with library and a data object is created, the GUI can be launced using this function.
}
\author{
Dhammapal Bharne & Vaibhav Vindal, Department of Biotechnology and Bioinformatics, School of Life Sciences, University of Hyderabad, Hyderabad, Telangana, India.
}
\examples{
x<-matrix(1:100,ncol=10)
plfMA(x)
}
\keyword{ GUI }
