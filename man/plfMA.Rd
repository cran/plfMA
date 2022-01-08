\name{plfMA}
\alias{plfMA}
\title{
A function to launch the plfMA GUI.
}
\description{
A function to launch the plfMA GUI.
}
\usage{
plfMA(h,gtype,...)
}
\arguments{
	\item{h}{Data matrix, data frame, RGList, MAList or Empty}
	\item{gtype}{Graph type or Empty if h is empty}
	\item{\dots}{Other arguments for h object or Empty}
}
\details{
Once the package is loaded with library function, the GUI can be launced using this function.
}
\author{
Dhammapal Bharne & Vaibhav Vindal, Department of Biotechnology and Bioinformatics, School of Life Sciences, University of Hyderabad, Hyderabad, Telangana, India.
}
\examples{
x<-matrix(1:60,ncol=6)
plfMA(x,gtype="heatmap")
}
\keyword{ GUI }
