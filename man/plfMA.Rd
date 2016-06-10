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
\value{
None.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Dhammapal Bharne & Vaibhav Vindal, Department of Biotechnology and Bioinformatics, School of Life Sciences, University of Hyderabad, Hyderabad, Telangana, India.
}
\examples{
plfMA()			# Launch the function and then import a tab delimited file
x<-matrix(1:100,ncol=10)	# Create a data object
plfMA(x)		# Execute the function for a data object
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ GUI }
