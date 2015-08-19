\name{plfMA}
\alias{plfMA}
\title{A GUI to view, design and export various graphs of data}
\description{
Provides a graphical user interface for viewing and designing various types of graphs of the data. The graphs can be saved in various formats of an image like .jpeg, .png, .bmp, .pdf, and .postscript.
}
\usage{
plfMA(h,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{Data matrix, data frame, RGList, MAList }
  \item{\dots}{Other arguments }
}
\details{
Function launches a Graphical User Interface using gWidgets package. It uses tcltk interface for smooth processing while exporting the graphs.
}
\author{Dhammapal Bharne
}
\examples{
x<-matrix(1:100,nc=10)
plfMA(x)
}
