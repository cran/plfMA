\name{plfMA-package}
\alias{plfMA-package}
\docType{package}
\title{
A GUI to view, design and export various graphs of data
}
\description{
Provides a graphical user interface for viewing, designing and exporting various types of graphs of the data. A table file can be directly imported for visualization after launching the GUI. A data object with graph type can also be passed to library function. A graph type could be either scatter (scatter-plot), hist (histogram), bar (bar-plot), box (box-plot), pie (pie-diagram), 3d (3d-plot), ma (MA-plot), density (density-plot) or heatmap. The graphs can be designed with different labels, colors, sizes and other features and saved at differnt resolution in bmp, jpeg, png, tiff, pdf and postscript formats.
}
\details{
	\tabular{ll}{
		Package: \tab plfMA\cr
		Type: \tab Package\cr
		Version: \tab 2.0\cr
		Date: \tab 2022-01-07\cr
		License: \tab GPL-2\cr
	}
	Just type pflMA() at R prompt to launch the GUI. Alternatively, create a data object such as x at R prompt and launch the GUI by typing plfMA(x,"heatmap") for heatmap.
}
\author{
Maintainer: Dhammapal Bharne <dhammapalb@uohyd.ac.in>
}
