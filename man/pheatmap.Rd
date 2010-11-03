\name{pheatmap}
\alias{pheatmap}
\title{
Pretty heatmaps
}
\description{
A function to draw clustered heatmaps. 
}
\usage{
pheatmap(mat, color = colorRampPalette(rev(c("#D73027", "#FC8D59", 
	    "#FEE090", "#FFFFBF", "#E0F3F8", "#91BFDB", "#4575B4")))(100), 
	    breaks = NA, border_color = "grey60", cellwidth = NA, cellheight = NA, 
	    scale = "none", cluster_rows = TRUE, cluster_cols = TRUE, 
	    clustering_distance = "euclidean", clustering_method = "complete", 
	    treeheight_row = ifelse(cluster_rows, 50, 0), 
	    treeheight_col = ifelse(cluster_cols, 50, 0), legend = TRUE, 
	    annotation = NA, annotation_colors = NA, annotation_legend = TRUE,
	    show_rownames = T, show_colnames = T, filename = NA, width = NA, 
	    height = NA, ...)
}

\arguments{
  \item{mat}{
numeric matrix of the values to be plotted.
}
  \item{color}{
vector of colors used in heatmap. 
}
  \item{breaks}{
a sequence of numbers that covers the range of values in mat and is one element longer than color vector. Used for mapping values to colors. Useful, if needed to map certain values to certain colors, to certain values. If value is NA then the breaks are calculated automatically.
}

\item{border_color}{
color of cell borders on heatmap, use NA if no border should be drawn.
}

\item{cellwidth, cellheight}{
individual cell measurements in points. If left as NA, then the values depend on the size of plotting window.
}

\item{scale}{
character indicating if the values should be centered and scaled in either the row direction or the column direction, or none. Corresponding values are \code{"row"}, \code{"column"} and \code{"none"}
}

\item{cluster_rows, cluster_cols}{
boolean values determining if either rows or columns should be clustered.
}

\item{clustering_distance}{
distance measure used in clustering. Possible values are \code{"correlation"} and all the distances supported by \code{\link{dist}}, such as \code{"euclidean"}, etc. 
}

\item{clustering_method}{
clustering method used. Accepts the same values as \code{\link{hclust}}.
}


  \item{treeheight_row, treeheight_col}{
the height of a tree, if data is clustered, default value 50 points.
}

  \item{legend}{
boolean value that determines if legend should be drawn or not.
}

  \item{annotation}{
data frame that specifies the annotations shown on top of the columns. Each row defines the features for a specific column. The columns in the data and rows in the annotation are matched using corresponding row and column names. Note that color schemes takes into account if variable is continuous or discrete.
}

  \item{annotation_colors}{
list for specifying annotation track colors manually. It is possible to define the colors for only some of the features. Check examples for details.
}

  \item{annotation_legend}{
boolean value showing if the legend for annotation tracks should be drawn. 
}


\item{show_rownames, show_colnames}{
booleans specifying if row and column names are be shown.
}

\item{filename}{
file path ending with .pdf where to save the picture. Even if the plot does not fit into the plotting window, the file size is calculated so that the plot would fit there, unless specified otherwise.
}
  \item{width, height}{
manual options for determining the output file measurements in inches.
}
  \item{\dots}{
graphical parameters for the text used in plot. Parameters passed to \code{\link{grid.text}}, see \code{\link{gpar}}. For text size use parameter fontsize (default = 10).
}
}


\author{
Raivo Kolde
}

\examples{
	# Generate some data
	test = matrix(rnorm(200), 20, 10)
	test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
	test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2
	colnames(test) = paste("Test", 1:10, sep = "")
	rownames(test) = paste("Gene", 1:20, sep = "")

	# Draw heatmaps
	pheatmap(test)
	pheatmap(test, scale = "row", clustering_distance = "correlation")
	pheatmap(test, color = colorRampPalette(c("navy", "white", "firebrick3"))(50))
	pheatmap(test, cluster_row = FALSE)
	pheatmap(test, legend = FALSE)
	pheatmap(test, cellwidth = 15, cellheight = 12)
	pheatmap(test, cellwidth = 15, cellheight = 12, fontsize = 8, filename = "test.pdf")

	# Generate column annotations
	annotation = data.frame(Var1 = factor(1:10 \%\% 2 == 0, labels = c("Class1", "Class2"))
	, Var2 = 1:10)
	rownames(annotation) = paste("Test", 1:10, sep = "")

	pheatmap(test, annotation = annotation)
	pheatmap(test, annotation = annotation, annotation_legend = FALSE)


	# Specify colors
	Var1 = c("navy", "darkgreen")
	names(Var1) = c("Class1", "Class2")
	Var2 = c("lightgreen", "navy")

	ann_colors = list(Var1 = Var1, Var2 = Var2)

	pheatmap(test, annotation = annotation, annotation_colors = ann_colors)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

