#' Multiple plot function
#'
#' \code{multiplot} takes  ggplot objects and combines them in one output
#'  as you would expect it from \code{\link[graphics]{par}}. This function is adapted
#'  from the Cookbook for R \link{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/}
#'
#' @param ... ggplot objects to be combined in one image.
#' @param cols   Desired number of columns in the final image.
#' @param byrow logical. If true, the image is filled row-wise (see \code{\link[base]{matrix}} for more}.)
#'
#' @return A combined image of multiple ggplot objects on a single page.
multiplot <- function(..., file, cols=1, byrow=TRUE) {
  plots <- c(list(...))
  numPlots = length(plots)
  layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                   ncol = cols, nrow = ceiling(numPlots/cols), byrow=byrow)
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#
#' A function to control decimal precision for ggplot axis labels
#'
#' This function was originally posted here \link{https://stackoverflow.com/questions/10035786/ggplot2-y-axis-label-decimal-precision#10036038}
#'
#' @param decimals Desired number of decimals on plot axis.
#'
#' @return A vector  of correctly formated labels when used as an argument for the \code{labels}-argument
#' of a ggplot scale function.
#'
#' @example
#' require(ggplot2)
#' df <- data.frame(x = 1:5,y = rexp(5))
#' ggplot(df,aes(x = x,y=y)) +
#'  geom_point() +
#'  scale_y_log10(labels = fmt_dcimals(2))
fmt_dcimals <- function(decimals=0){
  function(x) format(x,nsmall = decimals,scientific = FALSE)
}
