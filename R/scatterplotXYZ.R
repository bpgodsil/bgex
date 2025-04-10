#' Make a pretty scatter plot
#'
#' `scatterplotXYZ` returns a ggplot2-based scatter plot in the console,
#' and optionally as an image file.
#'
#' The base scatter plot uses x and y values with dark grey points. It can be further
#' customized to include a z-dimension coded by the size of the points. Colors
#' can be displayed for a grouping variable. The points can have text labels
#' with or with segments that connect each label with its point. The plot is
#' returned as an object and can also be saved as an image file.
#'
#' When the width and height parameters are set to NA, the output file will be
#' sized in relation to the plot window. This approach can be helpful for visualizing
#' the proportions before printing an image file.
#'
#' @param dat a data.frame
#' @param x a string that names the column in dat to be displayed on the x-axis
#' @param y a string that names the column in dat to be displayed on the y-axis
#' @param z NULL or a string that names the column in dat to be represented by
#' the size of the points.
#' @param group NULL or a string that names the column in dat that will give colors
#' to subgroups of point
#' @param point_label NULL or a string that names the column in dat that will provide
#' text labels for the points
#' @param label_size a numeric value that controls the size of the labels
#' @param min_segment_length a positive numeric value or Inf which controls the
#' length of segments between the point and label. If 0, then no segments are shown.
#' @param max_overlaps an integer from zero to Inf. Larger values increase the
#' number of text label overlaps that are permitted.
#' @param point_size a positive integer that controls the size of the points when 'z'
#' is NULL
#' @param title a string. NULL or a string. If the string is "xy" the plot's title
#' will use the for of 'x vs. y'. Else, the string will be the title.
#' @param filename NULL or a string that names the image file and optionally the
#' path to the file. The file name must include an image file suffix such as ".jpg"
#' ".png", ".tiff", or ".pdf".
#' @param width,height Plot size in units expressed by the units argument. If not
#' supplied, uses the size of the current graphics device.
#' @param units One of the following units in which the width and height arguments
#' are expressed: "in", "cm", "mm" or "px".
#' @param dpi Plot resolution
#' @return a ggplot2 and optionally a image file
#' @examples
#'
#'  # z' is NULL and title is "xy"
#'  scatterplotXYZ(dat = iris, x = "Sepal.Length", y = "Sepal.Width", z = NULL,
#'                 group = "Species", title = "xy")
#'
#'  # "z" is used to control point size and the title is customized
#'  scatterplotXYZ(dat = iris, x = "Sepal.Length", y = "Sepal.Width", z = "Petal.Width",
#'                 group = "Species", title = "Iris Scatter Plot")
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @export

scatterplotXYZ <- function(dat, x, y, z = NULL, group = NULL, point_label = NULL,
                           label_size = 3.5, min_segment_length = Inf, max_overlaps = Inf,
                           point_size = 2, title = NULL, filename = NULL,
                           width = 15, height = 15, units = "cm", dpi = 200){

  # establish palette IF using `group`
  if(!is.null(group)){

    # color palettes
    pal10 <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
               "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")

    pal20 <- c("#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F",
               "#8CD17D", "#B6992D", "#F1CE63", "#499894", "#86BCB6",
               "#E15759", "#FF9D9A", "#79706E", "#BAB0AC", "#D37295",
               "#FABFD2", "#B07AA1", "#D4A6C8", "#9D7660", "#D7B5A6")

    # select palette
    n_groups <- length(unique(na.omit(dat[[group]])))
    if(n_groups <= 10) pal <- pal10 else pal <- pal10
    if(n_groups > 20) warning("`scatterplot() supports only 20 colors.")

  }

  # establish base plot
  p1 <- ggplot(dat, aes(x = .data[[x]], y = .data[[y]])) +
    labs(x = x, y = y)+
    theme_minimal()+
    theme(panel.grid.minor = element_blank(),
          axis.title.x = element_text(family = "sans"),
          axis.title.y = element_text(family = "sans"))

  # include 'z' and/or 'group'
  if( !is.null(z) & !is.null(group) ){

    p1 <- p1 +
      geom_point(aes(size = .data[[z]], color = .data[[group]])) +
      scale_color_manual(values = pal)

  } else if( !is.null(z) & is.null(group) ){

    p1 <- p1 + geom_point(aes(size = .data[[z]]), shape=16)

  } else if( is.null(z) & !is.null(group) ){

    p1 <- p1 +
      geom_point(aes(color = .data[[group]]), size = point_size, shape=16) +
      scale_color_manual(values = pal)

  } else {

    p1 <- p1 + geom_point(size = point_size, shape=16, color = "grey20")

  }

  # include 'point_label'
  if(!is.null(point_label)){

    p1 <- p1 + geom_text_repel(aes(label = .data[[point_label]]),
                               min.segment.length = min_segment_length,
                               max.overlaps = max_overlaps,
                               size = label_size)

  }

  # include 'title'
  if( !is.null(title) && !title %in% "xy" ){

    p1 <- p1 + ggtitle(title)

  } else if( !is.null(title) && title %in% "xy" ){

    p1 <- p1 + ggtitle(paste0(x, " vs. ", y))

  }

  # print to image file
  if(!is.null(filename)){

    ggsave(p1, filename = filename, width = width, height = height,
           units = units, dpi = dpi)

    message(paste0("Scatterplot saved to ", filename))

  }

  return(p1)

}


