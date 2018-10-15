#' Plots a summary of used packages fround in files from a given folder
#'
#' @inheritParams pff_find_and_install_pkgs
#' @param n.most.used.pkgs Number of most used packages to show in plot (default = 10)
#'
#' @return A ggplot2 object that can be printed with print()
#' @export
#'
#' @examples
#'  my.dir <- dirname(system.file('extdata', package = 'PkgsFromFiles'))
#'  p <- pff_plot_summary_pkgs(my.dir)
#'  print(p)
pff_plot_summary_pkgs <- function(folder.in,
                                  n.most.used.pkgs = 10,
                                  do.recursive = TRUE) {

  df.files <- pff_find_R_files_from_folder(folder.in, do.recursive)

  pkgs <- unlist(lapply(df.files$pkgs,
                 function(x) return(stringr::str_split(x, ' ; ')[[1]])) )

  my.count <- NULL
  df.to.plot <- dplyr::data_frame(my.count = as.numeric(table(pkgs)),
                                  pkgs = names(table(pkgs)))

  df.to.plot <- df.to.plot[order(-df.to.plot$my.count)[1:n.most.used.pkgs], ]

  p <- ggplot2::ggplot(df.to.plot, ggplot2::aes(y = stats::reorder(my.count, my.count),
                                                x = stats::reorder(pkgs, my.count))) +
    ggplot2::geom_col() + ggplot2::coord_flip() +
    ggplot2::labs(y = 'Number of package occurences in folder',
                  x = 'Package Name')

  return(p)

}
