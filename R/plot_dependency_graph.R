#' Plot network of package dependencies
#'
#' Plots the network of package dependencies based on a package
#' \code{DESCRIPTION} file.
#'
#' @param pkg package description, can be path or package name. See
#'   \code{\link[devtools]{as.package}} for more information.
#' @inheritParams miniCRAN::makeDepGraph
#' @inheritParams ggplot2::scale_color_viridis_c
#' @inheritDotParams miniCRAN::makeDepGraph
#' @details The resulting plot visualizes the network of package dependencies
#'   with the number of upstream dependencies color coded and the number of
#'   downstream dependencies size coded.
#' @export

plot_dependency_graph <- function(pkg = ".", suggests = FALSE, option = "cividis", ...) {

  pkg <- devtools::as.package(pkg)

  dependencies <- unlist(strsplit(pkg$imports, split = "\n"))
  dependencies <- gsub("\\n| \\(.+\\)|,", "", dependencies)
  dependency_graph <- miniCRAN::makeDepGraph(
    pkg = dependencies
    , suggests = suggests
    , ...
  )
  class(dependency_graph) <- "igraph"
  dependency_graph <- dependency_graph +
    igraph::vertices(pkg$package) +
    igraph::edges(as.vector(rbind(dependencies, pkg$package)))
  dependency_graph <- igraph::simplify(dependency_graph)

  edge_list <- igraph::get.edgelist(dependency_graph)
  dependency_graph <- igraph::graph(rbind(edge_list[, 2], edge_list[, 1]))

  dependency_graph_df <- ggnetwork::fortify(
    dependency_graph
    , layout = igraph::with_fr()
    , arrow.gap = 0.015
  )

  dependency_graph_df$package <- dependency_graph_df$vertex.names
  dependency_graph_df$face <- ifelse(
    dependency_graph_df$name == pkg$package
    , "bold"
    , "plain"
  )

  dependency_graph_df$n_dependencies <- as.vector(
    table(
      gsub("\\|.+", "", attr(igraph::E(dependency_graph), "vnames"))
    )[as.character(dependency_graph_df$name)]
  )
  dependency_graph_df$n_dependencies[is.na(dependency_graph_df$n_dependencies)] <- 0

  dependency_graph_df$importance <- as.vector(
    table(
      gsub(".+\\|", "", attr(igraph::E(dependency_graph), "vnames"))
    )[as.character(dependency_graph_df$name)]
  )
  dependency_graph_df$importance[is.na(dependency_graph_df$importance)] <- 0

  max_downstream_deps <- max(dependency_graph_df$importance)

  dependency_graph_df$importance <- dependency_graph_df$importance / max_downstream_deps
  dependency_graph_df$importance <- abs(1 - dependency_graph_df$importance)

  dependency_graph_df <- as.data.frame(lapply(dependency_graph_df, as.vector))

  ggplot2::ggplot(
    dependency_graph_df
    , ggplot2::aes_(x = ~x, y = ~y, xend = ~xend, yend = ~yend)
  ) +
    ggnetwork::geom_nodes(
      ggplot2::aes_(color = ~n_dependencies)
      , size = 6.5
      , alpha = 0.4
    ) +
    ggnetwork::geom_edges(
      arrow = ggplot2::arrow(length = ggplot2::unit(4, "pt"), type = "closed")
      , color = grDevices::grey(0.4)
    ) +
    ggnetwork::geom_nodes(ggplot2::aes_(color = ~n_dependencies, size = ~importance * 7)) +
    ggnetwork::geom_nodelabel_repel(
      ggplot2::aes_(label = ~name, fontface = ~face, color = ~n_dependencies)
      , box.padding = ggplot2::unit(8, "pt")
    ) +
    ggplot2::scale_color_viridis_c(option = option) +
    ggplot2::scale_size(
      labels = function(x) abs(max_downstream_deps - ceiling(x / 7 * max_downstream_deps))
    ) +
    ggplot2::guides(
      size = ggplot2::guide_legend(
        title = "Downstream dependencies"
        , title.position = "top"
        , title.hjust = 0.5
        , label.position = "bottom"
        , label.hjust = 0.5
      )
      , color = ggplot2::guide_colorbar(
        title = "Upstream dependencies"
        , title.position = "top"
        , title.hjust = 0.5
        , barwidth = ggplot2::unit(130, "pt")
        , barheight = ggplot2::unit(4, "pt")
      )
    ) +
    ggnetwork::theme_blank(legend.position = "top")
}
