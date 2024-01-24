graph_plot <- function(graph, ...,
                       node_colour = NULL,
                       edge_colour = NULL,
                       node_size = NULL,
                       edge_width = NULL,
                       node_label = NULL,
                       edge_label = NULL) {
  node_colour <- rlang::enquo(node_colour)
  edge_colour <- rlang::enquo(edge_colour)
  node_size <- rlang::enquo(node_size)
  edge_width <- rlang::enquo(edge_width)
  node_label <- rlang::enquo(node_label)
  edge_label <- rlang::enquo(edge_label)
  graph <- tidygraph::as_tbl_graph(graph)
  tidygraph::.register_graph_context(graph, TRUE)
  # {
  p <- suppressMessages(ggraph::ggraph(graph) + ggplot2::coord_fixed())
  if (!rlang::quo_is_null(node_label)) {
    label_col <- rlang::quo_text(node_label)
    label_col <- paste0(c("node1.", "node2."), label_col)
    start_label <- rlang::sym(label_col[1])
    end_label <- rlang::sym(label_col[2])
    p <- p +
      ggraph::geom_edge_link(
        ggplot2::aes(
          colour = !!edge_colour,
          label = !!edge_label,
          width = !!edge_width,
          start_cap = ggraph::label_rect(!!start_label),
          end_cap = ggraph::label_rect(!!end_label)
        ),
        angle_calc = "along",
        label_dodge = grid::unit(2.5, "mm")
      )
  } else if (!rlang::quo_is_null(edge_label)) {
    p <- p + ggraph::geom_edge_link(
      ggplot2::aes(
        colour = !!edge_colour,
        label = !!edge_label,
        width = !!edge_width
      ),
      angle_calc = "along",
      label_dodge = grid::unit(2.5, "mm")
    )
  } else {
    p <- p + ggraph::geom_edge_link0(ggplot2::aes(
      colour = !!edge_colour,
      width = !!edge_width
    ))
  }
  if (rlang::quo_is_null(node_label)) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(
      colour = !!node_colour,
      size = !!node_size
    ))
  } else {
    p <- p + ggraph::geom_node_text(ggplot2::aes(
      label = !!node_label,
      colour = !!node_colour
    ), repel = FALSE)
  }
  # }
  p <- p  + ggraph::theme_graph()
  p
}
