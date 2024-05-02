#' @importFrom parsec incidence2cover.incidence vertices
#' @importFrom glue glue
tikz_lattice <- function(L,
                         scale = c(1, 1),
                         tags = NA,
                         latex_size = "\\normalsize",
                         circle = FALSE,
                         node_color = "black",
                         ...) {

  textmode <- !rlang::is_na(tags)
  node_style <- ifelse(textmode,
                 ifelse(circle, "textnode",
                        "textnode_rect"),
                 "node")

  pattern <- switch(
    node_style,
    "textnode" = "circle, fill=white, inner sep=1pt, minimum width=1pt",
    "textnode_rect" = "fill=white, inner sep=1pt, minimum width=1pt",
    "node" = "circle, draw, fill=white, {this_color}inner sep=1.5pt, minimum width=1pt"
    )

  M <- as.matrix(L$reduced_matrix) |> t()
  y <- parsec::incidence2cover.incidence(M)

  g <- igraph::graph_from_adjacency_matrix(t(y))
  ly <- igraph::layout_with_sugiyama(g, maxiter = 100)$layout

  vertices <- ly
  vertices <- as.data.frame(ly)
  colnames(vertices) <- c("x", "y")

  vertices$y <- 0.8 * vertices$y
  # Reverse x values:
  # m <- min(vertices$x)
  # M <- max(vertices$x)
  #
  # vertices$x <- M + m - vertices$x


  lab <- rownames(y)
  include <- "\\usepackage{tikz}\n"
  # begin <- paste("\\begin{figure}[!h]\n\\label{", label, "}\n\\centering\n\\begin{tikzpicture}[scale=1.2]\n",
  #                sep = ""
  # )
  begin <- glue::glue(
    "\\begin{{tikzpicture}}[scale=1.2]"
  )
  n <- nrow(y)
  nodes <- rep("", n)

  stopifnot(length(node_color) == 1 || length(node_color) == n)

  if (length(node_color) == 1) node_color <- rep(node_color, n)

  node_colors <- paste0("\\color{", node_color, "}")

  for (i in 1:n) {

    if (textmode) {

      this_label <- glue::glue(
        "{latex_size}${node_colors[i]}{tags[i]}$"
      )

    } else {

      this_label <- ""

    }

    if (node_color[i] != "black") {

      this_color <- glue::glue(
        "color={node_color[i]}, ")

    } else {

      this_color <- ""

    }
    this_node <- glue::glue(pattern)

    nodes[i] <- glue::glue(
      "\\node({i}) at ({vertices$x[i] * scale[1]}, {vertices$y[i] * scale[2]})[{this_node}]{{{this_label}}};"#{{}};"
    )

  }
  k <- sum(M)
  lines <- rep("", k)
  count <- 1
  for (a in seq(n)) {

    for (b in seq(n)) {

      if (L$reduced_matrix[a, b]) {

        lines[count] <- glue::glue(
          "\\draw[-, very thin] ({a}) to ({b});"
        )

        count <- count + 1

      }

    }

  }

  end <- paste("\\end{tikzpicture}\n")
  # "\\caption{", caption, "}\n\\end{figure}\n\\end{document}",
  #              sep = ""
  # )
  #
  message(glue::glue("You should include\n{include}\nin your document"))

  final_str <- paste(
    begin,
    paste(nodes, collapse = "\n"),
    paste(lines, collapse = "\n"),
    end,
    sep = "\n"
  )

  if (rlang::is_interactive()) {

    cat(final_str)
    return(invisible(final_str))

  }
  return(final_str)

}

