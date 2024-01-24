tikz_lattice <- function(L,
                         # label = "",
                         # caption = "",
                         scale = c(1, 1),
                         tags = L$names, ...) {

  textmode <- length(tags) > 0
  node <- ifelse(textmode, "textnode", "node")

  M <- as.matrix(L$reduced_matrix) |> t()
  y <- parsec::incidence2cover.incidence(M)

  # y <- L$order

  vertices <- -parsec::vertices(y)

  # g <- igraph::graph_from_adjacency_matrix(t(y))
  # ly <- igraph::layout_with_sugiyama(g, maxiter = 100)$layout

  # # if (equispaced) {
  #   rg <- range(ly[, 1])
  #   fr <- table(ly[, 2])
  #   w <- max(fr)
  #   d <- diff(rg)/(w - (w > 1))
  #   ctr <- diff(rg)/2
  #   xax <- tapply(ly[, 1], ly[, 2], function(x) {
  #     res <- order(x) - 1
  #     (res - median(res)) * d + ctr
  #   })
  #   hgt <- max(ly[, 2])
  #   for (i in 1:hgt) ly[ly[, 2] == i, 1] <- xax[[as.character(i)]]
  # # }

  # vertices <- ly
  # vertices <- as.data.frame(ly)
  colnames(vertices) <- c("x", "y")

  # Reverse x values:
  m <- min(vertices$x)
  M <- max(vertices$x)

  vertices$x <- M + m - vertices$x

  lab <- rownames(y)
  include <- "\\usepackage{tikz}\n\\tikzstyle{node}=[circle, draw, fill=white, inner sep=1pt, minimum width=1pt]\n\\tikzstyle{textnode}=[circle, fill=white, inner sep=1pt, minimum width=1pt]"
  # begin <- paste("\\begin{figure}[!h]\n\\label{", label, "}\n\\centering\n\\begin{tikzpicture}[scale=1.2]\n",
  #                sep = ""
  # )
  begin <- glue::glue(
    "\\begin{{tikzpicture}}[scale=1.2]"
  )
  n <- nrow(y)
  nodes <- rep("", n)

  for (i in 1:n) {

    # str <- ""
    # if (tags[i] != "") {
    #   str <- glue::glue("[label={{\\tiny {tags[i]}}}]")
    # }

    nodes[i] <- glue::glue(
      "\\node({i}) at ({vertices$x[i] * scale[1]}, {vertices$y[i] * scale[2]})[{node}]{{${tags[i]}$}};"#{{}};"
    )

    # nodes[i] <- glue::glue(
    #   "\\node({i}) at ({vertices$x[i] * scale[1]}, {vertices$y[i] * scale[2]})[auto, scale = 0.2]{{{tags[i]}}};"
    # )

    # nodes[i] <- paste("\\node(", i, ") at (", vertices$x[i] *
    #   scale[1], ",", vertices$y[i] * scale[2], ")[node]{",
    # lab[i], "};\n",
    # sep = ""
    # )
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
  return(paste(begin,
               paste(nodes, collapse = "\n"),
               paste(lines, collapse = "\n"),
               end,
               sep = "\n"
  ))

}

