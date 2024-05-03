from_conceptlattice <- function(concepts) {

  if (requireNamespace("fcaR")) {

    extents <- concepts$extents()
    order <- fcaR:::.subset(extents) |>
      t() |>
      as.matrix()

    els <- c()
    for (i in seq(concepts$size())) {

      S <- concepts$sub(i)$get_intent()
      els <- c(els,
               fcaR:::.set_to_string(
                 S$get_vector(),
                 S$get_attributes())
      )

    }

    colnames(order) <- rownames(order) <- els

    return(Lattice$new(order))

  }

}
