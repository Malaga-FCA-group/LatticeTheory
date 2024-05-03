#' Create a lattice from a concept lattice.
#'
#' @param concepts A concept lattice object.
#'
#' @return A lattice object.
#' @export
from_conceptlattice <- function(concepts) {

  if (requireNamespace("fcaR")) {

    # Get the extents of the concepts.
    extents <- concepts$extents()

    # Get the order of the lattice.
    order <- fcaR:::.subset(extents) %>%
      t() %>%
      as.matrix()

    # Get the elements of the lattice.
    els <- c()
    for (i in seq(concepts$size())) {

      # Get the intent of the concept.
      S <- concepts$sub(i)$get_intent()

      # Convert the intent to a string.
      els <- c(els,
               fcaR:::.set_to_string(
                 S$get_vector(),
                 S$get_attributes())
      )
    }

    # Set the column and row names of the order to the elements.
    colnames(order) <- rownames(order) <- els

    # Return the lattice.
    return(Lattice$new(order))
  }
}





