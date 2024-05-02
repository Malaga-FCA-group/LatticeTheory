#' Product of two posets
#'
#' Computes the product of two posets.
#'
#' @param P1 The first poset.
#' @param P2 The second poset.
#'
#' @return The product poset.
#'
#' @examples
#' P1 <- Poset$new(matrix(c(0, 1, 0, 
0, 0, 1), ncol = 2))
#' P2 <- Poset$new(matrix(c(0, 1, 0, 0, 0, 1), ncol = 2))
#' P <- P1 %*% P2
#'
#' @export
`%*%` <- function(P1, P2) {

  # Extract the order and names of the first poset.
  M1 <- P1$order
  nm1 <- P1$names

  # Extract the order and names of the second poset.
  M2 <- P2$order
  nm2 <- P2$names

  # Create a matrix of names for the product poset.
  nm <- expand.grid(nm2, nm1)[, 2:1]

  # Create a matrix of zeros for the product poset.
  M <- matrix(0, ncol = nrow(nm), nrow = nrow(nm))

  # Iterate over the rows and columns of the matrix.
  for (ii in seq(nrow(nm))) {

    for (jj in seq(nrow(nm))) {

      # Extract the names of the current row and column.
      v1 <- nm[ii, 1]
      v2 <- nm[ii, 2]

      w1 <- nm[jj, 1]
      w2 <- nm[jj, 2]

      # Set the value of the current cell to the intersection of the orders of the two posets.
      M[ii, jj] <- M1[v1, w1] & M2[v2, w2]

    }

  }

  # Apply the glue function to the names matrix to create a list of names for the product poset.
  nm <- apply(nm, 1, \(x) glue::glue("({x[1]}, {x[2]})"))

  # Set the column and row names of the matrix to the list of names.
  colnames(M) <- rownames(M) <- nm

  # If both posets are Lattice objects, then create a new Lattice object.
  if (inherits(P1, "Lattice") && inherits(P2, "Lattice")) {

    P <- Lattice$new(M)

  # Otherwise, create a new Poset object.
  } else {

    P <- Poset$new(M)

  }

  # Return the product poset.
  return(P)

}
