#' @export
`%*%` <- function(P1, P2) {

  M1 <- P1$order
  M2 <- P2$order

  nm1 <- P1$names
  nm2 <- P2$names

  nm <- expand.grid(nm2, nm1)[, 2:1]
  M <- matrix(0, ncol = nrow(nm), nrow = nrow(nm))
  for (ii in seq(nrow(nm))) {

    for (jj in seq(nrow(nm))) {

      v1 <- nm[ii, 1]
      v2 <- nm[ii, 2]

      w1 <- nm[jj, 1]
      w2 <- nm[jj, 2]
      M[ii, jj] <- M1[v1, w1] & M2[v2, w2]

    }

  }

  nm <- apply(nm, 1, \(x) glue::glue("({x[1]}, {x[2]})"))

  colnames(M) <- rownames(M) <- nm

  if (inherits(P1, "Lattice") && inherits(P2, "Lattice")) {

    P <- Lattice$new(M)

  } else {

    P <- Poset$new(M)

  }

  return(P)

}
