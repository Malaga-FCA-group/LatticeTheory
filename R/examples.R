#' Lattice of divisors
#'
#' @param n The number in the top of the lattice
#'
#' @return A Lattice object
#' @export
#'
#' @examples
#' P <- divisors_lattice(60)
divisors_lattice <- function(n) {


  factorization <- sfsmisc::factorize(n)[[1]]

  factors <- rep(factorization[, "p"],
                 times = factorization[, "m"])

  L <- length(factors)

  divisores <- c(1)

  for (k in seq(L)) {

    divisores <- c(divisores,
                   sort(unique(apply(combn(factors, m = k),
                                     2, prod))))

  }

  divisores <- sort(unique(divisores))

  num_div <- length(divisores)

  M <- matrix(FALSE,
              nrow = num_div,
              ncol = num_div)

  precedes <- function(i, j) {

    j %% i == 0

  }

  for (i in seq(num_div)) {

    for (j in seq(num_div)) {

      M[i, j] <- precedes(
        divisores[j],
        divisores[i])

    }

  }

  colnames(M) <- rownames(M) <- as.character(divisores)

  Lattice$new(M)

}

#' Chain
#'
#' @param ... The number of elements (single input) or the list of elements
#'
#' @return A chain lattice with the given configuration.
#' @export
#'
#' @examples
#' C1 <- chain(7)
#' C2 <- chain("a", "b", "c")
chain <- function(...) {

  arg <- list(...) |> unlist()

  if (length(arg) == 1 && is.numeric(arg)) {

    I <- POSetR::chain(letters[seq(arg)]) |>
      POSetR::incidenceMatrix() |>
      t()

  } else {

    I <- POSetR::chain(arg) |>
      POSetR::incidenceMatrix() |>
      t()

  }

  Lattice$new(order = I)

}

#' Antichain
#'
#' @param n Number of incomparable elements
#'
#' @return An antichain.
#' @export
#'
#' @examples
#' A <- antichain(3)
antichain <- function(n) {

  I <- matrix(0, ncol = n + 2, nrow = n + 2)
  I[, 1] <- TRUE
  I[n + 2, ] <- TRUE
  diag(I) <- TRUE
  colnames(I) <- rownames(I) <- letters[seq(n + 2)]

  Lattice$new(order = I)

}

#' Diamond Lattice
#'
#' @return A Lattice object representing the diamond lattice.
#' @export
#'
#' @examples
#' D <- diamond()
diamond <- function() {

  antichain(3)

}

#' Pentagon Lattice
#'
#' @return A Lattice object representing the pentagon lattice
#' @export
#'
#' @examples
#' P <- pentagon()
pentagon <- function() {

  M <- matrix(0, ncol = 5, nrow = 5)
  colnames(M) <- rownames(M) <- letters[1:5]

  M["a", ] <- 1
  M["b", c("b", "c", "e")] <- 1
  M["c", c("c", "e")] <- 1
  M["d", c("d", "e")] <- 1
  M["e", "e"] <- 1

  Lattice$new(t(M))

}

#' Boole Algebra
#'
#' @param n Number of atoms in the Boole algebra
#'
#' @return A Lattice object representing the Boole algebra with n atoms.
#' @export
#'
#' @examples
#' B <- boole(2)
boole <- function(n) {

  prims <- primes::generate_n_primes(n)
  L <- divisors_lattice(prod(prims))

  Lattice$new(order = unname(L$order))

}


is_lattice <- function(P,
                       verbose = FALSE) {

  stopifnot(inherits(P, "Poset"))

  for (x in P$names) {

    for (y in P$names) {

      if (x == y) next

      S <- P$supremum(c(x, y))
      if (length(S) != 1) {
        if (verbose) {

          glue::glue(
            "sup({x}, {y}) = {stringr::str_flatten(S, ', ')}"
          ) |> cat()
          cat("\n")

        }

        return(FALSE)
      }

      I <- P$infimum(c(x, y))
      if (length(I) != 1) {

        if (verbose) {

          glue::glue(
            "inf({x}, {y}) = {stringr::str_flatten(I, ', ')}"
          ) |> cat()
          cat("\n")

        }

        return(FALSE)
      }

    }

  }

  return(TRUE)

}
