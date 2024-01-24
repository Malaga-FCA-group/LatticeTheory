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

antichain <- function(n) {

  I <- matrix(0, ncol = n + 2, nrow = n + 2)
  I[, 1] <- TRUE
  I[n + 2, ] <- TRUE
  diag(I) <- TRUE
  colnames(I) <- rownames(I) <- letters[seq(n + 2)]

  Lattice$new(order = I)

}

diamond <- function() {

  antichain(3)

}

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

boole <- function(n) {

  prims <- primes::generate_n_primes(n)
  L <- divisors_lattice(prod(prims))

  Lattice$new(order = unname(L$order))

}


is_lattice <- function(order,
                       verbose = FALSE) {

  L <- Lattice$new(order)

  for (x in L$names) {

    for (y in L$names) {

      if (x == y) next

      S <- L$supremum(c(x, y))
      if (length(S) != 1) {
        if (verbose) {

          glue::glue(
            "sup({x}, {y}) = {stringr::str_flatten(S, ', ')}"
          ) |> cat()
          cat("\n")

        }
        # cat("Supremum:\n")
        # print(x)
        # print(y)
        # print(S)
        return(FALSE)
      }

      I <- L$infimum(c(x, y))
      if (length(I) != 1) {

        if (verbose) {

          glue::glue(
            "inf({x}, {y}) = {stringr::str_flatten(I, ', ')}"
          ) |> cat()
          cat("\n")

        }

        # cat("Infimum:\n")
        # print(x)
        # print(y)
        # print(I)
        return(FALSE)
      }

    }

  }

  return(TRUE)

}
