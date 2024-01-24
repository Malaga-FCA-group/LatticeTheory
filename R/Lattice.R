#' @title
#' R6 class for a lattice
#'
#' @description
#' This class implements the data structure and methods for lattices.
#' @export
Lattice <- R6::R6Class(

  classname = "Lattice",

  inherit = Poset,

  public = list(

    #' @description Print a Lattice
    #'
    #' @export
    #'
    print = function() {

      glue::glue(
        "Lattice with {length(self$names)} elements."
      ) |> cat()

      cat("\n")

      print(private$poset)

    },

    #' @description Top of a Lattice
    #'
    #' @return The top of the Concept Lattice
    #' @export
    #'
    top = function() {

      super$maximum()
      # self$names[which(Matrix::colSums(t(self$order)) == self$size())]

    },

    #' @description Bottom of a Lattice
    #'
    #' @return The bottom of the Concept Lattice
    #' @export
    #'
    bottom = function() {

      super$minimum()
      # self$names[which(Matrix::colSums(t(self$order)) == 1)]

    },

    # maximals = function(B) {
    #
    #   B[Matrix::which(Matrix::colSums(self$order[B, B]) == 1)]
    #
    # },
    #
    # minimals = function(B) {
    #
    #   B[Matrix::which(Matrix::rowSums(self$order[B, B]) == 1)]
    #
    # },

    #' @description
    #' Join-irreducible Elements
    #'
    #' @return
    #' The join-irreducible elements in the lattice.
    #'
    #' @export
    #'
    join_irreducibles = function() {

      self$names[which(Matrix::colSums(Matrix::t(self$reduced_matrix)) == 1)]

    },

    #' @description
    #' Meet-irreducible Elements
    #'
    #' @return
    #' The meet-irreducible elements in the lattice.
    #'
    #' @export
    meet_irreducibles = function() {

      M <- .reduce_transitivity(self$order)

      self$names[which(Matrix::colSums(M) == 1)]

    },

    #' @description
        #' Atoms of a lattice
    #' @return The atoms of the lattice.
    #' @export
    atoms = function() {

      self$upper_neighbours(self$bottom())

    },

    #' @description
    #' Coatoms of a lattice
    #' @return The coatoms (superatoms) of the lattice.
    #' @export
    coatoms = function() {

      self$lower_neighbours(self$top())

    },

    #' @description
        #' Complement(s) of a lattice element
    #' @param a Name of an element
    #' @return The name of the complement elements, if any; \code{NULL}, otherwise.
    #' @export
    complements = function(a) {

      idx <- Matrix::which(self$order[, a] > 0)

      id_top <- Matrix::which(Matrix::colSums(self$order[idx, ]) == 1)

      idx <- Matrix::which(self$order[a, ] > 0)

      id_bottom <- Matrix::which(Matrix::rowSums(self$order[, idx]) == 1)

      self$names[intersect(id_top,
                id_bottom)]

    },

    #' @description
        #' Interval in a lattice
    #' @param a First (lower) element
    #' @param b Secod (greater) element
    #' @return The elements which are greater than or equal to \code{a} and lower than or equal to \code{b}.
    #' @export
    interval = function(a, b) {

      intersect(self$upper_cone(a), self$lower_cone(b))

    },

    #' @description
        #' Chains in a lattice
    #' @param a First (lower) element
    #' @param b Secod (greater) element
    #' @return The chains starting at \code{a} and ending in \code{b}.
    #' @export
    chains = function(a, b) {

      if (self$order[a, b]) {

        tmp <- b
        b <- a
        a <- tmp

      }

      Matrix::t(self$reduced_matrix) |>
        igraph::graph_from_adjacency_matrix() |>
        igraph::all_simple_paths(from = a, to = b) |>
        lapply(names)

    },

    #' @description
        #' Standard context
    #' @return A formal context (in \code{fcaR} is installed) or a matrix of a context whose associated concept lattice is (isomorphic to) the current lattice.
    #' @export
    context = function() {

      J <- self$join_irreducibles()
      M <- self$meet_irreducibles()

      I <- self$order[J, M]
      # I <- double_sort2(I)#reorder_matrix_standard2(I)
      rownames(I) <- J#glue::glue("j{seq_along(J)}")
      colnames(I) <- M#glue::glue("m{seq_along(M)}")

      if ("fcaR" %in% installed.packages()) {

        # require(fcaR)

        fc <- fcaR::FormalContext$new(I)
        return(fc)

      }

      return(I)

    },

    #' @description
        #' Dual of the lattice
    #' @return Another lattice where the order is reversed.
    #' @export
    dual = function() {

      Lattice$new(order = t(self$order))

    }

  ),

  private = list(

    poset = NULL

  )


)

