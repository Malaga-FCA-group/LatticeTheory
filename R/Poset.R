#' @title
#' R6 class for a poset
#'
#' @description
#' This class implements the data structure and methods for partial ordered sets.
#' @export
Poset <- R6::R6Class(

  classname = "Poset",

  public = list(

    #' @field names Names of the elements.
    names = NULL,
    #' @field order Matrix representing the dominance.
    order = NULL,
    #' @field reduced_matrix Matrix representing dominance, once transitivity is taken into account.
    reduced_matrix = NULL,

    #' @description Initialize a poset
    #'
    #' @param order Matrix indicating the order relationship
    #' @param names Character vector with names of the elements
    #'
    #' @return An object of class Poset
    #' @export
    #'
    initialize = function(order, names = colnames(order)) {

      if (is.null(names)) {

        if (ncol(order) < 25) {

          names <- letters[seq(ncol(order))]

        } else {

          names <- paste0("v_", seq(ncol(order)))
        }

      }
      names <- as.character(names)
      colnames(order) <- rownames(order) <- names
      self$names <- names
      self$order <- order
      private$poset <- POSetR::poset_from_incidence(t(order))
      self$reduced_matrix <- fcaR:::.reduce_transitivity(self$order)

    },

    #' @description Print a Poset
    #'
    #' @export
    #'
    print = function() {

      glue::glue(
        "Poset with {length(self$names)} elements."
      ) |> cat()

      cat("\n")
      print(private$poset)

    },

    #' @description
    #' Size of poset
    #'
    #' @return
    #' The number of elements in the poset
    #'
    #' @export
    size = function() {

      return(length(self$names))

    },

    #' @description
    #' Maximals of elements
    #'
    #' @param ... element names (as vector or enumerated).
    #'
    #' @return
    #' The maximals of the specified elements
    #'
    #' @export
    maximals = function(...) {

      B <- list(...) |> unlist()

      if (length(B) == 0) B <- self$names

      if (length(B) == 1) return(B)

      B[Matrix::which(Matrix::colSums(self$order[B, B]) == 1)]

    },

    #' @description
    #' Maximum of elements
    #'
    #' @param ... element names (as vector or enumerated).
    #'
    #' @return
    #' The maximum of the specified elements, if it exists; \code{NULL} if it does not exist.
    #'
    #' @export
    maximum = function(...) {

      B <- list(...) |> unlist()

      if (length(B) == 0) B <- self$names

      maximals <- self$maximals(B)

      if (length(maximals) == 1) return(maximals)

      return(NULL)

    },

    #' @description
    #' Minimals of elements
    #'
    #' @param ... element names (as vector or enumerated).
    #'
    #' @return
    #' The minimals of the specified elements.
    #'
    #' @export
    minimals = function(...) {

      B <- list(...) |> unlist()

      if (length(B) == 0) B <- self$names

      if (length(B) == 1) return(B)

      B[Matrix::which(Matrix::rowSums(self$order[B, B]) == 1)]

    },

    #' @description
    #' Minimum of elements
    #'
    #' @param ... element names (as vector or enumerated).
    #'
    #' @return
    #' The minimum of the specified elements, if it exists; \code{NULL} if it does not exist.
    #'
    #' @export
    minimum = function(...) {

      B <- list(...) |> unlist()

      if (length(B) == 0) B <- self$names

      minimals <- self$minimals(B)

      if (length(minimals) == 1) return(minimals)

      return(NULL)

    },

    #' @description
    #' Supremum of elements
    #'
    #' @param ... Element names.
    #'
    #' @return
    #' The supremum of the list of elements (if it exists); \code{NULL}, otherwise.
    #'
    #' @export
    supremum = function(...) {

      B <- list(...) |> unlist()

      if (length(B) == 0) B <- self$names

      self$minimum(self$upper_bounds(B))

    },

    #' @description
    #' Infimum of elements
    #'
    #' @param ... Element names.
    #'
    #' @return
    #' The infimum of the list of elements (if it exists); \code{NULL}, otherwise.
    #'
    #' @export
    infimum = function(...) {

      B <- list(...) |> unlist()

      if (length(B) == 0) B <- self$names

      self$maximum(self$lower_bounds(B))

    },

    #' @description
    #' Upper bound of elements
    #'
    #' @param ... Element names.
    #'
    #' @return
    #' The set of upper bounds of the list of elements.
    #'
    #' @export
    upper_bounds = function(...) {

      B <- list(...) |> unlist()

      if (length(B) == 0) return(NULL)

      up <- self$names
      if (length(B) > 0) {

        for (b in B) {

          up <- intersect(self$upper_cone(b),
                          up)

        }

        return(up)

      }

    },

    #' @description
    #' Lower bound of elements
    #'
    #' @param ... Element names.
    #'
    #' @return
    #' The set of lower bounds of the list of elements.
    #'
    #' @export
    lower_bounds = function(...) {

      B <- list(...) |> unlist()

      if (length(B) == 0) return(NULL)

      down <- self$names
      if (length(B) > 0) {

        for (b in B) {

          down <- intersect(self$lower_cone(b),
                            down)

        }

        return(down)

      }

    },

    #' @description
    #' Lower cone of an element
    #'
    #' @param id The name of an element.
    #'
    #' @return
    #' A vector with the elements of the lower cone (that is, its lower bounds).
    #'
    #' @export
    lower_cone = function(id) {

      # Get the index of all sub-elements
      M <- self$order[id, ]
      candidates <- Matrix::which(M > 0)

      self$names[candidates]

    },

    #' @description
    #' Upper cone of an element
    #'
    #' @param id The name of an element.
    #'
    #' @return
    #' A vector with the elements of the upper cone (that is, its upper bounds).
    #'
    #' @export
    upper_cone = function(id) {

      # Get the index of all super-elements
      M <- Matrix::t(self$order)[id, ]
      candidates <- Matrix::which(M > 0)

      self$names[candidates]

    },

    #' @description
    #' Lower Neighbours of an element
    #'
    #' @param id The name of an element
    #'
    #' @return
    #' A list with the lower neighbours of element \code{id}.
    #'
    #' @export
    lower_neighbours = function(id) {

      self$names[which(self$reduced_matrix[id, ] > 0)]

    },

    #' @description
    #' Upper Neighbours of an element
    #'
    #' @param id The name of an element
    #'
    #' @return
    #' A list with the upper neighbours of element \code{id}.
    #'
    #' @export
    upper_neighbours = function(id) {

      self$names[which(self$reduced_matrix[, id] > 0)]

    },

    #' @description
    #' Order of two elements
    #'
    #' @param a The name of an element
    #' @param b The name of the other element
    #'
    #' @return
    #' \code{TRUE} if \code{a} is lower than or equal to \code{b}; \code{FALSE}, otherwise.
    #'
    #' @export
    is_lower_than = function(a, b) {

      self$order[b, a]

    },

    #' @description
    #' Linear extension of poset
    #'
    #' @return
    #' Another poset which is a linear extension of this.
    #'
    #' @export
    linear_extension = function() {

      order <- private$poset$pointer$firstLE()

      L <- chain(order)

      return(L)

    },

    #' @description
    #' Hasse diagram of Poset
    #'
    #' @param engine Which engine to use for the plot?
    #' @details
        #' Available engines are \code{own} (uses ggplot2), \code{hasseDiagram} (uses the hasseDiagram package, which should be installed); and \code{POSetR}, which uses the POSetR package.
        #'
    #' @export
    plot = function(engine = c("own", "hasseDiagram",
                               "POSetR")) {

      engine <- match.arg(engine)

      if (engine == "own") {

        M <- as.matrix(self$reduced_matrix) |> t()
        y <- parsec::incidence2cover.incidence(M)

        vertices <- -parsec::vertices(y)
        g2 <- igraph::graph_from_adjacency_matrix(
          self$reduced_matrix)
        g2$layout <- as.matrix(vertices)

        pl <- graph_plot(g2, node_label = name)
        # g2 <- g2 |>
        #   tidygraph::as_tbl_graph()
        # tidygraph::.register_graph_context(g2)
        #
        # label_col <- rlang::quo_text(igraph::V(g2)$name)
        # # label_col <- paste0(c("node1.", "node2."), label_col)
        # start_label <- sym(label_col[1])
        # end_label <- sym(label_col[2])
        #
        # pl <- g2 |>
        #   ggraph::ggraph(layout = vertices) +
        #   geom_edge_link(aes(
        #     # colour = !!edge_colour,
        #     # label = !!edge_label,
        #     # width = !!edge_width,
        #     start_cap = label_rect(!!start_label),
        #     end_cap = label_rect(!!end_label)),
        #     angle_calc = "along",
        #     label_dodge = unit(2.5, "mm")) +
        #   # ggraph::geom_node_point() +
        #   ggraph::geom_node_text(aes(label = name))
        #
        #
        #           ggraph::create_layout(vertices)

        # pl <- g2 |>
        #   ggraph::autograph(node_label = name) +
        #   ggnetwork::theme_blank()

        print(pl)

      }

      if (engine == "POSetR") {

        POSetR::plot.poset(private$poset)

      }

      if (engine == "hasseDiagram") {

        M <- matrix(FALSE,
                    ncol = ncol(self$order),
                    nrow = nrow(self$order))
        M[self$order > 0] <- TRUE
        colnames(M) <- rownames(M) <- self$names
        hasseDiagram::hasse(M, parameters = list(arrow = "backward"))

      }

      return(invisible(NULL))

    },

    #' @description
    #' Hasse diagram of Poset
    #'
    #' @param ... Other arguments.
    #'
    #' @return The LaTeX code to reproduce the Hasse diagram of the poset.
    #'
    #' @export
    latex = function(...) {

      dots <- list(...)
      if (!("tags" %in% names(dots))) {

        dots$tags <- self$names

      }
      dots$L <- self
      if ("scale" %in% names(dots)) {

        if (length(dots$scale) == 1) {

          dots$scale <- c(dots$scale,
                          dots$scale)

        }

        if (length(dots$scale) > 2) {

          dots$scale <- dots$scale[1:2]

        }

      }

      do.call(tikz_lattice, dots)

      # tikz_lattice(self, ..., tags = self$names)

    },

    #' @description
    #' Poset as graph
    #'
    #' @return An \code{igraph} object representing this poset.
    #'
    #' @export
    as_graph = function() {

      igraph::graph_from_adjacency_matrix(Matrix::t(self$order))

    }

  ),

  private = list(

    poset = NULL

  )


)

