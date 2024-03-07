#' Binary Relation
#' @export
BinaryRelation <- R6::R6Class(

  classname = "BinaryRelation",

  public = list(

    #' @description Constructor of a Binary Relation
    #'
    #' @param matrix    A squared matrix indicating the binary relation between elements.
    #' @param elements A vector with element names.
    #' @param file     If provided, a file to load.
    #'
    #' @return The BinaryRelation object.
    #' @export
    initialize = function(matrix = NULL,
                          elements = NULL,
                          file = NULL) {

      if (!is.null(elements)) {

        private$elements <- elements

      }

      if (!is.null(matrix)) {

        # TODO: Check that the matrix is squared and
        # its elements are in [0, 1]. Maybe an auxiliary
        # function "check_matrix()"?
        private$matrix <- Matrix::Matrix(matrix, sparse = TRUE)

        # Note that if `matrix` has column names, they will
        # overwrite the value of the `elements` argument.
        if (!is.null(colnames(matrix))) {

          private$elements <- colnames(matrix)

        }

      }

    },

    #' @description Prints the BinaryRelation
    #'
    #' @export
    print = function() {

      cat("BinaryRelation: ")

    }

  ),

  private = list(

    matrix = NULL,
    elements = NULL

  )

)
