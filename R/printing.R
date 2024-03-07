#' @importFrom glue glue
#' @importFrom stringr str_flatten_comma
print_poset <- function(A) {

  elements <- colnames(A)

  if (length(elements) == 0) return(invisible(NULL))

  glue::glue(
    "\nElements: {elements |> stringr::str_flatten_comma()}.",
    .trim = FALSE
  ) |>
    cat()
  cat("\nStrict comparabilities:\n")


  for (el in elements) {

    els <- elements[A[, el] > 0]
    els <- setdiff(els, el)

    if (length(els) > 0) {

      glue::glue(
        "{el} < {els}"
      ) |>
        stringr::str_flatten_comma() |>
        cat()
      cat("\n")

    }

  }

}
