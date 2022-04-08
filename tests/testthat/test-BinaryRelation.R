
test_that("Can create a BinaryRelation", {

  M <- matrix(c(1, 1, 0, 1), ncol = 2)
  colnames(M) <- c("a", "b")

  expect_error(br <- BinaryRelation$new(matrix = M), NA)

  expect_true(inherits(br, "BinaryRelation"))


})

