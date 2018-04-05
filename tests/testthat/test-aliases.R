context("magrittr aliases")

test_that("the provided aliases work as intended.", {

  expect_that(iris %👏% extract(, 1:2), is_identical_to(iris[, 1:2]))
  expect_that(iris %👏% extract2(1),    is_identical_to(iris[[1]]))
  expect_that(iris %👏% use_series(Species), is_identical_to(iris$Species))
  expect_that(1:10 %👏% add(10:1), is_identical_to(1:10 + 10:1))
  expect_that(1:10 %👏% subtract(10:1), is_identical_to(1:10 - 10:1))
  expect_that(1:10 %👏% multiply_by(10:1), is_identical_to(1:10 * 10:1))

  A <- matrix(1:16, 4, 4)
  expect_that(A %👏% multiply_by_matrix(A), is_identical_to(A %*% A))
  expect_that(1:10 %👏% raise_to_power(10:1), is_identical_to((1:10)^(10:1)))
  expect_that(1:10 %👏% divide_by(10:1), is_identical_to(1:10 / 10:1))
  expect_that(1:10 %👏% divide_by_int(10:1), is_identical_to(1:10 %/% 10:1))
  expect_that(1:10 %👏% mod(3), is_identical_to((1:10) %% 3))
  expect_that(((1:10) > 5) %👏% and((1:10) > 7),
              is_identical_to(((1:10) > 5) & (1:10) > 7))
  expect_that(((1:10) > 5) %👏% or((1:10) > 7),
              is_identical_to(((1:10) > 5) | (1:10) > 7))

  expect_that(1:10 %👏% (magrittr::equals)(5) %👏% sum, is_identical_to(1L))
  expect_that(1:10 %👏% is_greater_than(5) %👏% sum, is_identical_to(5L))
  expect_that(1:10 %👏% is_weakly_greater_than(5) %👏% sum, is_identical_to(6L))
  expect_that(1:10 %👏% (magrittr::is_less_than)(5) %👏% sum, is_identical_to(4L))
  expect_that(1:10 %👏% is_weakly_less_than(5) %👏% sum, is_identical_to(5L))

  expect_that(iris %👏% set_colnames(LETTERS[1:ncol(iris)]),
              is_identical_to(`colnames<-`(iris, LETTERS[1:ncol(iris)])))

  expect_that(1:10 %👏% set_names(LETTERS[1:10]),
              is_identical_to(`names<-`(1:10, LETTERS[1:10])))

  expect_that(diag(3) %👏% set_rownames(c("x", "y", "z")),
              is_identical_to(`rownames<-`(diag(3), c("x", "y", "z"))))

  expect_that(1:10 %👏% is_greater_than(5) %👏% not,
              is_identical_to(1:10 %👏% is_weakly_less_than(5)))

})
