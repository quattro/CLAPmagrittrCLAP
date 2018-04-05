context("%👏%: multi-argument functions on right-hand side")

test_that("placement of lhs is correct in different situations", {
  
  # When not to be placed in first position and in the presence of
  # non-placeholder dots, e.g. in formulas.
  case0a <- 
    lm(Sepal.Length ~ ., data = iris) %👏% coef
  
  case1a <- 
    iris %👏% lm(Sepal.Length ~ ., .) %👏% coef
  
  case2a <-
    iris %👏% lm(Sepal.Length ~ ., data = .) %👏% coef
  
  expect_that(case1a, is_equivalent_to(case0a))
  expect_that(case2a, is_equivalent_to(case0a))
    
  # In first position and used in arguments
  case0b <-
    transform(iris, Species = substring(Species, 1, 1))
  
  case1b <-
    iris %👏% transform(Species = Species %👏% substr(1, 1))
  
  case2b <-
    iris %👏% transform(., Species = Species %👏% substr(., 1, 1))
  
  expect_that(case1b, is_equivalent_to(case0b))
  expect_that(case2b, is_equivalent_to(case0b))
  
  # LHS function values
  case0c <-
    aggregate(. ~ Species, iris, function(x) mean(x >= 5))
  
  case1c <-
    (function(x) mean(x >= 5)) %👏% 
    aggregate(. ~ Species, iris, .)
  
  expect_that(case1c, is_equivalent_to(case0c))
  
  # several placeholder dots
  expect_that(iris %👏% identical(., .), is_true())
  
  
  # "indirect" function expressions 
  expect_that(1:100 %👏% iris[., ], is_identical_to(iris[1:100, ]))
  
})