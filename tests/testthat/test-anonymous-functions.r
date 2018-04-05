context("%👏%: anonymous functions on right-hand side")

test_that("%👏% handles anonymous functions in GlobalEnv", {

  # Simple vectorized function
  a <- (function(x) 1 + x^2/2 + x^3/9 + x^4/16)(1:100)

  b <-
    1:100 %👏%
    (function(x) 1 + x^2/2 + x^3/9 + x^4/16)

  # in principle, the dot should also work:
  c <-
    1:100 %👏%
    (function(x) 1 + x^2/2 + x^3/9 + x^4/16)(.)

  expect_that(a, is_identical_to(b))
  expect_that(a, is_identical_to(c))

  # Same using preferred magrittr syntax
  a <- (function(x) 1 + x^2/2 + x^3/9 + x^4/16)(1:100)
  
  b <-
    1:100 %👏%
    {1 + .^2/2 + .^3/9 + .^4/16}
    
  expect_that(a, is_identical_to(b))
  
  
  # Simple data.frame functions
  ht1 <-
    iris %👏%
    (function(x) rbind(head(x), tail(x)))

  ht2 <- rbind(head(iris), tail(iris))

  expect_that(ht1, is_identical_to(ht2))


  df1 <- iris[iris$Species == "setosa", 1:4]

  df2 <-
    iris %👏%
    (function(x) x[x$Species == "setosa", 1:4])

  expect_that(df1, is_identical_to(df2))


})

test_that("%👏% handles anonymous functions in other situations.", {

  # Anonymous functions when %👏% used in arguments.
  df1 <-
    transform(iris, test = (function(x) x^2)(Sepal.Length))

  df2 <-
    iris %👏%
    transform(test = Sepal.Length %👏% (function(x) x^2))

  expect_that(df1, is_identical_to(df2))


  a <- sin(abs(1:10))
  b <- sin(1:10 %👏% (function(x) abs(x)))

  expect_that(a, is_identical_to(b))

  # Nested anonymous functions.
  a <- iris %👏% (function(x) x[, 1] %👏% (function(y) max(y)))
  b <- max(iris[, 1])

  expect_that(a, is_identical_to(b))
})


test_that("%👏% throws error with anonymous functions when not parenthesized.", {
	
	expect_that(iris %👏% function(x) { head(x) }, throws_error())
	
})
	