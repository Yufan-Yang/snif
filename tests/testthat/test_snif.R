context("Test SNIF")
library(SNIF)
library(rlang)


test_that("make_interaction orders interactions correctly", {
  make_interaction <- SNIF:::make_interaction

  expect_equal(expr(X1:X2), make_interaction(expr(X1), expr(X2)))
  expect_equal(expr(X1:X2), make_interaction(expr(X2), expr(X1)))


  expect_equal(expr(bs(X):C), make_interaction(expr(C), expr(bs(X))))
  expect_equal(expr(bs(C):bs(X)), make_interaction(expr(bs(C)), expr(bs(X))))

  expect_equal(expr(bs(X):bs(X)[,-1]),
      make_interaction(expr(bs(X)[,-1]), expr(bs(X)))
  )


})



test_that("parse_formula initialises initial selected sets correctly", {
  parse_formula <- SNIF:::parse_formula

  out <- parse_formula(y ~ V2 + V1, 3)

  expect_true(setequal(list(expr(V1), expr(V2)), out$lin))
  expect_true(setequal(NULL, out$nl))
  expect_true(setequal(NULL, out$int))


  out <- parse_formula(y ~ V2 + bs(V1), 3)

  expect_true(setequal(list(expr(V1), expr(V2)), out$lin))
  expect_true(setequal(list(expr(bs(V1, degree = 3)[,-1])), out$nl))
  expect_true(setequal(NULL, out$int))


  out <- parse_formula(y ~ V3:V2 + bs(V1), 3)

  expect_true(setequal(list(expr(V1)), out$lin))
  expect_true(setequal(list(expr(bs(V1, degree = 3)[,-1])), out$nl))
  expect_true(setequal(list(expr(V2:V3)), out$int))


  out <- parse_formula(y ~ V1:bs(V2), 3)

  expect_true(setequal(list(expr(V1:V2), expr(bs(V2, degree = 3)[,-1]:V1)),
    out$int)
  )

  out <- parse_formula(y ~ V1:bs(V2)[,-1], 3)
  expect_true(setequal(list(expr(bs(V2, degree = 3)[,-1]:V1)), out$int))


  out <- parse_formula(y ~ bs(V1):bs(V2)[,-1], 3)

  expect_true(setequal(
    list(expr(bs(V1, degree = 3)[,-1]:bs(V2, degree = 3)[,-1]),
      expr(bs(V2, degree = 3)[,-1]:V1)),
    out$int))


    out <- parse_formula(y ~ bs(V1):bs(V2), 3)

    expect_true(
      setequal(
        list(expr(V1:V2), expr(bs(V1, degree = 3)[,-1]:bs(V2, degree = 3)[,-1]),
          expr(bs(V2, degree = 3)[,-1]:V1), expr(bs(V1, degree = 3)[,-1]:V2)),
        out$int)
    )

})
