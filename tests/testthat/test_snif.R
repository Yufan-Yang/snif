context("Test snif")

test_that("make_interaction orders interactions correctly", {
  make_interaction <- snif:::make_interaction

  e <- if ("X1" < "X2")
    expr(X1:X2)
  else
    expr(X1:X2)

  expect_equal(e, make_interaction(expr(X1), expr(X2)))
  expect_equal(e, make_interaction(expr(X2), expr(X1)))


  e <- if ("bs(X)" < "D")
    expr(bs(X):D)
  else
    expr(D:bs(X))

  expect_equal(e, make_interaction(expr(D), expr(bs(X))))

  e <- if ("bs(X)" < "bs(D)")
    expr(bs(X):bs(D))
  else
    expr(bs(D):bs(X))

  expect_equal(e, make_interaction(expr(bs(D)), expr(bs(X))))

  e <- if ("bs(X)" < "bs(X)[, -1]")
    expr(bs(X):bs(X)[, -1])
  else
    expr(bs(X)[,-1]:bs(X))

  expect_equal(e, make_interaction(expr(bs(X)[,-1]), expr(bs(X))))
})

test_that("parse_formula initialises initial selected sets correctly", {
  parse_formula <- snif:::parse_formula

  out <- parse_formula(y ~ V2 + V1, 3)
  expect_true(setequal(list(expr(V1), expr(V2)), out$lin))
  expect_true(setequal(NULL, out$nl))
  expect_true(setequal(NULL, out$int))

  out <- parse_formula(y ~ V2 + bs(V1), 3)
  expect_true(setequal(list(expr(V1), expr(V2)), out$lin))
  expect_true(setequal(list(expr(bs(V1, degree = 3)[,-1])), out$nl))
  expect_true(setequal(NULL, out$int))

  e <- if ("V2" < "V3")
    expr(V2:V3)
  else
    expr(V3:V2)

  out <- parse_formula(y ~ V3:V2 + bs(V1), 3)
  expect_true(setequal(list(expr(V1)), out$lin))
  expect_true(setequal(list(expr(bs(V1, degree = 3)[,-1])), out$nl))
  expect_true(setequal(list(e), out$int))

  out <- parse_formula(y ~ V1:bs(V2), 3)

  e1 <- if("V1" < "V2")
    expr(V1:V2)
  else
    expr(V2:V1)

  e2 <- if("V1" < "bs(V2, degree =3)[,-1]")
    expr(V1:bs(V2, degree = 3)[,-1])
  else
    expr(bs(V2, degree = 3)[,-1]:V1)

  expect_true(setequal(list(e1, e2), out$int))

  e <- if ("V1" < "bs(V2, degree = 3)[,-1]")
    expr(V1:bs(V2, degree = 3)[,-1])
  else
    expr(bs(V2, degree = 3)[,-1]:V1)

  out <- parse_formula(y ~ V1:bs(V2)[,-1], 3)
  expect_true(setequal(list(e), out$int))


  e1 <- if ("bs(V1, degree = 3)[,-1]" < "bs(V2, degree = 3)[,-1]")
    expr(bs(V1, degree = 3)[,-1]:bs(V2, degree = 3)[,-1])
  else
    expr(bs(V2, degree = 3)[,-1]:bs(V1, degree = 3)[,-1])

  e2 <- if ("V1" < "bs(V2, degree = 3)[,-1]")
    expr(V1:bs(V2, degree = 3)[,-1])
  else
    expr(bs(V2, degree = 3)[,-1]:V1)

  out <- parse_formula(y ~ bs(V1):bs(V2)[,-1], 3)
  expect_true(setequal(list(e1, e2), out$int))


  if ("V1" < "V2") {
      e1 <- expr(V1:V2)
      e2 <- expr(bs(V1, degree = 3)[,-1]:bs(V2, degree = 3)[,-1])
  }
  else {
    e1 <- expr(V2:V1)
    e2 <- expr(bs(V2, degree = 3)[,-1]:bs(V1, degree = 3)[,-1])
  }

  if ("V1" < "bs(V2, degree = 3)[,-1]") {
      e3 <- expr(V1:bs(V2, degree = 3)[,-1])
      e4 <- expr(V2:bs(V1, degree = 3)[,-1])
  }
  else {
    e3 <- expr(bs(V2, degree = 3)[,-1]:V1)
    e4 <- expr(bs(V1, degree = 3)[,-1]:V2)
  }

  out <- parse_formula(y ~ bs(V1):bs(V2), 3)
  expect_true(setequal(list(e1, e2, e3, e4), out$int))
})

test_that("parse_formula catches parsing errors", {
    parse_formula <- snif:::parse_formula
    expect_error(parse_formula(y ~ log(V2), 3))
    expect_error(parse_formula(y ~ s(V2)[,-1], 3))
    expect_error(parse_formula(y ~ splines::bs(V2)[,-1], 3))
    expect_error(parse_formula(y ~ bs(V2)[, -(1:2)], 3))
    expect_error(parse_formula(y ~ V1:splines::bs(V2)[,-1], 3))

})
