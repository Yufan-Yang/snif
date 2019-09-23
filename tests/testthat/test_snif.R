context("Test snif")

test_that("snif.init throws errors correctly", {
    expect_error(snif("y ~ V2", snif.df))
    expect_error(snif(y ~ V2, df))
    expect_error(snif(y ~ V2, snif.df, type = "gam"))
    expect_error(snif(y ~ V2, snif.df, degree = 1))
    expect_error(snif(y ~ V2, snif.df, maxnv = -1))
    expect_error(snif(y ~ V2, snif.df, main.only = 1:5))
    expect_error(snif(y ~ V2, snif.df, linear.only = 1:5))
    expect_error(snif(y ~ V2, snif.df, main.only = "x"))
    expect_error(snif(y ~ V2, snif.df, linear.only = "x"))

    init <- snif.init(y ~ NULL, snif.df, "linear", "BIC", 3, 1, NULL, NULL)
    expect_equal(sum(sapply(init$sel, is.null)), 3)

    init <- snif.init(y ~ V2 + V3, snif.df, "linear", "BIC", 3, 1, NULL, NULL)

    expect_equal(init$sel$lin, list(expr(V2), expr(V3)))
    expect_equal(init$can$int, list(make_interaction(expr(V2), expr(V3))))

    expect_false(any(expr(V2) == init$can$lin))
    expect_false(any(expr(V3) == init$can$lin))

    init <- snif.init(y ~ V2 + V3, snif.df, "linear", "BIC", 3, 1, "V2", NULL)
    expect_equal(init$sel$lin, list(expr(V2), expr(V3)))
    expect_equal(init$can$int, list())

    init <- snif.init(y ~ V2:V3, snif.df, "linear", "BIC", 3, 1, NULL, NULL)
    expect_true(is.null(init$sel$lin))
    expect_true(is.null(init$sel$nl))
    expect_equal(init$sel$int, list(make_interaction(expr(V2), expr(V3))))
    expect_equal(init$can$int, list())

    init <- snif.init(y ~ bs(V2), snif.df, "linear", "BIC", 3, 1, NULL, NULL)
    expect_equal(init$sel$lin, list(expr(V2)))
    expect_equal(init$sel$nl, list(expr(bs(V2, degree = 3)[,-1])))
    expect_equal(init$can$int, list())

    init <- snif.init(y ~ bs(V2):V3, snif.df, "linear", "BIC", 3, 1, NULL, NULL)
    expect_equal(init$sel$int, list(
        make_interaction(expr(V2), expr(V3)),
        make_interaction(expr(bs(V2, degree = 3)[,-1]), expr(V3))
    ))

    init <- snif.init(y ~ bs(V2):V3, snif.df, "linear", "BIC", 3, 1, NULL,
        names(snif.df))
    expect_equal(init$can$nl, list())
})



test_that("snif p.value scoring works", {
    init <- snif.init(y ~ NULL, snif.df, "linear", "PV", 3, 1, NULL, NULL)

    model <- init$model
    score.model <- init$score.model
    m0 <- model(y ~ NULL, snif.df)
    m1 <- model(y ~ V2, snif.df)

    score.model(m1, m0)
    expect_true(score.model(m1, m0) < 0.05)
    expect_true(score.model(m1) < score.model(m0))

    m0 <- model(y ~ V2, snif.df)
    m1 <- model(y ~ V2 + bs(V2)[, -1], snif.df)

    score.model(m1, m0)
    expect_true(score.model(m1, m0) < 0.05)
    expect_true(score.model(m1) < score.model(m0))

})
