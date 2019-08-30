parse_formula <- function(f)
{
    rhs <- rlang::f_rhs(f)
    parse.rec <- function(rhs)
    {
        if (length(rhs) == 3)
            list(parse.rec(rhs[[2]]), parse.rec(rhs[[3]]))
        else if (length(rhs) == 2)
            list(rhs)
        else
            list(rhs)
    }
    rlang::squash(parse.rec(rhs))
}

snif <- function(formula, df, score = "BIC", degree = 3, maxit = ncol(df),
                     main.only = NULL, linear.only = NULL)
{
    if (!rlang::is_formula(formula))
        stop("'formula' must be a valid formula.")

    score <- match.arg(score, c("BIC","AIC"))
    score <- switch(score,
        "BIC" = function(x) BIC(x),
        "AIC" = function(x) AIC(x)
    )

    vars <- setdiff(names(df), response)

    lin.cand   <- purrr::map(vars, ~ sym(expr(!!.x)))

    int.cand <- NULL
    nl.vars  <- setdiff(vars, linear.only)
    nl.cand  <- purrr::map(nl.vars, ~ expr(
        bs(!!sym(expr(!!.x)), degree = !!degree)[, -1])
    )

    lin.sel <- NULL
    nl.sel  <- NULL
    int.sel <- NULL

    score.candidate <- function(candidate)
    {
        f <- expr(!!f_lhs(f) ~ !!f_rhs(f) + !!enexpr(candidate))
        score(stats::lm(f, df))
    }

    add.int <- function(int.cand, sel)
    {
        if (all.vars(sel) %in% main.only || all.vars(term) %in% main.only)
            int.cand
        else
            c(int.cand, expr(!!sel:!!term))
    }

    f <- formula

    output <- list(formula = f, score = score(lm(f, df)), it = 0)
    for (it in 1:maxit) {
        print(f)
        lin.scores <- c(purrr::map_dbl(lin.cand, score.candidate), Inf)
        nl.scores  <- c(purrr::map_dbl(nl.cand, score.candidate), Inf)
        int.scores <- c(purrr::map_dbl(int.cand, score.candidate), Inf)

        i <- which.min(c(min(lin.scores), min(nl.scores), min(int.scores)))

        if (i == 1) {
            i <- which.min(lin.scores)
            term <- lin.cand[[i]]
            s <- lin.scores[[i]]

            int.cand <- purrr::reduce(c(lin.sel, nl.sel), add.int,
                          .init = int.cand
            )

            lin.cand <- lin.cand[-i]
            lin.sel  <- c(term, lin.sel)
        }
        else if (i == 2) {
            i <- which.min(nl.scores)
            term <- nl.cand[[i]]
            s <- nl.scores[[i]]

            int.cand <- purrr::reduce(c(lin.sel, nl.sel), add.int,
                          .init = int.cand)

            nl.sel  <- c(term, nl.sel)
            nl.cand <- nl.cand[-i]
        }
        else {
            i <- which.min(int.scores)

            term <- int.cand[[i]]
            s <- int.scores[i]

            int.sel  <- c(term, int.sel)
            int.cand <- int.cand[-i]

            s <- int.scores[[i]]
        }

        if (is.null(f_rhs(f)))
            f <- expr(!!f_lhs(f) ~ !!term)
        else
            f <- expr(!!f_lhs(f) ~ !!f_rhs(f) + !!term)

            output$formula <- c(output$formula, f)
            output$score   <- c(output$score, s)
            output$it      <- c(output$it, it)
    }

    # print(purrr::map_chr(lin.sel, expr_text))
    # print(purrr::map_chr(nl.sel, expr_text))
    # print(purrr::map_chr(int.sel, expr_text))

    return(output)
}
