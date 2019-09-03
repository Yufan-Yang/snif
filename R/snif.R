
# This parses the input formula of SNIF to do some complicated stuff.
# Understanding tihs code probably requires a good graph of programming
# langauge theory, R, and the tidyverse. This is also poorly written
parse_formula <- function(f, degree)
{
    # in the function 'bs', "x" is the variable that get expanded, and we must
    # extract it so we can properly account for it in SNIF.
    get_x <- function(e)
    {
       i <- 1 + match(fn_fmls_names(bs)[1], call_args_names(e))
       ifelse(is.na(i), e[[2]], e[[i]])
    }

    parse.rec <- function(list, e)
    {
        if (length(e) == 0) {
          # NULL formula
        }
        # If the length of the expression is 1, it is a (linear) variable
        else if (length(e) == 1)
          list$lin <- c(list$lin, e)

        else if (e[[1]] == expr(`+`))
          list <- parse.rec(parse.rec(list, e[[2]]), e[[3]])

        else if (e[[1]] == expr(`:`)) {
          l0 <- parse.rec(list(lin = c(), nl = c(), int = c()), e[[2]])
          l  <- parse.rec(l0, e[[3]])

          n.lin <- length(l$lin)
          n.nl  <- length(l$nl)
          if (n.lin == 2)
            list$int <- c(list$int, exprs(!!l$lin[[1]]:!!l$lin[[2]],
              !!l$lin[[2]]:!!l$lin[[1]]))

          if (n.nl == 2)
              list$int <- c(list$int, exprs(!!l$nl[[1]]:!!l$nl[[2]],
                !!l$nl[[2]]:!!l$nl[[1]]))

          if (n.lin == 2 && n.nl == 2) {
            list$int <- c(list$int, exprs(
              !!l$lin[[1]]:!!l$nl[[2]],  !!l$nl[[2]]:!!l$lin[[1]],
              !!l$lin[[2]]:!!l$nl[[1]],  !!l$nl[[1]]:!!l$lin[[2]]
            ))
          }
          if (n.lin == 2 && n.nl == 1) {
            if (is.null(l0$nl))
              list$int <- c(list$int, exprs(!!l$nl[[1]]:!!l$lin[[1]],
                !!l$lin[[1]]:!!l$nl[[1]]))
            else
              list$int <- c(list$int, exprs(!!l$nl[[1]]:!!l$lin[[2]],
                !!l$lin[[2]]:!!l$nl[[1]]))
          }
          if (n.lin == 1 && n.nl == 2) {
              if (is.null(l0$lin))
                list$int <- c(list$int, exprs(!!l$nl[[1]]:!!l$lin[[1]],
                  !!l$lin[[1]]:!!l$nl[[1]]))
              else
                list$int <- c(list$int, exprs(!!l$nl[[2]]:!!l$lin[[1]],
                  !!l$lin[[1]]:!!l$nl[[2]]))
          }
          if (n.lin == 1 && n.nl == 1) {
            list$int <- c(list$int, exprs(!!l$lin[[1]]:!!l$nl[[1]],
              !!l$nl[[1]]:!!l$lin[[1]]))
          }
        }

        else if (e[[1]] == expr(`[`)) {
          if (e[[2]][[1]] != expr(bs))
            stop(paste("Parsing failure: Unsupported term", expr_text(e),
              "in formula."))

          if (length(e) < 4 || e[[4]] != -1)
            stop(paste("Parsing failure: Unsupported subset of basis spline",
              expr_text(e), "in formula. Only [,-1] is allowed."))

          list$nl <- c(list$nl, expr(bs(!!sym(get_x(e[[2]])),
            degree = !!degree)[, -1]))
        }

        else if (e[[1]] == expr(bs)) {
            list$lin <- c(list$lin, get_x(e))
            list$nl  <- c(list$nl, expr(bs(!!sym(get_x(e)),
              degree = !!degree)[, -1]))
        }

        else
          stop(paste("Parsing failure: Unrecognized term ", expr_text(e),
            "in formula."))

      list
    }
    parse.rec(list(lin = c(), nl = c(), int = c()), f_rhs(f))
}

#' @references
#' @author Alexander
#' @export
snif <- function(formula, df, score = "BIC", degree = 3, maxit = ncol(df),
                     main.only = NULL, linear.only = NULL)
{
    if (!is_formula(formula))
        stop("'formula' must be a valid formula.")

    score <- match.arg(score, c("BIC","AIC"))
    score <- switch(score,
        "BIC" = function(x) BIC(x),
        "AIC" = function(x) AIC(x)
    )

    response <- deparse(f_lhs(formula))
    parsed   <- parse_formula(formula, degree)

    print(parsed$int, degree)
    lin.sel <- parsed$lin
    nl.sel  <- parsed$nl
    int.sel <- parsed$int

    vars <- setdiff(names(df), response)

    lin.cand <- setdiff(purrr::map(vars, ~ sym(expr(!!.x))), lin.sel)
    nl.vars  <- setdiff(vars, linear.only)
    nl.cand  <- setdiff(purrr::map(nl.vars, ~ expr(
        bs(!!sym(.x), degree = !!degree)[, -1])), nl.sel)

    int.cand <- NULL

    score.candidate <- function(candidate)
    {
        f <- expr(!!f_lhs(f) ~ !!f_rhs(f) + !!enexpr(candidate))
        score(stats::lm(f, df))
    }

    add.int <- function(int.cand, sel)
    {

        int <- expr(!!sel:!!term)
        if (all.vars(sel) %in% main.only || all.vars(term) %in% main.only)
            int.cand
        if (expr_text(int) %in% map_chr(int.sel, expr_text))
          int.cand
        else
            c(int.cand, expr(!!sel:!!term))
    }

    f <- formula
    output <- list(formula = f, score = score(lm(f, df)), it = 0)
    for (it in 1:maxit) {
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
