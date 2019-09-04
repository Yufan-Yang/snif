
make_interaction <- function(x, y)
{
  if (order(purrr::map_chr(c(x, y), expr_text))[1] == 1)
    expr(!!x:!!y)
  else
    expr(!!y:!!x)
}


# This parses the input formula of SNIF to extract what the linear, nonlinear
# and interaction effects are. This code is simultaneously horrible and
# complicated.
parse_formula <- function(f, degree)
{
    # in the function 'bs', "x" is the variable that gets expanded, and we must
    # extract it so we can properly account for it in SNIF.
    get_x <- function(e)
    {
       i <- 1 + match(rlang::fn_fmls_names(bs)[1], rlang::call_args_names(e))
       ifelse(is.na(i), e[[2]], e[[i]])
    }

    # sort of a recursive top down parser. processing interaction terms is ugly
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
            list$int <- c(list$int, make_interaction(l$lin[[1]], l$lin[[2]]))

          if (n.nl == 2)
            list$int <- c(list$int, make_interaction(l$nl[[1]], l$nl[[2]]))

          if (n.lin == 2 && n.nl == 2) {
            list$int <- c(list$int, make_interaction(l$lin[[1]], l$nl[[2]]),
              make_interaction(l$lin[[2]], l$nl[[1]]))
          }
          if (n.lin == 2 && n.nl == 1) {
            if (is.null(l0$nl))
              list$int <- c(list$int, make_interaction(l$nl[[1]], l$lin[[1]]))
            else
              list$int <- c(list$int, make_interaction(l$nl[[1]], l$lin[[2]]))
          }
          if (n.lin == 1 && n.nl == 2) {
            if (is.null(l0$lin))
              list$int <- c(list$int, make_interaction(l$nl[[1]], l$lin[[1]]))
            else
              list$int <- c(list$int, make_interaction(l$nl[[2]], l$lin[[1]]))
          }
          if (n.lin == 1 && n.nl == 1)
            list$int <- c(list$int, make_interaction(l$lin[[1]], l$nl[[1]]))
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
#' Selection of nonlinear interactions by a forward stepwise algorithm
#'
#' \code{snif} is a greedy forward stepwise selection algorithm that takes
#' nonlinear effects and interactions into account when adding variables.
#' \code{snif} follows the strong hereditary principle (for X1:X2 to be
#' included as an interaction, both X1 and X2 both must have had main effects
#' selected).
#' @param formula An initial formula for \code{snif} to start with.
#'   \code{formula} supports linear variables, basis spline expansions via
#'   \code{bs}, and interaction terms via \code{:}. It is highly recomended to
#'   read the documentation before trying anything fancy with \code{formula}
#' @param df A data.frame containing the data
#' @param score A character argument that specifies the kind of scoring method
#'   used to determine which variable to add to the model. supported options
#'   are "BIC" (default), "AIC", and "PV" (p.value)
#' @param degree Degree of basis spline expansion for nonlinear effects
#' @param maxit Number of iterations to run. Default is \code{ncol(df)}
#' @param main.only Character vector of variables that are only considered for
#'   main effects
#' @param linear.only Character vector of variables that are only considered to
#'   have only linear effects
#' @references
#'  Narisetty, Naveen N. and Mukherjee, Bhramar and Chen, Yin-Hsiu and Gonzalez,
#'  Richard and Meeker, John D. Selection of nonlinear interactions by a
#'  forward stepwise algorithm: Application to identifying environmental
#'  chemical mixtures affecting health outcomes. Statistics in Medicine.
#'  2019;38(9):1582-1600. \href{https://doi.org/10.1002/sim.8059}{10.1002/sim.8059}
#' @author Alexander Rix
#' @importFrom splines bs
#' @importFrom rlang f_lhs f_rhs expr sym expr_text
#' @export
snif <- function(formula, df, score = "BIC", degree = 3, maxit = ncol(df),
                     main.only = NULL, linear.only = NULL)
{
  if (!rlang::is_formula(formula))
    stop("'formula' must be a formula.")

  if (!is.data.frame(df))
    stop("'df' must be a data.frame.")

  score <- match.arg(score, c("BIC", "AIC", "PV"))

  if (!is.numeric(degree) || degree < 2)
    stop("'degree' must be an integer >= 2.")

  if (!is.integer(maxit) || maxit < 1)
    stop("'maxit' must be an integer > 0.")

  if (!is.null(main.only) && !is.character(main.only))
    stop("'main.only' must be a character vector.")

  if (!is.null(linear.only) && !is.character(linear.only))
    stop("'linear.only' must be a character vector.")

  score.model <- switch(score,
    "BIC" = function(x) stats::BIC(x),
    "AIC" = function(x) stats::AIC(x),
    "PV"  = function(x)
    {
      s <-summary(x)
      if (is.null(s$fstatistic))
        s$coefficients[1,4]
      else
        eval(expr(stats::pf(!!!unname(s$fstatistic), lower.tail = F)))
    }
  )

  score.candidate <- function(candidate)
  {
    f <- expr(!!f_lhs(f) ~ !!f_rhs(f) + !!candidate)
    score.model(stats::lm(f, df))
  }

  parsed   <- parse_formula(formula, degree)

  lin.sel <- parsed$lin
  nl.sel  <- parsed$nl
  int.sel <- parsed$int

  vars     <- setdiff(names(df), deparse(f_lhs(formula)))
  lin.cand <- setdiff(purrr::map(vars, ~ sym(.x)), lin.sel)

  vars    <- setdiff(vars, linear.only)
  nl.cand <- setdiff(purrr::map(vars, function(var)
    expr(bs(!!sym(var), degree = !!degree)[, -1])), nl.sel)

  add.int <- function(int.cand, sel, term)
  {
    int <- make_interaction(sel, term)

    if (sel == term)
      int.cand
    else if (all.vars(sel) %in% main.only || all.vars(term) %in% main.only)
      int.cand
    else if (expr_text(int) %in% purrr::map_chr(int.sel, expr_text))
      int.cand
    else
      c(int.cand, int)
  }

  int.cand <- purrr::map(c(lin.sel, nl.sel), function(term)
    purrr::reduce(c(lin.sel, nl.sel), add.int, term = term, .init = NULL)
  )
  int.cand <- unique(rlang::squash(int.cand))

  f <- formula
  output <- list(formula = f, score = score.model(stats::lm(f, df)), it = 0)
  for (it in 1:maxit) {
    lin.scores <- c(purrr::map_dbl(lin.cand, score.candidate), Inf)
    nl.scores  <- c(purrr::map_dbl(nl.cand, score.candidate), Inf)
    int.scores <- c(purrr::map_dbl(int.cand, score.candidate), Inf)

    s <- min(c(min(lin.scores), min(nl.scores), min(int.scores)))
    if (s == Inf) {
      message("No more valid candidates, returning.")
      break
    }

    i <- which.min(c(min(lin.scores), min(nl.scores), min(int.scores)))
    sel <- c(lin.sel, nl.sel)
    if (i == 1) {
      i <- which.min(lin.scores)
      term <- lin.cand[[i]]

      lin.sel  <- c(term, lin.sel)
      lin.cand <- lin.cand[-i]

      int.cand <- purrr::reduce(sel, add.int, term = term, .init = int.cand)
    }
    else if (i == 2) {
      i <- which.min(nl.scores)
      term <- nl.cand[[i]]

      nl.sel  <- c(term, nl.sel)
      nl.cand <- nl.cand[-i]

      int.cand <- purrr::reduce(sel, add.int, term = term, .init = int.cand)
    }
    else {
      i <- which.min(int.scores)
      term <- int.cand[[i]]

      int.sel  <- c(term, int.sel)
      int.cand <- int.cand[-i]
    }

    if (is.null(f_rhs(f)))
      f <- expr(!!f_lhs(f) ~ !!term)
    else
      f <- expr(!!f_lhs(f) ~ !!f_rhs(f) + !!term)

    output$score   <- c(output$score, s)
    output$it      <- c(output$it, it)
    output$formula <- c(output$formula, f)
  }

    return(output)
}
