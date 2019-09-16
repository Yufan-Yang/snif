#' Selection of nonlinear interactions by a forward stepwise algorithm
#'
#' \code{snif} is a greedy forward stepwise selection algorithm that takes
#' nonlinear effects and interactions into account when adding variables.
#' \code{snif} follows the strong hereditary principle (for X1:X2 to be
#' included as an interaction, both X1 and X2 both must be already included in
#' the model. \code{snif} supports both binary and continuous outcomes.
#'
#' \code{snif} uses a formula to initialize the model before the forward
#' stepwise stage of the algorithm. If you are not interested in using a
#' non-null initial model, something to the effect of \code{y ~ NULL} should do
#' just fine. On the other hand, if you do wish to use an initial model, some
#' care must be taken so \code{snif} will be happy.
#'
#' Basically, there are four valid ways to build snif formulas:
#' \enumerate{
#'     \item Add a linear main effect, as in \code{y ~ V2}
#'     \item Add a nonlinear main effect, as in \code{y ~ bs(V2)[,-1]}. Note
#'         that \code{[, -1]} is important.
#'   \item Add a linear and nonlinear main effect, as in \code{y ~ bs(V2)}.
#'       \code{snif} will automatically split a \code{bs} expansion without an
#'       index (\code{[ ,-1]}) into linear and nonlinear parts internally.
#'   \item Add an interaction term, as in \code{y + V2:V3} You may specify
#'       interactions between in any combination of 1-3. If you chose to include
#'       an interaction of type 3, note that \code{snif} will internally
#'       decompose the interaction into parts. For example \code{y ~ bs(V2):V3}
#'       is equivalent to \code{y ~ V2:V3 + bs(V2)[,-1]:V3}
#'}
#' \code{snif} formulas can be built out of any linear combination 1-4.
#' @param formula An initial formula for \code{snif} to start with.
#'   \code{formula} supports linear variables, basis spline expansions via
#'   \code{bs}, and interaction terms via \code{:}. It is highly recomended to
#'   read the details section of the documentation before trying anything fancy
#'   with \code{formula}
#' @param df A data.frame containing the data
#' @param type The type of regression to perform. Either "linear" (default) or
#'   "logistic".
#' @param score A character argument that specifies the kind of scoring method
#'   used to determine which variable to add to the model. supported options
#'   are "BIC" (default), "AIC", and "PV" (p.value). "PV" is not compatible when
#'   the 'type' is "logisitic".
#' @param degree Degree of basis spline expansion for nonlinear effects
#' @param maxnv Max number of varaiables to add. Default is \code{ncol(df)}
#' @param main.only Character vector of variables that are only considered for
#'   main effects
#' @param linear.only Character vector of variables that are considered to
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
snif <- function(formula, df, type = "linear", score = "BIC", degree = 3,
                   maxnv = ncol(df), main.only = NULL, linear.only = NULL)
{
  if (!rlang::is_formula(formula))
    stop("'formula' must be a formula.")

  if (!is.data.frame(df))
    stop("'df' must be a data.frame.")

  score <- match.arg(score, c("BIC", "AIC", "PV"))

  type <- match.arg(type, c("linear", "logistic"))

  if (!is.numeric(degree) || degree < 2)
    stop("'degree' must be an integer >= 2.")

  if (!is.numeric(maxnv) || maxnv < 1)
    stop("'maxnv' must be an integer > 0.")

  if (!is.null(main.only) && !is.character(main.only))
    stop("'main.only' must be a character vector.")

  if (!is.null(linear.only) && !is.character(linear.only))
    stop("'linear.only' must be a character vector.")

  if (!is.null(setdiff(main.only, names(df))))
    stop("main.only contains variables that are not in df")

  if (!is.null(setdiff(linear.only, names(df))))
    stop("main.only contains variables that are not in df")

  y <- deparse(f_lhs(formula))
  if (type == "logistic" && length(unique(df[[y]])) != 2)
    stop(paste(y, "is not a binary variable."))

  if (type == "logistic")
    model <- function(f, df) stats::glm(f, df, family = stats::binomial)
  else
    model <- stats::lm

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

  if (type == "logistic" && score == "PV")
    stop("PV scoring is incompatible with logisitic regression.")

  score.candidate <- function(candidate)
  {
    f <- expr(!!f_lhs(f) ~ !!f_rhs(f) + !!candidate)
    score.model(model(f, df))
  }

  parsed   <- parse_formula(formula, degree)

  lin.sel <- parsed$lin
  nl.sel  <- parsed$nl
  int.sel <- parsed$int

  vars     <- setdiff(names(df), y)
  lin.cand <- setdiff(purrr::map(vars, ~ sym(.x)), lin.sel)

  vars    <- setdiff(vars, linear.only)
  nl.cand <- setdiff(purrr::map(vars, function(var)
    expr(bs(!!sym(var), degree = !!degree)[, -1])), nl.sel)

  add.inter <- function(int.cand, sel, term)
  {
    int <- make_interaction(sel, term)

    if (all.vars(sel) == all.vars(term))
      int.cand
    else if (all.vars(sel) %in% main.only || all.vars(term) %in% main.only)
      int.cand
    else if (expr_text(int) %in% purrr::map_chr(int.sel, expr_text))
      int.cand
    else
      c(int.cand, int)
  }

  int.cand <- purrr::map(c(lin.sel, nl.sel), function(term)
    purrr::reduce(c(lin.sel, nl.sel), add.inter, term = term, .init = NULL)
  )
  int.cand <- unique(rlang::squash(int.cand))

  f <- formula
  output <- structure(list(formula = f, score = score.model(model(f, df)),
                        nv = 0, df = df, model = expr(!!model)),
                      class = "snif")

  for (nv in 1:maxnv) {
    lin.scores <- c(purrr::map_dbl(lin.cand, score.candidate), Inf)
    nl.scores  <- c(purrr::map_dbl(nl.cand, score.candidate), Inf)
    int.scores <- c(purrr::map_dbl(int.cand, score.candidate), Inf)

    s <- min(c(min(lin.scores), min(nl.scores), min(int.scores)))
    if (s == Inf) {
      message("No more candidate additions, returning.")
      break
    }

    i <- which.min(c(min(lin.scores), min(nl.scores), min(int.scores)))
    sel <- c(lin.sel, nl.sel)
    if (i == 1) {
      i <- which.min(lin.scores)
      term <- lin.cand[[i]]

      lin.sel  <- c(term, lin.sel)
      lin.cand <- lin.cand[-i]

      int.cand <- purrr::reduce(sel, add.inter, term = term, .init = int.cand)
    }
    else if (i == 2) {
      i <- which.min(nl.scores)
      term <- nl.cand[[i]]

      nl.sel  <- c(term, nl.sel)
      nl.cand <- nl.cand[-i]

      int.cand <- purrr::reduce(sel, add.inter, term = term, .init = int.cand)
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
    output$nv      <- c(output$nv, nv)
    output$formula <- c(output$formula, f)
  }

  return(output)
}

#'Summarizing SNIF Fits
#'
#' ‘summary’ method for class ‘"snif"’
#'
#' \code{summary.snif} extracts the best scoring model from \code{object} and
#' returns the linear model summary.
#' @param object An object of type "snif"
#' @param ... Additional arguments to \code{summary.lm}
#' @export
summary.snif <- function(object, ...)
{
    if (class(object) != "snif")
      stop("'object' is not a 'snif' object.")

   i <- which.min(object$score)
   formula <- object$formula[[i]]
   df <- object$df
   eval(expr(summary(stats::lm(!!formula, df), ...)))
}

#' Predict Using The Best SNIF Fit
#'
#' predict method for class ‘"snif"’
#'
#' \code{predict.snif} extracts the best scoring model from \code{object} and
#' returns the predictions from \code{predict.lm}.
#' @param object An object of type "snif"
#' @param newdata An optional data frame in which to look for variables with
#'   which to predict.  If omitted, the fitted values are used.
#' @param ... Additional arguments to \code{predict.lm}
#' @export
predict.snif <- function(object, newdata, ...)
{
    if (class(object) != "snif")
      stop("'object' is not a 'snif' object.")

   i <- which.min(object$score)
   formula <- object$formula[[i]]
   df <- object$df
   eval(expr(stats::predict(stats::lm(!!formula, df), newdata, ...)))
}
