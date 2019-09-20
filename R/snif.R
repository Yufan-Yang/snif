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
#' non-null initial model, \code{y ~ NULL} should do just fine. On the other
#' hand, if you do wish to use an initial model, some care must be taken so
#' \code{snif} will be happy.
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
#'   \code{bs}, and interaction terms via \code{:}. It is highly recommended to
#'   read the details section of the documentation before trying anything fancy
#'   with \code{formula}
#' @param df A data.frame containing the data
#' @param type The type of regression to perform. Either "linear" (default) or
#'   "logistic"
#' @param method A character argument that specifies the kind of scoring method
#'   used to determine which variable to add to the model. supported options
#'   are "BIC" (default), "AIC", and "PV" (p.value).
#' @param degree Degree of basis spline expansion for nonlinear effects
#' @param maxnv Max number of variables to add. Default is \code{ncol(df)}
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
#' @examples
#' library(snif)
#'
#' # snif contains a synthetic data.frame, snif.df
#' # if you not want to include any covariates in the initial model, you can
#' # just use NULL of the right hand side of formula
#' snif.out <- snif(formula = y ~ NULL, df = snif.df, type = "linear",
#'                      score = "BIC")
#'
#' # snif provides a summary method which will extract and summarise the best
#' # scoring subset
#' summary(snif.out)
#'
#' # you can specify an initial model by using the rules explained in the
#' # details section.
#' snif.out <- snif(formula = y ~ V2 + V2:V4, df = snif.df, type = "linear",
#'                      score = "BIC")
#'
#' # snif also supports binary outcomes. First, we copy snif.df and discretize y
#' snif.df.bin <- snif.df
#' snif.df.bin$y <- ifelse(snif.df.bin$y > 5, 1, 0)

#' snif.out <- snif(formula = y ~ NULL, df = snif.df.bin, type = "logistic",
#'                      score = "BIC")
#' summary(snif.out)
#' @importFrom splines bs
#' @importFrom rlang f_lhs f_rhs expr sym expr_text
#' @export
snif <- function(formula, df, type = "linear", method = "BIC", degree = 3,
                   maxnv = ncol(df), main.only = NULL, linear.only = NULL)
{
    # snif.init performs all the input checking and generates the initial
    # parameters for the forward selction stage.
    initial <- snif.init(formula, df, type, score, degree, maxnv, main.only,
                             linear.only)

    # score model is BIC, AIC, or PV
    score.model <- initial$score.model
    # model is glm or lm depending on type
    model       <- initial$model

    # 'sel' is the selected linear (lin), nonlinear (nl) and interaction (int)
    # terms as specified by 'formula'. 'can' is the candidate sets for lin, nl,
    # and int. 'can' takes into account 'main.only', 'linear only', and 'sel'
    sel <- initial$sel
    can <- initial$can

    add.inter <- function(int.can, selected, term)
    {
        int <- make_interaction(selected, term)

        if (all.vars(selected) == all.vars(term))
            int.can
        else if (any(c(all.vars(selected), all.vars(term)) %in% main.only))
            int.can
        else if (expr_text(int) %in% purrr::map_chr(sel$int, expr_text))
            int.can
        else
            c(int.can, int)
    }

    f <- formula
    score.candidate <- function(candidate)
    {
        f1 <- expr(!!f_lhs(f) ~ !!f_rhs(f) + !!candidate)
        score.model(x = model(f1, df), x0 = model(f, df))
    }

    output <- structure(
                  list(formula = f, score = score.model(model(f, df)), nv = 0,
                      df = df, model = model, score = score),
                  class = "snif"
    )

    # add the term that, when included in the current model, has the best score
    for (nv in 1:maxnv) {
        scores <- list(lin = c(purrr::map_dbl(can$lin, score.candidate), Inf),
                       nl  = c(purrr::map_dbl(can$nl, score.candidate), Inf),
                       int = c(purrr::map_dbl(can$int, score.candidate), Inf)
        )

        mins <- purrr::map_dbl(scores, min)

        # i is 1, 2, or 3 which correpsonds to lin, nl, and int
        i  <- which.min(mins)

        # j is the best scoring candidate
        j  <- which.min(scores[[i]])
        s  <- min(mins)

        # if the best score in Inf, we've exhausted all possible additions
        if (is.infinite(s))
            break

        prev.sel <- c(sel$lin, sel$nl)
        term <- can[[i]][[j]]

        sel[[i]] <- c(term, sel[[i]])
        can[[i]] <- can[[i]][-j]

        if (i < 3)
            can$int <- purrr::reduce(prev.sel, add.inter, term = term,
                                         .init = can$int)

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

snif.init <- function(f, df, type, score, degree, maxnv, main.only, linear.only)
{
    if (!rlang::is_formula(f))
        stop("'formula' must be a formula.")

    if (!is.data.frame(df))
        stop("'df' must be a data.frame.")

    score <- match.arg(score, c("BIC", "AIC", "PV"))
    type  <- match.arg(type, c("linear", "logistic"))

    if (!is.numeric(degree) || degree < 2)
        stop("'degree' must be an integer >= 2.")

    if (!is.numeric(maxnv) || maxnv < 1)
        stop("'maxnv' must be an integer > 0.")

    if (!is.null(main.only) && !is.character(main.only))
        stop("'main.only' must be a character vector.")

    if (!is.null(linear.only) && !is.character(linear.only))
        stop("'linear.only' must be a character vector.")

    if (length(setdiff(main.only, names(df))) > 0)
        stop("'main.only' contains variables that are not in 'df'.")

    if (length(setdiff(linear.only, names(df))) > 0)
        stop("'main.only' contains variables that are not in 'df'.")

    y <- deparse(f_lhs(f))

    if (type == "logistic" && length(unique(df[[y]])) != 2)
        stop(paste(y, "is not a binary variable."))

    if (type == "logistic")
        model <- function(f, df) stats::glm(f, df, family = stats::binomial)
    else
        model <- stats::lm

    if (score == "BIC")
        score.model <- function(x,...) stats::BIC(x)
    else if (score == "AIC")
        score.model <- function(x, ...) stats::AIC(x)
    else if (type == "linear") {
        score.model <- function(x, x0)
        {
            if (missing(x0)) {
                p <- anova(x)$`Pr(>F)`
                ifelse(is.na(p[1]), 1, p[1])
            }
            else
            anova(x0, x)$`Pr(>F)`[2]
        }
    }
    else {
        score.model <- function(x, x0)
        {
            if (missing(x0)) {
                p <- anova(x, test = "Chisq")$`Pr(>Chi)`
                ifelse(is.na(p[1]), 1, p[2])
            }
            else
                anova(x0, x, test = "Chisq")$`Pr(>Chi)`[2]
        }
    }
    parsed <- parse_formula(f, degree)
    sel    <- list(lin = parsed$lin, nl = parsed$nl, int = parsed$int)

    can <- NULL

    vars    <- setdiff(names(df), y)
    can$lin <- setdiff(purrr::map(vars, ~ sym(.x)), sel$lin)

    vars   <- setdiff(vars, linear.only)
    can$nl <- setdiff(purrr::map(vars, function(var)
        expr(bs(!!sym(var), degree = !!degree)[, -1])), sel$nl)

    add.inter <- function(int.can, selected, term)
    {
        int <- make_interaction(selected, term)

        if (all.vars(selected) == all.vars(term))
            int.can
        else if (any(c(all.vars(selected), all.vars(term)) %in% main.only))
            int.can
        else if (any(int == sel$int))
            int.can
        else
            c(int.can, int)
    }

    can$int <- purrr::map(c(sel$lin, sel$nl), function(term)
        purrr::reduce(c(sel$lin, sel$nl), add.inter, term = term,
                          .init = list()))

    can$int <- unique(rlang::squash(can$int))

    list(model = model, score.model = score.model, sel = sel, can = can)
}

#' Summarizing SNIF Fits
#'
#' \code{summary} method for class "snif"
#'
#' \code{summary.snif} extracts the best scoring model from \code{object} and
#' returns the linear model summary.
#' @param object An object of type "snif"
#' @param ... Additional arguments to \code{summary.lm} or \code{summary.glm},
#'     depending on the type of model fit
#' @export
summary.snif <- function(object, alpha = 0.05, ...)
{
    if (class(object) != "snif")
        stop("'object' is not a 'snif' object.")

    if (score == "PV") {
        i <- match(F, object$score < 0.05)
    }
    formula <- object$formula[[i]]

    list(
        formula = object$formula[[i]],
        score = object$score[i],
        summary = summary(object$model(formula, object$df), ...)
    )
}

#' Predict Using The Best SNIF Fit
#'
#' \code{predict} method for class "snif"
#'
#' \code{predict.snif} extracts the best scoring model from \code{object} and
#' returns the predictions from \code{predict.lm}.
#' @param object An object of type "snif"
#' @param newdata An optional data frame in which to look for variables with
#'   which to predict.  If omitted, the original values are used.
#' @param ... Additional arguments to \code{predict.lm} or \code{predict.glm},
#' depending on the type of model fit
#' @export
predict.snif <- function(object, newdata, ...)
{
    if (class(object) != "snif")
        stop("'object' is not a 'snif' object.")

    i <- which.min(object$score)
    formula <- object$formula[[i]]
    df <- object$df
    stats::predict(object$model(formula, df), newdata, ...)
}

#' Print \code{snif} objects
#'
#' \code{print} method for class "snip"
#' @param x An object of type "snif"
#' @param ... Additional parameters to pass onto \code{print}
#' @export
print.snif <- function(x, ...)
{
    if (class(x) != "snif")
        stop("'x' is not a snif object.")

    if (isTRUE(invisible(all.equal(x$model, stats::lm))))
        cat("snif run with type = 'linear'.\n")
    else
        cat("snif run with type = 'logisitic'.\n")

    for (i in 1:length(x$formula))
        cat(paste("Score:", signif(x$score[i]), "model:", expr_text(x$formula[[i]]), "\n"))
}
