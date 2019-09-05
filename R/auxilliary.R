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
          stop(paste("Parsing failure: Unrecognized term", expr_text(e),
            "in formula."))

      list
    }
    parse.rec(list(lin = c(), nl = c(), int = c()), f_rhs(f))
}
