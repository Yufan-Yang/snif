Introduction
============

`snif` is a R package that implements "Selection of Nonlinear
Interactions by a Forward Stepwise Algorithm". `snif` (the function) is
a a forward stepwise algorithm that takes nonlinearity and interactions
into account.

`snif` is currently in the middle of being tested and polished, and as
such it is *BETA* software.

Installation
============

`snif` requires the packages `purrr` and `rlang` to be installed, and it
is a good idea to update them to make sure the package works correctly.

    install.packages("rlang")
    install.packages("purrr")

    # install.packages("devtools")
    devtools::install_github("umich-cphds/snif")

Example
=======

The documentation is very much a work in progress, but the following
examples illustrate the usage of `snif`.

The first argument to `snif` is a R formula that defines the initial
model `snif` will use during the forward stage of the algorithm. the
second argument, `df`, is a R data.frame, which is used for scoring, and
the columns of `df` are the possible variables that `snif` may consider
adding (aside form the response, of course). In other words, if there
variables in `df` that you don't want snif to consider, you should
subset `df` first.

`snif` contains a toy dataset, `snif.df` which is used in these
examples. It has 500 rows and 11 columns.

Note that the initial model is the null model.

    library(snif)

    names(snif.df)

    ##  [1] "y"   "V2"  "V3"  "V4"  "V5"  "V6"  "V7"  "V8"  "V9"  "V10" "V11"

    snif.out <- snif(formula = y ~ NULL, df = snif.df, type = "linear",
                       score = "BIC")

    summary(snif.out)

    ## 
    ## Call:
    ## stats::lm(formula = y ~ bs(V2, degree = 3)[, -1] + V5 + V6 + 
    ##     V4 + V2 + bs(V2, degree = 3)[, -1]:V2, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.0131  -1.9990  -0.0609   1.8968  10.2542 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   22.0258     8.5009   2.591  0.00985 ** 
    ## bs(V2, degree = 3)[, -1]2    -42.2015    15.5340  -2.717  0.00683 ** 
    ## bs(V2, degree = 3)[, -1]3    -17.3672    18.9428  -0.917  0.35969    
    ## V5                             1.0908     0.1553   7.023 7.25e-12 ***
    ## V6                             0.9526     0.1487   6.406 3.49e-10 ***
    ## V4                             0.9665     0.1558   6.203 1.17e-09 ***
    ## V2                             4.2740     4.3798   0.976  0.32963    
    ## bs(V2, degree = 3)[, -1]2:V2  13.6240     3.4516   3.947 9.06e-05 ***
    ## bs(V2, degree = 3)[, -1]3:V2       NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.137 on 492 degrees of freedom
    ## Multiple R-squared:  0.5613, Adjusted R-squared:  0.5551 
    ## F-statistic: 89.95 on 7 and 492 DF,  p-value: < 2.2e-16

Reference
=========

Narisetty, Naveen N. and Mukherjee, Bhramar and Chen, Yin-Hsiu and
Gonzalez, Richard and Meeker, John D. Selection of nonlinear
interactions by a forward stepwise algorithm: Application to identifying
environmental chemical mixtures affecting health outcomes. Statistics in
Medicine. 2019;38(9):1582-1600.

Contact
=======

Please(!) contact Alex Rix (<alexrix@umich.edu>) if you have any
questions, comments, or possible bugs to report. You may also open up an
issue on github if that suits your fancy.
