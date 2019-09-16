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

    install.packages(c("rlang", "purrr"))

    # install.packages("devtools")
    devtools::install_github("umich-cphds/snif")

Example
=======

This section is based off the snif documentation.

The first argument to `snif` is a R formula that defines the initial
model `snif` will use during the forward stage of the algorithm. the
second argument, `df`, is a R data.frame, which is used for scoring, and
the columns of `df` are the possible variables that `snif` will
consider. In other words, if there variables in `df` that you don't want
snif to consider, you should subset `df` first.

`snif` supports binary variables by setting `type = "logistic"`. Then
`snif` will use a `glm` to fit the data instead of a `lm`. The default
for `type` is linear.

`snif` contains a toy dataset, `snif.df` which is used in this example.
It has 500 rows and 11 columns.

    library(snif)

    ## Loading required package: splines

    names(snif.df)

    ##  [1] "y"   "V2"  "V3"  "V4"  "V5"  "V6"  "V7"  "V8"  "V9"  "V10" "V11"

In this data.frame, the response, `y`, is continuous so we will use a
linear model by setting `type = "linear"`. We would also like to use BIC
scoring to determine which variable to model to the model, so we set
`score = "BIC"`. For the initial formula to fit, we do not know much
about this data so we will start with the null model, `y ~ NULL`.

    snif.out <- snif(formula = y ~ NULL, df = snif.df, type = "linear", score = "BIC")

    summary(snif.out)

    ## 
    ## Call:
    ## stats::lm(formula = y ~ bs(V2, degree = 3)[, -1] + V5 + V6 + 
    ##     V4 + V2, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.6943  -2.0696  -0.1403   2.0606   9.4985 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -8.4265     3.6224  -2.326   0.0204 *  
    ## bs(V2, degree = 3)[, -1]2  13.3611     6.6660   2.004   0.0456 *  
    ## bs(V2, degree = 3)[, -1]3  51.4487     7.5171   6.844 2.29e-11 ***
    ## V5                          1.1324     0.1572   7.202 2.23e-12 ***
    ## V6                          0.9949     0.1505   6.611 9.96e-11 ***
    ## V4                          0.9261     0.1577   5.871 7.96e-09 ***
    ## V2                        -11.0073     2.0782  -5.297 1.78e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.183 on 493 degrees of freedom
    ## Multiple R-squared:  0.5475, Adjusted R-squared:  0.5419 
    ## F-statistic:  99.4 on 6 and 493 DF,  p-value: < 2.2e-16

Alternatively, we can use a non null initial model

    snif.out <- snif(formula = y ~ V2, df = snif.df, type = "linear",
                       score = "BIC")

    summary(snif.out)

    ## 
    ## Call:
    ## stats::lm(formula = y ~ V2 + bs(V2, degree = 3)[, -1] + V5 + 
    ##     V6 + V4, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.6943  -2.0696  -0.1403   2.0606   9.4985 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -8.4265     3.6224  -2.326   0.0204 *  
    ## V2                        -11.0073     2.0782  -5.297 1.78e-07 ***
    ## bs(V2, degree = 3)[, -1]2  13.3611     6.6660   2.004   0.0456 *  
    ## bs(V2, degree = 3)[, -1]3  51.4487     7.5171   6.844 2.29e-11 ***
    ## V5                          1.1324     0.1572   7.202 2.23e-12 ***
    ## V6                          0.9949     0.1505   6.611 9.96e-11 ***
    ## V4                          0.9261     0.1577   5.871 7.96e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.183 on 493 degrees of freedom
    ## Multiple R-squared:  0.5475, Adjusted R-squared:  0.5419 
    ## F-statistic:  99.4 on 6 and 493 DF,  p-value: < 2.2e-16

`snif` also supports interaction terms via `:` and basis expansions via
`bs`.

    snif.out <- snif(formula = y ~ bs(V2), df = snif.df, type = "linear",
                       score = "BIC")

Note that `bs` implies that both `V2` and the nonlinear expansion of
`V2`, `bs(V2)[,-1]` are both included in the model. That is the
following two formulas are equivalent: `y ~ bs(V2)` and
`y ~ V2 + bs(V2)[,-1]` If you wish for only the non linear part of `V2`
to be included in `snif`, you need to drop the linear term from the
expansion, that is, `y ~ bs(V2)[, -1]`. Finally, you may specify the
degree of the expansion via the degree parameter in `snif`.

More detailed documentation is available on the `snif` help page, which
provides rules for building initial `snif` formulas, and documents other
optional parameters not mentioned here.

Reference
=========

Narisetty, Naveen N. and Mukherjee, Bhramar and Chen, Yin-Hsiu and
Gonzalez, Richard and Meeker, John D. Selection of nonlinear
interactions by a forward stepwise algorithm: Application to identifying
environmental chemical mixtures affecting health outcomes. Statistics in
Medicine. 2019;38(9):1582-1600.

Contact
=======

Please email Alex Rix (<alexrix@umich.edu>) if you have any questions,
comments, or possible bugs to report. You may also open up an issue on
github.
