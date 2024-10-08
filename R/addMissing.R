#' Add missing values to a vector given a MCAR, MAR, or MNAR scheme
#'
#' Given an input vector, replace elements of this vector with missing values according to some scheme.
#' Default method replaces input values with a MCAR scheme (where on average 10\% of the values will be
#' replaced with \code{NA}s). MAR and MNAR are supported by replacing the default \code{FUN} argument.
#'
#' Given an input vector y, and other relevant variables
#' inside (X) and outside (Z) the data-set, the three types of missingness are:
#'
#' \describe{
#'   \item{MCAR}{Missing completely at random (MCAR). This is realized by randomly
#'     sampling the values of the
#'     input vector (y) irrespective of the possible values in X and Z.
#'     Therefore missing values are randomly sampled and do not depend on any data characteristics and
#'     are truly random}
#'   \item{MAR}{Missing at random (MAR). This is realized when values in the dataset (X)
#'     predict the missing data  mechanism in y; conceptually this is equivalent to
#'     \eqn{P(y = NA | X)}. This requires the user to define a custom missing data function}
#'   \item{MNAR}{Missing not at random (MNAR). This is similar to MAR except
#'     that the missing mechanism comes
#'     from the value of y itself or from variables outside the working dataset;
#'     conceptually this is equivalent to \eqn{P(y = NA | X, Z, y)}. This requires
#'     the user to define a custom missing data function}
#' }
#'
#' @param y an input vector that should contain missing data in the form of \code{NA}'s
#'
#' @param fun a user defined function indicating the missing data mechanism for each element in \code{y}.
#'   Function must return a vector of probability values with the length equal to the length of \code{y}.
#'   Each value in the returned vector indicates the probability that
#'   the respective element in y will be replaced with \code{NA}.
#'   Function must contain the argument \code{y}, representing the
#'   input vector, however any number of additional arguments can be included
#'
#' @param ... additional arguments to be passed to \code{FUN}
#'
#' @return the input vector \code{y} with the sampled \code{NA} values
#'   (according to the \code{FUN} scheme)
#'
#' @aliases add_missing
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @export
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' set.seed(1)
#' y <- rnorm(1000)
#'
#' ## 10% missing rate with default FUN
#' head(ymiss <- addMissing(y), 10)
#'
#' ## 50% missing with default FUN
#' head(ymiss <- addMissing(y, rate = .5), 10)
#'
#' ## missing values only when female and low
#' X <- data.frame(group = sample(c('male', 'female'), 1000, replace=TRUE),
#'                 level = sample(c('high', 'low'), 1000, replace=TRUE))
#' head(X)
#'
#' fun <- function(y, X, ...){
#'     p <- rep(0, length(y))
#'     p[X$group == 'female' & X$level == 'low'] <- .2
#'     p
#' }
#'
#' ymiss <- addMissing(y, X, fun=fun)
#' tail(cbind(ymiss, X), 10)
#'
#' ## missingness as a function of elements in X (i.e., a type of MAR)
#' fun <- function(y, X){
#'    # missingness with a logistic regression approach
#'    df <- data.frame(y, X)
#'    mm <- model.matrix(y ~ group + level, df)
#'    cfs <- c(-5, 2, 3) #intercept, group, and level coefs
#'    z <- cfs %*% t(mm)
#'    plogis(z)
#' }
#'
#' ymiss <- addMissing(y, X, fun=fun)
#' tail(cbind(ymiss, X), 10)
#'
#' ## missing values when y elements are large (i.e., a type of MNAR)
#' fun <- function(y) ifelse(abs(y) > 1, .4, 0)
#' ymiss <- addMissing(y, fun=fun)
#' tail(cbind(y, ymiss), 10)
#'
#' }
#'
addMissing <- function(y, fun = function(y, rate = .1, ...) rep(rate, length(y)), ...){
    if(!('y' %in% names(formals(fun))))
        stop('fun must include a y argument')
    probs <- fun(y=y, ...)
    stopifnot(length(probs) == length(y))
    stopifnot(all(probs >= 0 & probs <= 1))
    is_na <- sapply(probs, function(p) sample(c(FALSE, TRUE), 1L, prob = c(1-p, p)))
    y[is_na] <- NA
    y
}

#' @export
add_missing <- function(...){
    .Deprecated('addMissing')
    addMissing(...)
}
