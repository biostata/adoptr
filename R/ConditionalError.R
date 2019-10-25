#' [WIP] Conditional error function of a Design
#'
#' At any point in time, the currently available trial data can be unblinded and
#' the the maximal type one error rate given the observed data under the design
#' can be computed.
#' Using the condiitonal error principle, this conditional error can be used
#' to implement unplanned design adaptation by making sure that the conditional
#' error under the new design is less than or equal to the conditional error
#' under the original design.
#'
#' @template design
#' @template s
#' @template x1
#' @template optimization
#' @template dotdotdot
#'
#' @seealso \link{Scores}
#'
#' @examples
#'
#' @export
conditional_error <- function(design, distribution, prior_null, x1, k1, x2 = NA_real_, k2 = 0, optimization = FALSE, ...) {
    # TODO: not vectorized
    res <- NULL
    if (is.na(x2) & k2 == 0 & k1 == n1(design, round == !optimization)) {
        # this is the case hitting the prespecified sample size k1 == n1 and
        # corresponds with the ConditionalPower class results
       res <- expectation(
           posterior(distribution, prior_null, x1, k1, ...),
           function(theta)
               1 - cumulative_distribution_function(distribution, c2(design, x1), n2(design, x1, round = !optimization), theta)
       )
    }
    if (is.na(x2) & k2 == 0 & k1 < n1(design, round == !optimization)) {
        # this is the case of underrunning in stage one, need to integrate over the
        # remainder of the first stage
        res <- expectation(
            posterior(distribution, prior_null, x1, k1, ...),
            function(theta)
                1 - cumulative_distribution_function(distribution, c2(design, x1), n2(design, x1, round = !optimization), theta)
        )
    }
}
