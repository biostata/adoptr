#' Data distributions
#'
#' \code{DataDistribution} is an abstract class used to represent the distribution
#' of a sufficient statistic \code{x} given a sample size \code{n} and a
#' single parameter value \code{theta}.
#'
#' This abstraction layer allows representation of t-distributions
#' (unknown variance), normal distribution (known variance), and normal
#' approximation of a binary endpoint.
#' Currently, only the normal case is implemented with \code{\link{Normal-class}}.
#'
#' @template DataDistributionTemplate
#'
#' @exportClass DataDistribution
setClass("DataDistribution")

#' @param probs numeric vector of probabilities
#' @describeIn DataDistribution quantile function of the respective distribution.
#' @export
setMethod("quantile", signature("DataDistribution"), function(x, probs, n, theta, ...) stop("not implemented"))


#' @rdname DataDistribution-class
#' @export
setGeneric("probability_density_function", function(dist, x, n, theta, ...) standardGeneric("probability_density_function"))

#' @describeIn DataDistribution probability density function given outcome,
#'     sample size and parameter; must be implemented.
#' @export
setMethod("probability_density_function", signature("DataDistribution", "numeric", "numeric", "numeric"),
          function(dist, x, n, theta, ...) stop("not implemented"))


#' @rdname DataDistribution-class
#' @export
setGeneric("cumulative_distribution_function", function(dist, x, n, theta, ...) standardGeneric("cumulative_distribution_function"))

#' @describeIn DataDistribution cumulative distribution function given outcome,
#'     sample size and parameter; must be implemented.
#' @export
setMethod("cumulative_distribution_function", signature("DataDistribution", "numeric", "numeric", "numeric"),
          function(dist, x, n, theta, ...) stop("not implemented"))





#' Normal data distribution
#'
#' Implements a normal data distribution for z-values given an observed z-value
#' and stage size.
#' Standard deviation is 1 and mean \eqn{\theta\sqrt n} where
#' \eqn{\theta} is the standardized effect size.
#' See \code{\link{DataDistribution-class}} for more details.
#'
#' @template DataDistributionTemplate
#'
#' @rdname NormalDataDistribution-class
#' @exportClass Normal
setClass("Normal", representation(
    dummy = "logical" # needed to make this non-abstract
    ),
    contains = "DataDistribution")


#' @rdname NormalDataDistribution-class
#' @export
Normal <- function() new("Normal", dummy = FALSE)


#' @rdname NormalDataDistribution-class
#' @export
setMethod("probability_density_function", signature("Normal", "numeric", "numeric", "numeric"),
          function(dist, x, n, theta, ...) stats::dnorm(x, mean = sqrt(n) * theta, sd = 1) )


#' @rdname NormalDataDistribution-class
#' @export
setMethod("cumulative_distribution_function", signature("Normal", "numeric", "numeric", "numeric"),
          function(dist, x, n, theta, ...) stats::pnorm(x, mean = sqrt(n) * theta, sd = 1) )


#' @param probs vector of probabilities
#' @rdname NormalDataDistribution-class
#' @export
setMethod("quantile", signature("Normal"),
          function(x, probs, n, theta, ...) stats::qnorm(probs, mean = sqrt(n) * theta, sd = 1) )