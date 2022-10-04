#' Generating data for knapsack problem with optional number of objects
#'
#' @param n the number of objects
#'
#' @return `data.frame` containing the `w` and `v`
#' @export
#'
#' @examples
#'
#' knapsack_function(n = 666)
knapsack_function <- function(n = 2000) {
  suppressWarnings(RNGversion(min(as.character(getRversion()), "3.5.3")))

  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  knapsack_objects <- data.frame(
    w = sample(1:4000, size = n, replace = TRUE),
    v = runif(n = n, 0, 10000)
  )
  return(knapsack_objects)
}
