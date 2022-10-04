#' Brute force function for solving knapsack problem
#'
#' @param x The data containing `w`, `v` representing the objects
#' @param W The weight limit of the knapsack
#'
#' @return A list containing the `elements` and the best `value`
#' @export
#'
#' @examples
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#'
brute_force_knapsack <- function(x, W) {
  # stopifnot({
  #
  # })

  final <- vector("numeric")
  max <- 0

  combs <- seq_len(2^nrow(x) - 1)
  for(comb in combs){
    idx <- which(intToBits(comb) == 1)
    if(sum(x[idx, "w"])  <= W){
      vals <- sum(x[idx, "v"])
      if(vals > max){
        max <- vals
        final <- idx
      }
    }
  }

return(list(
  "value" = max,
  "elements" = final
))
}

