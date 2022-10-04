#' Brute force function for solving knapsack problem
#'
#' @param x The data containing `w`, `v` representing the objects
#' @param W The weight limit of the knapsack
#'
#' @return A list containing the `elements` and the best `value`
#' @export
#'
#' @examples
#' data(knapsack_objects)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#'
brute_force_knapsack <- function(x, W) {
  # stopifnot({
  #
  # })

  final <- vector("numeric")
  max <- 0

  combs <- seq_len(2 ^ nrow(x) - 1)
  for (comb in combs) {
    idx <- which(intToBits(comb) == 1)
    if (sum(x[idx, "w"])  <= W) {
      vals <- sum(x[idx, "v"])
      if (vals > max) {
        max <- vals
        final <- idx
      }
    }
  }

  return(list("value" = max,
              "elements" = final))
}


max_val <- function(i, W, value_table, x) {
  if (i == 1 | W <= 1) {
    value_table[i, W] <- 0
    return(value_table[i, W])
  }
  if (value_table [i - 1, W] == -1) {
    value_table[i - 1, W] <- max_val(i - 1, W, value_table, x)
  }

  if (x[i, "w"] > W) {
    value_table[i, W] <- value_table[i - 1, W]
  } else{
    if (value_table[i - 1, W - x[i, "w"]] == -1) {
      value_table[i - 1, W - x[i, "w"]] <-
        max_val(i - 1, W - x[i, "w"], value_table, x)
    }
    value_table[i, W] <-
      max(value_table[i - 1, W], value_table[i - 1, W - x[i, "w"]] + x[i, "v"])
  }
}


#' Dynamic knapsack
#'
#' @param x The data containing `w`, `v` representing the objects
#' @param W The weight limit of the knapsack
#'
#' @return The indices of the objects to select
#' @export
#'
#' @examples
#' data(knapsack_objects)
#' dynamic_knapsack(knapsack_objects[1:12, ], 3500)
#'
dynamic_knapsack <- function(x, W) {
  i <- nrow(x)
  value_table <- matrix(-1, nrow = i, ncol = W)
  if (i == 1) {
    return()
  }
  # test <- memoise(max_val)
  # test <- memoise(dynamic_knapsack)

  if (max_val(i, W, value_table, x) > max_val(i - 1, W, value_table, x)) {
    return(c(i, dynamic_knapsack(x[1:i - 1,], W - x[i, "w"])))
  }  else{
    return(dynamic_knapsack(x[1:i - 1,], W))
  }
}

#' Greedy Knapsack
#'
#' @param x The data containing `w`, `v` representing the objects
#' @param W The weight limit of the knapsack
#'
#' @return a `list` containing the `elements` and the best `value`
#' @export
#'
#' @examples
#' data(knapsack_objects)
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#'
greedy_knapsack <- function(x, W) {
  n <- length(rownames(x))

  df <- data.frame(x)
  x$percentage <- x[[2]] / x[[1]]
  sorting <-
    sort(x$percentage,
         index.return = TRUE,
         decreasing = TRUE)$ix
  wsum <- 0
  vsum <- 0
  firstnot <- 0
  elements_ix <- c()
  for (i in sorting) {
    if ((wsum + x[i, 1]) <= W) {
      wsum <- wsum + x[i, 1]
      vsum <- vsum + x[i, 2]
      elements_ix <- append(elements_ix, i)
    } else if ((firstnot == 0) && (x[i, 2] < W)) {
      firstnot <- i
      break # For better approximation remove this line
    }
  }
  if (firstnot != 0) {
    wsum2 <- x[firstnot, 1]
    vsum2 <- x[firstnot, 2]
    elements_ix2 <- c(firstnot)
    for (i in sorting) {
      if ((firstnot != i) && ((wsum2 + x[i, 1]) <= W)) {
        wsum2 <- wsum2 + x[i, 1]
        vsum2 <- vsum2 + x[i, 2]
        elements_ix2 <- append(elements_ix2, i)
      } else if (firstnot != i) {
        # For better approximation remove this else if block
        break
      }
    }
    ifelse(vsum >= vsum2,
           return(list(
             value = vsum, elements = elements_ix
           )),
           return(list(
             value = vsum2, elements = elements_ix2
           )))
  } else{
    return(list(value = vsum, elements = elements_ix))
  }
}
