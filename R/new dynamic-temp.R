dynamic2 <- function(x, W) {
  max_val <- function(i, W) {
    if (i == 1 | W <= 1) {
      value_table[i, W] <- 0
      return(value_table[i, W])
    }
    if (value_table [i - 1, W] == -1) {
      value_table[i - 1, W] <- max_val(i - 1, W)
    }

    if (x[i, "w"] > W) {
      value_table[i, W] <- value_table[i - 1, W]
    } else{
      if (value_table[i - 1, W - x[i, "w"]] == -1) {
        value_table[i - 1, W - x[i, "w"]] <-
          max_val(i - 1, W - x[i, "w"])
      }
      value_table[i, W] <-
        max(value_table[i - 1, W], value_table[i - 1, W - x[i, "w"]] + x[i, "v"])
    }
  }

  dynamic_knapsack <- function(x, W) {
    stopifnot({
      is.data.frame(x)
      is.numeric(x[[1]])
      is.numeric(x[[2]])
      names(x) %in% c("v", "w")
      W > 0
    })

    i <- nrow(x)
    if (i == 1) {
      return()
    }
    # test <- memoise(max_val)
    # test <- memoise(dynamic_knapsack)

    if (max_val(i, W) > max_val(i - 1, W)) {
      return(c(i, dynamic_knapsack(x[1:i - 1,], W - x[i, "w"])))
    }  else{
      return(dynamic_knapsack(x[1:i - 1,], W))
    }
  }

  i <- nrow(x)
  value_table <- matrix(-1, nrow = i, ncol = W)
  max_val(i, W)
  elements <- dynamic_knapsack(x, W)

  return(list(
    "value" = sum(x[elements, "v"]),
    "elements" = elements
  ))

}
tik <- Sys.time()
dynamic2(knapsack_objects[1:12, ], 3500)
tok <- Sys.time()
tok-tik

