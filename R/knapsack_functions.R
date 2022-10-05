#' Brute force function for solving knapsack problem
#'
#' @param x A `data.frame` containing `w`, `v` representing the weights and values for each item
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
  stopifnot({
    is.data.frame(x)
    is.numeric(x[[1]])
    is.numeric(x[[2]])
    names(x) %in% c("v", "w")
    is.vector(x=W,mode="numeric")
    length(W)==1
    W > 0
  })

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


#' Dynamic knapsack
#'
#' @param x A `data.frame` containing `w`, `v` representing the weights and values for each item
#' @param W The weight limit of the knapsack
#'
#' @return A list containing the `elements` and the best `value`
#' @export
#'
#' @examples
#' data(knapsack_objects)
#' knapsack_dynamic(knapsack_objects[1:12, ], 3500)
#'
knapsack_dynamic <- function(x, W) {
  stopifnot({
    is.data.frame(x)
    is.numeric(x[[1]])
    is.numeric(x[[2]])
    names(x) %in% c("v", "w")
    is.vector(x=W,mode="numeric")
    length(W)==1
    W > 0
  })
  
  max_val <- function(i, j) {
    if (i == 1 | j <= 1) {
      value_table[i, j] <- 0
      return(value_table[i, j])
    }
    if (value_table [i - 1, j] == -1) {
      value_table[i - 1, j] <- max_val(i - 1, j)
    }
    
    if (x[i, "w"] >= j) {
      value_table[i, j] <- value_table[i - 1, j]
    } else{
      if (value_table[i - 1, j - x[i, "w"]] == -1) {
        value_table[i - 1, j - x[i, "w"]] <-
          max_val(i - 1, j - x[i, "w"])
      }
      value_table[i, j] <-
        max(value_table[i - 1, j], value_table[i - 1, j - x[i, "w"]] + x[i, "v"])
    }
  }
  
  dynamic_knapsack <- function(i, j) {
    
    if (i == 1) {
      return(c())
    }
    # test <- memoise(max_val)
    # test <- memoise(dynamic_knapsack)
    
    if (max_val(i, j) > max_val(i - 1, j)) {
      return(c(i, dynamic_knapsack(i - 1, j - x[i, "w"])))
    }  else{
      return(dynamic_knapsack(i - 1, j))
    }
  }
  
  n <- nrow(x)
  value_table <- matrix(-1, nrow = n, ncol = W)
  val <- max_val(n, W)
  elements <- dynamic_knapsack(n, W)
  
  return(list(
    "value" = val,
    "elements" = elements
  ))
  
}

#' Greedy Knapsack
#'
#' @param x A `data.frame` containing `w`, `v` representing the weights and values for each item
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
  stopifnot({
    is.data.frame(x)
    is.numeric(x[["w"]])
    is.numeric(x[["v"]])
    names(x) %in% c("v", "w")
    is.vector(x=W,mode="numeric")
    length(W)==1
    W > 0
  })

  n <- length(rownames(x))

  x$percentage <- x[["v"]] / x[["w"]]
  sorting <-
    sort(x$percentage,
         index.return = TRUE,
         decreasing = TRUE)$ix
  
  wsum <- 0
  vsum <- 0
  firstnot <- 0
  elements_ix <- c()
  
  wsum2 <- 0
  vsum2 <- 0
  elements_ix2 <- c()
  
  for (i in sorting) {
    if ((firstnot == 0) && (wsum + x[i, "w"]) <= W) {
      wsum <- wsum + x[i, "w"]
      vsum <- vsum + x[i, "v"]
      elements_ix <- append(elements_ix, i)
    } else if ((wsum2 + x[i, "w"]) <= W) {
      firstnot <- i
      wsum2 <- wsum2 + x[i, "w"]
      vsum2 <- vsum2 + x[i, "v"]
      elements_ix2 <- append(elements_ix2, i)
      
      ifelse(vsum >= vsum2,
             return(list(
               value = vsum, elements = elements_ix
             )),
             return(list(
               value = vsum2, elements = elements_ix2
             )))
      break
    }else if (x[i, "w"] <= W){
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
}

greedy_knapsack2 <- function(x, W) {
  stopifnot({
    is.data.frame(x)
    is.numeric(x[[1]])
    is.numeric(x[[2]])
    names(x) %in% c("v", "w")
    W > 0
  })
  
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
    } else if ((firstnot == 0) && (x[i, 1] <= W)) {
      firstnot <- i
      break
    }
  }
  if (firstnot != 0) {
    wsum2 <- x[firstnot, 1]
    vsum2 <- x[firstnot, 2]
    elements_ix2 <- c(firstnot)
    for (i in sorting){
      if((firstnot != i) && ((wsum2 + x[i,1])<=W)){
        wsum2 <- wsum2 + x[i,1]
        vsum2 <- vsum2 + x[i,2] 
        elements_ix2 <- append(elements_ix2,i)
      }else if(firstnot != i){
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

greedy_knapsack3 <- function(x, W) {
  stopifnot({
    is.data.frame(x)
    is.numeric(x[[1]])
    is.numeric(x[[2]])
    names(x) %in% c("v", "w")
    W > 0
  })
  
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
    } else if ((firstnot == 0) && (x[i, 1] <= W)) {
      firstnot <- i
    }
  }
  if (firstnot != 0) {
    wsum2 <- x[firstnot, 1]
    vsum2 <- x[firstnot, 2]
    elements_ix2 <- c(firstnot)
    for (i in sorting){
      if((firstnot != i) && ((wsum2 + x[i,1])<=W)){
        wsum2 <- wsum2 + x[i,1]
        vsum2 <- vsum2 + x[i,2] 
        elements_ix2 <- append(elements_ix2,i)
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