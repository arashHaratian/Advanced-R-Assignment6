#' Brute force function for solving knapsack problem
#'
#' @param x A `data.frame` containing `w`, `v` representing the weights and values for each item
#' @param W The weight limit of the knapsack
#' @param parallel Default to `FALSE`. For running the Parallelize brute force search.
#'
#'
#' @return A list containing the `elements` and the best `value`
#' @export
#'
#' @examples
#' data(knapsack_objects)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#'
brute_force_knapsack <- function(x, W, parallel = FALSE) {
  stopifnot({
    is.data.frame(x)
    is.numeric(x[[1]])
    is.numeric(x[[2]])
    names(x) %in% c("v", "w")
    is.vector(x=W,mode="numeric")
    length(W)==1
    W > 0
  })

  if(parallel){
    w <- x$w
    v <- x$v
    future::plan("multicore", workers = future::availableCores)
    vals <- furrr::future_map(1:(2^nrow(x)-1), ~{
      idx <- which(intToBits(.x) == 1)
      if(sum(w[idx]) <= W){
        sum(v[idx])
      }else{
        0
      }
    })

    best <- which.max(unlist(vals))
    elements <- which(intToBits(best) == 1)
    return(list(
      "value" = max(unlist(vals)),
      "elements" = elements
    ))
  }

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
  is.numeric(x[["w"]])
  is.numeric(x[["v"]])
  names(x) %in% c("v", "w")
  is.vector(x=W,mode="numeric")
  length(W)==1
  W > 0
  })

  dynamic_knapsack <- function(i, j) {

    if (i == 1) {
      return()
    }
    # test <- memoise(max_val)
    # test <- memoise(dynamic_knapsack)

    if (m[i, j] > m[i - 1, j]) {
      dynamic_knapsack(i - 1, j - x[i, "w"])
      ln <<- ln + 1
      elements_knapsack[ln] <<- i
      return()
    }  else{
      dynamic_knapsack(i - 1, j)
      return()
    }
  }
  n <- length(rownames(x))

  m <- matrix(0, nrow = n, ncol = W)

  for(i in 2:n){
    for(j in 1:W){
      if((x[i,"w"] >= j)){
        m[i,j] <- m[i-1,j]
      } else{
        m[i,j] <- max(m[i-1,j],m[i-1,j-x[i,"w"]]+x[i,"v"])
      }
    }
  }

  elements_knapsack <- rep(0,n)
  ln <- 0
  dynamic_knapsack(n,W)
  return(list(value = m[n,W],elements = elements_knapsack[1:ln]))
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

#' Greedy Knapsack Improved
#'
#' @param x A `data.frame` containing `w`, `v` representing the weights and values for each item
#' @param W The weight limit of the knapsack
#'
#' @return a `list` containing the `elements` and the best `value`
#' @export
#'
#' @examples
#' data(knapsack_objects)
#' greedy_knapsack2(x = knapsack_objects[1:800,], W = 3700)
#'
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
