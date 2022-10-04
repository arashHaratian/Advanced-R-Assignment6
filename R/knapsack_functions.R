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

#' Greedy Knapsack
#'
#' @param x The data containing `w`, `v` representing the objects
#' @param W The weight limit of the knapsack
#'
#' @return a `list` containing the `elements` and the best `value`
#' @export
#'
#' @examples
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' 
greedy_knapsack <- function(x,W){
  n <- length(rownames(x))
  
  df <- data.frame(x)
  x$percentage <- x[[2]]/x[[1]]
  sorting <- sort(x$percentage,index.return=TRUE, decreasing = TRUE)$ix
  wsum <- 0
  vsum <- 0
  firstnot <- 0
  elements_ix <- c()
  for(i in sorting){
    if((wsum + x[i,1])<=W){
      wsum <- wsum + x[i,1]
      vsum <- vsum + x[i,2] 
      elements_ix <- append(elements_ix,i)
    }else if((firstnot == 0) && (x[i,2] < W)){
      firstnot <- i
    }
  }
  if(firstnot != 0){
    wsum2 <- x[firstnot,1]
    vsum2 <- x[firstnot,2]
    elements_ix2 <- c(firstnot)
    for (i in sorting){
      if((firstnot != i) && ((wsum2 + x[i,1])<=W)){
        wsum2 <- wsum2 + x[i,1]
        vsum2 <- vsum2 + x[i,2] 
        elements_ix2 <- append(elements_ix2,i)
      }
    }
    ifelse(vsum >= vsum2,
           return(list(value=vsum,elements=elements_ix)),
           return(list(value=vsum2,elements=elements_ix2)))
  } else{
    return(list(value=vsum,elements=elements_ix))
  }
}

