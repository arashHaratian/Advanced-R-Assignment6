% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knapsack_functions.R
\name{knapsack_dynamic_old}
\alias{knapsack_dynamic_old}
\title{Old version of dynamic knapsack}
\usage{
knapsack_dynamic_old(x, W)
}
\arguments{
\item{x}{A \code{data.frame} containing \code{w}, \code{v} representing the weights and values for each item}

\item{W}{The weight limit of the knapsack}
}
\value{
A list containing the \code{elements} and the best \code{value}
}
\description{
Old version of dynamic knapsack
}
\examples{
data(knapsack_objects)
knapsack_dynamic_old(knapsack_objects[1:12, ], 3500)

}
