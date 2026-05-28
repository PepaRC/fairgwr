# Copyright 2023-2024 Pepa Ramirez-Cobo and Ismael Montero

# This file contains functions related to the calculation of N weight 
# matrices using a distance matrix and a bandwidth (h) parameter

#' @title Fair Geographically Weighted Regression
#'
#' @description Generate a matrix or list of matrices containing gaussian weights
#'      of the form exp{(-1/2) * (d / h) ^ 2}. When the distance between two
#'      points is smaller than the bandwidth, 0 is returned
#' 
#' @param d a distance matrix (n x n)
#' @param h bandwidth parameter
#' @param i the i-th observation for which we need a weight matrix. If NULL (default),
#'      all the observations are considered instead
#' @return a list of n weight matrices, or a single W_i matrix, in vector (non-diagonal) form
#' @export
wmat.gauss <- function(d, h, i = NULL) {
    if (is.null(i)) {
        n <- nrow(d)
        w <- lapply(1:n, function(i) {
            dr <- ifelse(d[i, ] > h, 0, exp(-0.5 * (d[i, ] / h)^2))
            return(dr)
        })
    }
    else {
        w <- ifelse(d[i, ] > h, 0, exp(-0.5 * (d[i, ] / h)^2))
    }
    return(w)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Generate a matrix or list of matrices containing bi-squared weights
#'      of the form {1 - (d/h)^2}^2. When the distance between two points is smaller
#'      than the bandwidth, 0 is returned
#' 
#' @param d a distance matrix (n x n)
#' @param h bandwidth parameter
#' @param i the i-th observation for which we need a weight matrix. If NULL (default),
#'      all the observations are considered instead
#' @return a list of n weight matrices, or a single W_i matrix, in vector (non-diagonal) form
#' @export
wmat.bisquare <- function(d, h, i = NULL) {
    if (is.null(i)) {
        n <- nrow(d)
        w <- lapply(1:n, function(i) {
            dr <- ifelse(d[i, ] > h, 0, (1 - (d[i, ] / h)^2)^2)
        })
    }
    else {
        w <- ifelse(d[i, ] > h, 0, (1 - (d[i, ] / h)^2)^2)
    }
    return(w)
}
