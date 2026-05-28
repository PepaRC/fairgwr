# Copyright 2023-2024 Pepa Ramirez-Cobo and Ismael Montero

# Fairness-related functions go in here

#' @title Fair Geographically Weighted Regression
#'
#' @description Get which observations belong to the sensitive or non-sensitive
#'      class in a given dataset
#' 
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param d a distance matrix (n x n)
#' @param select_col the index of the column in X containing the sensitive variable
#' @param select_val the value that will split observations between sensitive and 
#'      non sensitive class, usually the median is used
#' @return a list with components
#' \itemize{
#'  \item w.s: indices for the sensitive class observations
#'  \item w.ns: indices for the non-sensitive class observations
#'  \item n.s: number of observations in the sensitive class
#'  \item n.ns: number of observations in the non-sensitive class
#'  \item y.s: observations in the sensitive class
#'  \item y.ns: observations in the non-sensitive class
#' }
#' @export
fair.classes <- function(x, y, d,
                         select_col,
                         select_val = NULL) {
    if (is.null(select_val)) select_val <- median(x[, select_col])
# get which elements belong to each class
# .s = sensitive class
# .ns = non-sensitive class
    w.s <- as.numeric(which(x[, select_col] < select_val))
    w.ns <- as.numeric(which(x[, select_col] >= select_val))
    n.s <- length(w.s)
    n.ns <- length(w.ns)
    y.s <- y[w.s]
    y.ns <- y[w.ns]
    return(list(w.s = w.s, w.ns = w.ns,
                n.s = n.s, n.ns = n.ns,
                y.s = y.s, y.ns = y.ns))
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Get the epsilon value as the difference between the means of the
#'      observed values in the sensitive and non-sensitive classes
#' @param classes a "classes" object as returned by fair.classes()
#' @param fair.perc the percentage of unfairness we want to reduce
#' @return the epsilon value
#'
#' @export
fair.epsilon <- function(classes, fair.perc = 0) {
    if (fair.perc < 0 || fair.perc > 1) stop("fair.perc must be a percentage between 0 and 1")
    return(abs(mean(classes$y.s) - mean(classes$y.ns)) * (1 - fair.perc))
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Get the "u" metric of variation between subjects
#'      belonging to the sensitive or non-sensitive class
#' 
#' @param x an X (n x p) design matrix
#' @param classes a "classes" object as returned by fair.classes()
#' @param betas (optional) a Beta (n x p) matrix
#' @return the difference in absolute value between the 
#'      estimated means of the non-sensitive and sensitive classes
#' @export
fair.u <- function(x, classes, betas = NULL) {
    if (!is.null(betas)) {
        n <- nrow(betas)
        p <- ncol(betas)
        if (n != nrow(x)) stop("betas and x must have the same number of rows")
        if (p != ncol(x)) stop("betas and x must have the same number of columns")
        y.s <- sapply(classes$w.s,
                      function(i) as.numeric(betas[i, ]) %*% as.numeric(x[i, ]))
        y.ns <- sapply(classes$w.ns,
                       function(i) as.numeric(betas[i, ]) %*% as.numeric(x[i, ]))
    }
    else {
        y.s <- classes$y.s
        y.ns <- classes$y.ns
    }
    mean.s <- mean(y.s)
    mean.ns <- mean(y.ns)
    u <- abs(mean.ns - mean.s)
    return(u)
}
