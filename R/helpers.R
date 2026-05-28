# Copyright 2023-2024 Pepa Ramirez-Cobo and Ismael Montero

# This file contains helper functions used in the main GWR routines

#' @title Fair Geographically Weighted Regression
#'
#' @description Generate the D matrix needed when solving
#'      the GWR problem via quadprog
#' 
#' @param x an X (n x p) design matrix
#' @param w a list of length n containing the weight matrices for our problem,
#'      as returned by wmat.gauss() or wmat.bisquare()
#' @importFrom Matrix sparseMatrix
#' @export
gwr.Dmat <- function(x, w) {
    n <- nrow(x)
    p <- ncol(x)
    i <- rep(1:(n * p), each = p)
    j <- c(sapply(1:n, function(i) {
        idx.start <- (i - 1) * p + 1
        idx.end <- i * p
        rep(seq(idx.start, idx.end), times = p)
    }))
    subD <- lapply(w, function(wi) 2 * t(x) %*% diag(wi) %*% x)
    Dmat <- sparseMatrix(i = i,
                         j = j,
                         x = unlist(subD),
                         dims = rep(n * p, 2))
    return(Dmat)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Generate the d vector needed when solving
#'      the GWR problem via quadprog
#' 
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param w a list of length n containing the weight matrices for our problem,
#'      as returned by wmat.gauss() or wmat.bisquare()
#' @export
gwr.dvec <- function(x, y, w) {
    n <- nrow(x)
    p <- ncol(x)
    subd <- sapply(w, function(wi) 2 * t(y) %*% diag(wi) %*% x)
    dvec <- matrix(subd, 1, n * p)
    return(dvec)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Generate the A matrix needed when solving
#'      the constrained GWR problem via quadprog
#' 
#' @param x an X (n x p) design matrix
#' @param classes a "classes" object as returned by fair.classes()
#' @return an A (n x 2) matrix
#' @export
gwr.Amat <- function(x, classes) {
    n <- nrow(x)
    p <- ncol(x)
    A1 <- A2 <- x
    A1[classes$w.s, ] <- (-1 / classes$n.s) * A1[classes$w.s, ]
    A1[classes$w.ns, ] <- (1 / classes$n.ns) * A1[classes$w.ns, ]
    A2[classes$w.s, ] <- (1 / classes$n.s) * A2[classes$w.s, ]
    A2[classes$w.ns, ] <- (-1 / classes$n.ns) * A2[classes$w.ns, ]
    A1 <- matrix(t(A1))
    A2 <- matrix(t(A2))
    A <- cbind(A1, A2)
    return(A)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Get the objective function value for a GWR problem
#' 
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param w a list of length n containing the weight matrices for our problem,
#'      as returned by wmat.gauss() or wmat.bisquare()
#' @return the value for f
#' @export
gwr.fvalue <- function(x, y, w, betas) {
    n <- nrow(x)
    dvec <- gwr.dvec(x, y, w)
    Dmat <- gwr.Dmat(x, w)
    b <- matrix(t(betas))
    return(as.numeric(-dvec %*% b + 0.5 * t(b) %*% Dmat %*% b +
           sum(sapply(seq(n), function(i) t(y) %*% diag(w[[i]]) %*% y))))
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Get the objective function value for a GWR problem
#' 
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param w a list of length n containing the weight matrices for our problem,
#'      as returned by wmat.gauss() or wmat.bisquare()
#' @return the value for f
#' @export
gwr.fvalue2 <- function(x, y, w, betas) {
    n <- nrow(x)
    nss <- sapply(1:n, function(i) {
        wi <- w[[i]]
        sapply(1:n, function(k) {
            xb <- (x[k, ] %*% betas[i, ])[1]
            return(wi[k] * abs(y[k] - xb)^2)
        })
    })
    return(sum(nss))
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Get a color scale to be used with the plotting functions,
#'      see 'gwr.betamaps' and 'gwr.predmap', where it's used internally
#' 
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param breaks (optional) a numeric vector containing the break points for the
#'   plotted values
#' @param highs (optional) the color scale to use for values higher than 0,
#'   "Reds" by default, or any scale available in RColorBrewer::display.brewer.all()
#' @param lows (optional) the color scale to use for values lower than 0,
#'   "Blues" by default, or any scale available in RColorBrewer::display.brewer.all()
#' @return a list with components
#' \itemize{
#'  \item breaks_cut: a factor object containing the values to plot
#'  \item colors: a vector containing the colors associated to 
#' }
#' @importFrom forcats fct_rev
#' @importFrom RColorBrewer brewer.pal
#' @export
gwr.colors <- function(y,
                       breaks = NULL,
                       highs = "Reds",
                       lows = "Blues",
                       reverse = FALSE) {
    if (is.null(breaks)) breaks <- quantile(y, seq(from = 0, to = 1, by = 1/8))
    breaks_cut <- fct_rev(cut(y, breaks, labels = head(breaks, -1), include.lowest = TRUE))
    levels(breaks_cut) <-
        rev(head(sapply(seq_along(breaks),
            function(i) paste0(round(breaks[i + 1]), " -\n", round(breaks[i]))), -1))
    highs_vec <- as.numeric(levels(breaks_cut)[levels(breaks_cut) >= 0])
    lows_vec <- as.numeric(levels(breaks_cut)[levels(breaks_cut) < 0])
    highs_len <- length(highs_vec)
    lows_len <- length(lows_vec)
    highs <- brewer.pal(ifelse(highs_len < 3, 3, highs_len), highs)[1:highs_len]
    lows <- brewer.pal(ifelse(lows_len < 3, 3, lows_len), lows)[1:lows_len]
    if (highs_len == 0) highs <- NULL
    if (lows_len == 0) lows <- NULL
    if (reverse) color_vec <- c(highs, lows)
    else color_vec <- c(rev(highs), lows)
    return(list(cuts = breaks_cut, colors = color_vec))
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Get the mean squared error (MSE) for a GWR prediction compared to
#'      the original y values
#' 
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param yhat a vector of length n or a (n x 1) matrix containing the estimated values
#' @return the MSE value
#' @export
mse <- function(y, yhat) {
    n <- nrow(y)
    if (is.null(n)) n <- length(y)
    return((1 / n) * sum((y - yhat)**2))
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Get the median squared error (MSE) for a GWR prediction compared to
#'      the original y values
#' 
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param yhat a vector of length n or a (n x 1) matrix containing the estimated values
#' @return the median SE value
#' @export
medse <- function(y, yhat) {
    return(median((y - yhat)**2))
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Show an informative table with data biases to help in the election
#'      of a sensitive variable
#' 
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @return A data frame with p or (p + 1) rows and the following columns
#' \itemize{
#'  \item lower: the mean value for Y on individuals with an observed x_j value
#'      lower than median(x_j)
#'  \item greater: the mean value for Y on individuals with an observed x_j value
#'      greater or equal than median(x_j)
#'  \item u: the "u" measure of variation as returned by fair.u()
#'  \item variation: the percentage of variation between the observations in the
#'      lower/greater class
#'  \item median: the median value for the variable
#' }
#' @export
databias <- function(x, y) {
    all_median <- as.numeric(apply(sevilla$x, 2, median))
    all_data <- data.frame(t(sapply(seq_along(all_median), function(j) {
        jmedian <- all_median[j]
        w.lt <- as.numeric(which(sevilla$x[, j] < jmedian))
        w.gt <- as.numeric(which(sevilla$x[, j] >= jmedian))
        ymean.lt <- mean(sevilla$y[w.lt])
        ymean.gt <- mean(sevilla$y[w.gt])
        return(c(ymean.lt, ymean.gt))
    })))
# get u(x, y)
    nums <- abs(all_data[, 1] - all_data[, 2])
# get variations
    variations <- (apply(all_data, 1, max) - apply(all_data, 1, min)) /
                   apply(all_data, 1, min) * 100
    variations <- round(variations, 2)
# bind it all together
    all_data <- cbind(all_data, nums, variations, all_median)
    colnames(all_data) <- c("lower", "greater", "u", "variation", "median")
    rownames(all_data) <- colnames(x)
    return(all_data)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Simple helper function returning the names for the Beta matrix
#' 
#' @param p the number of columns of our Beta (n x p) matrix (usually returned
#'      by gwr())
#' @param intercept whether the first column is the intercept (beta_0) term
#' @return a character vector containing the new column names
#' @export
gwr.names <- function(p, intercept = TRUE) {
    p.back <- ifelse(intercept, 0, 1)
    beta.names <- sapply(1:p, function(j) paste0("beta", j - 1 + p.back))
    if (intercept) beta.names[1] <- "Intercept"
    return(beta.names)
}
