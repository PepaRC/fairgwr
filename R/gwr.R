# Copyright 2023-2024 Pepa Ramirez-Cobo and Ismael Montero

# All GWR routines are contained here

#' @title Fair Geographically Weighted Regression
#'
#' @description Solve the GWR problem for a unique location
#'
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param d a distance matrix (n x n)
#' @param h bandwidth parameter
#' @param i the i-th location in which we want to run this function
#' @param wfun weighting function
#' @param cv use cross-validation (weight of i is 0)
#' @param Amat the A matrix to use in the constrained problem
#' @param bvec the b vector to use in the constrained problem
#' @param constrained whether this is a constrained or unconstrained problem
#' @importFrom quadprog solve.QP
#' @return a list with components
#' \itemize{
#'  \item solution: the solution beta vector 
#'  \item value: the value of the objective function evaluated at the solution
#'  \item pred: a (n x 1) matrix containing the estimated y values
#'  \item bandwidth: the bandwidth used for the calculations
#'  \item unconstrained.solution: the solution beta vector for the
#'      unconstrained problem
#'  \item unconstrained.pred: a (n x 1) matrix containing the estimated y values
#'      for the unconstrained problem
#' }
#' @export
gwr.simple_i <- function(x, y, d, h, i,
                         wfun,
                         cv = FALSE,
                         #wmat = FALSE,
                         Amat = NULL,
                         bvec = NULL,
                         constrained = FALSE) {
    n <- nrow(x)
    p <- ncol(x)
    w <- wfun(d, h, i)
    if (cv) w[i] <- 0
    dvec <- 2 * t(y) %*% diag(w) %*% x
    Dmat <- 2 * t(x) %*% diag(w) %*% x
    if (!constrained) {
        Amat <- diag(0, p)
        bvec <- rep(0, p)
    }
    sol <- solve.QP(Dmat = Dmat,
                    dvec = dvec,
                    Amat = Amat,
                    bvec = bvec)
    sol$value <- as.numeric(sol$value + (t(y) %*% diag(w) %*% y))
    sol$bandwidth <- h
    #if (wmat) sol$w <- w
    sol$solution <- matrix(sol$solution)
    sol$unconstrained.solution <- matrix(sol$unconstrained.solution)
    sol$pred <- matrix(sapply(1:n, function(i) x[i, ] %*% sol$solution))
    sol$unconstrained.pred <- matrix(sapply(1:n,
                              function(i) x[i, ] %*% sol$unconstrained.solution))
    return(sol)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Solve the GWR problem using an iterative approach
#'
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param d a distance matrix (n x n)
#' @param h bandwidth parameter
#' @param wfun weighting function
#' @param verbose show progress while running
#' @return a list with components
#' \itemize{
#'  \item solution: the solution Beta (n x p) matrix
#'  \item value: the value of the objective function evaluated at the solution
#'  \item pred: a (n x 1) matrix containing the estimated y values
#'  \item bandwidth: the bandwidth used for the calculations
#' }
#' @export
gwr.simple <- function(x, y, d, h,
                       wfun,
                       verbose = FALSE) {
    n <- nrow(x)
    p <- nrow(x)
    if (verbose) cat("i: ")
    all_solution <- lapply(1:n, function(i) {
        if (verbose) cat(i, "")
        #w <- wfun(d, h, i)
        sol <- gwr.simple_i(x, y, d, h, i, wfun)
        return(sol)
    })
    if (verbose) cat("\n")
    sol <- t(sapply(all_solution, function(x) x$solution))
    value <- sum(sapply(all_solution, function(x) x$value))
    pred <- matrix(sapply(1:n, function(i) x[i,] %*% sol[i,]))
    return(list(solution = sol,
                value = value,
                pred = pred,
                bandwidth = h))
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Solve the (constrained) GWR problem using a global approach
#'
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param d a distance matrix (n x n)
#' @param h bandwidth parameter
#' @param centroids (optional) ...
#' @param wfun weighting function
#' @param wmat (optional) use an alternate extended weight matrix, such as when
#'      doing cross-validation or k-fold cross-validation
#' @param fair.perc (optional) the percentage of unfairness we want to reduce
#' @param classes (optional) a "classes" object as returned by fair.classes()
#' @param fold.classes (optional) a subset of the classes parameter as returned
#'  by fair.classes_fold(), in case we need a k-fold CV solution
#' @param constrained whether to run this as a constrained or unconstrained problem. If FALSE,
#'      the "classes" and "fair.perc" parameters will be ignored
#' @importFrom quadprog solve.QP
#' @return a list with components
#' \itemize{
#'  \item solution: the solution Beta (n x p) matrix
#'  \item value: the value of the objective function evaluated at the solution
#'  \item pred: a (n x 1) matrix containing the estimated y values
#'  \item unconstrained.solution: the unconstrained solution Beta (n x p) matrix
#'  \item unconstrained.pred: a (n x 1) matrix containing the estimated y values for
#'      with the unconstrained solution
#'  \item bandwidth: the bandwidth used for the calculations
#' }
#' @export
gwr <- function(x, y, d, h,
                wfun = NULL,
                wmat = NULL,
                fair.perc = NULL,
                classes = NULL,
                fold.classes = NULL,
                constrained = FALSE) {
    n <- nrow(x)
    p <- ncol(x)
    if (is.null(wfun) && is.null(wmat)) {
        stop("One of wfun or wmat must be provided")
    }
    if (inherits(x, "data.frame")) x <- as.matrix(x)
    if (!is.null(wfun)) w <- wfun(d, h)
    if (!is.null(wmat)) w <- wmat
    Dmat <- gwr.Dmat(x, w)
    dvec <- gwr.dvec(x, y, w)
    if (constrained) {
        if (is.null(fold.classes)) {
            e <- fair.epsilon(classes, fair.perc)
            Amat <- gwr.Amat(x, classes)
        }
        # in case we are running k-fold CV, use the provided
        # classes object for the current fold
        else {
            e <- fair.epsilon(fold.classes, fair.perc)
            train.idx <- c(fold.classes$w.s, fold.classes$w.ns)
            test.idx <- seq(n)[-train.idx]
            x.fold <- x
            x.fold[test.idx, ] <- 0
            Amat <- gwr.Amat(x.fold, fold.classes)
        }
        bvec <- rep(-e, 2)
    }
    else {
        Amat <- diag(0, n * p)
        bvec <- rep(0, n * p)
    }
    sol <- solve.QP(Dmat = Dmat,
                    dvec = dvec,
                    Amat = Amat,
                    bvec = bvec)
    sol$bandwidth <- h
    sol$value <- sol$value +
        sum(sapply(seq(n), function(i) t(y) %*% diag(w[[i]]) %*% y))
    sol$solution <- matrix(sol$solution,
                           nrow = n,
                           ncol = p,
                           byrow = TRUE)
    sol$unconstrained.solution <- matrix(sol$unconstrained.solution,
                                         nrow = n,
                                         ncol = p,
                                         byrow = TRUE)
    sol$pred <- matrix(sapply(1:n, function(i) x[i, ] %*% sol$solution[i, ]))
    sol$unconstrained.pred <- matrix(sapply(1:n,
                                     function(i) x[i, ] %*% sol$unconstrained.solution[i, ]))
    colnames(sol$solution) <- colnames(sol$unconstrained.solution) <- gwr.names(p)
    if (!is.null(fold.classes)) {
        sol$fold <- fold.classes$fold
        #foldcv.values <- foldcv.fvalue(x, y, d, list(sol), wfun)
        #sol$value.train <- foldcv.values$train
        #sol$value.test <- foldcv.fvalues$test
    }
    return(sol)
}
