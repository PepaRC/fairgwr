# Copyright 2023-2024 Pepa Ramirez-Cobo and Ismael Montero

# Everything k-fold CV goes in here

#' @title Fair Geographically Weighted Regression
#'
#' @description Get folds from a dataset
#'
#' @param x an X (n x p) design matrix
#' @param nfolds the number of folds
#' @param seed (optional) set a fixed seed so results can be reproduced
#' @return a list with length = folds containing the indexes pertaining to each fold
#' @export
foldcv.split <- function(x, nfolds, seed = NULL) {
    n <- nrow(x)
    if (is.null(n)) stop("x must be some kind of matrix, data.frame or tibble")
    if (nfolds < 2) stop("folds must be at least 2")
    if (!is.null(seed)) set.seed(seed)
    n.seq <- sample(seq(1, n))
    fold.list <- split(n.seq,
                       cut(seq_along(n.seq),
                           nfolds,
                           labels = FALSE))
    fold.list_sorted <- lapply(fold.list, function(fold) sort(fold))
    return(fold.list_sorted)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Adjust a CV (fair) GWR model
#'
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param d a distance matrix (n x n)
#' @param folds.h a numeric vector with the h value for every fold
#' @param wfun weighting function
#' @param folds a list with the indexes of every fold, as returned by foldcv.split()
#' @param fair.perc (optional) the percentage of unfairness we want to reduce
#' @param fold.classes (optional) a list of lists with the sensitive/non-sensitive
#'  elements for each fold, as returned by foldcv.fair_classes()
#' @param constrained whether to run this as a constrained or unconstrained problem.
#'  If FALSE, the "fold.classes" and "fair.perc" parameters will be ignored
#' @return a list with a (fair) GWR solution for each fold, containing
#' \itemize{
#'  \item solution: the solution Beta (n x p) matrix
#'  \item pred: a (n x 1) matrix containing the estimated y values
#'  \item fold: the indexes for the "test" fold
#'  \item fair.perc: the requested percentage of unfairness reduction
#'  \item bandwidth: the bandwidth used for the calculations
#'  \item value: the value of the objective function evaluated at the solution
#'  \item value.train: the value of the objective function for the "train" subset
#'  \item value.test: the value of the objective function for the "test" subset
#' }
#' @export
foldcv.gwr <- function(x, y, d, folds.h,
                       wfun,
                       folds,
                       fair.perc = NULL,
                       fold.classes = NULL,
                       constrained = FALSE) {
    n <- nrow(x)
    folds.n <- length(folds)
# run through all folds
    lapply(seq(folds.n), function(f) {
        fold <- folds[[f]]
        fold.h <- folds.h[f]
        cat("Running fold", paste0(f, "/", folds.n),
            "(h =", paste0(fold.h, ")"))#, "\n")
        if (constrained) cat(paste0(" (", fair.perc * 100, "%"), "fairness)", "\n")
        else cat("(unconstrained)\n")
# get a local weight matrix for each fold and modify its weights to
# hide the test fold from the GWR routine
        fold.w <- foldcv.wmat(wfun(d, fold.h), fold)
        fold.sol <- try(gwr(x, y, d, fold.h,
                        wmat = fold.w,
                        fair.perc = fair.perc,
                        fold.classes = fold.classes[[f]],
                        constrained = constrained), silent = TRUE)
        if (inherits(fold.sol, "try-error")) return(NA)
        else {
            fvalues <- foldcv.fvalue(x, y, d, list(fold.sol), wfun)
            return(list(solution = fold.sol$solution,
                        pred = fold.sol$pred,
                        fold = fold,
                        fair.perc = fair.perc,
                        bandwidth = fold.h,
                        value = fold.sol$value,
                        value.train = fvalues$train,
                        value.test = fvalues$test))
        }
    })
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Get the MSE value for a CV (fair) GWR solution
#'
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param foldgwr.result the result of running k-fold CV fair GWR as returned by
#'  foldcv.gwr()
#' @param fun after getting the MSE for every fold, which value to keep? Usually, mean
#'  is used, but median, min or max can also be considered. Not supplying a function
#'  only means the MSE value for every fold will be returned
#' @param type whether we want the MSE value for the "test" or the "train" subsets
#' @return the MSE values for every fold, or a single value as calculated by "fun"
#' @export
foldcv.mse <- function(x, y, foldgwr.result, fun = NULL,
                       type = c("test", "train")) {
# if there's missing data for any fold, return NA
    if (any(sapply(foldgwr.result, function(result) is.null(names(result))))) return(NA)
    mse.type <- match.arg(type)
    n <- nrow(x)
    folds <- lapply(foldgwr.result, function(result) result$fold)
    folds.n <- length(folds)
    folds.mse <- sapply(seq(folds.n), function(f) {
        if (mse.type == "test") fold <- folds[[f]]
        else fold <- -folds[[f]]
        fold.x <- x[fold, ]
        fold.y <- y[fold]
        fold.yhat <- foldgwr.result[[f]]$pred[fold]
        return(mse(fold.y, fold.yhat))
    })
    if (!is.null(fun)) return(fun(folds.mse))
    else return(folds.mse)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Get the objective function value when running k-fold CV
#'
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param d a distance matrix (n x n)
#' @param foldgwr.result the result of k-fold CV as returned by foldcv.gwr
#' @param wfun weighting function
#' @return an (nfolds x 2) matrix with the values for f on both test and train datasets
#' @export
foldcv.fvalue <- function(x, y, d, foldgwr.result, wfun) {
    if (inherits(x, "data.frame")) x <- as.matrix(x)
    #train.fvalues <- test.fvalues <- numeric()
    fold.n <- length(foldgwr.result)
    values.mat <- t(sapply(foldgwr.result, function(result) {
        fold.test <- result$fold
        fold.train <- seq(nrow(x))[-fold.test]
        fold.h <- result$bandwidth
        fold.betas <- result$solution
        w <- wfun(d, fold.h)
        # weight matrix for "train" dataset
        # set every "test" element to 0
        train.w <- w
        for (i in seq(length(train.w))) {
            train.w[[i]][fold.test] <- 0
        }
        # weight matrix for "test" dataset
        # set every "train" element to 0
        test.w <- w
        for (i in seq(length(test.w))) {
            test.w[[i]][fold.train] <- 0
        }
        return(c(
            gwr.fvalue(x, y, train.w, fold.betas) / length(fold.train),
            gwr.fvalue(x, y, test.w, fold.betas) / length(fold.test)
        ))
    }))
    colnames(values.mat) <- c("train", "test")
    values.df <- as.data.frame(values.mat)
    return(values.df)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Transform a weight matrix to work with the k-fold CV problem
#'
#' @param wmat a weight matrix in list form, as returned by wmat.gauss() or wmat.bisquare()
#' @param fold a numeric vector containing the elements belonging to the "test" fold
#' @return a weight matrix where the elements of the current fold have 0 weight
#' @export
foldcv.wmat <- function(wmat, fold) {
    wmat2 <- wmat
    for (i in seq_along(wmat2)) wmat2[[i]][fold] <- 0
    return(wmat2)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Find the optimal h value for a CV (fair) GWR model
#'
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param d a distance matrix (n x n)
#' @param folds a list with the indexes of every fold, as returned by foldcv.split()
#' @param fair.perc (optional) the percentage of unfairness we want to reduce
#' @param fold.classes (optional) a list of lists with the sensitive/non-sensitive
#'  elements for each fold, as returned by foldcv.fair_classes()
#' @param constrained whether to run this as a constrained or unconstrained problem.
#'  If FALSE, the "fold.classes" and "fair.perc" parameters will be ignored
#' @param verbose show internal calculations
#' @param record return the points used in the internal calculations
#' @return a list with the optimal h values for each fold
#' @export
foldcv.h_sel <- function(x, y, d,
                         wfun,
                         folds,
                         fair.perc = NULL,
                         fold.classes = NULL,
                         constrained = FALSE,
                         verbose = TRUE,
                         record = FALSE) {
    folds.n <- length(folds)
    lapply(seq(folds.n), function(f) {
        cat("Finding h for fold", paste0(f, "/", folds.n))
        if (constrained) cat(paste0(" (", fair.perc * 100, "%"), "fairness)", "\n")
        else cat("(unconstrained)\n")
        test.idx <- folds[[f]]
        gwr.h_sel(x, y, d, wfun,
                  fair.perc = fair.perc,
                  fold.classes = fold.classes[[f]],
                  constrained = constrained,
                  verbose = verbose,
                  record = record)
    })
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Get which observations belong to the sensitive or non-sensitive
#'      class in every fold
#'
#' @param classes a "classes" list, as returned by fair.classes()
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param folds a list with the indexes of every fold, as returned by foldcv.split()
#' @return a list with the sensitive/non-sensitive elements for each fold
#' @export
foldcv.fair_classes <- function(classes, y, folds) {
    lapply(folds, function(fold) {
        w.s <- unlist(sapply(classes$w.s, function(x) if(!(x %in% fold)) x))
        w.ns <- unlist(sapply(classes$w.ns, function(x) if(!(x %in% fold)) x))
        n.s <- length(w.s)
        n.ns <- length(w.ns)
        y.s <- y[w.s]
        y.ns <- y[w.ns]
        return(list(w.s = w.s, w.ns = w.ns,
                    n.s = n.s, n.ns = n.ns,
                    y.s = y.s, y.ns = y.ns,
                    fold = fold))
    })
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Unfinished Fold-CV documentation, subject to change
#'
#' @param x an X (n x p) design matrix
#' @return TODO (4)
#' @export
foldcv.y <- function(y, foldgwr.result) {
    n <- nrow(y)
    if (is.null(n)) n <- length(y)
    y.all <- sapply(foldgwr.result, function(fold.result) {
        if ("pred" %in% names(fold.result)) fold.result$pred
        else matrix(NA, n, 1)
    })
    #print(y.all)
    y.new <- matrix(NA, n, 1)
    for (j in seq(ncol(y.all))) {
        fold.col <- y.all[, j]
        if (all(is.na(fold.col))) next
        else {
            fold <- foldgwr.result[[j]]$fold
            y.new[fold] <- fold.col[fold]
        }
    }
    return(y.new)
}
