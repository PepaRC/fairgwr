# Copyright 2023-2024 Pepa Ramirez-Cobo and Ismael Montero

#' @title Fair Geographically Weighted Regression
#'
#' @description Return the CV-score value for the given parameters. This function
#'      is used when optimizing the bandwidth (h) parameter, hence the need to
#'      have it as first parameter
#'
#' @param h bandwidth parameter
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param d a distance matrix (n x n)
#' @param wfun weighting function
#' @param cv use the cross-validated score function (TRUE by default)
#' @param fair.perc the percentage of unfairness we want to reduce
#' @param classes a "classes" object as returned by fair.classes()
#' @param fold.classes (optional) a subset of the classes parameter as returned
#'  by fair.classes_fold(), in case we need a k-fold CV solution
#' @param constrained whether to run this as a constrained or unconstrained problem. If FALSE,
#'      the "classes" and "fair.perc" parameters will be ignored
#' @return the CV-score value at h
#' @export
gwr.cvscore <- function(h, x, y, d, wfun,
                        cv = TRUE,
                        fair.perc = NULL,
                        classes = NULL,
                        fold.classes = NULL,
                        constrained = FALSE) {
    n <- nrow(x)
    p <- ncol(x)
    if (is.null(wfun) && is.null(wmat)) {
        stop("One of wfun or wmat must be provided")
    }
    w <- wfun(d, h)
# make a cross-validated weight matrix by "removing" the i-th element
    if (cv) for (i in seq_along(w)) w[[i]][i] <- 0
# for the k-fold CV case, also remove the elements in the test fold
    if (!is.null(fold.classes)) w <- foldcv.wmat(w, fold.classes$fold)
# use it to get the cv solution
    sol <- try(gwr(x, y, d, h,
               wmat = w,
               classes = classes,
               fold.classes = fold.classes,
               fair.perc = fair.perc,
               constrained = constrained), silent = TRUE)
    if (inherits(sol, "try-error")) return(NA)
    else {
        score1 <- y - sol$pred # get score
        score2 <- sum(t(score1) %*% score1) # squared
        return(score2)
    }
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Select the optimal bandwidth on a given dataset
#'
#' @param x an X (n x p) design matrix
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param d a distance matrix (n x n)
#' @param wfun weighting function
#' @param verbose show internal calculations
#' @param progress like "verbose", but prettier, can't use when "verbose" = TRUE
#' @param shiny.progress a "progress" object as given by shiny::Progress$new(), to use
#'  with a shiny webapp
#' @param record return the points used in the internal calculations
#' @return a list with components
#' \itemize{
#'  \item h: optimal bandwidth
#'  \item value: CV-score value at h
#'  \item record: (optional) matrix with h and CV score values used
#' }
#' @export
gwr.h_sel <- function(x, y, d,
                      wfun,
                      fair.perc = NULL,
                      classes = NULL,
                      fold.classes = NULL,
                      constrained = FALSE,
                      verbose = TRUE,
                      progress = FALSE,
                      shiny.progress = NULL,
                      record = FALSE) {
    if (verbose == TRUE && progress == TRUE) {
        stop("\"verbose\" and \"progress\" can't be TRUE at the same time")
    }
    h.range <- range(d)
    h.grid <- seq(from = h.range[1],
                  to = h.range[2],
                  by = h.range[2] * 0.01)
# sweep gwr.cvscore on h.grid to get all CV score values for all h
    if (verbose) cat("Sweeping h from", h.range[1], "to", h.range[2], "\n")
    if (progress) {
        progress.bar <- 50
        progress.total <- length(h.grid)
        cursor <- c("\\","|","/","-")
        cat("Step 1: sweeping the h grid\n")
    }
    if (!is.null(shiny.progress)) { shiny.n <- length(h.grid) + 1 }
    cv.values <- sapply(h.grid, function(h) {
        if (progress) {
            progress.i <- which(h.grid == h)
            progress.step <- progress.i / progress.total * progress.bar
            charSpinningCursor <- (progress.i %% 4)+1
            progress.text <- sprintf('%s |%s%s % 3s%%',
                cursor[charSpinningCursor],
                strrep('>', progress.step),
                strrep(' ', progress.bar - progress.step),
                round(progress.i / progress.total * 100, 2)
                #round(30/progress.total*difference*100.00, digits=2)
            )
            cat(progress.text)
            #Sys.sleep(0.2)
            cat(if (progress.step == progress.total) '\n' else '\r')
            flush.console()
        }
        if (!is.null(shiny.progress)) {
            shiny.progress$inc(1 / shiny.n, detail = paste("Evaluating for", round(h, 2)))
        }
        current_val <- gwr.cvscore(h, x, y, d, wfun,
                                   fair.perc = fair.perc,
                                   classes = classes,
                                   fold.classes = fold.classes,
                                   constrained = constrained)
        if (verbose) cat("h =", h, "CV score =", current_val, "\n")
        return(current_val)
    })
    cv.seq <- cbind(h = h.grid, score = cv.values)
    cv.min <- which.min(cv.seq[, 2])
    cv.min_seq <- c(ifelse(cv.min == 1, 1, cv.min - 1),
                    cv.min,
                    ifelse(cv.min == nrow(cv.seq), nrow(cv.seq), cv.min + 1))
    cv.search_region <- as.data.frame(cv.seq[cv.min_seq, ])
    if (verbose) cat("Finding a minimum between",
                     cv.search_region$h[1], "and",
                     cv.search_region$h[3])
    if (progress) cat("\nStep 2: finding a minimum between",
                      cv.search_region$h[1], "and",
                      cv.search_region$h[3])
    if (!is.null(shiny.progress)) {
        shiny.finish_str <- paste("Final step: find a minimum between",
                                  round(cv.search_region$h[1], 2), "and",
                                  round(cv.search_region$h[3], 2))
        shiny.progress$inc(1 / shiny.n, detail = shiny.finish_str)
    }
# choose optimizing method: if NA values are likely to be found, use "Brent",
# otherwise, use "L-BFGS-B"
    if (any(is.na(cv.search_region$score))) optim.method <- "Brent"
    else optim.method <- "L-BFGS-B"
    optim.args <- list(par = cv.search_region$h[2],
                       fn = gwr.cvscore,
                       method = optim.method,
                       lower = cv.search_region$h[1],
                       upper = cv.search_region$h[3],
                       x = x, y = y, d = d, wfun = wfun,
                       fair.perc = fair.perc,
                       classes = classes,
                       fold.classes = fold.classes,
                       constrained = constrained)
    h.sol <- try(do.call(optim, optim.args), silent = TRUE)
    if (inherits(h.sol, "try-error")) {
# corner case: NA values are found while optimising but L-BFGS-B was chosen
        if (optim.method == "L-BFGS-B") {
            optim.args$method <- "Brent"
            h.sol <- try(do.call(optim, optim.args), silent = TRUE)
            #h.sol <- do.call(optim, optim.args)
        }
        if (inherits(h.sol, "try-error")) retlist <- list(h = NA, value = NA)
        else retlist <- list(h = h.sol$par, value = h.sol$value)
    }
    else retlist <- list(h = h.sol$par, value = h.sol$value)
    if (record) retlist$record <- cv.seq
    if (verbose || progress) cat(": ", h.sol$par, "\n")
    return(retlist)
}
