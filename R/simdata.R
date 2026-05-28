# Copyright 2023-2024 Pepa Ramirez-Cobo and Ismael Montero

#' @title Fair Geographically Weighted Regression
#'
#' @description Simulate an X (n x 2) matrix
#'
#' @param n the number of elements
#' @return an X (n x 2) matrix
#' @export
sim.getx <- function(n, p) {
    x0 <- rep(1, n)
    x.all <- matrix(runif(n * (p - 1), -(n * p), n * p), n, p - 1)
    colnames(x.all) <- paste0("x", seq(p - 1))
    x <- data.frame(cbind(x0, x.all))
    return(x)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Simulate the coordinates for n points
#'
#' @param n the number of elements
#' @importFrom sf st_sfc st_point
#' @return an X (n x 2) matrix
#' @export
sim.getcoords <- function(n) {
    n.seq <- seq(1, n)
    u <- 0.5 * ((n.seq - 1) %% 25)
    v <- 0.5 * as.integer((n.seq - 1) / 25)
    uv <- st_sfc(lapply(n.seq, function(i) st_point(c(u[i], v[i]))))
    return(uv)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Return the Beta matrix for a simulated dataset
#' 
#' @param x an X (n x p) design matrix
#' @return a Beta (n x p) matrix
sim.getbetas <- function(x, coords) {
    n <- nrow(x)
    p <- ncol(x)
    u <- coords[, 1]
    v <- coords[, 2]
    #beta0 <- runif(n, -(p^3), p^3)
    #betas <- sapply(2:p, function(j) runif(n, -(j^2), j^2))
    #beta0 <- 1 + 4 * sin((1/12) * pi * u)
    beta0 <- 1 + (1/6) * (u + v)
    betas <- sapply(2:p, function(j) 1 + (1/324) * (36 - (6 - u)^j) * (36 - (6 - v)^j))
    #betas <- sapply(2:p, function(j) betas[, j - 1] / (max(betas[, j - 1]) / j))
    #betas <- sapply(p:2, function(j) (u * v) / j)
    #betas <- u / 3
    betas <- cbind(beta0, betas)
    colnames(betas) <- gwr.names(p)
    return(betas)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Meta-function returning a simulated dataset of n points
#' @references Harris, P., Fotheringham, A. S., Crespo, R., & Charlton, M. (2010). The use of geographically weighted regression for spatial prediction: an evaluation of models using simulated data sets. Mathematical Geosciences, 42, 657-680.
#'
#' @param n the number of elements
#' @param p the number of variables (first one being the Intercept variable)
#' @importFrom sf st_as_sf st_distance st_coordinates
#' @importFrom mvtnorm rmvnorm
#' @return a list with components
#' \itemize{
#'  \item data: an st_sfc object containing the simulated data
#'  \item dist: the distance matrix for the simulated data
#'  \item h: the used bandwidth
#'  \item w: the used weight matrices
#'  \item betas: the Beta (n x p) coefficients matrix from which y is generated
#'  \item residuals: residuals used to estimate y, generated from a MVN(0, sigma)
#'  \item sigma: the sigma matrices used to generate the residuals, in vector form
#' }
#' @export
sim.getall <- function(n, p) {
    x <- sim.getx(n, p)
    xmat <- as.matrix(x)
    xnorm <- cbind(x0 = xmat[, 1], scale(xmat[, -1]))
    geometry <- sim.getcoords(n)
    d <- st_distance(geometry)
    # get h so that some nearby points will be used
    h.idx <- round(nrow(d) * 0.5)
    h <- max(sapply(1:nrow(d), function(i) d[i, order(d[i, ])[h.idx]]))
    # use a bisquared kernel to get the W weight matrix
    w <- wmat.bisquare(d, h)
    # get the sigma/omega variance/covariance matrix
    varmat <- lapply(w, function(x) ifelse(1 / diag(x) == Inf, 0, 1 / diag(x)))
    varmat.ret <- lapply(varmat, function(sigma) diag(sigma))
    # generate residuals
    # each row in res.mat matches with the i-th observation
    res.mat <- t(sapply(1:n, function(i) rmvnorm(1, sigma = varmat[[i]])))
    res <- matrix(apply(res.mat, 1, function(row) sample(row, 1)))
    betas <- sim.getbetas(x, st_coordinates(geometry))
    #xb <- xmat %*% t(betas) # old, I think incorrect
    y.init <- matrix(sapply(1:n, function(i) (xmat[i,] %*% betas[i,])[1]))# + res.mat[i,]))
    #y.init <- xb + res
    #y <- matrix(apply(y.init, 2, function(col) sample(col, 1)))
    y <- y.init + res
    xy <- data.frame(x, y)
    xy.norm <- data.frame(xnorm, y)
    simdata <- st_as_sf(xy, geometry)
    simdata.norm <- st_as_sf(xy.norm, geometry)
    return(list(data = simdata,
                data.norm = simdata.norm,
                x = xmat,
                y.init = y.init,
                y = y,
                dist = d,
                h = h,
                w = w,
                betas = betas,
                residuals = res,
                sigma = varmat.ret))
}
