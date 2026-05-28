# Copyright 2023-2024 Pepa Ramirez-Cobo and Ismael Montero

options(warn = -1)
cat("This is the fairgwr wizard\n")
library(fairgwr)
sourced <- interactive()
platform <- tolower(Sys.info()["sysname"])

varnames <- list("x1" = "Total population",
                 "x2" = "Mean net income per person (EUR)",
                 "x3" = "Underage (<18) population (%)",
                 "x4" = "Elderly (>64) population (%)",
                 "x5" = "Unemployed population (%)",
                 "x6" = "Foreign population (%)",
                 "x7" = "Loneliness index (%)")

autoargs <- commandArgs(trailingOnly = TRUE)
if (length(autoargs) > 0) {
    if (length(grep("rds", autoargs[1], ignore.case = TRUE)) > 0) RDS <- autoargs[1]
} else {
    cat("Please select an RDS file to work with\n")
    if (platform == "linux") {
        RDS <- tcltk::tk_choose.files()
    } else { RDS <- file.choose() }
}

alldata <- readRDS(RDS)

reqs <- c("x", "y", "d") %in% ls(alldata)
xnorm_avail <- "xnorm" %in% ls(alldata)
if (!all(reqs)) {
    req_str <- paste("Please ensure your data has at least an x, y and d objects",
                     "as shown in the manual and try again")
    stop(req_str)
}

bias <- databias(alldata$x, alldata$y)[-1, ]
rownames(bias) <- sapply(1:nrow(bias), function(i) {
                             paste0("x", i, ": ", varnames[i]) })
# choose a sensitive variable here
biasvar.suggest <- which.max(bias$variation)

biasvar.show_str <- paste("This is the information table for the available",
                          "variables and their variation on the sensitive and",
                          "non-sensitive classes:\n\n")
cat(biasvar.show_str)
print(round(bias, 2))
biasvar.suggest_str <- paste("\nIt is suggested to use",
                             paste0("\"", rownames(bias)[biasvar.suggest],"\""),
                             "as your sensitive variable,",
                             "unless you have already decided on another one\n")
biasvar.choose_str <- paste("Type the variable you want to use as sensitive",
                            "like \"x1\", or hit Enter to use the suggested one: ")
cat(biasvar.suggest_str)

biasvar <- NULL
while (is.null(biasvar)) {
    cat(biasvar.choose_str)
    if (sourced) {
        biasvar.input <- readline()
    } else {
        biasvar.input <- readLines("stdin", n = 1)
    }
    if (biasvar.input == "") biasvar <- biasvar.suggest
    else {
        biasvar.match <- grep(biasvar.input, rownames(bias))
        if (length(biasvar.match) == 0) biasvar <- NULL
        else biasvar <- biasvar.match[1]
    }
}

cat("Classifying on sensitive and non-sensitive classes... ")
alldata.classes <- fair.classes(alldata$x,
                                alldata$y,
                                alldata$d,
                                biasvar + 1)
cat("OK\n")
alldata.u <- fair.u(alldata$x, alldata.classes)
# choose a fairness level here
perc.choose_str <- paste("The current level of unfairness is",
                         paste0(round(alldata.u, 2), ","),
                         "what's the percentage of reduction",
                         "we should apply? (i.e. 10, 20, 40...): ")
fair.perc <- NULL
while (is.null(fair.perc)) {
    cat(perc.choose_str)
    if (sourced) {
        fair.perc_input <- as.numeric(readline())
    } else {
        fair.perc_input <- as.numeric(scan("stdin", character(), n = 1, quiet = TRUE))
    }
    fair.perc <- fair.perc_input / 100
    if (fair.perc_input < 0 || fair.perc_input > 100) {
        cat("Error: The reduction percentage must be between 0 and 100\n")
        fair.perc <- NA
    }
    if (is.na(fair.perc)) fair.perc <- NULL
}

# Use a normalized X if available, else use regular X
xnorm_local <- alldata$x
if (xnorm_avail) xnorm_local <- alldata$xnorm

cat("Finding the optimal bandwidth...\n")
alldata.h <- gwr.h_sel(xnorm_local,
                       alldata$y,
                       alldata$d,
                       wmat.bisquare,
                       fair.perc,
                       alldata.classes,
                       constrained = TRUE,
                       verbose = FALSE,
                       progress = TRUE)$h
#cat("h =", alldata.h, "\n")
cat("Getting Fair GWR results... ")
alldata.gwr <- gwr(xnorm_local,
                   alldata$y,
                   alldata$d,
                   alldata.h,
                   wmat.bisquare,
                   fair.perc = fair.perc,
                   classes = alldata.classes,
                   constrained = TRUE)
alldata.gwr$classes <- alldata.classes
cat("OK\n")
filename <- file.path(dirname(RDS),
                      paste(sub("\\.[^.]*$", "", basename(RDS)),
                            paste0("x", biasvar),
                            fair.perc * 100,
                            "gwr.rds",
                            sep = "-"))
#cat(filename, "\n")
saveRDS(alldata.gwr, file = filename)
cat(paste0("Autogwr is done! Results are available in RDS format at:\n",
           filename, "\n"))
#if (!.Platform$OS.type == "windows") {
if (!sourced) {
    cat("Hit enter to close this window...")
    tmp <- readLines("stdin", n = 1)
}

options(warn = 0)
