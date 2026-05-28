# This routine shows how to do k-fold CV fair GWR

# This assumes you have created a "mydata.gwr" object
# as shown in "fairgwr_main.R"

library(ggplot2)
library(fairgwr)

# Load our dataset if not already loaded
filename <- "something.rds"
mydata <- readRDS(filename)

# Split the data in 5 folds. Use the "seed" parameter if you need your
# results to be repeatable
mydata.folds <- foldcv.split(mydata$xnorm, nfolds = 5, seed = 1)

# Get the S/NS classes for every fold, based on the previously defined classes
mydata.fold_classes <- foldcv.fair_classes(mydata.classes,
                                            mydata$y,
                                            mydata.folds)

# Calculate the optimal h for every fold and fairness level. Caution!! This is
# the most computationally intensive part of the process and may take a while.
# Using "record = TRUE" will keep the CVSS value for every evaluation, useful
# if we want to plot them later
mydata.foldh <- lapply(fairperc.seq, function(perc) {
    foldcv.h_sel(mydata$xnorm,
                 mydata$y,
                 mydata$d,
                 wmat.bisquare,
                 mydata.folds,
                 perc,
                 mydata.fold_classes,
                 constrained = TRUE,
                 verbose = TRUE,
                 record = TRUE)
})
names(mydata.foldh) <- fairperc.seq

# Extract the h values in matrix form
mydata.foldh_values <- sapply(fairperc.seq, function(perc) {
    sapply(mydata.foldh[[as.character(perc)]], function(fold) fold$h)
})
rownames(mydata.foldh_values) <- paste0("Fold", 1:5)
colnames(mydata.foldh_values) <- paste0(as.character(fairperc.seq * 100), "%")

# Get the k-fold fair GWR solution
mydata.foldgwr <- lapply(seq_along(fairperc.seq), function(i) {
    perc <- fairperc.seq[i]
    folds.h <- as.numeric(mydata.foldh_values[, i])
    foldcv.gwr(mydata$xnorm,
               mydata$y,
               mydata$d,
               folds.h,
               wmat.bisquare,
               mydata.folds,
               perc,
               mydata.fold_classes,
               TRUE)
})
names(mydata.foldgwr) <- fairperc.seq

# Extract the objective function values for the train and test samples
mydata.fold_fvalues <- lapply(mydata.foldgwr, function(result) {
    as.data.frame(t(sapply(result, function(fold.result) {
    if (all(!is.na(fold.result))) {
        data.frame(train = fold.result$value.train,
                   test = fold.result$value.test)
    }
    else data.frame(train = NA, test = NA)
    })))
})
mydata.fold_fvalues <- data.frame(fair.perc = names(mydata.fold_fvalues),
        train = sapply(mydata.fold_fvalues, function(x) mean(unlist(x$train))),
        test = sapply(mydata.fold_fvalues, function(x) mean(unlist(x$test))),
    row.names = NULL)

# Calculate the mean squared error
mydata.fold_msevalues <- data.frame(t(
    sapply(mydata.foldgwr, function(result) {
        c(foldcv.mse(mydata$x, mydata$y, result, mean, "train"),
          foldcv.mse(mydata$x, mydata$y, result, mean, "test"))
})), row.names = NULL)
mydata.fold_msevalues <- cbind(fairperc.seq, mydata.fold_msevalues)
colnames(mydata.fold_msevalues) <- c("fair.perc", "train", "test")

# Calculate the median squared error
mydata.fold_medsevalues <- data.frame(t(
    sapply(mydata.foldgwr, function(result) {
        c(foldcv.mse(mydata$x, mydata$y, result, median, "train"),
          foldcv.mse(mydata$x, mydata$y, result, median, "test"))
})), row.names = NULL)
mydata.fold_medsevalues <- cbind(fairperc.seq,
                                       mydata.fold_medsevalues)
colnames(mydata.fold_medsevalues) <- c("fair.perc", "train", "test")

# Plot f values
ggplot(mapping = aes(x = fairperc.seq)) +
    geom_line(aes(y = mydata.fold_fvalues$test, color = "Test"), size = 2) +
    geom_point(aes(y = mydata.fold_fvalues$test, color = "Test"), size = 3) +
    geom_line(aes(y = mydata.fold_fvalues$train, color = "Train"), size = 2) +
    geom_point(aes(y = mydata.fold_fvalues$train, color = "Train"), size = 3) +
    xlab("Unfairness reduction") + ylab("f value") +
    scale_color_manual(name = "Dataset", values = c("Test" = 2, "Train" = 3)) +
    theme_minimal()#+
    # Uncomment below to reposition the legend if needed
    #theme(legend.position = c(0.9, 0.15),
    #      legend.background = element_rect(linewidth = 0.25))
ggsave("Fvalues.png") # save as png if needed

# Plot mean SE values
ggplot(mapping = aes(x = fairperc.seq)) +
    geom_line(aes(y = mydata.fold_msevalues$test, color = "Test"), size = 2) +
    geom_point(aes(y = mydata.fold_msevalues$test, color = "Test"), size = 3) + 
    geom_line(aes(y = mydata.fold_msevalues$train, color = "Train"), size = 2) +
    geom_point(aes(y = mydata.fold_msevalues$train, color = "Train"), size = 3) +
    xlab("Unfairness reduction") + ylab("Mean SE value") +
    scale_color_manual(name = "Dataset", values = c("Test" = 2, "Train" = 3)) +
    theme_minimal()#+
    #theme(legend.position = c(0.85, 0.25),
    #      legend.background = element_rect(linewidth = 0.25))
ggsave("MeanSEvalues.png")

# Plot median SE values
ggplot(mapping = aes(x = fairperc.seq)) +
    geom_line(aes(y = mydata.fold_medsevalues$test, color = "Test"), size = 2) +
    geom_point(aes(y = mydata.fold_medsevalues$test, color = "Test"), size = 3) + 
    geom_line(aes(y = mydata.fold_medsevalues$train, color = "Train"), size = 2) +
    geom_point(aes(y = mydata.fold_medsevalues$train, color = "Train"), size = 3) +
    xlab("Unfairness reduction") + ylab("Median SE value") +
    scale_color_manual(name = "Dataset", values = c("Test" = 2, "Train" = 3)) +
    theme_minimal()#+
    #theme(legend.position = c(0.85, 0.25),
    #      legend.background = element_rect(linewidth = 0.25))
ggsave("MedianSEvalues.png")
