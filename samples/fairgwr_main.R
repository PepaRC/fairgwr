# This file describes how to generate a set of (fair gwr) results
# provided that we already have a dataset to work with.

library(fairgwr)

# Load our dataset, remember to set your working directory via setwd()
# or using the provided function in RStudio
filename <- "something.rds"
mydata <- readRDS(filename)

# Define our fair percentages grid
fairperc.seq <- seq(from = 0, to = 1, by = 0.1)

# Show a data table to help choose the sensitive variable
databias(mydata$x, mydata$y)

# Split individuals in S/NS
# Select the sensitive variable
svar.name <- "x2"
svar.idx <- which(colnames(mydata$x) == svar.name)
mydata.classes <- fair.classes(mydata$x,
                               mydata$y,
                               mydata$d,
                               select_col = svar.idx)

# Calculate the optimal h for every fairness level using normalized
# data (xnorm), bi-squared weights (wmat.bisquare). record = TRUE
# means the results from every evaluation in the process are kept.
# Using lapply() means a list is generated for every fairness level
# thus obtaining a list of lists with all the results. Depending on
# the dataset, this may take a while. Some warnings may appear after
# running, this is the optimizer not being able to find the solution
# for the problem for certain h values, nothing to worry about.
mydata.h <- lapply(fairperc.seq, function(perc) {
    gwr.h_sel(x = mydata$xnorm,
              y = mydata$y,
              d = mydata$d,
              wfun = wmat.bisquare,
              fair.perc = perc,
              classes = mydata.classes,
              constrained = TRUE,
              record = TRUE)
})
# Assign names to this list, so the results can be accesed to by using
# the fairness level used to get them
names(mydata.h) <- fairperc.seq
# If we want to get the results for the 0% unfairness reduction
mydata.h[['0']]
# For 20%
mydata.h[['0.2']]
# Or 100% (if available)
mydata.h[['1']]

# When done, we can simplify these results and get the h values in a single vector
mydata.hvalues <- sapply(mydata.h, function(result) result$h)

# Now that the h values are available, the fair gwr routine can be used
# Use lapply() in the same way as before, but iterating through indexes this time instead
mydata.gwr <- lapply(1:length(fairperc.seq), function(i) {
    result <- try({
        gwr(x = mydata$xnorm,
            y = mydata$y,
            d = mydata$d,
            h = mydata.hvalues[i],
            wfun = wmat.bisquare,
            fair.perc = fairperc.seq[i],
            classes = mydata.classes,
            constrained = TRUE)
    }, silent = TRUE)
    if (inherits(result, "try-error")) { return(NULL) }
    return(result)
})
# Assign names to this list in the same way as before
names(mydata.gwr) <- fairperc.seq

# Access the Y predicted values for 50% unfairness reduction
mydata.gwr[['0.5']]$pred
# Access the beta matrix values for 30% unfairness reduction
head(mydata.gwr[['0.3']]$solution)
# Create a matrix with the original Y and estimated Y (10% unfairness reduction)
y10 <- cbind(mydata$y, mydata.gwr[['0.1']]$pred)
