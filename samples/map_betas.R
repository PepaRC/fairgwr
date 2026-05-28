# Generate maps for the beta values of a fair GWR model

# This assumes you have created a "mydata.gwr" object
# as shown in "fairgwr_main.R"

library(ggplot2)
library(fairgwr)

# Load our dataset if not already loaded
filename <- "something.rds"
mydata <- readRDS(filename)

# Generate the base map
mydata.basemap <- gwr.basemap(mydata$geometry)

# (CASE 1) Generate all the beta maps for a given justice level, say 50%
mydata.betamaps <- gwr.betamaps(mydata.basemap,
                                mydata$geometry,
                                mydata.gwr[["0.5"]]$solution)
# Plot all of them
for (i in 1:length(mydata.betamaps)) {
    x11()
    #plot(mydata.betamaps[[i]] + theme_minimal()) # basic grid
    #plot(mydata.betamaps[[i]] + theme_void()) # grid-less
    plot(mydata.betamaps[[i]])
}

# Plot only one of them, reminder that the first map is the one for the
# intercept variable, so the map for "beta1" will be the second one, i.e.:
plot(mydata.betamaps[[2]])
# Save the current plot as png
ggsave("mybetamap-x1.png")

# (CASE 2) Generate all the beta maps for multiple justice levels at once

# Define the breaks for the maps. Doing this will actually ensure that
# the legend stays consistent through all fairness levels, so the map for
# "beta1" for a 10% unfairness reduction will look the same as the one for
# a 70% unfairness reduction.
# The usual way of doing this is, generate every possible result first, then
# calculate the breaks as shown below. This will be an (8 x p) matrix, where
# every column contains the breaks for a certain variable.
mydata.betabreaks <- sapply(seq(ncol(mydata$x)), function(j) {
    quantile(c(unlist(sapply(mydata.gwr, function(x) x$solution[, j]))),
             seq(0, 1, 1/8), na.rm = TRUE)
})

# Get the fairness percentage reduction levels from the results themselves
fairperc.seq <- names(mydata.gwr)

# Store the maps in a list of lists. This is, a list is created for every
# unfairness reduction level, which will contain the p beta maps.
mydata.betamaps <- lapply(seq_along(fairperc.seq), function(p) {
    tmp.betamaps <- gwr.betamaps(mydata.basemap,
                                 mydata$geometry,
                                 mydata.gwr[[p]]$solution,
                                 mydata.betabreaks)
    return(tmp.betamaps)
})
names(mydata.betamaps) <- fairperc.seq

# Plot and store as png if needed
sapply(seq_along(fairperc.seq), function(i) {
    perc.name <- sprintf("%02.0f", as.numeric(fairperc.seq[i]) * 100)
    sapply(seq(length(mydata.betamaps[[i]])), function(b) {
# Uncomment below if plotting is needed
        #plot(mydata.betamaps[[i]][[b]])
        #plot(tmp.betamaps[[i]][[b]] + theme_minimal()) # basic grid
        #plot(tmp.betamaps[[i]][[b]] + theme_void()) # grid-less
# Uncomment the 2 lines below if you need to store the maps as png images
        #filename <- paste0("p", perc.name, "_beta", b - 1, ".png")
        #ggsave(filename)
        return()
        })
    return()
})

# Plot individually
plot(mydata.betamaps[["0"]][[2]])
plot(mydata.betamaps[["0.7"]][[4]])
