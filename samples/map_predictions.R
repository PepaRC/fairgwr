# Plot maps for the predicted values of a fair GWR model

# This assumes you have created a "mydata.gwr" object
# as shown in "fairgwr_main.R"

library(ggplot2)
library(fairgwr)

# Load our dataset if not already loaded
filename <- "something.rds"
mydata <- readRDS(filename)

# Generate the base map
mydata.basemap <- gwr.basemap(mydata$geometry)

# Get the legend breaks for every possible Y value, so every map
# has the same aspect and legend.
mydata.ybreaks <- quantile(c(as.numeric(sapply(mydata.gwr, function(x) x$pred)),
                             mydata$y), seq(0, 1, 1/8), na.rm = TRUE)

# Store maps in a list. This list will have as many maps as
# unfairness reduction levels have been defined in "fairperc.seq".
mydata.predmaps <- lapply(seq_along(fairperc.seq), function(i) {
    gwr.predmap(mydata.basemap,
                mydata$geometry,
                mydata.gwr[[i]]$pred,
                mydata.ybreaks)
})
names(mydata.predmaps) <- fairperc.seq

# Access them like
mydata.predmaps[["0.2"]] # for 20% unfairness reduction
mydata.predmaps[["0.5"]] # for 50% unfairness reduction

# Save them as png if needed
sapply(seq_along(fairperc.seq), function(i) {
    perc.name <- sprintf("%02.0f", as.numeric(fairperc.seq[i]) * 100)
    plot(mydata.predmaps[[i]])
    filename <- paste0("Y", perc.name, ".png")
    ggsave(filename)
    return()
})

# In case you want to play with colors, the gwr.predmap() function
# exposes parameters to do so. It is possible to assign different color
# scales to positive and negative values, as well as invert them.
# For a comprehensive list of available color scales, use
# RColorBrewer::display.brewer.all()

# The code below will plot the map for 0% unfairness reduction,
# with green values for positive values, and blue for negatives ones.
mydata.predmap_test <- gwr.predmap(mydata.basemap,
                                   mydata$geometry,
                                   mydata.gwr[["0"]]$pred,
                                   color.scales = list(highs = "Greens",
                                                       lows = "Blues"),
                                   color.rev = TRUE)
plot(mydata.predmap_test)
