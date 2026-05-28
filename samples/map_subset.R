# Plot maps for a subset of elements of a fair GWR model

# This assumes you have created a "mydata.gwr" object
# as shown in "fairgwr_main.R"

library(ggplot2)
library(fairgwr)

# Load our dataset if not already loaded
filename <- "something.rds"
mydata <- readRDS(filename)

# Generate the base map
mydata.basemap <- gwr.basemap(mydata$geometry)

# Specify the subset of elements we want. In this case, we'll
# be plotting only the elements belonging to the sentitive class.
mydata.subset <- mydata.classes$w.s

# Plot the predicted values for 40% unfairness reduction
gwr.predmap(mydata.basemap,
            mydata$geometry,
            mydata.gwr[["0.4"]]$pred,
            subset.i = mydata.subset)

# Do the same for the beta maps, this time for 20% unfairness reduction
mydata.betamaps_subset <- gwr.betamaps(mydata.basemap,
                                       mydata$geometry,
                                       mydata.gwr[["0.2"]]$solution,
                                       subset.i = mydata.subset)

# Visualize the S/NS classes in a map with the colors we want
gwr.highlightmap(mydata.basemap,
                 mydata$geometry,
                 subsets = list(mydata.classes$w.s,
                                mydata.classes$w.ns),
                 fill.colors = c("orange", "blue"))

