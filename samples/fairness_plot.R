# Create a plot showing the unfairness reductions percentage
# on X and the objective function value on Y

# This assumes you have created a "mydata.gwr" object
# as shown in "fairgwr_main.R"

library(ggplot2)
library(fairgwr)

# Load our dataset if not already loaded
filename <- "something.rds"
mydata <- readRDS(filename)

# In the same way as in "fairgwr_main.R", define a sensitive
# variable, get its index, and classify individuals in S/NS
svar.name <- "x2"
svar.idx <- which(colnames(mydata$x) == svar.name)
mydata.classes <- fair.classes(mydata$x,
                               mydata$y,
                               mydata$d,
                               select_col = svar.idx)

# If needed, get the u values
mydata.u <- sapply(mydata.gwr, function(result) {
    round(fair.u(mydata$xnorm, mydata.classes, result$solution))
})

# Data frame with the fairness percentage and its associated f value
mydata.fvalues <- data.frame(
    fair.perc = as.numeric(names(mydata.gwr)) * 100,
    f.value = round(sapply(mydata.gwr, function(result) {
                           if (is.null(result)) NA
                           else result$value
})))

mydata.msevalues <- data.frame(
    fair.perc = as.numeric(names(mydata.gwr)) * 100,
    mse.value = round(sapply(mydata.gwr, function(result) {
                           if (is.null(result)) NA
                           else mse(mydata$y, result$pred)
})))

mydata.medsevalues <- data.frame(
    fair.perc = as.numeric(names(mydata.gwr)) * 100,
    medse.value = round(sapply(mydata.gwr, function(result) {
                           if (is.null(result)) NA
                           else medse(mydata$y, result$pred)
})))

# Plot F values
ggplot(data = mydata.fvalues,
       mapping = aes(x = fair.perc,
                     y = f.value)) +
   geom_line(color = "red", linewidth = 2) +
   geom_point(color = "red", size = 3) +
   ylab("f value") +
   xlab("Unfairness reduction (%)") +
   theme_minimal()

# Plot MSE values
ggplot(data = mydata.msevalues,
       mapping = aes(x = fair.perc,
                     y = mse.value)) +
   geom_line(color = "red", linewidth = 2) +
   geom_point(color = "red", size = 3) +
   ylab("Mean SE value") +
   xlab("Unfairness reduction (%)") +
   theme_minimal()

# Plot Median SE values
ggplot(data = mydata.medsevalues,
       mapping = aes(x = fair.perc,
                     y = medse.value)) +
   geom_line(color = "red", linewidth = 2) +
   geom_point(color = "red", size = 3) +
   ylab("Median SE value") +
   xlab("Unfairness reduction (%)") +
   theme_minimal()
