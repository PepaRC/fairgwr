# Create a scatterplot with showing which estimated values are above or
# below their Ytrue value. Also plot a map locating them

# This assumes you have created a "mydata.gwr" object
# as shown in "fairgwr_main.R"

library(ggplot2)
library(fairgwr)

# Reduced data frame with Ytrue and their Y_50 prediction
mydata.y50comp <- data.frame(ytrue = mydata$y,
                             ypred50 = mydata.gwr[["0.5"]]$pred)

# Get both the sensitive and non-sensitive subsets
mydata.y50comp_s <- mydata.y50comp[mydata.classes$w.s, ]
mydata.y50comp_ns <- mydata.y50comp[mydata.classes$w.ns, ]

# How many non-sensitive items items have a bigger Ytrue than Y_50?
sum(mydata.y50comp_ns[, 1] > mydata.y50comp_ns[ ,2])
# Same, in percentage form
sum(mydata.y50comp_ns[, 1] > mydata.y50comp_ns[ ,2]) / mydata.classes$n.ns

# Map highlighting the non-sensitive items
gwr.highlightmap(mydata.basemap,
                 mydata$geometry,
                 subsets = list(as.numeric(mydata.classes$w.ns)),
                 fill.colors = c("darkgrey")) +
    theme_void()#+
    #theme(legend.position = c(0.85, 0.25))
ggsave("mydata_non-sensitive.png")

# Scatterplot comparing Ytrue and Y_50 for non-sensitive items
par(mgp = c(2, 0.5, 0))
plot(x = mydata.y50comp_ns$ytrue,
     y = mydata.y50comp_ns$ypred50,
     xlab = expression(y[i] ~ " | " ~ i %in% S),
     ylab = expression(hat(y)[i] ~ " | " ~ i %in% S))
abline(0, 1)

# How many sensitive items items have a bigger Ytrue than Y_50?
sum(mydata.y50comp_s[, 1] > mydata.y50comp_s[ ,2])
# Same, in percentage form
sum(mydata.y50comp_s[, 1] > mydata.y50comp_s[ ,2]) / mydata.classes$n.s

# Scatterplot comparing Ytrue and Y_50 for sensitive items
par(mgp = c(2, 0.5, 0))
plot(x = mydata.y50comp_s$ytrue,
     y = mydata.y50comp_s$ypred50,
     xlab = expression(y[i] ~ " | " ~ i %in% N-S),
     ylab = expression(hat(y)[i] ~ " | " ~ i %in% N-S))
abline(0, 1)

# Which non-sensitive items have Y_50 >= Ytrue?
# g1 means "group 1" here
mydata.y50comp_ns_g1 <- mydata.y50comp_ns[mydata.y50comp_ns$ypred50 >=
                                          mydata.y50comp_ns$ytrue, ]
mydata.y50comp_ns_g1 <- which(rownames(mydata.y50comp) %in% rownames(mydata.y50comp_ns_g1))

# Which non-sensitive items have Y_50 < Ytrue?
# g2 means "group 2" here
mydata.y50comp_ns_g2 <- mydata.y50comp_ns[mydata.y50comp_ns$ypred50
                                            < mydata.y50comp_ns$ytrue, ]
mydata.y50comp_ns_g2 <- which(rownames(mydata.y50comp) %in% rownames(mydata.y50comp_ns_g2))

# Plot in map: group 1 as light grey, group 2 as dark grey
gwr.highlightmap(mydata.basemap,
                 mydata$geometry,
                 subsets = list(mydata.y50comp_ns_g1,
                                mydata.y50comp_ns_g2),
                 fill.colors = c("lightgrey", "darkgrey")) + theme_void()
ggsave("ypred50_comp.png")
