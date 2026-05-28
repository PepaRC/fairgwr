# fairgwr
An R package adding fairness constraints to the Geographically Weighted Regression model, originally developed by Brunsdon, Fotheringham and Charlton[^1].
[^1]: Brunsdon, C., Fotheringham, S., & Charlton, M. (1998). Geographically weighted regression. Journal of the Royal Statistical Society: Series D (The Statistician), 47(3), 431-443..

## Installing
Using the `install_github` function from the `remotes` package:
```
remotes::install_github("https://github.com/PepaRC/fairgwr")
```

## Basic usage
Adjusting a Fair GWR model usually implies the following steps:
- Selecting a sensitive variable. This is the variable we want to reduce disparities in.
- Classifying elements in sensitive and non-sensitive classes, according to their sensitive variable values.
- Specify the level of unfairness reduction we want, as a percentage. The use of multiple values is also supported.
- Calculate the optimal bandwidth, or $h$ parameter. This parameter will determine how many points are used in the regression model. In case we have expert knowledge on the matter, it can be manually specified. Currently, only fixed bandwidth is supported, i.e. the same bandwidth is used for all points.
- Adjust a Fair GWR model with the parameters provided above.
Sample routines showcasing the pacakge's functions are found in the [samples](samples) folder.

## Functionality
fairgwr exposes a set of functions aiding in the process of adjusting a (fair) GWR model to a spatial dataset.
Datasets can be obtained using the routines shown in the [Unfair urban data](https://github.com/jimontero4/unfair-urban-data) repository.
Alternatively, a user-provided dataset can be used provided the following are available:
- An $\mathbf{X}_{(n\times p+1)}$ design matrix, where the first column is a $\mathbf{1}$ vector.
- A $Y_{(n\times 1)}$ matrix with the observed values for the variable of interest.
- A $D_{(n\times n)}$ distance matrix between points.
- geometry data compatible with the `sf` R package.

### Fair GWR model adjustment
Using the `gwr` function:
```
gwr(x = mydata$x,
    y = mydata$y,
    d = mydata$d,
    h = 1500,
    wfun = wmat.bisquare,
    fair.perc = 0.1,
    classes = mydata.classes,
    constrained = TRUE)
```
See the complete process in [samples/fairgwr_main.R](samples/fairgwr_main.R).

### Map plotting
Different functions are provided depending on the features we want to plot: predictive variables, predicted values (using `gwr.predmap`) or beta coefficients (using `gwr.betamaps`).
```
mydata.basemap <- gwr.basemap(mydata$geometry)
gwr.predmap(mydata.basemap,
            mydata$geometry,
            mydata.gwr[['0.5']]$pred) + theme_minimal()
```
<img src="https://ecourbanbayes.uca.es/wp-content/uploads/2024/11/yvalues_show.png" alt="Alt Text" width="450"></img>

More on plots can be seen in [samples/map_predictions.R](samples/map_predictions.R) or [samples/map_betas.R](samples/map_betas.R).

### Result plotting
With the help of `ggplot2`, graphics such as an evolution for the objective function value depending on the desired fairness can be plotted:
```
mydata.fvalues <- data.frame(
    fair.perc = as.numeric(names(mydata.gwr)) * 100,
    f.value = round(sapply(mydata.gwr, function(result) {
                           if (is.null(result)) NA
                           else result$value
})))
ggplot(data = mydata.fvalues,
       mapping = aes(x = fair.perc,
                     y = f.value)) +
   geom_line(color = "red", linewidth = 2) +
   geom_point(color = "red", size = 3) +
   ylab("f value") +
   xlab("Unfairness reduction (%)") +
   theme_minimal()
```
<img src="https://ecourbanbayes.uca.es/wp-content/uploads/2024/11/fvalues_show.png" alt="Alt Text" width="450"></img>

More plots are shown in [samples/fairness_plot.R](samples/fairness_plot.R).

<!--
### k-fold Cross-validated GWR model

Functions automating the assignment of train/test subsets and adjusting a k-fold CV GWR are also available. See [samples/cv.R](samples/cv.R) for a complete example.
-->
