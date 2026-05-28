## Authors
This package was developed by:
- Ismael Montero
- Pepa Ramírez-Cobo
# fairgwr  
An open-source R package implementing fairness-regularized Geographically Weighted Regression (GWR) models for fairness-aware spatial prediction and urban accessibility analysis. The methodology extends the classical GWR framework originally introduced by Brunsdon, Fotheringham and Charlton[^1].
[^1]: Brunsdon, C., Fotheringham, S., & Charlton, M. (1998). Geographically weighted regression. Journal of the Royal Statistical Society: Series D (The Statistician), 47(3), 431-443.

## Installing
Using the `install_github` function from the `remotes` package:
```
remotes::install_github("https://github.com/PepaRC/fairgwr")
```

## Basic usage

Fitting a fair GWR model typically involves the following steps:

- Selecting a sensitive attribute representing the population characteristic for which predictive disparities are to be reduced.
- Defining sensitive and non-sensitive groups according to the selected attribute.
- Specifying the target unfairness-reduction level, expressed as a percentage. Multiple reduction levels can also be considered simultaneously.
- Estimating the optimal bandwidth parameter \(h\), which controls the spatial neighborhood used in the local regression calibration process. Alternatively, the bandwidth can be manually specified when expert knowledge is available. Currently, only fixed-bandwidth GWR models are supported.
- Calibrating the fair GWR model using the selected fairness and bandwidth parameters.

Example scripts illustrating the main functionalities of the package are available in the [samples](samples) folder.

## Functionality

`fairgwr` provides a collection of functions for fitting fairness-regularized geographically weighted regression models to spatial datasets.

Urban accessibility datasets compatible with the package can be generated using the open-source [UrbanIneq](https://github.com/JoseCarlos1611/UrbanIneqDataset) platform, which provides reproducible urban inequality and accessibility datasets at census-tract level.

Alternatively, user-provided datasets can also be employed, provided that the following components are available:

- An \(\mathbf{X}_{(n\times (p+1))}\) design matrix, where the first column corresponds to a vector of ones.
- A \(\mathbf{Y}_{(n\times 1)}\) vector containing the observed values of the response variable.
- A \(\mathbf{D}_{(n\times n)}\) spatial distance matrix between observations.
- Geometry information compatible with the `sf` R package.

### Fair GWR model fitting

A fairness-constrained GWR model can be fitted using the `gwr` function as follows:

```r
gwr(x = mydata$x,
    y = mydata$y,
    d = mydata$d,
    h = 1500,
    wfun = wmat.bisquare,
    fair.perc = 0.1,
    classes = mydata.classes,
    constrained = TRUE)
```

A complete reproducible example illustrating the full calibration workflow is available in [samples/fairgwr_main.R](samples/fairgwr_main.R).

### Spatial visualization utilities

The package provides several visualization functions for displaying predictive variables, predicted accessibility values, and local regression coefficients. In particular, predicted values can be visualized using `gwr.predmap`, whereas spatial distributions of local coefficients can be explored through `gwr.betamaps`.

```r

mydata.basemap <- gwr.basemap(mydata$geometry)

gwr.predmap(mydata.basemap,

            mydata$geometry,

            mydata.gwr[['0.5']]$pred) +

  theme_minimal()

```
<img src="https://ecourbanbayes.uca.es/wp-content/uploads/2024/11/yvalues_show.png" alt="Alt Text" width="450"></img>

Additional visualization examples are available in [samples/map_predictions.R](samples/map_predictions.R) and [samples/map_betas.R](samples/map_betas.R).

### Result visualization

Using `ggplot2`, several graphical summaries can be generated to analyze the behaviour of the fair GWR model under different unfairness-reduction levels.
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

Additional visualization examples are provided in [samples/fairness_plot.R](samples/fairness_plot.R).

<!--
### k-fold Cross-validated GWR model

Functions automating the assignment of train/test subsets and adjusting a k-fold CV GWR are also available. See [samples/cv.R](samples/cv.R) for a complete example.
-->
