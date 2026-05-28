# Copyright 2023-2024 Pepa Ramirez-Cobo and Ismael Montero

# All the functions related to plotting maps and functions
# are in this file

#' @title Fair Geographically Weighted Regression
#'
#' @description Generate the base map only containing the outline of the regions
#'
#' @param geometry a geometry object from the original data as given by sf::st_geometry()
#' @importFrom ggplot2 ggplot geom_sf
#' @return a ggplot map
#' @export
gwr.basemap <- function(geometry) {
    return(ggplot() + geom_sf(data = geometry, fill = NA))
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Generate maps for a GWR Beta (n x p) matrix
#'
#' @param basemap the original map of the data as returned by gwr.basemap() or,
#'      alternatively, a compatible ggplot() object
#' @param geometry a geometry object from the original data as given by sf::st_geometry()
#' @param betas a Beta (n x p) matrix
#' @param breaks a numeric vector containing the break points for the plotted values
#' @param titles a character vector of length p containing the title for the
#'      p plots, use NULL for no title
#' @param legend.titles a character vector of length p containing the title for the
#'      legend of the p plots, use NULL for no legend title
#' @param subset.i (optional) the subset of areas to color as an integer vector
#' @importFrom sf st_as_sf st_crs
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual ggtitle
#' @return a list of length p containing a ggplot map for every beta column
#' @export
gwr.betamaps <- function(basemap,
                         geometry,
                         betas,
                         breaks = NULL,
                         titles = NULL,
                         legend.titles = NULL,
                         subset.i = NULL) {
    if (!is.null(subset.i)) {
        geometry <- geometry[subset.i]
        betas <- betas[subset.i, ]
    }
    betas_df <- st_as_sf(as.data.frame(cbind(betas,
                                             geometry)),
                         crs = st_crs(geometry))
    beta_maps <- lapply(1:ncol(betas), function(j) {
        if (is.null(breaks)) j_breaks <- quantile(betas[, j], seq(from = 0, to = 1, by = 1/8))
        else j_breaks <- breaks[, j]
        colorscale <- gwr.colors(betas[, j], j_breaks)
        if (inherits(geometry, c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
            return(basemap +
                       ggtitle(titles[j]) +
                       geom_sf(data = betas_df, mapping = aes(fill = colorscale$cuts)) +
                       scale_fill_manual(values = colorscale$colors,
                                         name = legend.titles[j]))
        }
        if (inherits(geometry, "sfc_POINT")) {
            return(basemap +
                       ggtitle(titles[j]) +
                       geom_sf(data = betas_df, mapping = aes(color = colorscale$cuts)) +
                       scale_color_manual(values = colorscale$colors,
                                          name = legend.titles[j]))
        }
    })
    return(beta_maps)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Generate a colored prediction map
#'
#' @param basemap the original map of the data as returned by gwr.basemap() or,
#'      alternatively, a compatible ggplot() object
#' @param geometry a geometry object as given by sf::st_geometry()
#' @param y a vector of length n or a (n x 1) matrix containing the observed values
#' @param breaks a numeric vector containing the break points for the plotted values
#' @param title (optional) the plot title
#' @param legend.title (optional) the legend title
#' @param color.scales (optional) a list containing a "highs" and "lows" value for the
#'   colors to use in the plot, as needed by gwr.colors()
#' @param color.rev whether to use inversed colors (FALSE by default)
#' @param subset.i (optional) the subset of areas to color as an integer vector
#' @importFrom sf st_as_sf st_crs
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual scale_color_manual ggtitle
#' @return the plotted map with colored predictions per tract
#' @export
gwr.predmap <- function(basemap,
                        geometry,
                        y,
                        breaks = NULL,
                        title = NULL,
                        legend.title = NULL,
                        color.scales = NULL,
                        color.rev = FALSE,
                        subset.i = NULL) {
    if (!is.null(subset.i)) {
        y <- matrix(y[subset.i])
        geometry <- geometry[subset.i]
    }
    pred_df <- st_as_sf(as.data.frame(cbind(y, geometry)),
                        crs = st_crs(geometry))
    if (is.null(color.scales)) colorscale <- gwr.colors(y, breaks)
    else colorscale <- gwr.colors(y, breaks,
                                  color.scales$highs,
                                  color.scales$lows,
                                  color.rev)
    if (inherits(geometry, c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
        pred_map <- basemap +
                        geom_sf(data = pred_df, mapping = aes(fill = colorscale$cuts)) +
                        scale_fill_manual(values = colorscale$colors,
                                          name = legend.title, drop = FALSE) +
                        ggtitle(title)
    }
    if (inherits(geometry, "sfc_POINT")) {
        pred_map <- basemap +
                        geom_sf(data = pred_df, mapping = aes(color = colorscale$cuts)) +
                        scale_color_manual(values = colorscale$colors,
                                           name = legend.title, drop = FALSE) +
                        ggtitle(title)
    }
    return(pred_map)
}

#' @title Fair Geographically Weighted Regression
#'
#' @description Generate a map highlighting the specified areas, useful e.g.
#'      when illustrating methods such as k-fold CV
#'
#' @param basemap the original map of the data as returned by gwr.basemap() or,
#'      alternatively, a compatible ggplot() object
#' @param geometry a geometry object as given by sf::st_geometry()
#' @param subsets the indexes of the subsets to be plotted, as a list
#'  of integer vectors
#' @param colors the color for every subset, as a character vector
#' @importFrom sf st_as_sf st_crs st_as_text
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual guides
#' @return the plotted map with colored predictions per tract
#' @examples
#' # Example for two colors on some data
#' subsets <- list(c(1,2,3), c(4,5,6))
#' fill.colors <- c("lightgrey", "lightyellow")
#' gwr.highlightmap(mydata.basemap,
#'                  mydata$geometry,
#'                  subsets,
#'                  fill.colors)
#' @export
gwr.highlightmap <- function(basemap,
                             geometry,
                             subsets,
                             fill.colors) {
    s <- length(subsets)
    hl.map <- basemap

    all_geom <- do.call(rbind, lapply(1:s, function(s.i) {
        sub.geometry <- geometry[subsets[[s.i]]]
        data.frame(geometry = st_as_text(sub.geometry),
                   fill_color = fill.colors[s.i])
    }))
    
    fill_df <- st_as_sf(all_geom,
                        wkt = "geometry",
                        crs = st_crs(geometry))
    hl.map <- hl.map +
              geom_sf(data = fill_df,
                      mapping = aes(fill = fill_color)) +
              scale_fill_manual(values = fill.colors) +
              guides(fill = "none")
    return(hl.map)
}
