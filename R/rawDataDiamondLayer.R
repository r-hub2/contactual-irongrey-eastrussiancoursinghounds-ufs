#' @rdname basicDiamondplotFunctions
#' @export
rawDataDiamondLayer <- function(dat, items = NULL, itemOrder = 1:length(items),
                                dataAlpha = .1,
                                dataColor = "#444444",
                                jitterWidth = .5,
                                jitterHeight = .4,
                                size=3,
                                ...) {

  rawData <-
    stats::na.omit(data.frame(value = unlist(dat[, items[itemOrder]]),
                              labels = rep(1:length(items),
                                           each=nrow(dat))));

  rawDataLayer <-
    ggplot2::geom_jitter(data=rawData,
                         mapping=ggplot2::aes(x=.data$value, y=.data$labels),
                         size = size,
                         color = dataColor,
                         alpha = dataAlpha,
                         stroke = 0,
                         width=jitterWidth,
                         height=jitterHeight,
                         ...);

  return(rawDataLayer);

}



### Avoiding the R CHECK error
if(getRversion() >= "2.15.1") { invisible(utils::globalVariables(".data")); };
