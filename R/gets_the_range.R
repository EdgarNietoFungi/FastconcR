
#' Gets the range of at least 6 data points
#' @param mynumber is any vector of numbers
#' @return a dataframe with upper and lower data
#' @export

get_range <- function(mynumber) {
  bb <- boxplot.stats(mynumber)
  cc <- bb$stats
  dd <- max(cc)
  ee <- min(cc)
  return(data.frame(upper = dd, lower = ee))
}
