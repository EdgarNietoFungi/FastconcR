#' Getting_EC50

#' @export


getting_EC50 <- function(filename){

  getting.EC50 <- EC_table(filename, form = response ~ dose)
  getting.EC50 <- getting.EC50 %>%
    dplyr::rename(ID = sample )
  }
