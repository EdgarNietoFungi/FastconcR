#' Getting_DC

#' @export

DC_2 <-  function(filename){
  data <-filename
  data1 <-  data %>% select(logEC50, contains("r2")) %>% max.col()
  data2 <- data1[1]
  data3 <- names(data)
  data3[data2[1]]
}
