
#' add_groups
#' @export

add_groups <- function(filename, group1, group2) {
  data <- filename
  ID <- data$ID

  data2 <- data  %>%
    mutate(group = ifelse(
      ID %in% group1,
      "group1",
      ifelse(
        ID %in% group2,
        "group2","group3"
      ))) %>%
    mutate(
      group = as.factor(group))

}
