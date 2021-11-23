#' getting_EC50DC
#' @export
getting_EC50D_survey <- function(filename, name,vardep, varindep ) {
  data <-filename
  compa <- DC_2(filename)
  data1 <- filename %>% select(ID, logEC50, EC50, ends_with(compa)) %>%  rename(RG = starts_with("RG"))
  data2 <-   lm(paste(vardep, "~", varindep), data = data1)
  data3<- name %>% group_by(ID) %>%
    summarise(
      mean_response = mean(response, na.rm = TRUE),
      mean_control = mean(control, na.rm = TRUE)
    ) %>% ungroup() %>%
    mutate(RG = (mean_response / mean_control) * 100) %>%
    mutate(EC50D = exp (data2 [[1]][[1]] + data2[[1]][[2]] * RG))
}
