#' testing_DC_linear_model_1

#' @export

lineal_model_1 <- function (filename,vardep, varindep,  desiredR2 = 0.8) {
  compa <- DC_2(filename)
  data1 <- filename %>% select(ID, logEC50, EC50, ends_with(compa)) %>%  rename(RG = starts_with("RG"))
  # desiredR2 (0.8 by default).
  data2 <-   lm(paste(vardep, "~", varindep), data = data1)
  data3 <-  summary( lm(paste(vardep, "~", varindep), data = data1))



  if (data3[[9]][[1]] >= desiredR2) {

    data1 %>%mutate(Estimate.50DC =
                      #using the model the intercept and the coefficient of the model
                      exp(data2[[1]][[1]] + data2[[1]][[2]] * RG)) %>%  select(ID, Estimate.50DC, EC50)

  } else {
    print("Not good DC")
  }}
