#' getting RG of serial dilution
#' @export
getting_RG <- function(filename){
  data <-filename

  data1<- data %>% group_by(ID, dose) %>% summarise(mean_response = mean(response, na.rm = TRUE)) %>%
    ungroup() %>%
    spread(dose, mean_response) %>%
    dplyr::rename(control = `0`)  #taking out "different host" becuse does not belong to drybean nor soybean
  #filter(!ID == 8, !ID == 129)


  data2 <- as_data_frame( lapply(data1,function(x) {x[1] ; x}))
  # data3 <- data.frame(data2)
  # data4 <-  lapply(data3,name <- function(variables) {
  # })

  colnames(data2)[-c(1:2)] <- paste("RG_", colnames(data2[,-c(1:2)]), sep = "")
  data3 <- data2 %>% mutate_each(funs((./control)*100), starts_with("RG_")) %>%
    select(-c(control))
  data4 <- data3 %>% left_join(getting_EC50(data)) %>%  # normalizing data
    rename(EC50 = Estimate.50) %>% # #Since here EC50 is known as Estimate EC50 from EC50 table maybe I dont ahve to change it

    mutate(logEC50 = (log(EC50))) %>%
    mutate_each(funs(r2 = summary( lm(logEC50 ~ .))[[9]][[1]]), starts_with("RG_"))



}
