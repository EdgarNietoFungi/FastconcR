
#' Reading serial dilution entry
#' @export

reading_data_serial_dilution <- function(filename, length_repeats, plug ){
  data <-  read.csv(filename)
  # data <- subset(dareading_data("data/Benomyl.Colletotrichum.csv")ta, select = -X)
  # length_repeats (4 by default).
  data$repeats <- rep_len(1:length_repeats, length.out = nrow(data))


  #data$ID <- as.numeric(data$ID)
  data <- data %>%
    mutate(polar= replace(polar, polar == 0, plug)) %>%  #replacing 0 cm growth for the size of plug that is 0.6
    mutate(ecuatorial= replace(ecuatorial, ecuatorial == 0, plug)) %>% #replacing 0 cm growth for the size of plug that is 0.6
    # group_by(ID, experimental_replicate, dose, ecuatorial, polar, repeats) %>%
    mutate(growth = ((ecuatorial + polar) / 2)) %>%
    group_by(ID, dose) %>%
    mutate(growth_range = list(get_range(growth))) %>%
    unnest(cols = c(growth_range)) %>%
    filter(growth <= upper & growth >= lower) %>%
    dplyr::rename(response = growth) %>%
    ungroup() %>%
    select(c(ID, experimental_replicate, repeats, dose, response))

}
