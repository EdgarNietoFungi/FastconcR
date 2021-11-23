#' reading_data_discriminatory_concentrations
#' @export
reading_data_discriminatory_concentration <- function(filename, length_repeats, plug) {
  data <-read.csv(filename)
  # data <- subset(data, select = -X)
  #data$repeats <- rep_len(1:3, length.out = nrow(data))
  # renaming ID for the current names
  # length_repeats (3 by default).
  data$repeats <- rep_len(1:length_repeats, length.out = nrow(data))
  #data$ID <- as.numeric(data$ID)
  data <- data %>%
    mutate(fungicide_growth = ((fungicide_ecuatorial + fungicide_polar) / 2),
           control_growth = ((control_ecuatorial + control_polar) / 2)) %>%
    dplyr::rename(response = fungicide_growth, control = control_growth) %>%
    select(c(ID, experimental_replicate, repeats, response, control)) %>%

    mutate(response = replace(response, response == 0, plug)) %>%
    #replacing 0 cm growth for the size of plug that is 0.6
    # group_by(ID, experimental_replicate, concentration, growth, repeats) %>%
    group_by(ID) %>%
    mutate(response_range = list(get_range(response))) %>%
    unnest() %>%
    mutate(control_range = list(get_range(control))) %>%
    unnest() %>%
    filter(response <= upper &
             response >= lower,
           control <= upper1 & control >= lower1) %>%
    ungroup() #%>%

}
