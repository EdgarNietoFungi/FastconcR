#' myplot_model_1
#' @export

myplot_model_1 <- function(filename, RG, logEC50) {
  compa <- DC_2(filename)
  data <- filename %>% select(ID, logEC50, EC50, ends_with(compa)) %>%  rename(RG = starts_with("RG"))
  RG <- data$RG
  logEC50 <- data$logEC50
  ggplot(data = filename,  aes(x = RG, y = logEC50))  +
    geom_point(aes(),
               size = 2, stroke= 1, colour= "black")  + scale_shape_manual(values = c(21, 24))+ scale_fill_grey(start = 0, end = 1) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      colour = "black",
      size = 0.3
    ) +
    theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5,
        family = "Arial"
      ),
      axis.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5
      ),
      axis.text = element_text(
        face = "bold",
        size = 18,
        family = "Arial"
      ),
      panel.background = element_rect(fill = "white", colour = "grey50")
    ) + labs(x =
               "Relative growth (%)", y = expression(bold(Log  (EC[bold("50")])))) + geom_abline(
                 intercept = 0,
                 slope = 1,
                 color = "black",
                 lty = "dashed",
                 size = 0.3
               ) + expand_limits(x = c(15, 65)) + scale_x_continuous(name = waiver(), breaks = c(15, 25, 35, 45, 55, 65))
}
