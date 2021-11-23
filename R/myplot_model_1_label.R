#' myplot_model_1_label
#' @export

myplot_model_1_label <- function(filename,vardep, varindep,  desiredR2 = 0.8) {
  compa <- DC_2(filename)
  data1 <- filename %>% select(ID, logEC50, EC50, ends_with(compa)) %>%  rename(RG = starts_with("RG"))
  RG <- data1$RG
  logEC50 <- data1$logEC50
  vardep <- logEC50
  varindep <- RG
  data2 <-   lm(vardep~ varindep, data = data1)
  data3 <-  summary( lm(vardep~ varindep, data = data1))

  ggplot(data = filename,  aes(x = RG, y = logEC50))  +
    geom_point(
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
               ) + expand_limits(x = c(15, 65)) + scale_x_continuous(name = waiver(), breaks = c(15, 25, 35, 45, 55, 65))  + geom_label(aes(x = 55,
                                                                                                                                            y = -2.75)
                                                                                                                                        ,
                                                                                                                                        label = c(paste  (
                                                                                                                                          " Y =",
                                                                                                                                          paste0 (round(data3[[4]][2], 4), "x"),
                                                                                                                                          "-",
                                                                                                                                          round(data3[[4]][1], 3) *
                                                                                                                                            -1,
                                                                                                                                          paste  ("\n R =",
                                                                                                                                                  paste0 (round(
                                                                                                                                                    data3[[9]][1], 4
                                                                                                                                                  ), "\n p < 0.001"))
                                                                                                                                        )),size = 7,
                                                                                                                                        fontface = "bold")

}
