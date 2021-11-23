#' myplot_model_2_label
#' @export
myplot_model_2_label <- function(filename) {

  data1 <- filename
  Estimate.50DC <- data1$Estimate.50DC
  EC50 <- data1$EC50
  vardep <-Estimate.50DC
  varindep <- EC50

  data2 <-   lm(Estimate.50DC ~ EC50, data = data1)
  data3 <-   summary(lm(Estimate.50DC ~ EC50, data = data1))

  ggplot(data = filename,  aes(x = EC50, y = Estimate.50DC))  +
    geom_point(aes( ),
               size = 2, stroke= 1, colour= "black")  + scale_shape_manual(values = c(21, 24))+ scale_fill_grey(start = 0, end = 1)  +
    geom_smooth(method = "lm",
                se = FALSE,
                colour = "black",
                size = 0.3) +
    theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5,
        family = "Arial"
      ),
      axis.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.text = element_text(
        face = "bold",
        size = 18,
        family = "Arial"
      ),
      panel.background = element_rect(fill = "white", colour = "grey50")
    ) +
    labs(x =
           expression(bold(EC[bold("50")]) ~ (bold(ppm ~ bold(
             "a.i."
           )))), y = expression(bold(EC[bold("50") ~ (bold("D"))]) ~ (bold(ppm ~ bold(
             "a.i."
           ))))) + geom_abline(
             intercept = 0,
             slope = 1,
             color = "black",
             lty = "dashed",
             size = 0.3)    + geom_label(aes(x = 0.15,
                                             y = 0.05)
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

