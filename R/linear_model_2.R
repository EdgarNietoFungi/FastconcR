#'linear_model_2
#'@export

lineal_model_2 <- function ( filename,vardep, varindep, desiredR2 = 0.8) {
  data1 <-filename
  # desiredR2 (0.8 by default).
  data2 <-   lm(paste(vardep, "~", varindep), data = data1)
  data3 <-  summary( lm(paste(vardep, "~", varindep), data = data1))

  if (data3[[9]][[1]] >= desiredR2) {
    print("Good DC")
  } else {
    print("Not good DC")
  }}

myplot_model_2 <- function(filename) {
  data <- filename
  EC50 <- data$EC50
  Estimate.50DC <- data$Estimate.50DC
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
             size = 0.3)

}
