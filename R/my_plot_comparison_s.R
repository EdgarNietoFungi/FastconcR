#' myplot_comparison_s
#' @export
myplot_comparison_s <- function(filename) {
  data <- filename
  EC50D<- data$EC50D

  ggplot(data = filename,  aes(x = group, y = EC50D)) + geom_boxplot()  +     labs(x = "Group", y = expression(bold(EC[bold("50") ~ (bold("D"))]) ~
                                                                                                                 (bold(ppm ~ bold(
                                                                                                                   "a.i."
                                                                                                                 ))))) + theme(
                                                                                                                   panel.border = element_rect(
                                                                                                                     colour = "black",
                                                                                                                     fill = NA,
                                                                                                                     size = 1
                                                                                                                   ),
                                                                                                                   axis.title = element_text(size = 18, face = "bold", hjust = 0.5),
                                                                                                                   axis.text.x = element_text(
                                                                                                                     face = "bold",
                                                                                                                     size = 9,
                                                                                                                     family = "Arial",
                                                                                                                     angle = 10,
                                                                                                                     hjust = 1
                                                                                                                   ),
                                                                                                                   axis.text.y = element_text(
                                                                                                                     face = "bold",
                                                                                                                     size = 9,
                                                                                                                     family = "Arial"
                                                                                                                   ),
                                                                                                                   panel.background = element_rect(fill = "white", colour = "grey50")
                                                                                                                 )


}


