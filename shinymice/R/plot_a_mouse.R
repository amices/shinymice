# # mouse plot
# #mouse <- read.csv("C:/Users/4216318/Downloads/mouse.csv")
# 
# mouse$X <- mouse$X+3
# ggplot2::ggplot(mouse, ggplot2::aes(x=X, y=Y))+
#   ggplot2::geom_point(size = 5)+
#   ggplot2::scale_x_continuous(limits = c(0,100)) +
#   ggplot2::scale_y_continuous(limits = c(0,100)) +
#   ggplot2::geom_text(ggplot2::aes(x = 50, y = 10, label = "No need for mice!"), size = 10) +
#   ggplot2::theme_classic()
