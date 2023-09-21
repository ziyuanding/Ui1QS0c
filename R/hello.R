# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello from Ui1QS0c!")
}

library(ggplot2)
library(ggpubr)

show_dists <- function(df) {
  plots <- list()
  for(colname in colnames(df)) {

    if(class(df[,colname]) == "numeric" | class(df[,colname]) == "integer" ) {
      plot <- (ggplot(df) + geom_histogram(bins=30, aes_string(x = colname)))
      plots <- append(plots, list(plot))
    }
  }
  plots_per_row <- ifelse(length(plots)>=3, 3, length(plots))
  print(ggarrange(plotlist=plots, ncol = plots_per_row, nrow = ceiling(as.double(length(plots)) / as.double(plots_per_row))))
}
