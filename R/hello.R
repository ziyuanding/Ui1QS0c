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

my_show_dists <- function(df) {
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

my_na_rate <- function(df) {
  na_p <- t(df |> summarise_all(list(name = ~sum(is.na(.))/length(.))))
  NA_perctage_df <- data.frame (
    column_name = rownames(na_p),
    NA_percent = na_p
  )
  rownames(NA_perctage_df) <- NULL
  NA_perctage_df <- NA_perctage_df |> arrange(desc(NA_percent)) |> filter(NA_percent!=0)
  return (NA_perctage_df)
}

my_impute_column <- function(df, columns, exp_value_to_be_replaced, func_new_value) {

}

my_assess_model <- function() {

}
