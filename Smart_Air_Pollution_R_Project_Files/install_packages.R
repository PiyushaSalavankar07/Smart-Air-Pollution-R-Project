# Run this file in an R console:
# source("install_packages.R")

packages <- c(
  "dplyr", "ggplot2", "caret", "randomForest",
  "rpart", "rpart.plot", "corrplot", "shiny", "plotly"
)

missing <- packages[!packages %in% rownames(installed.packages())]
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

cat("Package setup complete.\n")
