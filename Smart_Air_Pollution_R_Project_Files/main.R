# ============================================================
# Smart Air Pollution Risk Prediction System in R
# File: main.R
# Run this file first to train and save the Random Forest model.
# ============================================================

# Use the folder containing this script when run from RStudio/VS Code.
# If running from another working directory, set it manually:
# setwd("C:/Users/YourName/Downloads/Smart_Air_Pollution_R_Project")

required_packages <- c(
  "dplyr", "ggplot2", "caret", "randomForest",
  "rpart", "rpart.plot", "corrplot"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))

if (!file.exists("city_day.csv")) {
  stop(
    "city_day.csv was not found. Download and extract the Kaggle dataset, ",
    "then place city_day.csv in this project folder. See README.txt."
  )
}

data <- read.csv("city_day.csv", stringsAsFactors = FALSE, check.names = FALSE)

features <- c(
  "PM2.5", "PM10", "NO", "NO2", "NOx", "NH3",
  "CO", "SO2", "O3", "Benzene", "Toluene", "Xylene"
)
required_columns <- c("AQI", "City", "Date", features)
missing_columns <- setdiff(required_columns, names(data))
if (length(missing_columns) > 0) {
  stop("Dataset is missing columns: ", paste(missing_columns, collapse = ", "))
}

# Convert relevant columns to numeric.
for (column in c("AQI", features)) {
  data[[column]] <- suppressWarnings(as.numeric(data[[column]]))
}

# AQI is used to create the target class, not as a model input.
data <- data[!is.na(data$AQI), ]
data$Risk <- cut(
  data$AQI,
  breaks = c(-Inf, 100, 200, Inf),
  labels = c("Low", "Medium", "High"),
  right = TRUE
)
data$Risk <- droplevels(data$Risk)
data <- data[!is.na(data$Risk), ]

# Median-impute pollutant columns. Fail clearly if a column has no valid values.
for (column in features) {
  column_median <- median(data[[column]], na.rm = TRUE)
  if (!is.finite(column_median)) {
    stop("Column ", column, " has no usable numeric values.")
  }
  data[[column]][is.na(data[[column]])] <- column_median
}

cat("\nDataset loaded and cleaned.\n")
cat("Rows:", nrow(data), " Columns:", ncol(data), "\n\n")
cat("Risk distribution:\n")
print(table(data$Risk))

# Save cleaned data for dashboard historical visuals.
write.csv(data, "cleaned_air_quality.csv", row.names = FALSE)

# Visual 1: risk distribution
p1 <- ggplot(data, aes(x = Risk, fill = Risk)) +
  geom_bar() +
  scale_fill_manual(values = c(Low = "#10AD91", Medium = "#F2B544", High = "#E65353")) +
  labs(title = "Air Pollution Risk Distribution", x = "Risk Level", y = "Records") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
ggsave("risk_distribution.png", p1, width = 8, height = 5, dpi = 150)

# Visual 2: average AQI by city
city_aqi <- data |>
  dplyr::group_by(City) |>
  dplyr::summarise(avg_AQI = mean(AQI, na.rm = TRUE), .groups = "drop")
p2 <- ggplot(city_aqi, aes(x = reorder(City, avg_AQI), y = avg_AQI)) +
  geom_col(fill = "#168BB5") +
  coord_flip() +
  labs(title = "Average AQI by City", x = "City", y = "Average AQI") +
  theme_minimal(base_size = 12)
ggsave("city_aqi.png", p2, width = 8, height = 6, dpi = 150)

# Visual 3: annual AQI trend
data$Date <- as.Date(data$Date)
year_aqi <- data |>
  dplyr::filter(!is.na(Date)) |>
  dplyr::mutate(Year = format(Date, "%Y")) |>
  dplyr::group_by(Year) |>
  dplyr::summarise(avg_AQI = mean(AQI, na.rm = TRUE), .groups = "drop")
p3 <- ggplot(year_aqi, aes(x = Year, y = avg_AQI, group = 1)) +
  geom_line(color = "#10AD91", linewidth = 1.2) +
  geom_point(color = "#087F76", size = 2.5) +
  labs(title = "Average AQI by Year", x = "Year", y = "Average AQI") +
  theme_minimal(base_size = 12)
ggsave("aqi_trend.png", p3, width = 8, height = 5, dpi = 150)

# Visual 4: pollutant correlation heatmap
cor_matrix <- cor(data[, features], use = "pairwise.complete.obs")
png("pollutant_correlation.png", width = 1200, height = 950, res = 130)
corrplot::corrplot(
  cor_matrix, method = "color", type = "upper",
  tl.cex = 0.8, addCoef.col = "grey25",
  title = "Pollutant Correlation", mar = c(0, 0, 2, 0)
)
dev.off()

# Split data for model evaluation.
model_data <- data[, c(features, "Risk")]
model_data <- model_data[complete.cases(model_data), ]
model_data$Risk <- droplevels(model_data$Risk)
if (nlevels(model_data$Risk) < 2) {
  stop("At least two risk classes are required to train a classifier.")
}

set.seed(123)
train_index <- caret::createDataPartition(model_data$Risk, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Decision Tree
tree_model <- rpart::rpart(Risk ~ ., data = train_data, method = "class")
tree_prediction <- predict(tree_model, newdata = test_data, type = "class")
tree_cm <- caret::confusionMatrix(tree_prediction, test_data$Risk)
cat("\n===== DECISION TREE RESULTS =====\n")
print(tree_cm)

png("decision_tree.png", width = 1200, height = 900, res = 130)
rpart.plot::rpart.plot(tree_model, main = "Decision Tree - Pollution Risk")
dev.off()

# Random Forest
set.seed(123)
rf_model <- randomForest::randomForest(
  Risk ~ ., data = train_data, ntree = 150, importance = TRUE
)
rf_prediction <- predict(rf_model, newdata = test_data)
rf_cm <- caret::confusionMatrix(rf_prediction, test_data$Risk)
cat("\n===== RANDOM FOREST RESULTS =====\n")
print(rf_cm)

png("pollutant_importance.png", width = 1200, height = 900, res = 130)
varImpPlot(rf_model, main = "Pollutant Importance")
dev.off()

# Model comparison
comparison <- data.frame(
  Model = c("Decision Tree", "Random Forest"),
  Accuracy = round(100 * c(
    as.numeric(tree_cm$overall["Accuracy"]),
    as.numeric(rf_cm$overall["Accuracy"])
  ), 2)
)
print(comparison)
write.csv(comparison, "model_comparison.csv", row.names = FALSE)

p4 <- ggplot(comparison, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0(Accuracy, "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Decision Tree" = "#168BB5", "Random Forest" = "#10AD91")) +
  coord_cartesian(ylim = c(0, 105)) +
  labs(title = "Model Accuracy Comparison", x = NULL, y = "Accuracy (%)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
ggsave("model_comparison.png", p4, width = 8, height = 5, dpi = 150)

saveRDS(rf_model, "air_risk_model.rds")
saveRDS(features, "pollutant_features.rds")

cat("\nSUCCESS: Model saved as air_risk_model.rds\n")
cat("Now run: shiny::runApp('app.R')\n")
