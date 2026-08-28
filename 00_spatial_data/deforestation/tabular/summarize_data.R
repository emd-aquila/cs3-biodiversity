library(dplyr)
library(ggplot2)

input_data_name  <- "deforestation_tile_attributes_SPAM2020_2001_2024.csv"
output_file_name <- "deforestation_data_summarized.csv"
histogram_file   <- "histogram_livestock.png"

data <- read.csv(input_data_name)

crop_cols <- c(
  "chickpea", "citrics", "coconut", "cocoa", "coffee", "cotton", "cowpea",
  "groundnut", "lentil", "potato", "maize", "millet", "other_cereals",
  "other_fibre_crops", "palm_oil", "onion", "other_oil_crops", "other_pulse",
  "other_roots", "pigeonpea", "plantain", "rice", "pearl_millet", "rapeseed",
  "robusta_coffee", "rest_of_crops", "yams", "rubber", "sesame", "sorghum",
  "sugarbeet", "soybean", "sugarcane", "sunflower", "sweetpotato", "teas",
  "temperate_fruits", "tobacco", "tomato", "vegetables", "wheat", "banana",
  "barley", "bean", "cassava"
)
livestock_cols <- c("cattle", "goat", "buffalo", "sheep")

data_summary <- data %>%
  mutate(
    total_deforested_ha_crops     = rowSums(across(all_of(crop_cols)),     na.rm = TRUE),
    total_deforested_ha_livestock = rowSums(across(all_of(livestock_cols)), na.rm = TRUE)
  ) %>%
  dplyr::select(id, lossyear_mode,
                total_deforested_ha,
                total_deforested_ha_crops,
                total_deforested_ha_livestock)

write.csv(data_summary, output_file_name, row.names = FALSE)
message("Saved: ", output_file_name)

p <- ggplot(data_summary, aes(x = total_deforested_ha_livestock)) +
  geom_histogram(fill = "steelblue", color = "white", binwidth = 250, boundary = 0) +
  coord_cartesian(xlim = c(0, 5000)) +
  scale_x_continuous(breaks = seq(0, 5000, by = 1000)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = "Livestock-Associated Deforestation",
    x     = "Total Deforested Area (ha)",
    y     = "Number of Tile-years"
  ) +
  theme_minimal(base_size = 14)

ggsave(histogram_file, plot = p, width = 8, height = 6, dpi = 150)