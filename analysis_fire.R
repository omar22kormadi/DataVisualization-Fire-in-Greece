# analysis_fire.R
# Analysis and visualization script for cleaned.csv
# Produces descriptive stats, temporal plots, spatial plots, and an interactive map.

# ---- Packages ----
# Ensure required packages are installed (explicitly include ggplot2)
required_pkgs <- c("ggplot2", "tidyverse", "lubridate", "leaflet", "sf", "htmlwidgets")
for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org", dependencies = TRUE)
  }
}
library(tidyverse)
library(lubridate)
library(leaflet)
library(sf)
library(htmlwidgets)

# ---- Paths ----
data_file <- "cleaned.csv"
out_dir <- "outputs"
dir.create(out_dir, showWarnings = FALSE)

# ---- Read data ----
if (!file.exists(data_file)) stop(paste("Data file not found:", data_file))
df <- readr::read_csv(data_file, show_col_types = FALSE)

# ---- Basic preparation ----
# Convert acq_date to Date (try common formats)
if ("acq_date" %in% names(df)) {
  df <- df %>% mutate(acq_date = ymd(acq_date))
} else {
  stop("`acq_date` column not found in data")
}

# Ensure numeric types for key vars
num_vars <- c("brightness", "frp", "tavg", "tmin", "tmax", "prcp", "wspd", "latitude", "longitude")
for (v in intersect(num_vars, names(df))) {
  df[[v]] <- as.numeric(df[[v]])
}

# Drop rows with missing coords
df <- df %>% filter(!is.na(latitude), !is.na(longitude))

# ---- Descriptive statistics ----
summary_stats <- df %>%
  ungroup() %>%
  summarise(across(c(brightness, frp, tavg, tmin, tmax, prcp, wspd), list(mean = ~mean(.x, na.rm = TRUE),
                                                                      sd = ~sd(.x, na.rm = TRUE),
                                                                      median = ~median(.x, na.rm = TRUE),
                                                                      n_missing = ~sum(is.na(.x))), .names = "{col}_{fn}"))
print(summary_stats)
readr::write_csv(summary_stats, file.path(out_dir, "summary_stats.csv"))

# ---- Distribution plots ----
p1 <- ggplot(df, aes(x = brightness)) +
  geom_histogram(bins = 50, fill = "tomato", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of brightness", x = "brightness", y = "count")

p2 <- ggplot(df, aes(y = brightness)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Boxplot of brightness", y = "brightness")

ggsave(file.path(out_dir, "brightness_hist.png"), p1, width = 7, height = 4)
ggsave(file.path(out_dir, "brightness_box.png"), p2, width = 4, height = 4)

# ---- Temporal analyses ----
# Number of detections per day
by_day <- df %>%
  group_by(acq_date) %>%
  summarise(n_fires = n(), mean_brightness = mean(brightness, na.rm = TRUE)) %>%
  arrange(acq_date)

p_day_count <- ggplot(by_day, aes(x = acq_date, y = n_fires)) +
  geom_line(color = "darkred") +
  theme_minimal() +
  labs(title = "Number of detections per day", x = "Date", y = "Count")

p_day_brightness <- ggplot(by_day, aes(x = acq_date, y = mean_brightness)) +
  geom_line(color = "darkorange") +
  theme_minimal() +
  labs(title = "Daily mean brightness", x = "Date", y = "Mean brightness")

ggsave(file.path(out_dir, "daily_count.png"), p_day_count, width = 8, height = 4)
ggsave(file.path(out_dir, "daily_mean_brightness.png"), p_day_brightness, width = 8, height = 4)

# ---- Spatial plots (static) ----
# Simple scatter on lon/lat colored by brightness
p_map <- ggplot(df, aes(x = longitude, y = latitude, color = brightness)) +
  geom_point(alpha = 0.6, size = 1) +
  scale_color_viridis_c(option = "plasma", na.value = "grey50") +
  theme_minimal() +
  labs(title = "Fire detections (colored by brightness)")

ggsave(file.path(out_dir, "spatial_brightness.png"), p_map, width = 7, height = 6)

# ---- Interactive map (leaflet) ----
# Create a color palette
pal <- colorNumeric(palette = "YlOrRd", domain = df$brightness, na.color = "gray")

lf <- leaflet(df) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude,
                   radius = 4,
                   color = ~pal(brightness),
                   stroke = FALSE,
                   fillOpacity = 0.7,
                   popup = ~paste0("Date: ", acq_date, "<br>",
                                   "Brightness: ", round(brightness,1), "<br>",
                                   ifelse(!is.na(frp), paste0("FRP: ", round(frp,1), "<br>"), ""),
                                   ifelse(!is.na(confidence), paste0("Conf: ", confidence), ""))) %>%
  addLegend("bottomright", pal = pal, values = ~brightness, title = "Brightness")

# Save interactive map without requiring pandoc (avoids selfcontained = TRUE error)
htmlwidgets::saveWidget(lf,
                        file.path(out_dir, "interactive_map.html"),
                        selfcontained = FALSE,
                        libdir = file.path(out_dir, "interactive_map_files"))

# ---- Fire-weather relationships ----
# Scatter brightness vs tmax with smooth
if ("tmax" %in% names(df)) {
  p_rel <- ggplot(df, aes(x = tmax, y = brightness)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", color = "blue") +
    theme_minimal() +
    labs(title = "Brightness vs Tmax", x = "Tmax", y = "brightness")
  ggsave(file.path(out_dir, "brightness_vs_tmax.png"), p_rel, width = 6, height = 4)
}

# Simple correlation table
corr_vars <- intersect(c("brightness", "frp", "tmax", "prcp", "wspd", "tavg"), names(df))
if (length(corr_vars) >= 2) {
  # convert selected columns to numeric without summarising (avoids dplyr summarise lifecycle warnings)
  corr_df <- df %>% select(all_of(corr_vars)) %>% mutate(across(everything(), as.numeric))
  corr_mat <- cor(corr_df, use = "pairwise.complete.obs")
  print(corr_mat)
  readr::write_csv(as.data.frame(corr_mat), file.path(out_dir, "correlations.csv"))
}

cat("All outputs saved to:", normalizePath(out_dir), "\n")
