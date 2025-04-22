library(R.matlab)
idf_data <- readMat("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/projects/IDF_project/matlab/results/belowCanopyBCI2024.mat")
idf_data[[2]]
class(idf_data)
summary(idf_data)

x<- idf_data$idf[,,7]

# Get number of transects
n_transects <- dim(idf_data$idf)[3]

# Initialize list to store all data
plot_data <- list()

# Loop through all transects
for (i in 1:n_transects) {
  tr <- idf_data$idf[,,i]
  
  transect_name <- tr$name[[1]]
  distance      <- as.vector(tr$distance)
  
  # Prepare all slices
  slices <- list(
    IDF045 = tr$IDF045,
    IDF4590 = tr$IDF4590,
    IDFMinus450 = tr$IDFMinus450,
    IDFMinus9045 = tr$IDFMinus9045,
    IDFAll = tr$IDFAll
  )
  
  for (slice_name in names(slices)) {
    mat <- slices[[slice_name]]
    
    # Extract 180° row
    rms_profile <- mat[180, ]
    
    # Create dataframe
    df <- data.frame(
      transect = transect_name,
      distance = distance,
      rms_diff = rms_profile,
      slice = slice_name
    )
    
    plot_data[[length(plot_data) + 1]] <- df
  }
}

# Combine into one dataframe
plot_df <- bind_rows(plot_data)
plot_df$slice <- factor(plot_df$slice, levels = c(
  "IDFMinus9045",
  "IDFMinus450",
  "IDF045",
  "IDF4590",
  "IDFAll"
))


# Plot using facet grid
ggplot(plot_df, aes(x = distance, y = rms_diff, color = transect)) +
  geom_line() +
  geom_point(size = 0.5) +
  labs(
    title = "Image Difference at Rotation 180° for Each Elevation Slice",
    x = "Distance (m)",
    y = "RMS Pixel Difference"
  ) +
  theme_minimal() +
  facet_grid(transect ~ slice) +
  theme(legend.position = "none")

# reorder the slices 