# Load required libraries
library(R.matlab)
library(ggplot2)
library(tidyverse)

process_single_idf <- function(mat_path) {
  # Load .mat file and extract 'idf' object
  idf <- readMat(mat_path)
  idf <- idf$idf
  # Drop singleton second dimension if present
  idf <- idf[, 1, , drop = TRUE]
  # Transpose so transects are rows
  idf <- aperm(idf, c(2, 1))
  # Convert to tibble
  tibble_idf <- as_tibble(idf)
  
  # Define matrix columns to extract
  matrix_cols <- c("IDF4590", "IDFAll", "IDF045", "IDFMinus450", "IDFMinus9045")
  # Reshape and extract row 180
  tbl_long <- tibble_idf %>%
    pivot_longer(
      cols = all_of(matrix_cols),
      names_to = "slice",
      values_to = "rms_matrix"
    ) %>%
    mutate(
      rms_diff = lapply(rms_matrix, function(mat) as.vector(mat[180, ])),
      distance = lapply(distance, as.vector)
    ) %>%
    unnest_longer(col = c(rms_diff, distance))
  
  return(tbl_long)
}
idf_2024 <- process_single_idf("../matlab/results/belowCanopyBCI2024.mat")
idf_2023 <- process_single_idf("../matlab/results/jochen_results/2023Panama_below.mat")

# === Define a helper function to extract data from each file ===
extract_transect_data <- function(idf, source_label) {
  n_transects <- dim(idf)[3]
  plot_data <- list()
  
  for (i in 1:n_transects) {
    tr <- idf[,,i]
    
    transect_name <- tr$name[[1]]
    distance      <- as.vector(tr$distance)
    
    slices <- list(
      IDFMinus9045 = tr$IDFMinus9045,
      IDFMinus450  = tr$IDFMinus450,
      IDF045       = tr$IDF045,
      IDF4590      = tr$IDF4590,
      IDFAll       = tr$IDFAll
    )
    
    for (slice_name in names(slices)) {
      mat <- slices[[slice_name]]
      rms_profile <- mat[180, ]  # Extract row at 180° rotation
      
      df <- data.frame(
        transect = transect_name,
        distance = distance,
        rms_diff = rms_profile,
        slice = slice_name,
        source = source_label
      )
      
      plot_data[[length(plot_data) + 1]] <- df
    }
  }
  
  return(do.call(rbind, plot_data))
}

# === Extract data from both .mat files ===
df_2024 <- extract_transect_data(idf_2024, source_label = "2024")
df_2023 <- extract_transect_data(idf_2023, source_label = "2023")

# === Combine and prepare for plotting ===
plot_df <- rbind(df_2023, df_2024)

# Factor order for consistent plotting
plot_df$slice <- factor(plot_df$slice, levels = c(
  "IDFMinus9045", "IDFMinus450", "IDF045", "IDF4590", "IDFAll"
))
plot_df$transect <- factor(plot_df$transect)
plot_df$source <- factor(plot_df$source)

unique(plot_df$transect)

# === Plot ===
df_2024 %>% filter(transect != "below Trail 18") %>%
  ggplot(aes(x = distance, y = rms_diff, color = source)) +
  geom_line() +
  labs(
    x = "Distance (m)",
    y = "RMS Pixel Difference"
  ) +
  theme_minimal() +
  theme(
    #   legend.title = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = "top"
  ) +
  facet_grid(transect ~ slice, switch = "both", scales = "free", space = "free",
             labeller = labeller(label_wrap_gen(1)))

p <- plot_df %>% filter(transect != "below Trail 18") %>%
  ggplot(aes(x = distance, y = rms_diff, color = source)) +
  geom_line() +
  labs(
    x = "Distance (m)",
    y = "RMS Pixel Difference"
  ) +
  theme_minimal() +
  theme(
 #   legend.title = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = "top"
  ) +
  facet_grid(transect ~ slice, switch = "both", scales = "free", space = "free",
             labeller = labeller(label_wrap_gen(1)))
ggsave("plots/belowCanopyTransectsMetrop.png",
       plot = p, width = 12, height = 8, dpi = 300)

# load above canopy data
canopy <- readMat("../matlab/results/jochen_results/2023Panama_canopy.mat")
canopy <- canopy$idf
canopy<- canopy[-20:-25,,-1, drop = F]

# change names of structure to match the function above
dimnames(idf_2023)
newNames <- dimnames(canopy)[[1]]
newNames[which(newNames %in% c("IDF45", "IDF90", "IDF135"))] <- c("IDFMinus9045", "IDFMinus450", "IDF045")
dimnames(canopy)[[1]] <- newNames

# next up, change function so that it doesnt try to work on rows that dont exist

extract_transect_data <- function(idf, source_label) {
  n_transects <- dim(idf)[3]
  plot_data <- list()
  
  for (i in 1:n_transects) {
    tr <- idf[,,i]
    
    transect_name <- tr$name[[1]]
    distance      <- as.vector(tr$distance)
    
    # Define slices to check
    slice_names <- c("IDFMinus9045", "IDFMinus450", "IDF045", "IDF4590", "IDFAll")
    
    for (slice_name in slice_names) {
      if (!is.null(tr[[slice_name]]) && length(tr[[slice_name]]) > 0) {
        mat <- tr[[slice_name]]
        rms_profile <- mat[180, ]
        
        df <- data.frame(
          transect = transect_name,
          distance = distance,
          rms_diff = rms_profile,
          slice = slice_name,
          source = source_label
        )
        
        plot_data[[length(plot_data) + 1]] <- df
      }
    }
  }
  
  return(do.call(rbind, plot_data))
}

canopy <- extract_transect_data(canopy, source_label = "canopy")

p <- canopy %>%
  ggplot(aes(x = distance, y = rms_diff, color = source)) +
  geom_line() +
  labs(
    x = "Distance (m)",
    y = "RMS Pixel Difference"
  ) +
  theme_minimal() +
  theme(
    #   legend.title = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = "top"
  ) +
  facet_grid(transect ~ slice, switch = "both", scales = "free", space = "free",
             labeller = labeller(label_wrap_gen(1)))
ggsave("plots/aboveCanopyTransectsMetrop.png",
       plot = p, width = 12, height = 8, dpi = 300)

# Why does idf045 look higher than idfALL?







