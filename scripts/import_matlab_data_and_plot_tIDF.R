# to do!
# there was a transect with 1600 or so frames. change that. Or is that Jochens analysed data?

# Load required libraries
library(R.matlab)
library(ggplot2)
library(tidyverse)

process_single_idf <- function(mat_path) {
  idf <- readMat(mat_path)
  idf <- idf$idf
  idf <- idf[, 1, , drop = TRUE]
  idf <- aperm(idf, c(2, 1))
  tibble_idf <- as_tibble(idf)
  
  print("Tibble columns:")
  print(names(tibble_idf))  # <-- see what columns actually exist
  
  matrix_cols <- c("IDF4590", "IDFAll", "IDF045", "IDFMinus450", "IDFMinus9045")
  available_cols <- intersect(matrix_cols, names(tibble_idf))
  
  print("Available columns:")
  print(available_cols)
  
  tbl_long <- tibble_idf %>%
    pivot_longer(
      cols = all_of(available_cols),
      names_to = "slice",
      values_to = "rms_matrix"
    )
  
  print("After pivot_longer:")
  print(tbl_long)
  
  tbl_long <- tbl_long %>%
    mutate(
      rms_diff = lapply(rms_matrix, function(mat) {
        if (is.null(mat) || length(mat) == 0) {
          NA  # return NA if missing
        } else {
          as.vector(mat[180, ])
        }
      }),
      distance = lapply(distance, as.vector)
    )
  
  print("After mutate rms_diff and distance:")
  glimpse(tbl_long)
  
  tbl_long <- tbl_long %>%
    unnest_longer(col = c(rms_diff, distance))
  
  return(tbl_long)
}

plot_df$transects_renamed <- recode(plot_df$name,
                                    "below Trail 16" = "belowTrail16",
                                    "below Trail 17" = "belowTrail17",
                                    "below Trail 18" = "belowTrail18",
                                    "below Trail 19" = "belowTrail19",
                                    "below Trail 20" = "belowTrail20",
                                    "below Trail 22" = "belowTrail22",
                                    "16-04-2024_t1" = "160424_t1",
                                    "16-04-2024_t2" = "160424_t2",
                                    "16-04-2024_t3" = "160424_t3",
                                    "15-04-2024_t4" = "150424_t4",
                                    "15-04-2024_t3" = "150424_t3",
                                    "15-04-2024_t2" = "150424_t2",
                                    "15-04-2024_t1" = "150424_t1"
)

source("scripts/functions/gradient_descent_function.R")
# quantify catchments

# load above and below data produced by my own scripts
idf_above_and_below <- process_single_idf("../matlab/results/belowCanopyBCI2024.mat")
idf_above_and_below <- idf_above_and_below %>% select(all_of(vars_to_include)) 
idf_above_and_below$name <- unlist(idf_above_and_below$name)

glimpse(idf_above_and_below)

idf_above_and_below %>% 
  ggplot(aes(x = distance, y = rms_diff_smoothed)) +
  geom_line() +
  labs(
    x = "Distance (m)",
    y = "RMS Pixel Difference"
  ) +
  theme_bw() +
  theme(
    #   legend.title = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = "top"
  ) +
  facet_grid(name ~ slice, switch = "both", scales = "free", space = "free")

library(zoo)
idf_above_and_below <- idf_above_and_below %>%
  group_by(name, slice) %>%
  arrange(distance) %>%
  mutate(rms_diff_smoothed = rollmean(rms_diff, k = 5, fill = NA, align = "center")) %>%
  ungroup()

catchment_area_results <- idf_above_and_below %>%
  filter(rms_diff_smoothed != "NaN") %>%
  #filter(slice == "IDFAll") %>% # function doesnt work if you pass it slices that are NA!!
  group_by(name, slice) %>%
  group_modify(~ find_extremum_simple(.x, distance_col = "distance", img_diff_col = "rms_diff_smoothed", direction = "ascend")) %>%
  ungroup()
glimpse(idf_above_and_below)
# why se to nw transect all slices give same catchment. bug?

# 'type' of transect needs to be added for this to work!
ggplot(catchment_area_results, aes())


# change names of structure to match the function above
# dimnames(idf_2023)
# newNames <- dimnames(canopy)[[1]]
# newNames[which(newNames %in% c("IDF45", "IDF90f", "IDF135"))] <- c("IDFMinus9045", "IDFMinus450", "IDF045")
# dimnames(canopy)[[1]] <- newNames

# Why does idf045 look higher than idfALL?







