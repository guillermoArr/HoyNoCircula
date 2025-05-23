# ############################## RDs with Time Controls ############################## 
# 
# # Libraries
# library(dplyr)
# library(lubridate)
# library(ggplot2)
# library(rdrobust)
# library(rddensity)
# library(patchwork)
# 
# # Adjust corresponding path
# setwd("/Users/Alvlopzam/Desktop/ITAM/Semestre 10/PolPubII/HoyNoCircula/Codigo")
# 
# # Data loading
# data_ma <- read.csv("../Datos/procesados/promedios_horas_moving_avg_24h.csv")
# 
# # Ensure 'date' is POSIXct
# data_ma <- data_ma %>%
#   mutate(date = ymd_hms(date))
# 
# # Extract date part to group by day
# data_ma <- data_ma %>%
#   arrange(date) %>%
#   mutate(date_day = as_date(date))
# 
# # Identify days where policy was active all 24 hours
# dates_with_policy <- data_ma %>%
#   group_by(date_day) %>%
#   summarize(policy_day = all(activacion_doble_no_circula == 1)) %>%
#   filter(policy_day) %>%
#   pull(date_day)
# 
# # Shift the treatment to 6:00am – 6:00am of the next day
# data_ma <- data_ma %>%
#   mutate(
#     activacion_doble_no_circula = if_else(
#       (as_date(date) %in% dates_with_policy & hour(date) >= 6) |
#         (as_date(date - hours(6)) %in% dates_with_policy & hour(date) < 6),
#       1, 0
#     )
#   )
# 
# #Function to create time-based dummy variables
# create_time_dummies <- function(df) {
#   # Ensure date column is in POSIXct format
#   df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S")
#   
#   # Sort by date to ensure proper ordering
#   df <- df[order(df$date), ]
#   
#   # Initialize all dummy columns with 0
#   # 23 hours prior
#   for(i in 23:1) {
#     df[[paste0(i, "hr_prior")]] <- 0
#   }
#   
#   # Treatment period
#   df$treatment <- 0
#   
#   # 23 hours post
#   for(i in 1:23) {
#     df[[paste0(i, "hr_post")]] <- 0
#   }
#   
#   # Find treatment periods (where activacion_doble_no_circula == 1)
#   treatment_indices <- which(df$activacion_doble_no_circula == 1)
#   
#   # For each treatment period, mark the appropriate time windows
#   for(treatment_idx in treatment_indices) {
#     
#     # Mark treatment period
#     df$treatment[treatment_idx] <- 1
#     
#     # Mark prior hours (23 hours before)
#     for(i in 1:23) {
#       prior_idx <- treatment_idx - i
#       if(prior_idx > 0) {
#         df[[paste0(i, "hr_prior")]][prior_idx] <- 1
#       }
#     }
#     
#     # Mark post hours (23 hours after)
#     for(i in 1:23) {
#       post_idx <- treatment_idx + i
#       if(post_idx <= nrow(df)) {
#         df[[paste0(i, "hr_post")]][post_idx] <- 1
#       }
#     }
#   }
#   
#   return(df)
# }
# 
# # Function to create time controls
# create_time_controls <- function(df) {
#   df <- df %>%
#     mutate(
#       # Month of year (1-12)
#       month = month(date),
#       
#       # Day of week (1=Sunday, 7=Saturday)
#       day_of_week = wday(date),
#       
#       # Hour of day (0-23)
#       hour_of_day = hour(date),
#       
#       # Weekend indicator (Saturday=7, Sunday=1)
#       is_weekend = ifelse(day_of_week %in% c(1, 7), 1, 0),
#       
#       # Interaction: weekend * hour
#       weekend_hour_interaction = is_weekend * hour_of_day
#     )
#   
#   # Create month dummy variables (using January as reference)
#   for(i in 2:12) {
#     df[[paste0("month_", i)]] <- ifelse(df$month == i, 1, 0)
#   }
#   
#   # Create day of week dummy variables (using Sunday as reference)
#   for(i in 2:7) {
#     df[[paste0("dow_", i)]] <- ifelse(df$day_of_week == i, 1, 0)
#   }
#   
#   # Create hour of day dummy variables (using hour 0 as reference)
#   for(i in 1:23) {
#     df[[paste0("hour_", i)]] <- ifelse(df$hour_of_day == i, 1, 0)
#   }
#   
#   return(df)
# }
# 
# # Apply the function to your dataset
# data_ma_rd <- create_time_dummies(data_ma)
# data_ma_rd <- create_time_controls(data_ma_rd)
# 
# # Step 1: Create the running variable (time relative to treatment)
# create_rd_data <- function(df) {
#   # Create a time-to-treatment variable
#   df$time_to_treatment <- NA
#   
#   # Find treatment periods
#   treatment_indices <- which(df$treatment == 1)
#   
#   # For each observation, calculate distance to nearest treatment
#   for(i in 1:nrow(df)) {
#     if(length(treatment_indices) > 0) {
#       # Find the closest treatment
#       distances <- abs(i - treatment_indices)
#       closest_treatment_idx <- treatment_indices[which.min(distances)]
#       
#       # Calculate relative time (negative = before treatment, positive = after)
#       df$time_to_treatment[i] <- i - closest_treatment_idx
#     }
#   }
#   
#   # Create treatment indicator (1 if during or after treatment period)
#   df$treated <- ifelse(df$time_to_treatment >= 0, 1, 0)
#   
#   return(df)
# }
# 
# # Step 2: Basic RD visualization
# plot_rd_raw <- function(data, bandwidth = 24) {
#   # Filter data around discontinuity
#   plot_data <- data %>%
#     filter(abs(time_to_treatment) <= bandwidth) %>%
#     filter(!is.na(PM10))
#   
#   ggplot(plot_data, aes(x = time_to_treatment, y = PM10)) +
#     geom_point(alpha = 0.6, color = "steelblue") +
#     geom_smooth(aes(group = treated), method = "loess", se = TRUE, span = 0.75) +
#     geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
#     labs(
#       title = "RD Plot: PM10 Levels Around Treatment",
#       x = "Hours from Treatment (0 = Treatment Start)",
#       y = "PM10 Concentration",
#       subtitle = paste("Bandwidth:", bandwidth, "hours")
#     ) +
#     theme_minimal() +
#     theme(plot.title = element_text(hjust = 0.5))
# }
# 
# # Step 3: Binned scatter plot for cleaner visualization
# plot_rd_binned <- function(data, bandwidth = 24, n_bins = 20) {
#   plot_data <- data %>%
#     filter(abs(time_to_treatment) <= bandwidth) %>%
#     filter(!is.na(PM10))
#   
#   # Create bins
#   plot_data$bin <- cut(plot_data$time_to_treatment, 
#                        breaks = n_bins, 
#                        include.lowest = TRUE)
#   
#   # Calculate bin means
#   bin_data <- plot_data %>%
#     group_by(bin, treated) %>%
#     summarise(
#       mean_time = mean(time_to_treatment, na.rm = TRUE),
#       mean_pm10 = mean(PM10, na.rm = TRUE),
#       se_pm10 = sd(PM10, na.rm = TRUE) / sqrt(n()),
#       .groups = 'drop'
#     )
#   
#   ggplot(bin_data, aes(x = mean_time, y = mean_pm10, color = factor(treated))) +
#     geom_point(size = 3) +
#     geom_errorbar(aes(ymin = mean_pm10 - 1.96*se_pm10, 
#                       ymax = mean_pm10 + 1.96*se_pm10), width = 0.5) +
#     geom_smooth(method = "lm", se = TRUE, span = 0.75) +
#     geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
#     labs(
#       title = "RD Plot: Binned PM10 Levels Around Treatment",
#       x = "Hours from Treatment (0 = Treatment Start)",
#       y = "Mean PM10 Concentration",
#       color = "Treated"
#     ) +
#     scale_color_manual(values = c("0" = "blue", "1" = "red"),
#                        labels = c("Pre-treatment", "Post-treatment")) +
#     theme_minimal()
# }
# 
# # Modified RD estimation function with time controls
# run_rd_analysis_with_controls <- function(data, bandwidth = NULL, include_controls = TRUE) {
#   # Filter out missing values
#   analysis_data <- data %>%
#     filter(!is.na(PM10), !is.na(time_to_treatment))
#   
#   if (include_controls) {
#     # Prepare covariates matrix
#     # Month dummies (excluding reference month 1)
#     month_vars <- paste0("month_", 2:12)
#     
#     # Day of week dummies (excluding reference day 1 = Sunday)
#     dow_vars <- paste0("dow_", 2:7)
#     
#     # Hour dummies (excluding reference hour 0)
#     hour_vars <- paste0("hour_", 1:23)
#     
#     # Weekend-hour interaction
#     interaction_var <- "weekend_hour_interaction"
#     
#     # Combine all control variables
#     control_vars <- c(month_vars, dow_vars, hour_vars, interaction_var)
#     
#     # Create covariates matrix (only include variables that exist in data)
#     existing_vars <- control_vars[control_vars %in% names(analysis_data)]
#     
#     if (length(existing_vars) > 0) {
#       covs_matrix <- as.matrix(analysis_data[, existing_vars, drop = FALSE])
#       
#       # Run RD estimation with covariates
#       if(is.null(bandwidth)) {
#         rd_result <- rdrobust(y = analysis_data$PM10, 
#                               x = analysis_data$time_to_treatment,
#                               c = 0,
#                               covs = covs_matrix)
#       } else {
#         rd_result <- rdrobust(y = analysis_data$PM10, 
#                               x = analysis_data$time_to_treatment,
#                               c = 0, 
#                               h = bandwidth,
#                               covs = covs_matrix)
#       }
#     } else {
#       warning("No control variables found in data. Running without controls.")
#       if(is.null(bandwidth)) {
#         rd_result <- rdrobust(y = analysis_data$PM10, 
#                               x = analysis_data$time_to_treatment,
#                               c = 0)
#       } else {
#         rd_result <- rdrobust(y = analysis_data$PM10, 
#                               x = analysis_data$time_to_treatment,
#                               c = 0, 
#                               h = bandwidth)
#       }
#     }
#   } else {
#     # Run without controls (original specification)
#     if(is.null(bandwidth)) {
#       rd_result <- rdrobust(y = analysis_data$PM10, 
#                             x = analysis_data$time_to_treatment,
#                             c = 0)
#     } else {
#       rd_result <- rdrobust(y = analysis_data$PM10, 
#                             x = analysis_data$time_to_treatment,
#                             c = 0, 
#                             h = bandwidth)
#     }
#   }
#   
#   return(rd_result)
# }
# 
# # Function to compare results with and without controls
# compare_rd_results <- function(data, particle_var = "PM10") {
#   # Temporarily rename particle variable for analysis
#   data$PM10 <- data[[particle_var]]
#   rd_data <- create_rd_data(data)
#   
#   # Run analysis without controls
#   cat("\n=== RESULTS WITHOUT CONTROLS ===\n")
#   rd_no_controls <- run_rd_analysis_with_controls(rd_data, include_controls = FALSE)
#   print(summary(rd_no_controls))
#   
#   # Run analysis with controls
#   cat("\n=== RESULTS WITH TIME CONTROLS ===\n")
#   rd_with_controls <- run_rd_analysis_with_controls(rd_data, include_controls = TRUE)
#   print(summary(rd_with_controls))
#   
#   # Extract key results for comparison
#   results_comparison <- data.frame(
#     Specification = c("Without Controls", "With Controls"),
#     Coefficient = c(rd_no_controls$coef[1], rd_with_controls$coef[1]),
#     SE = c(rd_no_controls$se[1], rd_with_controls$se[1]),
#     P_value = c(rd_no_controls$pv[1], rd_with_controls$pv[1]),
#     CI_lower = c(rd_no_controls$ci[1,1], rd_with_controls$ci[1,1]),
#     CI_upper = c(rd_no_controls$ci[1,2], rd_with_controls$ci[1,2]),
#     Bandwidth = c(rd_no_controls$bws[1,1], rd_with_controls$bws[1,1]),
#     N_left = c(rd_no_controls$N[1], rd_with_controls$N[1]),
#     N_right = c(rd_no_controls$N[2], rd_with_controls$N[2])
#   )
#   
#   cat("\n=== COMPARISON TABLE ===\n")
#   print(results_comparison)
#   
#   return(list(
#     no_controls = rd_no_controls,
#     with_controls = rd_with_controls,
#     comparison = results_comparison
#   ))
# }
# 
# ############################# Final run with controls ################################
# 
# # Function to generate RD plots for a given particle and dataset
# generate_rd_plots <- function(data, particle_var, title_prefix) {
#   if (!particle_var %in% names(data)) {
#     warning(paste("Skipping:", particle_var, "not found in data"))
#     return(NULL)
#   }
#   
#   data$PM10 <- data[[particle_var]]  # temporarily rename for plotting
#   rd_data <- create_rd_data(data)
#   
#   p_raw <- plot_rd_raw(rd_data, bandwidth = 24) +
#     labs(title = paste(title_prefix, "-", particle_var, "(Raw)"))
#   p_binned <- plot_rd_binned(rd_data, bandwidth = 24) +
#     labs(title = paste(title_prefix, "-", particle_var, "(Binned)"))
#   
#   return(p_raw + p_binned + plot_layout(ncol = 2))
# }
# 
# # Define particles
# particles <- c("PM10", "PM2.5", "O3")
# 
# # === NO SHIFT ===
# data_ma_no_shift <- data_ma
# data_ma_no_shift$activacion_doble_no_circula <- ifelse(
#   as_date(data_ma_no_shift$date) %in% dates_with_policy, 1, 0
# )
# data_ma_no_shift <- create_time_dummies(data_ma_no_shift)
# data_ma_no_shift <- create_time_controls(data_ma_no_shift)  # Add time controls
# 
# plots_no_shift <- lapply(particles, function(p) {
#   generate_rd_plots(data_ma_no_shift, p, "No Shift")
# })
# 
# # === WITH 6HR SHIFT ===
# data_ma_shift <- data_ma  # already includes shifted treatment
# data_ma_shift <- create_time_dummies(data_ma_shift)
# data_ma_shift <- create_time_controls(data_ma_shift)  # Add time controls
# 
# plots_shift <- lapply(particles, function(p) {
#   generate_rd_plots(data_ma_shift, p, "6hr Shift")
# })
# 
# # === DISPLAY RESULTS ===
# 
# # NO SHIFT
# print(plots_no_shift[[1]] + plot_annotation(title = "PM10 – No Shift"))
# print(plots_no_shift[[2]] + plot_annotation(title = "PM2.5 – No Shift"))
# print(plots_no_shift[[3]] + plot_annotation(title = "O3 – No Shift"))
# 
# # 6HR SHIFT
# print(plots_shift[[1]] + plot_annotation(title = "PM10 – 6hr Shift"))
# print(plots_shift[[2]] + plot_annotation(title = "PM2.5 – 6hr Shift"))
# print(plots_shift[[3]] + plot_annotation(title = "O3 – 6hr Shift"))
# 
# # === ANALYSIS WITH AND WITHOUT CONTROLS ===
# 
# cat("\n", paste(rep("=", 80), collapse = ""), "\n")
# cat("REGRESSION DISCONTINUITY ANALYSIS WITH TIME CONTROLS\n")
# cat(paste(rep("=", 80), collapse = ""), "\n")
# 
# # Generate plots WITH CONTROLS for comparison
# cat("\n### GENERATING PLOTS WITH CONTROLS ###\n")
# 
# # Function to generate residualized plots (after removing time controls)
# generate_rd_plots_residualized <- function(data, particle_var, title_prefix) {
#   if (!particle_var %in% names(data)) {
#     warning(paste("Skipping:", particle_var, "not found in data"))
#     return(NULL)
#   }
#   
#   # Create temporary copy for residualization
#   temp_data <- data
#   temp_data$outcome <- temp_data[[particle_var]]
#   
#   # Create formula for time controls
#   month_vars <- paste0("month_", 2:12)
#   dow_vars <- paste0("dow_", 2:7)
#   hour_vars <- paste0("hour_", 1:23)
#   interaction_var <- "weekend_hour_interaction"
#   
#   control_vars <- c(month_vars, dow_vars, hour_vars, interaction_var)
#   existing_vars <- control_vars[control_vars %in% names(temp_data)]
#   
#   if(length(existing_vars) > 0) {
#     # Residualize outcome against time controls
#     control_formula <- as.formula(paste("outcome ~", paste(existing_vars, collapse = " + ")))
#     residual_model <- lm(control_formula, data = temp_data, na.action = na.exclude)
#     temp_data$PM10 <- residuals(residual_model)  # Use residualized outcome for plotting
#     
#     # Add back the mean for interpretability
#     temp_data$PM10 <- temp_data$PM10 + mean(temp_data$outcome, na.rm = TRUE)
#   } else {
#     temp_data$PM10 <- temp_data$outcome  # No residualization if no controls
#   }
#   
#   rd_data <- create_rd_data(temp_data)
#   
#   p_raw <- plot_rd_raw(rd_data, bandwidth = 24) +
#     labs(title = paste(title_prefix, "-", particle_var, "(Raw, Residualized)"),
#          subtitle = "Outcome residualized against time controls")
#   p_binned <- plot_rd_binned(rd_data, bandwidth = 24) +
#     labs(title = paste(title_prefix, "-", particle_var, "(Binned, Residualized)"),
#          subtitle = "Outcome residualized against time controls")
#   
#   return(p_raw + p_binned + plot_layout(ncol = 2))
# }
# 
# # NO SHIFT - WITH CONTROLS PLOTS
# cat("\n--- NO SHIFT: Plots with Time Controls (Residualized) ---\n")
# plots_no_shift_controls <- lapply(particles, function(p) {
#   generate_rd_plots_residualized(data_ma_no_shift, p, "No Shift + Controls")
# })
# 
# print(plots_no_shift_controls[[1]] + plot_annotation(title = "PM10 – No Shift (With Time Controls)"))
# print(plots_no_shift_controls[[2]] + plot_annotation(title = "PM2.5 – No Shift (With Time Controls)"))
# print(plots_no_shift_controls[[3]] + plot_annotation(title = "O3 – No Shift (With Time Controls)"))
# 
# # 6HR SHIFT - WITH CONTROLS PLOTS
# cat("\n--- 6HR SHIFT: Plots with Time Controls (Residualized) ---\n")
# plots_shift_controls <- lapply(particles, function(p) {
#   generate_rd_plots_residualized(data_ma_shift, p, "6hr Shift + Controls")
# })
# 
# print(plots_shift_controls[[1]] + plot_annotation(title = "PM10 – 6hr Shift (With Time Controls)"))
# print(plots_shift_controls[[2]] + plot_annotation(title = "PM2.5 – 6hr Shift (With Time Controls)"))
# print(plots_shift_controls[[3]] + plot_annotation(title = "O3 – 6hr Shift (With Time Controls)"))
# 
# # === COMPARISON: SIDE-BY-SIDE PLOTS ===
# cat("\n### SIDE-BY-SIDE COMPARISON PLOTS ###\n")
# 
# # Function to create side-by-side comparison
# create_comparison_plots <- function(plots_original, plots_controls, particle_names, shift_type) {
#   for(i in seq_along(particle_names)) {
#     if(!is.null(plots_original[[i]]) && !is.null(plots_controls[[i]])) {
#       combined_plot <- plots_original[[i]] / plots_controls[[i]] + 
#         plot_layout(nrow = 2) +
#         plot_annotation(
#           title = paste(particle_names[i], "–", shift_type, ": Original vs With Controls"),
#           subtitle = "Top: Original specification | Bottom: With time controls (residualized)"
#         )
#       print(combined_plot)
#     }
#   }
# }
# 
# # Generate comparison plots
# cat("\n--- Comparison: No Shift Specification ---\n")
# create_comparison_plots(plots_no_shift, plots_no_shift_controls, particles, "No Shift")
# 
# cat("\n--- Comparison: 6hr Shift Specification ---\n")
# create_comparison_plots(plots_shift, plots_shift_controls, particles, "6hr Shift")
# 
# # Analysis for each particle type - NO SHIFT
# cat("\n### NO SHIFT SPECIFICATION - NUMERICAL RESULTS ###\n")
# for(particle in particles) {
#   if(particle %in% names(data_ma_no_shift)) {
#     cat(paste("\n--- Analysis for", particle, "---\n"))
#     results <- compare_rd_results(data_ma_no_shift, particle)
#   }
# }
# 
# # Analysis for each particle type - 6HR SHIFT
# cat("\n### 6HR SHIFT SPECIFICATION - NUMERICAL RESULTS ###\n")
# for(particle in particles) {
#   if(particle %in% names(data_ma_shift)) {
#     cat(paste("\n--- Analysis for", particle, "---\n"))
#     results <- compare_rd_results(data_ma_shift, particle)
#   }
# }
# 
# print("\n=== INTERPRETATION GUIDE ===")
# cat("1. Compare coefficients between specifications with and without controls\n")
# cat("2. Time controls include: month FE, day-of-week FE, hour-of-day FE, weekend×hour interaction\n")
# cat("3. If results are robust, coefficients should be similar across specifications\n")
# cat("4. Controls help account for systematic time patterns in pollution levels\n")

############################## RDs with Time and Weather Controls ############################## 

# Libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(rdrobust)
library(rddensity)
library(patchwork)

# Adjust corresponding path
setwd("/Users/Alvlopzam/Desktop/ITAM/Semestre 10/PolPubII/HoyNoCircula/Codigo")

# Data loading
data_ma <- read.csv("../Datos/procesados/promedios_horas_moving_avg_24h.csv")

# Ensure 'date' is POSIXct
data_ma <- data_ma %>%
  mutate(date = ymd_hms(date))

# Extract date part to group by day
data_ma <- data_ma %>%
  arrange(date) %>%
  mutate(date_day = as_date(date))

# Identify days where policy was active all 24 hours
dates_with_policy <- data_ma %>%
  group_by(date_day) %>%
  summarize(policy_day = all(activacion_doble_no_circula == 1)) %>%
  filter(policy_day) %>%
  pull(date_day)

# Shift the treatment to 6:00am – 6:00am of the next day
data_ma <- data_ma %>%
  mutate(
    activacion_doble_no_circula = if_else(
      (as_date(date) %in% dates_with_policy & hour(date) >= 6) |
        (as_date(date - hours(6)) %in% dates_with_policy & hour(date) < 6),
      1, 0
    )
  )

#Function to create time-based dummy variables
create_time_dummies <- function(df) {
  # Ensure date column is in POSIXct format
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S")
  
  # Sort by date to ensure proper ordering
  df <- df[order(df$date), ]
  
  # Initialize all dummy columns with 0
  # 23 hours prior
  for(i in 23:1) {
    df[[paste0(i, "hr_prior")]] <- 0
  }
  
  # Treatment period
  df$treatment <- 0
  
  # 23 hours post
  for(i in 1:23) {
    df[[paste0(i, "hr_post")]] <- 0
  }
  
  # Find treatment periods (where activacion_doble_no_circula == 1)
  treatment_indices <- which(df$activacion_doble_no_circula == 1)
  
  # For each treatment period, mark the appropriate time windows
  for(treatment_idx in treatment_indices) {
    
    # Mark treatment period
    df$treatment[treatment_idx] <- 1
    
    # Mark prior hours (23 hours before)
    for(i in 1:23) {
      prior_idx <- treatment_idx - i
      if(prior_idx > 0) {
        df[[paste0(i, "hr_prior")]][prior_idx] <- 1
      }
    }
    
    # Mark post hours (23 hours after)
    for(i in 1:23) {
      post_idx <- treatment_idx + i
      if(post_idx <= nrow(df)) {
        df[[paste0(i, "hr_post")]][post_idx] <- 1
      }
    }
  }
  
  return(df)
}

# Function to create time controls
create_time_controls <- function(df) {
  df <- df %>%
    mutate(
      # Month of year (1-12)
      month = month(date),
      
      # Day of week (1=Sunday, 7=Saturday)
      day_of_week = wday(date),
      
      # Hour of day (0-23)
      hour_of_day = hour(date),
      
      # Weekend indicator (Saturday=7, Sunday=1)
      is_weekend = ifelse(day_of_week %in% c(1, 7), 1, 0),
      
      # Interaction: weekend * hour
      weekend_hour_interaction = is_weekend * hour_of_day
    )
  
  # Create month dummy variables (using January as reference)
  for(i in 2:12) {
    df[[paste0("month_", i)]] <- ifelse(df$month == i, 1, 0)
  }
  
  # Create day of week dummy variables (using Sunday as reference)
  for(i in 2:7) {
    df[[paste0("dow_", i)]] <- ifelse(df$day_of_week == i, 1, 0)
  }
  
  # Create hour of day dummy variables (using hour 0 as reference)
  for(i in 1:23) {
    df[[paste0("hour_", i)]] <- ifelse(df$hour_of_day == i, 1, 0)
  }
  
  return(df)
}

# NEW FUNCTION: Create weather controls
create_weather_controls <- function(df) {
  # Weather variables: RH (Relative Humidity), TMP (Temperature), WDR (Wind Direction), WSP (Wind Speed)
  
  # Check if weather variables exist in the dataset
  weather_vars <- c("RH", "TMP", "WDR", "WSP")
  existing_weather <- weather_vars[weather_vars %in% names(df)]
  
  if(length(existing_weather) == 0) {
    warning("No weather variables found in dataset. Skipping weather controls.")
    return(df)
  }
  
  cat("Weather controls created for variables:", paste(existing_weather, collapse = ", "), "\n")
  
  return(df)
}

# Apply the functions to your dataset
data_ma_rd <- create_time_dummies(data_ma)
data_ma_rd <- create_time_controls(data_ma_rd)
data_ma_rd <- create_weather_controls(data_ma_rd)  # NEW: Add weather controls

# Step 1: Create the running variable (time relative to treatment)
create_rd_data <- function(df) {
  # Create a time-to-treatment variable
  df$time_to_treatment <- NA
  
  # Find treatment periods
  treatment_indices <- which(df$treatment == 1)
  
  # For each observation, calculate distance to nearest treatment
  for(i in 1:nrow(df)) {
    if(length(treatment_indices) > 0) {
      # Find the closest treatment
      distances <- abs(i - treatment_indices)
      closest_treatment_idx <- treatment_indices[which.min(distances)]
      
      # Calculate relative time (negative = before treatment, positive = after)
      df$time_to_treatment[i] <- i - closest_treatment_idx
    }
  }
  
  # Create treatment indicator (1 if during or after treatment period)
  df$treated <- ifelse(df$time_to_treatment >= 0, 1, 0)
  
  return(df)
}

# Step 2: Basic RD visualization
plot_rd_raw <- function(data, bandwidth = 24) {
  # Filter data around discontinuity
  plot_data <- data %>%
    filter(abs(time_to_treatment) <= bandwidth) %>%
    filter(!is.na(PM10))
  
  ggplot(plot_data, aes(x = time_to_treatment, y = PM10)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(aes(group = treated), method = "loess", se = TRUE, span = 0.75) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
    labs(
      title = "RD Plot: PM10 Levels Around Treatment",
      x = "Hours from Treatment (0 = Treatment Start)",
      y = "PM10 Concentration",
      subtitle = paste("Bandwidth:", bandwidth, "hours")
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Step 3: Binned scatter plot for cleaner visualization
plot_rd_binned <- function(data, bandwidth = 24, n_bins = 20) {
  plot_data <- data %>%
    filter(abs(time_to_treatment) <= bandwidth) %>%
    filter(!is.na(PM10))
  
  # Create bins
  plot_data$bin <- cut(plot_data$time_to_treatment, 
                       breaks = n_bins, 
                       include.lowest = TRUE)
  
  # Calculate bin means
  bin_data <- plot_data %>%
    group_by(bin, treated) %>%
    summarise(
      mean_time = mean(time_to_treatment, na.rm = TRUE),
      mean_pm10 = mean(PM10, na.rm = TRUE),
      se_pm10 = sd(PM10, na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'
    )
  
  ggplot(bin_data, aes(x = mean_time, y = mean_pm10, color = factor(treated))) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = mean_pm10 - 1.96*se_pm10, 
                      ymax = mean_pm10 + 1.96*se_pm10), width = 0.5) +
    geom_smooth(method = "lm", se = TRUE, span = 0.75) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
    labs(
      title = "RD Plot: Binned PM10 Levels Around Treatment",
      x = "Hours from Treatment (0 = Treatment Start)",
      y = "Mean PM10 Concentration",
      color = "Treated"
    ) +
    scale_color_manual(values = c("0" = "blue", "1" = "red"),
                       labels = c("Pre-treatment", "Post-treatment")) +
    theme_minimal()
}

# UPDATED: Modified RD estimation function with time AND weather controls
run_rd_analysis_with_controls <- function(data, bandwidth = NULL, include_controls = TRUE) {
  # Filter out missing values
  analysis_data <- data %>%
    filter(!is.na(PM10), !is.na(time_to_treatment))
  
  if (include_controls) {
    # TIME CONTROLS
    # Month dummies (excluding reference month 1)
    month_vars <- paste0("month_", 2:12)
    
    # Day of week dummies (excluding reference day 1 = Sunday)
    dow_vars <- paste0("dow_", 2:7)
    
    # Hour dummies (excluding reference hour 0)
    hour_vars <- paste0("hour_", 1:23)
    
    # Weekend-hour interaction
    time_interaction_var <- "weekend_hour_interaction"
    
    # WEATHER CONTROLS
    weather_base_vars <- c("RH", "TMP", "WDR", "WSP")
    
    # Combine ALL control variables
    all_control_vars <- c(
      month_vars,                 # Time controls
      dow_vars, 
      hour_vars, 
      time_interaction_var,
      weather_base_vars          # Weather controls (linear terms only)
    )
    
    # Create covariates matrix (only include variables that exist in data)
    existing_vars <- all_control_vars[all_control_vars %in% names(analysis_data)]
    
    cat("Using", length(existing_vars), "control variables:\n")
    cat("Time controls:", length(c(month_vars, dow_vars, hour_vars, time_interaction_var)[c(month_vars, dow_vars, hour_vars, time_interaction_var) %in% existing_vars]), "\n")
    cat("Weather controls:", length(existing_vars) - length(c(month_vars, dow_vars, hour_vars, time_interaction_var)[c(month_vars, dow_vars, hour_vars, time_interaction_var) %in% existing_vars]), "\n")
    
    if (length(existing_vars) > 0) {
      covs_matrix <- as.matrix(analysis_data[, existing_vars, drop = FALSE])
      
      # Run RD estimation with covariates
      if(is.null(bandwidth)) {
        rd_result <- rdrobust(y = analysis_data$PM10, 
                              x = analysis_data$time_to_treatment,
                              c = 0,
                              covs = covs_matrix)
      } else {
        rd_result <- rdrobust(y = analysis_data$PM10, 
                              x = analysis_data$time_to_treatment,
                              c = 0, 
                              h = bandwidth,
                              covs = covs_matrix)
      }
    } else {
      warning("No control variables found in data. Running without controls.")
      if(is.null(bandwidth)) {
        rd_result <- rdrobust(y = analysis_data$PM10, 
                              x = analysis_data$time_to_treatment,
                              c = 0)
      } else {
        rd_result <- rdrobust(y = analysis_data$PM10, 
                              x = analysis_data$time_to_treatment,
                              c = 0, 
                              h = bandwidth)
      }
    }
  } else {
    # Run without controls (original specification)
    cat("Running analysis WITHOUT controls\n")
    if(is.null(bandwidth)) {
      rd_result <- rdrobust(y = analysis_data$PM10, 
                            x = analysis_data$time_to_treatment,
                            c = 0)
    } else {
      rd_result <- rdrobust(y = analysis_data$PM10, 
                            x = analysis_data$time_to_treatment,
                            c = 0, 
                            h = bandwidth)
    }
  }
  
  return(rd_result)
}

# UPDATED: Function to compare results with and without controls (now includes weather)
compare_rd_results <- function(data, particle_var = "PM10") {
  # Temporarily rename particle variable for analysis
  data$PM10 <- data[[particle_var]]
  rd_data <- create_rd_data(data)
  
  # Run analysis without controls
  cat("\n=== RESULTS WITHOUT CONTROLS ===\n")
  rd_no_controls <- run_rd_analysis_with_controls(rd_data, include_controls = FALSE)
  print(summary(rd_no_controls))
  
  # Run analysis with controls (time + weather)
  cat("\n=== RESULTS WITH TIME + WEATHER CONTROLS ===\n")
  rd_with_controls <- run_rd_analysis_with_controls(rd_data, include_controls = TRUE)
  print(summary(rd_with_controls))
  
  # Extract key results for comparison
  results_comparison <- data.frame(
    Specification = c("Without Controls", "With Time + Weather Controls"),
    Coefficient = c(rd_no_controls$coef[1], rd_with_controls$coef[1]),
    SE = c(rd_no_controls$se[1], rd_with_controls$se[1]),
    P_value = c(rd_no_controls$pv[1], rd_with_controls$pv[1]),
    CI_lower = c(rd_no_controls$ci[1,1], rd_with_controls$ci[1,1]),
    CI_upper = c(rd_no_controls$ci[1,2], rd_with_controls$ci[1,2]),
    Bandwidth = c(rd_no_controls$bws[1,1], rd_with_controls$bws[1,1]),
    N_left = c(rd_no_controls$N[1], rd_with_controls$N[1]),
    N_right = c(rd_no_controls$N[2], rd_with_controls$N[2])
  )
  
  cat("\n=== COMPARISON TABLE ===\n")
  print(results_comparison)
  
  return(list(
    no_controls = rd_no_controls,
    with_controls = rd_with_controls,
    comparison = results_comparison
  ))
}

############################# Final run with time + weather controls ################################

# Function to generate RD plots for a given particle and dataset
generate_rd_plots <- function(data, particle_var, title_prefix) {
  if (!particle_var %in% names(data)) {
    warning(paste("Skipping:", particle_var, "not found in data"))
    return(NULL)
  }
  
  data$PM10 <- data[[particle_var]]  # temporarily rename for plotting
  rd_data <- create_rd_data(data)
  
  p_raw <- plot_rd_raw(rd_data, bandwidth = 24) +
    labs(title = paste(title_prefix, "-", particle_var, "(Raw)"))
  p_binned <- plot_rd_binned(rd_data, bandwidth = 24) +
    labs(title = paste(title_prefix, "-", particle_var, "(Binned)"))
  
  return(p_raw + p_binned + plot_layout(ncol = 2))
}

# Define particles
particles <- c("PM10", "PM2.5", "O3")

# === NO SHIFT ===
data_ma_no_shift <- data_ma
data_ma_no_shift$activacion_doble_no_circula <- ifelse(
  as_date(data_ma_no_shift$date) %in% dates_with_policy, 1, 0
)
data_ma_no_shift <- create_time_dummies(data_ma_no_shift)
data_ma_no_shift <- create_time_controls(data_ma_no_shift)
data_ma_no_shift <- create_weather_controls(data_ma_no_shift)  # NEW: Add weather controls

# === WITH 6HR SHIFT ===
data_ma_shift <- data_ma  # already includes shifted treatment
data_ma_shift <- create_time_dummies(data_ma_shift)
data_ma_shift <- create_time_controls(data_ma_shift)
data_ma_shift <- create_weather_controls(data_ma_shift)  # NEW: Add weather controls

plots_no_shift <- lapply(particles, function(p) {
  generate_rd_plots(data_ma_no_shift, p, "No Shift")
})

plots_shift <- lapply(particles, function(p) {
  generate_rd_plots(data_ma_shift, p, "6hr Shift")
})

# === DISPLAY RESULTS ===

# NO SHIFT
print(plots_no_shift[[1]] + plot_annotation(title = "PM10 – No Shift"))
print(plots_no_shift[[2]] + plot_annotation(title = "PM2.5 – No Shift"))
print(plots_no_shift[[3]] + plot_annotation(title = "O3 – No Shift"))

# 6HR SHIFT
print(plots_shift[[1]] + plot_annotation(title = "PM10 – 6hr Shift"))
print(plots_shift[[2]] + plot_annotation(title = "PM2.5 – 6hr Shift"))
print(plots_shift[[3]] + plot_annotation(title = "O3 – 6hr Shift"))

# === ANALYSIS WITH AND WITHOUT CONTROLS ===

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("REGRESSION DISCONTINUITY ANALYSIS WITH TIME + WEATHER CONTROLS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# UPDATED: Function to generate residualized plots (after removing time + weather controls)
generate_rd_plots_residualized <- function(data, particle_var, title_prefix) {
  if (!particle_var %in% names(data)) {
    warning(paste("Skipping:", particle_var, "not found in data"))
    return(NULL)
  }
  
  # Create temporary copy for residualization
  temp_data <- data
  temp_data$outcome <- temp_data[[particle_var]]
  
  # Create formula for time controls
  month_vars <- paste0("month_", 2:12)
  dow_vars <- paste0("dow_", 2:7)
  hour_vars <- paste0("hour_", 1:23)
  time_interaction_var <- "weekend_hour_interaction"
  
  # Weather controls
  weather_base_vars <- c("RH", "TMP", "WDR", "WSP")
  
  all_control_vars <- c(month_vars, dow_vars, hour_vars, time_interaction_var,
                        weather_base_vars)
  existing_vars <- all_control_vars[all_control_vars %in% names(temp_data)]
  
  if(length(existing_vars) > 0) {
    # Residualize outcome against all controls
    control_formula <- as.formula(paste("outcome ~", paste(existing_vars, collapse = " + ")))
    residual_model <- lm(control_formula, data = temp_data, na.action = na.exclude)
    temp_data$PM10 <- residuals(residual_model)  # Use residualized outcome for plotting
    
    # Add back the mean for interpretability
    temp_data$PM10 <- temp_data$PM10 + mean(temp_data$outcome, na.rm = TRUE)
  } else {
    temp_data$PM10 <- temp_data$outcome  # No residualization if no controls
  }
  
  rd_data <- create_rd_data(temp_data)
  
  p_raw <- plot_rd_raw(rd_data, bandwidth = 24) +
    labs(title = paste(title_prefix, "-", particle_var, "(Raw, Residualized)"),
         subtitle = "Outcome residualized against time + weather controls")
  p_binned <- plot_rd_binned(rd_data, bandwidth = 24) +
    labs(title = paste(title_prefix, "-", particle_var, "(Binned, Residualized)"),
         subtitle = "Outcome residualized against time + weather controls")
  
  return(p_raw + p_binned + plot_layout(ncol = 2))
}

# NO SHIFT - WITH CONTROLS PLOTS
cat("\n--- NO SHIFT: Plots with Time + Weather Controls (Residualized) ---\n")
plots_no_shift_controls <- lapply(particles, function(p) {
  generate_rd_plots_residualized(data_ma_no_shift, p, "No Shift + Controls")
})

print(plots_no_shift_controls[[1]] + plot_annotation(title = "PM10 – No Shift (With Time + Weather Controls)"))
print(plots_no_shift_controls[[2]] + plot_annotation(title = "PM2.5 – No Shift (With Time + Weather Controls)"))
print(plots_no_shift_controls[[3]] + plot_annotation(title = "O3 – No Shift (With Time + Weather Controls)"))

# 6HR SHIFT - WITH CONTROLS PLOTS
cat("\n--- 6HR SHIFT: Plots with Time + Weather Controls (Residualized) ---\n")
plots_shift_controls <- lapply(particles, function(p) {
  generate_rd_plots_residualized(data_ma_shift, p, "6hr Shift + Controls")
})

print(plots_shift_controls[[1]] + plot_annotation(title = "PM10 – 6hr Shift (With Time + Weather Controls)"))
print(plots_shift_controls[[2]] + plot_annotation(title = "PM2.5 – 6hr Shift (With Time + Weather Controls)"))
print(plots_shift_controls[[3]] + plot_annotation(title = "O3 – 6hr Shift (With Time + Weather Controls)"))

# === COMPARISON: SIDE-BY-SIDE PLOTS ===
cat("\n### SIDE-BY-SIDE COMPARISON PLOTS ###\n")

# Function to create side-by-side comparison
create_comparison_plots <- function(plots_original, plots_controls, particle_names, shift_type) {
  for(i in seq_along(particle_names)) {
    if(!is.null(plots_original[[i]]) && !is.null(plots_controls[[i]])) {
      combined_plot <- plots_original[[i]] / plots_controls[[i]] + 
        plot_layout(nrow = 2) +
        plot_annotation(
          title = paste(particle_names[i], "–", shift_type, ": Original vs With Controls"),
          subtitle = "Top: Original specification | Bottom: With time + weather controls (residualized)"
        )
      print(combined_plot)
    }
  }
}

# Generate comparison plots
cat("\n--- Comparison: No Shift Specification ---\n")
create_comparison_plots(plots_no_shift, plots_no_shift_controls, particles, "No Shift")

cat("\n--- Comparison: 6hr Shift Specification ---\n")
create_comparison_plots(plots_shift, plots_shift_controls, particles, "6hr Shift")

# Analysis for each particle type - NO SHIFT
cat("\n### NO SHIFT SPECIFICATION - NUMERICAL RESULTS ###\n")
for(particle in particles) {
  if(particle %in% names(data_ma_no_shift)) {
    cat(paste("\n--- Analysis for", particle, "---\n"))
    results <- compare_rd_results(data_ma_no_shift, particle)
  }
}

# Analysis for each particle type - 6HR SHIFT
cat("\n### 6HR SHIFT SPECIFICATION - NUMERICAL RESULTS ###\n")
for(particle in particles) {
  if(particle %in% names(data_ma_shift)) {
    cat(paste("\n--- Analysis for", particle, "---\n"))
    results <- compare_rd_results(data_ma_shift, particle)
  }
}

print("\n=== INTERPRETATION GUIDE ===")
cat("1. Compare coefficients between specifications with and without controls\n")
cat("2. Time controls include: month FE, day-of-week FE, hour-of-day FE, weekend×hour interaction\n")
cat("3. Weather controls include: RH, TMP, WDR, WSP (linear terms only)\n")
cat("4. If results are robust, coefficients should be similar across specifications\n")
cat("5. Controls help account for systematic time patterns and weather effects on pollution levels\n")