############################## RDs ############################## 

# Ajustar el path como corresponda
setwd("/Users/Alvlopzam/Desktop/ITAM/Semestre 10/PolPubII/HoyNoCircula/Codigo")

# Lectura de datos
data_ma <- read.csv("../Datos/procesados/promedios_horas_moving_avg_24h.csv")


library(dplyr)
library(lubridate)

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




# Load required libraries
library(dplyr)
library(lubridate)

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

# Apply the function to your dataset
# Assuming your dataset is called 'your_data'
data_ma_rd <- create_time_dummies(data_ma)


# Load required libraries
library(dplyr)
library(ggplot2)
library(rdrobust)
library(rddensity)
library(lubridate)

# Assuming you already have your data with dummy variables from the previous step
# Let's call it 'data_ma_rd'

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

# Apply the function
rd_data <- create_rd_data(data_ma_rd)

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

# Step 4: RD estimation using rdrobust
run_rd_analysis <- function(data, bandwidth = NULL) {
  # Filter out missing values
  analysis_data <- data %>%
    filter(!is.na(PM10), !is.na(time_to_treatment))
  
  # Run RD estimation
  if(is.null(bandwidth)) {
    # Let rdrobust choose optimal bandwidth
    rd_result <- rdrobust(y = analysis_data$PM10, 
                          x = analysis_data$time_to_treatment,
                          c = 0)
  } else {
    # Use specified bandwidth
    rd_result <- rdrobust(y = analysis_data$PM10, 
                          x = analysis_data$time_to_treatment,
                          c = 0, 
                          h = bandwidth)
  }
  
  return(rd_result)
}

# Step 7: Execute the analysis
print("=== REGRESSION DISCONTINUITY ANALYSIS FOR PM10 ===")

# Create RD data
rd_data <- create_rd_data(data_ma_rd)

# Basic summary statistics
cat("\nSummary of data around treatment:\n")
summary_stats <- rd_data %>%
  filter(abs(time_to_treatment) <= 24) %>%
  group_by(treated) %>%
  summarise(
    n_obs = n(),
    mean_pm10 = mean(PM10, na.rm = TRUE),
    sd_pm10 = sd(PM10, na.rm = TRUE),
    .groups = 'drop'
  )
print(summary_stats)

# Generate plots
print("\nGenerating RD plots...")
p1 <- plot_rd_raw(rd_data, bandwidth = 24)
p2 <- plot_rd_binned(rd_data, bandwidth = 24)

print(p1)
print(p2)

# Main RD estimation
print("\n=== MAIN RD RESULTS ===")
main_rd <- run_rd_analysis(rd_data)
print(summary(main_rd))

print("\n=== INTERPRETATION GUIDE ===")
cat("1. Look at the RD plots to visually assess if there's a discontinuity at time 0\n")
cat("2. The main RD estimate shows the treatment effect size and significance\n")


############################# Final run ################################
library(patchwork)

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

plots_no_shift <- lapply(particles, function(p) {
  generate_rd_plots(data_ma_no_shift, p, "No Shift")
})

# === WITH 6HR SHIFT ===
data_ma_shift <- data_ma  # already includes shifted treatment
data_ma_shift <- create_time_dummies(data_ma_shift)

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








############################## RDs WITH FIXED EFFECTS & PLOTS ##############################

# Set working directory
setwd("/Users/Alvlopzam/Desktop/ITAM/Semestre 10/PolPubII/HoyNoCircula/Codigo")

# Load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(fixest)

# Load data
data_ma <- read.csv("../Datos/procesados/promedios_horas_moving_avg_24h.csv") %>%
  mutate(date = ymd_hms(date)) %>%
  arrange(date) %>%
  mutate(date_day = as_date(date))

# Identify days with full-day policy
dates_with_policy <- data_ma %>%
  group_by(date_day) %>%
  summarize(policy_day = all(activacion_doble_no_circula == 1)) %>%
  filter(policy_day) %>%
  pull(date_day)

# Apply 6am-6am shift to treatment variable
data_ma <- data_ma %>%
  mutate(
    activacion_doble_no_circula_shifted = if_else(
      (as_date(date) %in% dates_with_policy & hour(date) >= 6) |
        (as_date(date - hours(6)) %in% dates_with_policy & hour(date) < 6),
      1, 0
    )
  )

# Function to create RD variables
create_rd_data <- function(df, treatment_col) {
  df$treatment <- df[[treatment_col]]
  df$time_to_treatment <- NA
  treatment_indices <- which(df$treatment == 1)
  for(i in 1:nrow(df)) {
    if(length(treatment_indices) > 0) {
      distances <- abs(i - treatment_indices)
      closest_treatment_idx <- treatment_indices[which.min(distances)]
      df$time_to_treatment[i] <- i - closest_treatment_idx
    }
  }
  df$treated <- ifelse(df$time_to_treatment >= 0, 1, 0)
  return(df)
}

# Prepare data for both versions
data_noshift <- create_rd_data(data_ma, "activacion_doble_no_circula")
data_shifted <- create_rd_data(data_ma, "activacion_doble_no_circula_shifted")

# Create fixed effects
add_fixed_effects <- function(df) {
  df$year_fe <- factor(year(df$date))
  df$dow_fe <- factor(wday(df$date, label = TRUE))
  df$hour_fe <- factor(hour(df$date))
  df$weekend <- ifelse(wday(df$date) %in% c(1, 7), 1, 0)
  df$weekend_hour <- interaction(df$weekend, df$hour_fe)
  return(df)
}

data_noshift <- add_fixed_effects(data_noshift)
data_shifted <- add_fixed_effects(data_shifted)

# Function to run RD with fixed effects
run_rd_with_fe <- function(data, particle) {
  df <- data %>%
    filter(!is.na(time_to_treatment), !is.na(.data[[particle]]), abs(time_to_treatment) <= 24)
  
  formula <- as.formula(paste0(particle, " ~ treated | year_fe + dow_fe + hour_fe + weekend_hour"))
  
  model <- feols(formula, data = df)
  
  return(list(model = model, data = df))
}

# Function to plot results
plot_rd_result <- function(df, model, pollutant, shift_label) {
  df$pred <- predict(model, newdata = df)
  avg_df <- df %>%
    group_by(time_to_treatment) %>%
    summarize(mean_pollutant = mean(.data[[pollutant]], na.rm = TRUE),
              mean_pred = mean(pred, na.rm = TRUE),
              se = sd(.data[[pollutant]], na.rm = TRUE) / sqrt(n())) %>%
    filter(abs(time_to_treatment) <= 24)
  
  ggplot(avg_df, aes(x = time_to_treatment)) +
    geom_point(aes(y = mean_pollutant), color = "black", alpha = 0.6) +
    geom_line(aes(y = mean_pred), color = "blue", size = 1) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(ymin = mean_pollutant - 1.96*se, ymax = mean_pollutant + 1.96*se), width = 0.5) +
    labs(title = paste("RD with Fixed Effects:", pollutant, "-", shift_label),
         x = "Hours from treatment",
         y = pollutant) +
    theme_minimal()
}

# Loop over pollutants and display both model summaries and plots
for (pollutant in c("PM10", "PM2.5", "O3")) {
  # No Shift
  result_noshift <- run_rd_with_fe(data_noshift, pollutant)
  cat("=== RD with Fixed Effects: ", pollutant, " (No Shift) ===\n")
  print(summary(result_noshift$model, cluster = "date_day"))
  print(plot_rd_result(result_noshift$data, result_noshift$model, pollutant, "No Shift"))
  
  # Shifted
  result_shifted <- run_rd_with_fe(data_shifted, pollutant)
  cat("=== RD with Fixed Effects: ", pollutant, " (Shifted) ===\n")
  print(summary(result_shifted$model, cluster = "date_day"))
  print(plot_rd_result(result_shifted$data, result_shifted$model, pollutant, "Shifted"))
}

