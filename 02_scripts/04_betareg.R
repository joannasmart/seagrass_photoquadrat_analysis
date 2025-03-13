# Load required packages
library(tidyverse)
library(broom.mixed)
library(betareg)
library(gridExtra)
library(lmtest)  # For likelihood ratio tests
library(car)     # For diagnostic plots
library(performance)  # For model performance checks


# Load the dataset
df <- read.csv("03_outputs/supplementary_tables/supplementary_table_2_individual_bank_summary.csv")
dictionary <- read_csv("01_data/Dictionary.csv")

total_seagrass <- df %>% filter(category_name == "total_seagrass")

seagrasses <- dictionary %>% 
  filter(category == "seagrass") %>% 
  filter(name != "Cymodocea.rotundata") %>% 
  filter(name != "Seagrass") %>% 
  filter(name != "Dead.seagrass")

df <- df %>%
  filter(category_name %in% c(seagrasses$name, "total_seagrass")) %>%
  filter(bank != "CH")

# Convert year_month to Date format and extract year as numeric
df$year_month <- as.Date(df$year_month)
df$year <- as.numeric(format(df$year_month, "%Y"))
df$Mean_Cover <- as.numeric(df$Mean_Cover)
df$category_name <- as.factor(df$category_name)
df$bank <- as.factor(df$bank)

# Convert Mean_Cover to proportions and handle bounds for beta regression
df <- df %>%
  mutate(Mean_Cover = Mean_Cover / 100) %>%  # Convert from 0-100 to proportion
  mutate(Mean_Cover = ifelse(Mean_Cover <= 0, 0.001, Mean_Cover),
         Mean_Cover = ifelse(Mean_Cover >= 1, 0.999, Mean_Cover))

# Check for NAs and infinites
if(sum(is.na(df$Mean_Cover)) > 0) warning("NAs found in Mean_Cover")
if(sum(is.na(df$year)) > 0) warning("NAs found in year")
if(sum(is.na(df$category_name)) > 0) warning("NAs found in category_name")
if(sum(is.infinite(df$Mean_Cover)) > 0) warning("Infinite values found in Mean_Cover")
if(sum(is.infinite(df$year)) > 0) warning("Infinite values found in year")

# Get unique species and banks
species_list <- levels(df$category_name)
bank_list <- levels(df$bank)

# Initialize results list
results_list <- list()

# Loop through each species and bank
for (current_species in species_list) {
  for (current_bank in bank_list) {
    # Print progress
    cat(sprintf("\nProcessing species: %s at bank: %s\n", current_species, current_bank))
    
    # Subset the data
    species_bank_data <- df %>% 
      filter(bank == current_bank & category_name == current_species)
    
    # Debug print
    cat(sprintf("Number of observations: %d\n", nrow(species_bank_data)))
    
    # Store results in a list first
    result <- list(
      bank = current_bank,
      species = current_species
    )
    
    # Check if there is enough data
    if (nrow(species_bank_data) >= 3 && length(unique(species_bank_data$year)) > 1) {
      tryCatch({
        # Fit beta regression model
        model <- betareg(Mean_Cover ~ year, data = species_bank_data)
        
        # Get the summary
        model_summary <- summary(model)
        
        # Extract coefficients
        result$intercept <- model_summary$coefficients$mean["(Intercept)", "Estimate"]
        result$slope <- model_summary$coefficients$mean["year", "Estimate"]
        result$intercept_se <- model_summary$coefficients$mean["(Intercept)", "Std. Error"]
        result$slope_se <- model_summary$coefficients$mean["year", "Std. Error"]
        result$intercept_pvalue <- model_summary$coefficients$mean["(Intercept)", "Pr(>|z|)"]
        result$slope_pvalue <- model_summary$coefficients$mean["year", "Pr(>|z|)"]
        
        # Model Diagnostics
        # 1. Likelihood Ratio Test (compare to null model)
        null_model <- betareg(Mean_Cover ~ 1, data = species_bank_data)
        lrt_result <- lrtest(null_model, model)
        result$lr_chi2 <- lrt_result$Chisq[2]
        result$lr_pvalue <- lrt_result$`Pr(>Chisq)`[2]
        
        # 2. Pseudo R-squared (McFadden's)
        result$pseudo_r2 <- 1 - (logLik(model) / logLik(null_model))
        
        # 3. Model Performance Metrics
        perf_metrics <- model_performance(model)
        result$aic <- perf_metrics$AIC
        result$bic <- perf_metrics$BIC
        
        # 4. Residual Diagnostics
        
        # Extract residuals (use "pearson" or "response")
        residuals <- residuals(model, type = "pearson")
        
        # Check normality of residuals
        shapiro_test <- shapiro.test(residuals)
        result$residuals_normality_pvalue <- shapiro_test$p.value
        
        # Diagnostic Plots 
        # PDF of diagnostic plots
        pdf(paste0("03_outputs/diagnostic_plots/diagnostic_plots_", current_species, "_", current_bank, ".pdf"))
        par(mfrow=c(2,2))
        plot(model)  # Standard model diagnostic plots
        # QQ plot of residuals
        qqPlot(residuals, main = "Q-Q Plot of Residuals")
        dev.off()
        
        # Check normality of residuals
        shapiro_test <- shapiro.test(residuals)
        result$residuals_normality_pvalue <- shapiro_test$p.value
        
      }, error = function(e) {
        warning(sprintf("Error fitting model for %s at %s: %s", 
                        current_species, current_bank, e$message))
        result$intercept <- result$slope <- result$intercept_se <- 
          result$slope_se <- result$intercept_pvalue <- result$slope_pvalue <- 
          result$lr_chi2 <- result$lr_pvalue <- result$pseudo_r2 <- 
          result$aic <- result$bic <- result$residuals_normality_pvalue <- NA
      })
    } else {
      warning(sprintf("Insufficient data for %s at %s", current_species, current_bank))
      result$intercept <- result$slope <- result$intercept_se <- 
        result$slope_se <- result$intercept_pvalue <- result$slope_pvalue <- 
        result$lr_chi2 <- result$lr_pvalue <- result$pseudo_r2 <- 
        result$aic <- result$bic <- result$residuals_normality_pvalue <- NA
    }
    
    # Add to results list
    results_list[[paste(current_species, current_bank)]] <- result
  }
}

# Modify results conversion to handle potential missing values
results_df <- do.call(rbind, lapply(results_list, function(x) {
  # Check if x is NULL or lacks essential elements
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  
  # Create a data frame with NA as default for missing values
  data.frame(
    bank = x$bank %||% NA_character_,
    species = x$species %||% NA_character_,
    intercept = x$intercept %||% NA_real_,
    intercept_se = x$intercept_se %||% NA_real_,
    slope = x$slope %||% NA_real_,
    slope_se = x$slope_se %||% NA_real_,
    intercept_pvalue = x$intercept_pvalue %||% NA_real_,
    slope_pvalue = x$slope_pvalue %||% NA_real_,
    intercept_sig = if (!is.na(x$intercept_pvalue)) {
      case_when(
        x$intercept_pvalue < 0.001 ~ "***",
        x$intercept_pvalue < 0.01 ~ "**",
        x$intercept_pvalue < 0.05 ~ "*",
        TRUE ~ "ns"
      )
    } else {
      NA_character_
    },
    slope_sig = if (!is.na(x$slope_pvalue)) {
      case_when(
        x$slope_pvalue < 0.001 ~ "***",
        x$slope_pvalue < 0.01 ~ "**",
        x$slope_pvalue < 0.05 ~ "*",
        TRUE ~ "ns"
      )
    } else {
      NA_character_
    },
    likelihood_ratio_chi2 = x$lr_chi2 %||% NA_real_,
    likelihood_ratio_pvalue = x$lr_pvalue %||% NA_real_,
    pseudo_r2 = x$pseudo_r2 %||% NA_real_,
    aic = x$aic %||% NA_real_,
    bic = x$bic %||% NA_real_,
    residuals_normality_pvalue = x$residuals_normality_pvalue %||% NA_real_
  )
}))

# Convert rownames to numeric indices
rownames(results_df) <- NULL

# Print the results
print(results_df)

# Write results to CSV
write.csv(results_df, "03_outputs/beta_regression_results.csv", row.names = FALSE)

# Create predicted values with confidence intervals
pred_data <- map_df(1:nrow(results_df), function(i) {
  row <- results_df[i,]
  
  # Get the original data for this species and bank
  original_data <- df %>% 
    filter(category_name == row$species, bank == row$bank)
  
  # Create sequence of years for prediction
  year_seq <- seq(min(df$year), max(df$year), by = 0.1)
  
  # Only calculate predictions and CIs if we have a valid model
  if(!is.na(row$slope)) {
    # Recreate the model
    model <- betareg(Mean_Cover ~ year, data = original_data)
    
    # Create design matrix for predictions
    X_new <- cbind(1, year_seq)
    
    # Get coefficients and vcov matrix
    beta_hat <- coef(model)[1:2]  # Just the mean model coefficients
    vcov_mat <- vcov(model)[1:2, 1:2]  # Just the mean model variance-covariance
    
    # Compute standard errors for fitted values
    se_fit <- sqrt(rowSums((X_new %*% vcov_mat) * X_new))
    
    # Calculate predicted values and CIs on link scale
    link_pred <- X_new %*% beta_hat
    link_lwr <- link_pred - 1.96 * se_fit
    link_upr <- link_pred + 1.96 * se_fit
    
    # Transform to response scale using inverse link function
    pred <- plogis(link_pred)
    lwr <- plogis(link_lwr)
    upr <- plogis(link_upr)
  } else {
    pred <- rep(NA, length(year_seq))
    lwr <- rep(NA, length(year_seq))
    upr <- rep(NA, length(year_seq))
  }
  
  data.frame(
    year = year_seq,
    pred = pred,
    lwr = lwr,
    upr = upr,
    species = row$species,
    bank = row$bank,
    significance = row$slope_sig
  )
})

# Scale the confidence intervals
df <- df %>%
  mutate( 
    Lower_CI = Lower_CI/100,  # Convert to proportions and bound at 0
    Upper_CI = Upper_CI/100  # Convert to proportions and bound at 1
  )

# Set up labels
bank.labs <- c("Amity", "Maroom", "Moreton", "Wanga-Wallen")
names(bank.labs) <- c("AM", "MA", "MO", "WA")

new_labels <- c("Oceana serrulata", "Halodule uninervis", "Halophila ovalis", 
                "Halophila spinsulosa", "Syringodium iseotifolium", "Total seagrass", "Zostera muelleri")
names(new_labels) <- c("Cymodocea.serrulata", "Halodule.uninervis", "Halophila.ovalis", 
                       "Halophila.spinulosa", "Syringodium", "total_seagrass", "Zostera.muelleri")

# Create the plot with confidence intervals and dotted lines for non-significant trends
p <- ggplot() +
  # Add confidence intervals as ribbons
  geom_ribbon(data = pred_data %>% filter(species != "total_seagrass"),
              aes(x = year, ymin = lwr, ymax = upr, fill = species),
              alpha = 0.1) +
  # Add trend lines with different line types based on significance
  geom_line(data = pred_data %>% filter(species != "total_seagrass"),
            aes(x = year, y = pred, color = species,
                linetype = significance == "ns"),  # non-significant gets TRUE = dotted
            size = 0.5) +
  geom_point(data = df %>% filter(category_name != "total_seagrass"),
             aes(x = year, y = Mean_Cover, color = category_name),
             size = 1) +
  geom_errorbar(data = df %>% filter(category_name != "total_seagrass"),
                aes(x = year,
                    y = Mean_Cover,
                    ymin = Lower_CI,
                    ymax = Upper_CI,
                    color = category_name),
                alpha = 0.5,
                width = 0.3,
                size = 0.5) +
  scale_linetype_manual(values = c(`TRUE` = "dotted", `FALSE` = "solid"), guide = "none") +
  facet_wrap(. ~ bank,
             scales = "free_y",
             labeller = labeller(bank = bank.labs,
                                 category_name = new_labels)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 1, by = 0.25)) +
  scale_colour_brewer(palette = "Dark2",
                      labels = new_labels) +
  scale_fill_brewer(palette = "Dark2",
                    labels = new_labels) +
  labs(x = "Year",
       y = "Cover (%)",
       color = "Species",
       fill = "Species") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.05),
        legend.text = element_text(face = "italic"))

# Display the plot
p


# Save the plot
ggsave("04_figures/individual_bank/seagrass_trends.png", p, width = 12, height = 7, dpi = 300)


# Total seagrass plot with confidence intervals and dotted lines for non-significant trends
p_total_seagrass <- ggplot() +
  # Add confidence intervals as ribbons
  geom_ribbon(data = pred_data %>% filter(species == "total_seagrass"),
              aes(x = year, ymin = lwr, ymax = upr),
              fill = "grey70",
              alpha = 0.2) +
  # Add trend lines with different line types based on significance
  geom_line(data = pred_data %>% filter(species == "total_seagrass"),
            aes(x = year, y = pred,
                linetype = significance == "ns"),  # non-significant gets TRUE = dotted
            size = 0.5) +
  geom_point(data = df %>% filter(category_name == "total_seagrass"),
             aes(x = year, y = Mean_Cover),
             size = 1) +
  geom_errorbar(data = df %>% filter(category_name == "total_seagrass"),
                aes(x = year,
                    y = Mean_Cover,
                    ymin = Lower_CI,
                    ymax = Upper_CI),
                alpha = 0.5,
                width = 0.3,
                size = 0.5) +
  scale_linetype_manual(values = c(`TRUE` = "dotted", `FALSE` = "solid"), guide = "none") +
  facet_wrap(. ~ bank,
             scales = "free_y",
             labeller = labeller(bank = bank.labs)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 1, by = 0.25)) +
  labs(x = "Year",
       y = "Cover (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.05),
        legend.text = element_text(face = "italic"))

# Display the plot
p_total_seagrass

# Save the total seagrass plot
ggsave("04_figures/individual_bank/total_seagrass_trends.png", p_total_seagrass, width = 12, height = 7, dpi = 300)



# Create a function to calculate absolute change in percentage points
calculate_percentage_point_change <- function(intercept, slope, start_year, end_year) {
  # Calculate predictions for start and end years on logit scale
  # and convert to percentages (multiply by 100)
  start_pred <- plogis(intercept + slope * start_year) * 100
  end_pred <- plogis(intercept + slope * end_year) * 100
  
  # Calculate absolute change in percentage points
  change <- end_pred - start_pred
  
  return(change)
}

# Get the year range from the original data
year_range <- range(df$year, na.rm = TRUE)
start_year <- year_range[1]
end_year <- year_range[2]

# Create summary table
summary_table <- results_df %>%
  mutate(
    # Round numeric columns
    intercept = round(intercept, 4),
    intercept_se = round(intercept_se, 4),
    slope = round(slope, 4),
    slope_se = round(slope_se, 4),
    slope_pvalue = round(slope_pvalue, 4),
    # Calculate absolute change in percentage points
    percent_change = mapply(calculate_percentage_point_change, 
                            intercept, 
                            slope, 
                            MoreArgs = list(start_year = start_year, 
                                            end_year = end_year)),
    # Round the change
    percent_change = round(percent_change, 1)
  ) %>%
  # Select and rename columns
  select(
    Bank = bank,
    Species = species,
    Intercept = intercept,
    `Intercept SE` = intercept_se,
    Slope = slope,
    `Slope SE` = slope_se,
    `Slope p-value` = slope_pvalue,
    Significance = slope_sig,
    `Change in Cover (percentage points)` = percent_change
  ) %>%
  # Replace species codes with full names
  dplyr::mutate(
    Species = case_when(
      Species == "Cymodocea.serrulata" ~ "Oceana serrulata",
      Species == "Halodule.uninervis" ~ "Halodule uninervis",
      Species == "Halophila.ovalis" ~ "Halophila ovalis",
      Species == "Halophila.spinulosa" ~ "Halophila spinulosa",
      Species == "Syringodium" ~ "Syringodium iseotifolium",
      Species == "Zostera.muelleri" ~ "Zostera muelleri", 
      Species == "total_seagrass" ~ "Total seagrass",
      TRUE ~ Species
    ),
    # Replace bank codes with full names
    Bank = case_when(
      Bank == "AM" ~ "Amity",
      Bank == "MA" ~ "Maroom",
      Bank == "MO" ~ "Moreton",
      Bank == "WA" ~ "Wanga-Wallen",
      TRUE ~ Bank
    )
  ) %>%
  # Arrange by Bank and Species
  dplyr::arrange(Bank, Species)

# Add plus sign to positive changes
summary_table <- summary_table %>%
  mutate(`Change in Cover (percentage points)` = ifelse(`Change in Cover (percentage points)` > 0, 
                                                        paste0("+", `Change in Cover (percentage points)`),
                                                        as.character(`Change in Cover (percentage points)`)))

# Print the table
print(summary_table)

# Save the table
write.csv(summary_table, "03_outputs/summary_table_for_publication.csv", row.names = FALSE)




