# ----- Eastern Banks Water Quality Data ----- #

# data is sourced from the Healthy Land and Water EHMP program

library(tidyverse)
library(readxl)
library(lubridate)
library(corrplot)

water_quality <- read.csv("01_data/02_water_quality/EHMP_WQ.csv")

colnames(water_quality) <- gsub("\\.+", ".", colnames(water_quality))
colnames(water_quality) <- gsub("\\.$", "", colnames(water_quality))
water_quality[, 3:4] <- lapply(water_quality[, 3:4], as.numeric)
water_quality$Secchi.depth.metres <- as.numeric(water_quality$Secchi.depth.metres)

water_quality$Survey.date <- as.Date(water_quality$Survey.date, "%d/%m/%Y")

water_quality_long <- water_quality %>%
  pivot_longer(cols = -c(Location, Location.name, Lat.GDA94, Long.GDA94, Survey.date, Survey.time, Depth.m, Secchi.depth.metres), 
               names_to = "Parameter", 
               values_to = "Value")

water_quality_long$Survey.date <- as.Date(water_quality_long$Survey.date)

write.csv(water_quality_long, "03_outputs/water_quality_long.csv")

water_quality_long <- read.csv("03_outputs/water_quality_long.csv")
water_quality <- read.csv("03_outputs/water_quality_tidy.csv")

water_quality_long$Survey.date <- as.Date(water_quality_long$Survey.date)

# explore the correlation between parameters

correlation <- cor(water_quality[, 8:23], use = "pairwise.complete.obs")
corrplot(correlation, method = "square", order = "FPC", type = "lower", diag = FALSE)


flood_dates <- as.Date(c("2013-01-01", "2011-01-01", "2015-05-01", "2022-02-01" ))
flood_dates_numeric <- as.numeric(flood_dates)


parameter_labels <- c("Total Nitrogen (mg/L)", "Ammonia (mg/L)", "Salinity (g/L)", "Turbidity (NTU)")

names(parameter_labels) <- c("Total.N.mg.L", "Ammonia.mg.L", 
                       "Salinity.g.L", "Turbidity.NTU")



df_percentiles <- water_quality_long %>%
  group_by(Parameter) %>%
  summarise(
    percentile_5 = quantile(Value, 0.05, na.rm = TRUE),
    percentile_95 = quantile(Value, 0.95, na.rm = TRUE)
  )




# Combined plot -----------------------------------------------------------


combined_plot <- ggplot(water_quality_long %>% filter(Parameter %in% c("Total.N.mg.L", "Ammonia.mg.L", 
                                                                       "Salinity.g.L", "Turbidity.NTU")), 
       aes(x = Survey.date, y = Value, group = Location, colour = Location)) +
  geom_hline(data = df_percentiles %>% filter(Parameter %in% c("Total.N.mg.L", "Ammonia.mg.L", 
                                                               "Salinity.g.L", "Turbidity.NTU")), 
             aes(yintercept = percentile_5), color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_hline(data = df_percentiles %>% filter(Parameter %in% c("Total.N.mg.L", "Ammonia.mg.L", 
                                                               "Salinity.g.L", "Turbidity.NTU")), 
             aes(yintercept = percentile_95), color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = flood_dates, colour = "lightblue", linewidth = 1.2) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ Parameter, scales = "free_y", labeller = labeller(Parameter = parameter_labels)) +
  labs(x = "Year") +
  scale_x_date(date_labels = "%Y", date_breaks = "2 year") +
  scale_colour_brewer(palette = "Dark2") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

combined_plot

ggsave(combined_plot, 
       file = "04_Figures/water_quality/combined_water_quality.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)









# Single plots

ggplot(water_quality_long %>% filter(Parameter == "Ammonia.mg.L"), 
       aes(x = Survey.date, y = Value * 1000, group = Location, colour = Location)) +
  geom_point() +
  geom_path() +
  theme_bw() +
  labs(x = "Date", y = "Ammonia concentration (µg/L)") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed")

ggplot(water_quality_long %>% filter(Parameter == "DO.mg.L"), 
       aes(x = Survey.date, y = Value, group = Location, colour = Location)) +
  geom_point() +
  geom_path() +
  theme_bw() + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(x = "Date", y = "Concentraiton (mg/L)", title = "Dissolved Oxygen") +
  geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed")

ggplot(water_quality_long %>% filter(Parameter == "Salinity.g.L"), 
       aes(x = Survey.date, y = Value, group = Location, colour = Location)) +
  geom_point() +
  geom_path() +
  theme_bw() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(x = "Date", y = "Concentraiton (mg/L)", title = "Salinity (PPT)") +
  geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed") 

ggplot(water_quality_long %>% filter(Parameter == "Turbidity.NTU"), 
       aes(x = Survey.date, y = Value, group = Location, colour = Location)) +
  geom_point() +
  geom_path() +
  theme_bw() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(x = "Date", y = "Concentraiton (mg/L)", title = "Turbidity (NTU)") +
  geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed") 


# create water quality averages

seagrass_data <- read.csv("01_Data/compiled_data_wideform.csv")
sampling_dates <- unique(seagrass_data$year_month)


# average the water quality data for each month

water_quality_long$year_month <- format(as.Date(water_quality_long$Survey.date, "%Y-%m-%d"), "%Y-%m") 


water_quality_monthly_average <- water_quality_long %>%
  drop_na(Value) %>%
  select(Location, year_month, Parameter, Value) %>%
  group_by(year_month, Parameter) %>%
  dplyr:: summarise(monthly_mean = mean(Value))


# identify which sampling dates do not have matching water quality dates

setdiff(sampling_dates, water_quality_monthly_average$year_month)

# since there are four dates where there is no WQ, we will select the closest dates

unique(water_quality_monthly_average$year_month)

closest_dates <- c("2021-05", "2022-05", "2023-05", "2014-06", "2015-05")

sampling_dates <- c(sampling_dates, closest_dates)

# select the water quality sampling dates that are closest to our sampling

wq_sampling <- water_quality_monthly_average %>%
  filter(year_month %in% sampling_dates)

# make a new column of modified dates to join to the other dataset

wq_sampling$original_date <- wq_sampling$year_month
wq_sampling$year_month <- factor(wq_sampling$original_date )

wq_sampling$year_month <- revalue(wq_sampling$year_month, c("2021-05" =  "2021-06", 
                                                          "2022-05" = "2022-06",
                                                          "2023-05" = "2023-07", 
                                                          "2014-06" = "2014-07", 
                                                          "2015-05" = "2015-06"))

unique(wq_sampling$year_month)

wq_sampling_wide <- pivot_wider(wq_sampling, names_from = Parameter, values_from = monthly_mean)

write_csv(wq_sampling_wide,"01_Data/wq_means.csv")


# Combined plots
# to make these plots you need to have the mean data from "02_graphs" loaded

combined_salinty <- ggplot() +
  geom_point(data = all_data_whole_bank_summary %>% filter(category_name == "total_sg"), 
             aes(x = year_month, y = Mean_Cover), 
             size = 1, colour = "red") +
  geom_errorbar(data = all_data_whole_bank_summary %>% filter(category_name == "total_sg"),
                aes(x = year_month, ymin = Lower_CI, ymax = Upper_CI), colour = "red") +
  geom_point(data = water_quality, aes(x = Survey.date, y = Salinity.g.L*2), colour = "grey36") + # Scale Salinity for visualization
  geom_line(data = water_quality, aes(x = Survey.date, y = Salinity.g.L*2), colour = "grey36") +
   theme_bw() +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Category",
       fill = "Category") +
  scale_color_manual(values = speciescols, labels = new_labels) +
  scale_fill_manual(values = speciescols, labels = new_labels) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_vline(xintercept = flood_dates, colour = "dodgerblue", linetype = "dashed") +
  scale_y_continuous(
    name = "Mean Coverage (%)",
    sec.axis = sec_axis(~./2, name = "Salinity (PPT)")
  )

combined_salinty

ggsave(combined_salinty, 
       file = "03_Figures/final_figures/combined_sg_salinity.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)


combined_organic_nitrogen <- ggplot() +
  geom_point(data = all_data_whole_bank_summary %>% filter(category_name == "total_sg"), 
             aes(x = year_month, y = Mean_Cover), 
             size = 1, colour = "red") +
  geom_errorbar(data = all_data_whole_bank_summary %>% filter(category_name == "total_sg"),
                aes(x = year_month, ymin = Lower_CI, ymax = Upper_CI), colour = "red") +
  geom_point(data = water_quality, aes(x = Survey.date, y = Organic.N.mg.L*100), colour = "grey36") + # Scale Salinity for visualization
  geom_line(data = water_quality, aes(x = Survey.date, y = Organic.N.mg.L*100), colour = "grey36") +
  theme_bw() +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Category",
       fill = "Category") +
  scale_color_manual(values = speciescols, labels = new_labels) +
  scale_fill_manual(values = speciescols, labels = new_labels) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_vline(xintercept = flood_dates, colour = "dodgerblue", linetype = "dashed") +
  scale_y_continuous(
    name = "Mean Coverage (%)",
    sec.axis = sec_axis(~./100, name = "Organic Nitrogen (mg/L)"))

combined_organic_nitrogen 


ggsave(combined_organic_nitrogen, 
       file = "03_Figures/final_figures/combined_sg_ON.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)

combined_DO <- ggplot() +
  geom_point(data = all_data_whole_bank_summary %>% filter(category_name == "total_sg"), 
             aes(x = year_month, y = Mean_Cover), 
             size = 1, colour = "red") +
  geom_errorbar(data = all_data_whole_bank_summary %>% filter(category_name == "total_sg"),
                aes(x = year_month, ymin = Lower_CI, ymax = Upper_CI), colour = "red") +
  geom_point(data = water_quality, aes(x = Survey.date, y = DO.saturation), colour = "grey36") + # Scale Salinity for visualization
  geom_line(data = water_quality, aes(x = Survey.date, y = DO.saturation), colour = "grey36") +
  theme_bw() +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Category",
       fill = "Category") +
  scale_color_manual(values = speciescols, labels = new_labels) +
  scale_fill_manual(values = speciescols, labels = new_labels) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_vline(xintercept = flood_dates, colour = "dodgerblue", linetype = "dashed") +
  scale_y_continuous(
    name = "Mean Coverage (%)",
    sec.axis = sec_axis(~., name = "Dissolved Oxygen (% Sat)"))

combined_DO

ggsave(combined_DO, 
       file = "03_Figures/final_figures/combined_sg_DO.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)



combined_temp <- ggplot() +
  geom_point(data = water_quality, aes(x = Survey.date, y = Temp.deg.C*2), colour = "grey36") + # Scale Salinity for visualization
  geom_line(data = water_quality, aes(x = Survey.date, y = Temp.deg.C*2), colour = "grey36") +
  geom_point(data = all_data_whole_bank_summary %>% filter(category_name == "total_sg"), 
             aes(x = year_month, y = Mean_Cover), 
             size = 1, colour = "red") +
  geom_errorbar(data = all_data_whole_bank_summary %>% filter(category_name == "total_sg"),
                aes(x = year_month, ymin = Lower_CI, ymax = Upper_CI), colour = "red") +
  theme_bw() +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Category",
       fill = "Category") +
  scale_color_manual(values = speciescols, labels = new_labels) +
  scale_fill_manual(values = speciescols, labels = new_labels) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_vline(xintercept = flood_dates, colour = "dodgerblue", linetype = "dashed") +
  scale_y_continuous(
    name = "Mean Coverage (%)",
    sec.axis = sec_axis(~./2, name = "Temperature (°C)"))

combined_temp

ggsave(combined_temp, 
       file = "03_Figures/final_figures/combined_sg_temp.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)


combined_turbidity <- ggplot() +
  geom_point(data = water_quality, aes(x = Survey.date, y = Turbidity.NTU*10), colour = "grey36") + # Scale Salinity for visualization
  geom_line(data = water_quality, aes(x = Survey.date, y = Turbidity.NTU*10), colour = "grey36") +
  geom_point(data = all_data_whole_bank_summary %>% filter(category_name == "total_sg"), 
             aes(x = year_month, y = Mean_Cover), 
             size = 1, colour = "red") +
  geom_errorbar(data = all_data_whole_bank_summary %>% filter(category_name == "total_sg"),
                aes(x = year_month, ymin = Lower_CI, ymax = Upper_CI), colour = "red") +
  theme_bw() +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Category",
       fill = "Category") +
  scale_color_manual(values = speciescols, labels = new_labels) +
  scale_fill_manual(values = speciescols, labels = new_labels) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_vline(xintercept = flood_dates, colour = "dodgerblue", linetype = "dashed") +
  scale_y_continuous(
    name = "Mean Coverage (%)",
    sec.axis = sec_axis(~./10, name = "Turbidity (NTU)"))

combined_turbidity

ggsave(combined_turbidity, 
       file = "03_Figures/final_figures/combined_sg_turbidity.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)

