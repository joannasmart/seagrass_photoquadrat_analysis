# -------- Graphs -------- #

# load required packages

library(tidyverse)
library(RColorBrewer)
library(ggridges)
library(viridis)

# read in the data and set up graph labels and colours --------------------------------------------------------

long_data <- read.csv("03_outputs/compiled_cleaned_data_long.csv")

long_data <- long_data %>%
  dplyr:: mutate(date = lubridate:: ymd(date)) # make sure the dates are dates

long_data <- long_data %>% filter(bank != "EP")

# Define the flood dates

flood_dates <- as.Date(c("2013-01-01", "2011-01-01", "2015-05-01", "2022-02-01" ))
flood_dates_numeric <- as.numeric(flood_dates)


# Create summary data across the whole of the banks -----------------------


all_data_whole_bank_summary <- long_data %>%
  dplyr:: group_by(year_month, category_name) %>%
  dplyr:: summarise(Mean_Cover = mean(value),
                    SE_Cover = sd(value) / sqrt(n())) %>%
  dplyr:: mutate(Lower_CI = Mean_Cover - qnorm(0.975) * SE_Cover,
                 Upper_CI = Mean_Cover + qnorm(0.975) * SE_Cover)

all_data_whole_bank_summary <- all_data_whole_bank_summary %>%
  dplyr:: mutate(year_month = lubridate:: ym(year_month)) # make sure dates are dates


write_csv(all_data_whole_bank_summary, "03_outputs/supplementary_tables/supplementary_table_1_whole_bank_summary.csv")



indidivudal_bank_summary <- long_data %>%
  dplyr:: group_by(year_month, bank, category_name) %>%
  dplyr:: summarise(Mean_Cover = mean(value),
                    SE_Cover = sd(value) / sqrt(n())) %>%
  dplyr:: mutate(Lower_CI = Mean_Cover - qnorm(0.975) * SE_Cover,
                 Upper_CI = Mean_Cover + qnorm(0.975) * SE_Cover)

indidivudal_bank_summary  <- indidivudal_bank_summary  %>%
  dplyr:: mutate(year_month = lubridate:: ym(year_month)) # make sure dates are dates

write_csv(indidivudal_bank_summary, "03_outputs/supplementary_tables/supplementary_table_4_total_bank_summary.csv")



# Load in the dictionary for graphing later -------------------------------


dictionary <- read_csv("01_data/Dictionary.csv")

# create a list of all the seagrass species

seagrasses <- dictionary %>% filter(category == "seagrass") %>% filter(name != "Cymodocea.rotundata") %>% filter(name != "Seagrass") %>% filter(name != "Dead.seagrass")

# create a list of all the  algae

algae <- dictionary %>% filter(category %in% c("green macroalgae", "macroalgae other", 
                                               "brown macroalgae"))





# Organise the names -------------------------------------------------------------------------


new_labels <- c("Oceana serrulata", "Halodule uninervis", "Halophila ovalis", 
                "Halophila spinsulosa", "Syringodium iseotifolium", "Zostera muelleri")

names(new_labels) <- c("Cymodocea.serrulata", "Halodule.uninervis", "Halophila.ovalis", 
                       "Halophila.spinulosa", "Syringodium", "Zostera.muelleri")


zone.labs <- c("Amity", "Chain", "Maroom", "Moreton", "Wanga-Wallen")
names(zone.labs) <- c("AM", "CH", "MA", "MO", "WA")



# Make plots of trends across the whole bank ------------------------------

# Note to check about the 2011 dates and if the later ones should be removed

# Plot total seagrass cover across all banks
total_seagrass_whole_bank <- ggplot(all_data_whole_bank_summary %>% 
                            filter(category_name == "total_seagrass") %>% 
                            filter(year_month != "2013-01-01"), # remove the feb sampling event as only one transect was surveyed
                          aes(x = year_month, y = Mean_Cover, group = category_name)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  geom_smooth(method = "lm") +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Bank",
       fill = "Bank", 
       title = "Mean total seagrass cover") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") #+
 # geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed") 

total_seagrass_whole_bank

ggsave(total_seagrass_whole_bank, 
       file = "04_Figures/whole_bank/total_seagrass_whole_bank.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)



species_whole_bank <- ggplot(all_data_whole_bank_summary %>% filter(category_name %in% seagrasses$name) %>% filter(year_month != "2013-01-01"), 
                          aes(x = year_month, y = Mean_Cover, group = category_name, colour = category_name)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  geom_smooth(aes(group = category_name), method = "lm") +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Bank",
       fill = "Bank") +
  facet_wrap(~ category_name, labeller = labeller(category_name = new_labels)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") #+
  #geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed") 

species_whole_bank


ggsave(species_whole_bank, 
       file = "04_Figures/whole_bank/species_whole_bank.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)



# Lyngba

lyngba_whole_bank <- ggplot(all_data_whole_bank_summary %>% filter(category_name == "Lyngbya.majuscula") %>% filter(year_month != "2013-01-01"), 
                              aes(x = year_month, y = Mean_Cover, group = category_name, colour = category_name)) +
  geom_point(size = 1) +
  scale_color_brewer(palette = "Dark2") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species") +
  geom_smooth(method=lm, formula = y ~ x) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #+
#geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed") 

lyngba_whole_bank 


ggsave(lyngba_whole_bank , 
       file = "04_Figures/whole_bank/lyngba_whole_bank.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)




dominant_whole_bank <- ggplot(all_data_whole_bank_summary %>% filter(category_name %in% c("Zostera.muelleri", "Cymodocea.serrulata", "Halodule.uninervis")) %>% filter(year_month != "2013-01-01"), 
                             aes(x = year_month, y = Mean_Cover, group = category_name, colour = category_name)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  geom_smooth(method = "lm", aes(group = category_name)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Bank",
       fill = "Bank") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #+
#geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed") 

dominant_whole_bank


ggsave(dominant_whole_bank, 
       file = "04_Figures/whole_bank/dominant_species_whole_bank_with_hu.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)




# create summary data for each zone and save ----------------------------------------------------------


bank_summary <- long_data %>%
  dplyr:: group_by(bank, year_month, category_name) %>%
  dplyr::summarise(Mean_Cover = mean(value),
                   SE_Cover = sd(value) / sqrt(n())) %>%
  dplyr:: mutate(Lower_CI = Mean_Cover - qnorm(0.975) * SE_Cover,
                 Upper_CI = Mean_Cover + qnorm(0.975) * SE_Cover)

bank_summary <- bank_summary %>%
  dplyr:: mutate(year_month = lubridate:: ym(year_month))


write_csv(bank_summary, "03_outputs/supplementary_tables/supplementary_table_2_individual_bank_summary.csv")





# Create plots of each species across each zone ---------------------------

seagrass_bank_plot_combined <- ggplot(bank_summary %>% 
                               filter(category_name %in% seagrasses$name) %>%
                                 filter(bank != "CH"),
                             aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 2) +
  geom_path(aes(colour = category_name), linetype = "dashed") +
  #geom_smooth(aes(colour = category_name), se = F) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ bank, labeller = labeller(bank = zone.labs)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species") +
  scale_colour_brewer(palette = "Dark2") +
 # scale_color_manual(values = speciescols, labels = new_labels) +
 # scale_fill_manual(values = speciescols, labels = new_labels) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #+
  #geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed")

seagrass_bank_plot_combined 


ggsave(seagrass_bank_plot_combined, 
       file = "04_Figures/individual_bank/seagrass_bank_plot_combined.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)




seagrass_bank_totals <- ggplot(bank_summary %>% 
                                        filter(category_name == "total_seagrass") %>%
                                        filter(bank != "CH"),
                                      aes(x = year_month, y = Mean_Cover)) +
  geom_point( size = 2) +
  geom_smooth(method = "lm") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ bank, labeller = labeller(bank = zone.labs), scales = "free_y") +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species") +
  scale_colour_brewer(palette = "Dark2") +
  # scale_color_manual(values = speciescols, labels = new_labels) +
  # scale_fill_manual(values = speciescols, labels = new_labels) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #+
#geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed")

seagrass_bank_totals

ggsave(seagrass_bank_totals, 
       file = "04_Figures/individual_bank/seagrass_bank_totals.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)


# plot of cymodocea

cymodocea_plot <- ggplot(bank_summary %>% 
                        filter(category_name == "Cymodocea.serrulata") %>%
                        filter(bank != "CH"),
                      aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 2) +
  #geom_smooth(aes(colour = category_name), se = F) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ bank, labeller = labeller(bank = zone.labs)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species", 
       title = "Cymodocea serrulata") +
  geom_smooth(aes(group = bank), method = "lm") +
  scale_colour_brewer(palette = "Dark2") +
  # scale_color_manual(values = speciescols, labels = new_labels) +
  # scale_fill_manual(values = speciescols, labels = new_labels) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") #+
#geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed")

cymodocea_plot


ggsave(cymodocea_plot, 
       file = "04_Figures/individual_bank/cymodocea_serrulata.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)


# Zostera muelleri plot

zostera_plot <- ggplot(bank_summary %>% 
                           filter(category_name == "Zostera.muelleri") %>%
                           filter(bank != "CH"),
                         aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 2) +
  #geom_smooth(aes(colour = category_name), se = F) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ bank, labeller = labeller(bank = zone.labs)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species", 
       title = "Zostera muelleri") +
  geom_smooth(aes(group = bank), method = "lm") +
  scale_colour_brewer(palette = "Dark2") +
  # scale_color_manual(values = speciescols, labels = new_labels) +
  # scale_fill_manual(values = speciescols, labels = new_labels) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") #+
#geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed")

zostera_plot

ggsave(zostera_plot, 
       file = "04_Figures/individual_bank/zostera_muelleri.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)

# Halodule uninervis

hu_plot <- ggplot(bank_summary %>% 
                         filter(category_name == "Halodule.uninervis") %>%
                         filter(bank != "CH"),
                       aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 2) +
  #geom_smooth(aes(colour = category_name), se = F) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ bank, labeller = labeller(bank = zone.labs)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species", 
       title = "Halodule uninvervis") +
  geom_smooth(aes(group = bank), method = "lm") +
  scale_colour_brewer(palette = "Dark2") +
  # scale_color_manual(values = speciescols, labels = new_labels) +
  # scale_fill_manual(values = speciescols, labels = new_labels) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") #+
#geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed")

hu_plot

ggsave(hu_plot, 
       file = "04_Figures/individual_bank/halodule_uninervis.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)


# Halophila spinulosa

hs_plot <- ggplot(bank_summary %>% 
                    filter(category_name == "Halophila.spinulosa") %>%
                    filter(bank != "CH"),
                  aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 2) +
  #geom_smooth(aes(colour = category_name), se = F) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ bank, labeller = labeller(bank = zone.labs)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species", 
       title = "Halodule spinulosa") +
  geom_smooth(aes(group = bank), method = "lm") +
  scale_colour_brewer(palette = "Dark2") +
  # scale_color_manual(values = speciescols, labels = new_labels) +
  # scale_fill_manual(values = speciescols, labels = new_labels) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") #+
#geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed")

hs_plot

ggsave(hs_plot, 
       file = "04_Figures/individual_bank/halophila_spinulosa.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)



# Halophila ovalis

ho_plot <- ggplot(bank_summary %>% 
                    filter(category_name == "Halophila.ovalis") %>%
                    filter(bank != "CH"),
                  aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 2) +
  #geom_smooth(aes(colour = category_name), se = F) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ bank, labeller = labeller(bank = zone.labs)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species", 
       title = "Halophila ovalis") +
  geom_smooth(aes(group = bank), method = "lm") +
  scale_colour_brewer(palette = "Dark2") +
  # scale_color_manual(values = speciescols, labels = new_labels) +
  # scale_fill_manual(values = speciescols, labels = new_labels) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") #+
#geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed")

ho_plot

ggsave(ho_plot, 
       file = "04_Figures/individual_bank/halophila_ovalis.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)



# Syringodium

syringodium_plot <- ggplot(bank_summary %>% 
                    filter(category_name == "Syringodium") %>%
                    filter(bank != "CH"),
                  aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 2) +
  #geom_smooth(aes(colour = category_name), se = F) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ bank, labeller = labeller(bank = zone.labs)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species", 
       title = "Syringodium isoetifolium") +
  geom_smooth(aes(group = bank), method = "lm") +
  scale_colour_brewer(palette = "Dark2") +
  # scale_color_manual(values = speciescols, labels = new_labels) +
  # scale_fill_manual(values = speciescols, labels = new_labels) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") #+
#geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed")

syringodium_plot

ggsave(syringodium_plot, 
       file = "04_Figures/individual_bank/syrigodium isoetifolium.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)



# plot of lyngba

lyngba_plot <- ggplot(bank_summary %>% 
                           filter(category_name == "Lyngbya.majuscula") %>%
                           filter(bank != "CH"),
                         aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 2) +
  #geom_smooth(aes(colour = category_name), se = F) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ bank, labeller = labeller(bank = zone.labs)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species", 
       title = "Lyngbya majuscula") +
  geom_smooth(aes(group = bank), method = "lm") +
  scale_colour_brewer(palette = "Dark2") +
  # scale_color_manual(values = speciescols, labels = new_labels) +
  # scale_fill_manual(values = speciescols, labels = new_labels) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") #+
#geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed")

lyngba_plot


ggsave(lyngba_plot, 
       file = "04_Figures/individual_bank/lyngbya_majuscula.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)




# Combined dominant species

comb_plot <- ggplot(bank_summary %>% 
                        filter(category_name %in% c("Cymodocea.serrulata", "Zostera.muelleri")) %>%
                      filter(bank != "CH"),
                      aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 2) +
  geom_smooth(aes(colour = category_name), method = "lm") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ bank, labeller = labeller(bank = zone.labs)) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species") +
  scale_colour_brewer(palette = "Dark2") +
  # scale_color_manual(values = speciescols, labels = new_labels) +
  # scale_fill_manual(values = speciescols, labels = new_labels) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #+
#geom_vline(xintercept = flood_dates, colour = "red", linetype = "dashed")

comb_plot


ggsave(comb_plot, 
       file = "04_Figures/individual_bank/dominant_species.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)













# Histograms


long_data$bank <- as.factor(long_data$bank)

total_hist <- ggplot(long_data %>% filter(category_name == "total_seagrass") %>% filter(bank != "CH"), aes(x = value, y = as.factor(year))) +
  geom_density_ridges(alpha = 0.8) +
  facet_wrap(~ bank,labeller = labeller(bank = zone.labs)) +
  labs(x = "Percentage Cover", y = "Year", title = "Total Seagrass") +
  #scale_fill_brewer(palette = 4) +
  theme_bw() +
  theme(legend.position = "none") 

ggsave(total_hist, 
       file = "04_Figures/histograms/total_seagrass.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)




dominant_species_histogram <- ggplot(long_data %>% filter(category_name %in% c("Cymodocea.serrulata", 
                                                 "Zostera.muelleri")) %>%
         filter(bank != "CH"), aes (x = value, y = as.factor(year), fill = bank)) +
  geom_density_ridges() +
  facet_grid(bank ~ category_name, labeller = labeller(bank = zone.labs, category_name = new_labels)) +
  labs(x = "Percentage Cover", y = "Year") +
  scale_fill_brewer(palette = 4) +
  theme(legend.position = "none") +
  theme_bw() 

ggsave(dominant_species_histogram, 
       file = "04_Figures/histograms/dominant_species_histogram.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)





# Graphs by individual transect ------------------------------------------------------

# create a summary for each transect

data_by_site <- long_data %>%
  dplyr:: group_by(bank, transect, year_month, category_name) %>%
  dplyr:: summarise(Mean_Cover = mean(value),
                    SE_Cover = sd(value) / sqrt(n())) %>%
  dplyr:: mutate(Lower_CI = Mean_Cover - qnorm(0.975) * SE_Cover,
                 Upper_CI = Mean_Cover + qnorm(0.975) * SE_Cover)

data_by_site <- data_by_site %>%
  dplyr:: mutate(year_month = lubridate:: ym(year_month))


write_csv(data_by_site, "03_outputs//supplementary_tables/supplementary_table_3_transect_scale_summary.csv")

# create a plot of coverage for each individual transect and save

# For amity banks

amity_transect_plots <- ggplot(data_by_site %>% filter(category_name %in% seagrasses$name) %>% 
                                 filter(bank == "AM"), 
                               aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ transect) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

amity_transect_plots

ggsave(amity_transect_plots, 
       file = "04_Figures/all_transects/amity_transect_plots.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)


# For wanga-wallen banks

wa_transect_plots <- ggplot(data_by_site %>% filter(category_name %in% seagrasses$name) %>% 
                              filter(bank == "WA"), 
                               aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ transect) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

wa_transect_plots

ggsave(wa_transect_plots, 
       file = "04_Figures/all_transects/wangawallen_transect_plots.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)

# For maroom banks

maroom_transect_plots <- ggplot(data_by_site %>% filter(category_name %in% seagrasses$name) %>% 
                                  filter(bank == "MA"), 
                                aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ transect) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

maroom_transect_plots

ggsave(wa_transect_plots, 
       file = "04_Figures/all_transects/maroom_transect_plots.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)


# For Moreton Banks

moreton_transect_plots <- ggplot(data_by_site %>% filter(category_name %in% seagrasses$name) %>% 
                                  filter(bank == "MO"), 
                                aes(x = year_month, y = Mean_Cover, group = category_name, color = category_name)) +
  geom_point(aes(colour = category_name), size = 1) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI)) +
  facet_wrap(~ transect) +
  labs(x = "Year",
       y = "Mean Coverage (%)",
       color = "Species",
       fill = "Species") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

moreton_transect_plots

ggsave(wa_transect_plots, 
       file = "04_Figures/all_transects/maroom_transect_plots.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)


