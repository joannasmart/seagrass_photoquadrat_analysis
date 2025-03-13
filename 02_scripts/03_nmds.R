# -------------------------------- nMDS Plots ------------------------------------ #

# load required packages

library(tidyverse)
library(vegan)
library(lattice)
library(permute)
library(viridis)
library(ggtext)

# read in the data

data_wide <- read.csv("03_outputs/compiled_cleaned_data.csv")

# select the data for the NMDS plot (only percentage columns)

data_nmds <- data_wide %>%
  dplyr:: group_by(year, bank) %>%
  dplyr:: select(12:61) %>%
  dplyr:: summarise_if(is.numeric, mean, na.rm = TRUE) # calculate the mean for each site

data_nmds <- data_nmds %>%
  dplyr::select(c("year", "bank", "Cymodocea.serrulata", "Halodule.uninervis", "Halophila.ovalis", "Halophila.spinulosa", 
           "Lyngbya.majuscula", "Syringodium", "Zostera.muelleri", "total_sessile.invertebrate", 
           "total_microalgae", "total_green.macroalgae", "total_seagrass", "total_mobile.invertebrate", 
           "total_brown.macroalgae", "total_macroalgae.other")) %>%
  filter(bank != "CH") %>%
  filter(bank != "EP")

# add the labels


new_labels <- c("*Oceana serrulata*", "*Halodule uninervis*", "*Halophila ovalis*", "*Halophila spinulosa*", 
                "*Lyngbya majuscula*", "*Syringodium isoetifolium*", "*Zostera muelleri*", "Sessile invertebrates", 
                "Microalgae", "Green macroalgae", "Total seagrass", "Mobile invertebrates", "Brown macroalgae", 
                "Other macroalgae")

names(new_labels) <- c("Cymodocea.serrulata", "Halodule.uninervis", "Halophila.ovalis", "Halophila.spinulosa", 
                       "Lyngbya.majuscula", "Syringodium", "Zostera.muelleri", "total_sessile.invertebrate", 
                       "total_microalgae", "total_green.macroalgae", "total_seagrass", "total_mobile.invertebrate", 
                       "total_brown.macroalgae", "total_macroalgae.other")

# calculate a bray-curtis dissimilarity matrix

bray_dist <- vegan:: vegdist(as.matrix(sqrt(data_nmds[,3:16])), method = "bray") 

# run the NMDS

nMDS_data <- vegan:: metaMDS(bray_dist, k = 3)

# Extract the species data from the 'data_nmds' dataframe

species <- data_nmds[,3:16]

# Combine the data with the NMDS results (MDS1 and MDS2)

nmds <- cbind.data.frame(data_nmds[, 1:2], nMDS_data$points)

# Calculate the stress value from the NMDS analysis

stress <- nMDS_data$stress

# Calculate convex hulls for the NMDS data points, grouped by 'year'

nmds_hulls <- nmds %>%
  dplyr:: group_by(year) %>%
  dplyr:: slice(grDevices:: chull(MDS1, MDS2))

# Calculate convex hulls for the NMDS data points, grouped by 'bank'

nmds_hulls_bank <- nmds %>%
  dplyr:: group_by(bank) %>%
  dplyr:: slice(grDevices:: chull(MDS1, MDS2))

# Determine env fit

ef <- envfit(nMDS_data, data_nmds)
ef_scrs <- as.data.frame(scores(ef, display = "vectors"))
ef_scrs <- cbind(ef_scrs, species = rownames(ef_scrs))

#ef_scrs$full_name <- c("Year", "Not Seagrass", "Cymodocea sp.", "Zostera muelleri/Halophila uninervis","Halophila ovalis", "Syringodium", "Halophila spinulosa", "Unknown Seagrass")
ef_scrs <- ef_scrs %>% filter(species != "year")

# Create a mapping of bank labels

zone.labs <- c("Amity", "Maroom", "Moreton", "Wanga-Wallen")
names(zone.labs) <- c("AM", "MA", "MO", "WA")



# Create a function to adjust label positions
spread_labels <- function(points, min_dist = 0.1) {
  library(MASS)
  
  # Initialize the adjusted positions
  adj_points <- as.data.frame(points)
  n <- nrow(adj_points)
  
  # Iterate multiple times to gradually adjust positions
  for(iter in 1:50) {
    for(i in 1:n) {
      for(j in 1:n) {
        if(i != j) {
          # Calculate distance between points
          dist <- sqrt((adj_points$NMDS1[i] - adj_points$NMDS1[j])^2 + 
                         (adj_points$NMDS2[i] - adj_points$NMDS2[j])^2)
          
          if(dist < min_dist) {
            # Calculate repulsion vector
            angle <- atan2(adj_points$NMDS2[j] - adj_points$NMDS2[i],
                           adj_points$NMDS1[j] - adj_points$NMDS1[i])
            
            # Adjust positions to maintain relative positions while increasing separation
            adj_points$NMDS1[i] <- adj_points$NMDS1[i] - cos(angle) * (min_dist - dist) / 4
            adj_points$NMDS2[i] <- adj_points$NMDS2[i] - sin(angle) * (min_dist - dist) / 4
            adj_points$NMDS1[j] <- adj_points$NMDS1[j] + cos(angle) * (min_dist - dist) / 4
            adj_points$NMDS2[j] <- adj_points$NMDS2[j] + sin(angle) * (min_dist - dist) / 4
          }
        }
      }
    }
  }
  return(adj_points)
}

# Adjust the label positions
ef_scrs_adjusted <- spread_labels(ef_scrs[, c("NMDS1", "NMDS2")], min_dist = 0.15)
ef_scrs$NMDS1_adj <- ef_scrs_adjusted$NMDS1
ef_scrs$NMDS2_adj <- ef_scrs_adjusted$NMDS2



# Create the NMDS plot with year groupings and save 
nmds_plot <- ggplot() +
  geom_point(data = nmds, aes(x = MDS1, y = MDS2, col = as.factor(year)), size =  4) +
  geom_polygon(data = nmds_hulls, aes(x = MDS1, y = MDS2, group = year, fill = as.factor(year)), alpha = 0.1) +
  annotate(geom = "text", label = paste("Stress =", round(stress, 2)), x = Inf, y = -Inf, hjust = 1.2, vjust = -1) +
  theme_bw(base_size = 12) +
  geom_segment(data = ef_scrs, 
               aes(x = 0, xend = NMDS1/2, y = 0, yend = NMDS2/2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
  geom_richtext(data = ef_scrs, 
                aes(x = NMDS1_adj/2, y = NMDS2_adj/2, label = new_labels),
                size = 3,
                fill = NA, label.color = NA, # remove background and border
                label.padding = unit(0.1, "lines")) +
  stat_ellipse() +
  scale_fill_brewer(palette = "Paired" ) +
  scale_colour_brewer(palette = "Paired") +
  scale_shape(labels = zone.labs) +
  labs(colour = "Year", fill = "Year") +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

nmds_plot

ggsave(nmds_plot, 
       file = "04_Figures/nmds/nmds.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)

ggsave(nmds_plot, 
       file = "04_Figures/nmds/nmds.pdf", 
       dpi = 300, 
       width = 12, 
       height = 8)

# Create a NMDS plot with bank grouping and save

nmds_plot_zone <- ggplot() +
  geom_point(data = nmds, aes(x = MDS1, y = MDS2, col = bank), size = 4) +
  geom_polygon(data = nmds_hulls_bank, aes(x = MDS1, y = MDS2, group = as.factor(bank), fill = as.factor(bank)), alpha = 0.1) +
  annotate(geom = "text", label = paste("Stress =", round(stress, 2)), x = Inf, y = -Inf, hjust = 1.2, vjust = -1) +
  theme_bw(base_size = 12) +
  stat_ellipse() +
  scale_fill_brewer(palette = "Dark2", labels = zone.labs) +
  scale_color_brewer(palette = "Dark2", labels = zone.labs) +
  geom_segment(data = ef_scrs, 
               aes(x = 0, xend = NMDS1/2, y = 0, yend = NMDS2/2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
  geom_richtext(data = ef_scrs, 
                aes(x = NMDS1_adj/2, y = NMDS2_adj/2, label = new_labels),
                size = 3,
                fill = NA, label.color = NA, # remove background and border
                label.padding = unit(0.1, "lines")) +
  scale_shape(labels = zone.labs) +
  labs( colour = "Bank", fill = "Bank") +
  theme(legend.title = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

nmds_plot_zone

ggsave(nmds_plot_zone, 
       file = "04_Figures/nmds/nmds_zone.tiff", 
       dpi = 300, 
       width = 12, 
       height = 8)