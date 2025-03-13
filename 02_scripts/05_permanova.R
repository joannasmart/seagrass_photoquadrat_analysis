# Load required libraries
library(tidyverse)
library(vegan)
library(reshape2)

# Read the cleaned data
seagrass_data <- read.csv("03_outputs/compiled_cleaned_data_long.csv")

# Reshape the data to create a community matrix
community_matrix <- dcast(seagrass_data, 
                          image_name + year + bank + transect + year_month ~ category_name, 
                          value.var = "value", 
                          fun.aggregate = sum)


# Aggregate data by year, bank, and transect
community_matrix_transect <- community_matrix %>%
  group_by(year, transect, bank) %>%
  dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(c(year, bank, transect), as.factor))  # Convert grouping variables to factors


community_matrix_transect <- community_matrix_transect %>%
  mutate(
    year = as.factor(year),
    bank = as.factor(bank),
    transect = as.factor(transect)
  )

# Remove non-species columns for distance matrix calculation
community_matrix_clean_transect <- community_matrix_transect %>%
  select(-year, -bank, -transect)

# Create a Bray-Curtis distance matrix
dist_matrix1 <- vegdist(community_matrix_clean_transect, method = "bray")


community_matrix_transect$year <- as.factor(community_matrix_transect$year)
community_matrix_transect$bank <- as.factor(community_matrix_transect$bank)
community_matrix_transect$transect <- as.factor(community_matrix_transect$transect)

# Run PERMANOVA 
permanova_result <- adonis2(dist_matrix1 ~ year*bank + bank/transect,  
                                             data = community_matrix_transect, 
                                             permutations = 999)



# View and save the results
print(permanova_result)
results_df <- as.data.frame(permanova_result)
write.csv(results_df, file = "05_results/permanova_results.csv", row.names = TRUE)

# Function for pairwise comparisons
pairwise.adonis2 <- function(resp, fact, p.method = "none", nperm = 999) {
  require(vegan)
  resp <- as.matrix(resp)
  fact <- factor(fact)
  
  fun.p <- function(i, j) {
    fact2 <- droplevels(fact[as.numeric(fact) %in% c(i, j)])
    index <- which(fact %in% levels(fact2))
    resp2 <- as.dist(resp[index, index])
    result <- adonis2(resp2 ~ fact2, permutations = nperm)
    result$`Pr(>F)`[1]
  }
  
  multcomp <- pairwise.table(fun.p, levels(fact), p.adjust.method = p.method)
  return(list(fact = levels(fact), p.value = multcomp, p.adjust.method = p.method))
}

# Pairwise comparisons for bank
bank_pairwise <- pairwise.adonis2(dist_matrix1, community_matrix_transect$bank)
bank_df <- bank_pairwise$p.value
write.csv(bank_df, file = "05_results/pairwise_bank_results.csv", row.names = TRUE)

# Pairwise comparisons for year
year_pairwise <- pairwise.adonis2(dist_matrix1, community_matrix_transect$year)
year_df <- year_pairwise$p.value
write.csv(year_df, file = "05_results/pairwise_year_results.csv", row.names = TRUE)






