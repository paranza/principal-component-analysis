# PCA Analysis of World Development Indicators

# Load essential libraries
library(tidyverse) 
library(factoextra)
library(mice)    
library(knitr)

set.seed(123)

# Data Acquisition and Preparation

wdi_data_long = Data

# Check the structure of the data
str(wdi_data_long)
head(wdi_data_long)

# Long to Wide

# Clean the column names properly
names(wdi_data_long) = gsub(" ", "_", names(wdi_data_long))
names(wdi_data_long) = gsub("\\[|\\]", "", names(wdi_data_long))

# Find and rename the value column correctly
value_col = names(wdi_data_long)[grep("2017", names(wdi_data_long))]

# Rename the identified column to "value"
names(wdi_data_long)[names(wdi_data_long) == value_col] = "value"

# Convert to wide format
wdi_data = wdi_data_long %>%
  # Remove rows with missing values
  filter(!is.na(value) & value != "..") %>%
  # Convert value column to numeric
  mutate(value = as.numeric(value)) %>%
  # Reshape to wide format
  pivot_wider(
    id_cols = c(Country_Name, Country_Code),
    names_from = Series_Code,
    values_from = value
  )

# Check the structure of the reshaped data
str(wdi_data)
head(wdi_data)

# Get the list of indicators present (all columns except Country_Name and Country_Code)
indicators = names(wdi_data)[!names(wdi_data) %in% c("Country_Name", "Country_Code")]

# Create a mapping for indicator labels 
indicator_labels = wdi_data_long %>%
  select(Series_Name, Series_Code) %>%
  distinct() %>%
  deframe()

# Data Cleaning

# Select only needed columns: country identifiers and indicators
wdi_analysis = wdi_data %>%
  # Rename columns for consistency with original script
  rename(country = Country_Name, iso2c = Country_Code)

# Check missing data
missing_pattern = md.pattern(wdi_analysis[, indicators], plot = FALSE)
print(missing_pattern)
missing_summary = colSums(is.na(wdi_analysis[, indicators])) / nrow(wdi_analysis) * 100
print(missing_summary)

# Only keep countries with at least 70% of indicators available
wdi_filtered = wdi_analysis %>%
  mutate(missing_count = rowSums(is.na(select(., all_of(indicators))))) %>%
  filter(missing_count <= length(indicators) * 0.3) %>%
  select(-missing_count)

# Impute remaining missing values using MICE
if(anyNA(wdi_filtered[, indicators])) {
  imputed_data = mice(wdi_filtered[, indicators], m = 5, method = "pmm", seed = 123)
  wdi_complete = wdi_filtered
  wdi_complete[, indicators] = complete(imputed_data)
} else {
  wdi_complete = wdi_filtered
}

# Principal Component Analysis

# Scale the data
wdi_scaled = scale(wdi_complete[, indicators])
rownames(wdi_scaled) = wdi_complete$country

# Perform PCA
pca_result = prcomp(wdi_scaled, scale. = FALSE) # Already scaled

# Summary of PCA results
pca_summary = summary(pca_result)
print(pca_summary)

# Variance explained by each component
variance_explained = pca_summary$importance[2, ] * 100
cumulative_variance = pca_summary$importance[3, ] * 100

# Determine optimal number of components

# Scree plot
scree_plot = fviz_eig(pca_result, addlabels = TRUE)
print(scree_plot)

# Kaiser criterion: eigenvalues > 1
kaiser_components = sum(pca_result$sdev^2 > 1)
cat("Number of components according to Kaiser criterion (eigenvalues > 1):", kaiser_components, "\n")

# Analyze PCA Results

# Examine variable loadings for first three components
loadings = pca_result$rotation[, 1:min(3, ncol(pca_result$rotation))]
loadings_table = round(loadings, 3)
print(kable(loadings_table))

# Create a loading plot for variables
loading_plot = fviz_pca_var(pca_result, 
                             col.var = "contrib",
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                             repel = TRUE)
print(loading_plot)

# Visualize countries in PC space

# Prepare data for plotting
pca_scores = as.data.frame(pca_result$x)
pca_scores$country = rownames(pca_scores)

# Join with original data to get country metadata
country_pca = left_join(pca_scores, wdi_complete, by = "country")

# Create simple scatterplot of countries in PC space
pc_plot = ggplot(country_pca, aes(x = PC1, y = PC2)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(aes(label = country), 
            size = 3, 
            vjust = 1.5, 
            hjust = 0.5,
            check_overlap = TRUE) +  # Avoid text overlap
  theme_minimal() +
  labs(title = "Countries projected onto PC1 and PC2",
       x = paste0("PC1 (", round(variance_explained[1], 1), "% of variance)"),
       y = paste0("PC2 (", round(variance_explained[2], 1), "% of variance)"))

print(pc_plot)

# Variables factor map
var_plot = fviz_pca_var(pca_result,
                         col.var = "black",
                         repel = TRUE)
print(var_plot)

# Biplot
biplot = fviz_pca_biplot(pca_result,
                          repel = TRUE,
                          col.var = "red",
                          col.ind = "blue")
print(biplot)

# Analyze top and bottom countries by PC1

# Create a data frame with countries and their PC scores
development_indicators = indicators[1:min(3, length(indicators))]

dev_pca_comparison = country_pca %>%
  select(country, PC1, PC2, all_of(development_indicators)) %>%
  arrange(desc(PC1))

# Show top and bottom 10 countries by PC1
top_countries = head(dev_pca_comparison, 10)
bottom_countries = tail(dev_pca_comparison, 10)

print("Top 10 countries by PC1 (development index):")
print(kable(top_countries))

print("Bottom 10 countries by PC1 (development index):")
print(kable(bottom_countries))

# Save PCA results for further analysis
save(pca_result, country_pca, wdi_complete, file = "wdi_pca_results.RData")

