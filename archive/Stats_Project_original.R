# PCA Analysis of World Development Indicators

# Load essential libraries
library(tidyverse) 
library(factoextra)
library(mice)    
library(knitr)
library(ggrepel)

set.seed(123)

# Data Acquisition and Preparation

wdi_data = fcb74d33_6feb_47e2_8f94_37bd27355a72_Data

# Clean column names for easier handling
names(wdi_data) = gsub(" ", "_", names(wdi_data))
names(wdi_data) = gsub("\\[|\\]", "", names(wdi_data))
names(wdi_data) = gsub(",", "", names(wdi_data))
names(wdi_data) = gsub("\\$", "", names(wdi_data))

# Rename key columns and select relevant data
wdi_analysis = wdi_data %>%
  rename(
    country = Country_Name,
    iso2c = Country_Code
  ) %>%
  # Remove unnecessary columns
  select(-Time, -Time_Code)

# Get list of indicators
indicators = names(wdi_analysis)[!names(wdi_analysis) %in% c("country", "iso2c")]

# Convert all indicator columns to numeric
wdi_analysis = wdi_analysis %>%
  mutate(across(all_of(indicators), ~as.numeric(as.character(ifelse(. == "..", NA, .)))))

# Only keep countries with at least 70% of indicators available
wdi_filtered = wdi_analysis %>%
  mutate(missing_count = rowSums(is.na(select(., all_of(indicators))))) %>%
  filter(missing_count <= length(indicators) * 0.3) %>%
  select(-missing_count)

# Impute remaining missing values using MICE
if(anyNA(wdi_filtered[, indicators])) {
  imputed_data = mice(wdi_filtered[, indicators], m = 5, method = "pmm", seed = 123, printFlag = FALSE)
  wdi_complete = wdi_filtered
  wdi_complete[, indicators] = complete(imputed_data)
} else {
  wdi_complete = wdi_filtered
}

# Principal Component Analysis

wdi_scaled = scale(wdi_complete[, indicators])
rownames(wdi_scaled) = wdi_complete$country

# PCA
pca_result = prcomp(wdi_scaled, scale. = FALSE) # Already scaled

# Extract variance information
variance_explained = summary(pca_result)$importance[2, ] * 100
cumulative_variance = summary(pca_result)$importance[3, ] * 100

# Visualizations

# Scree Plot
scree_plot = fviz_eig(pca_result, 
                      addlabels = TRUE, 
                      barfill = "#4682B4", 
                      barcolor = "#4682B4",
                      linecolor = "#363636",
                      ggtheme = theme_minimal()) +
  labs(title = "Scree Plot: Explained Variance by Principal Components",
       subtitle = paste0("First two components explain ", 
                         round(cumulative_variance[2], 1), "% of total variance")) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

# Variables Factor Map

var_names = names(wdi_complete[, indicators])
short_names = c(
  "GDP per capita", "Life expectancy", "Tertiary education", 
  "Measles immunization", "Urban population", "CO2 emissions",
  "Electricity access", "Internet usage", "Unemployment",
  "Agriculture % GDP", "Industry % GDP", "Inflation"
)
var_mapping = setNames(short_names, var_names)

# Loadings and coordinates for variables
loadings_df <- as.data.frame(pca_result$rotation)
loadings_df$variables <- rownames(loadings_df)
loadings_df$short_names <- var_mapping[loadings_df$variables]

# Calculate contribution (quality of representation)
var_cos2 <- get_pca_var(pca_result)$cos2
loadings_df$contrib <- rowSums(var_cos2[, 1:2]) * 100 / sum(var_cos2[, 1:2])

# Variables plot
var_plot <- ggplot(loadings_df, aes(x = PC1, y = PC2)) +
  # Unit circle
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = "gray65", fill = NA) +
  # Arrows
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2, color = contrib),
               arrow = arrow(length = unit(0.2, "cm")), size = 0.8) +
  # Variable labels
  geom_text_repel(aes(label = short_names, color = contrib),
                  size = 3.5,
                  segment.alpha = 0.8,
                  max.overlaps = 12,
                  box.padding = 0.5,
                  min.segment.length = 0.1) +
  # Colors for contribution
  scale_color_gradient2(low = "#00AFBB", mid = "#E7B800", high = "#FC4E07", 
                        midpoint = median(loadings_df$contrib)) +
  # Axis lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Variables Factor Map",
       subtitle = "Contribution of variables to principal components",
       x = paste0("Dim1 (", round(variance_explained[1], 1), "%)"),
       y = paste0("Dim2 (", round(variance_explained[2], 1), "%)"),
       color = "contrib") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )

# Countries Plot with Development Categories
# Countries by quartiles of PC1 
country_pca = as.data.frame(pca_result$x)
country_pca$country = rownames(country_pca)

# Development category based on PC1 quartiles
country_pca = country_pca %>%
  mutate(dev_category = case_when(
    PC1 >= quantile(PC1, 0.75) ~ "High development",
    PC1 >= quantile(PC1, 0.5) ~ "Upper-middle development",
    PC1 >= quantile(PC1, 0.25) ~ "Lower-middle development",
    TRUE ~ "Low development"
  )) %>%
  mutate(dev_category = factor(dev_category, 
                               levels = c("High development", "Upper-middle development", 
                                          "Lower-middle development", "Low development")))

# Select representative countries to label
# Top 5 and bottom 5 on PC1, plus 5 extremes on PC2
countries_to_label = c(
  country_pca %>% arrange(desc(PC1)) %>% head(5) %>% pull(country),
  country_pca %>% arrange(PC1) %>% head(5) %>% pull(country),
  country_pca %>% arrange(desc(PC2)) %>% head(3) %>% pull(country),
  country_pca %>% arrange(PC2) %>% head(3) %>% pull(country)
)

# Country plot
country_plot = ggplot(country_pca, aes(x = PC1, y = PC2, color = dev_category)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text_repel(
    data = country_pca %>% filter(country %in% countries_to_label),
    aes(label = country),
    size = 3,
    box.padding = 0.35,
    point.padding = 0.5,
    segment.color = "grey50",
    max.overlaps = 40
  ) +
  scale_color_manual(values = c("#4DAF4A", "#377EB8", "#FF7F00", "#E41A1C")) +
  theme_minimal() +
  labs(
    title = "Countries Projected onto Principal Components",
    subtitle = paste0("PC1 (", round(variance_explained[1], 1), 
                      "% of variance) represents overall development"),
    x = paste0("PC1 (", round(variance_explained[1], 1), "% of variance)"),
    y = paste0("PC2 (", round(variance_explained[2], 1), "% of variance)"),
    color = "Development Category"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# Biplot
simplified_biplot <- ggplot() +
  # Points for selected countries only
  geom_point(data = country_pca %>% filter(country %in% countries_to_label), 
             aes(x = PC1, y = PC2),
             size = 2, alpha = 0.7, color = "blue") +
  # Arrows for variables 
  geom_segment(data = loadings_df,
               aes(x = 0, y = 0, xend = PC1*5, yend = PC2*5),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "red", alpha = 0.8) +
  # Labels for all variables
  geom_text_repel(data = loadings_df,
                  aes(x = PC1*5.2, y = PC2*5.2, label = short_names),
                  color = "red",
                  size = 3.5,
                  max.overlaps = 15,
                  box.padding = 0.4) +
  # Add labels for selected countries
  geom_text_repel(data = country_pca %>% filter(country %in% countries_to_label),
                  aes(x = PC1, y = PC2, label = country),
                  color = "blue",
                  size = 3.5,
                  max.overlaps = 15,
                  box.padding = 0.5,
                  segment.alpha = 0.8) +
  # Add axis lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70", size = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70", size = 0.3) +
  # Set theme and labels
  theme_minimal() +
  labs(title = "PCA Biplot: Selected Countries and Variables",
       subtitle = "Relationship between development indicators and country positions",
       x = paste0("PC1 (", round(variance_explained[1], 1), "% of variance)"),
       y = paste0("PC2 (", round(variance_explained[2], 1), "% of variance)")) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# Top and bottom countries analysis
# Top and bottom countries by PC1
top_bottom_countries = bind_rows(
  country_pca %>% 
    arrange(desc(PC1)) %>% 
    head(10) %>%
    mutate(rank_group = "Top 10 (high development)"),
  country_pca %>% 
    arrange(PC1) %>% 
    head(10) %>%
    mutate(rank_group = "Bottom 10 (low development)")
)

# Display plots
print(scree_plot)
print(var_plot)
print(country_plot)
print(simplified_biplot)

# Save results
save(pca_result, country_pca, wdi_complete, top_bottom_countries, 
     file = "wdi_pca_results.RData", compress = "xz")

