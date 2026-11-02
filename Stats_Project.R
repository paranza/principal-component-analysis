# PCA of World Development Indicators, 2017.
# Run from the repository root: Rscript --vanilla Stats_Project.R
# Original code: archive/Stats_Project_original.R. Revision notes: docs/methodology.md.
if (!file.exists("Data.csv")) stop("Run from the repository root, where Data.csv is located.")
if (dir.exists(".R-library")) .libPaths(c(normalizePath(".R-library"), .libPaths()))
packages <- c("mice", "ggplot2", "ggrepel")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "),
                          ". First run: Rscript --vanilla setup.R")
library(ggplot2)
set.seed(123)
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

# Select by WDI code, so labels remain correct if CSV columns are reordered.
indicator_codes <- c(
  "NY.GDP.PCAP.PP.KD", "SP.DYN.LE00.IN", "SE.TER.ENRR", "SH.IMM.MEAS",
  "SP.URB.TOTL.IN.ZS", "EN.GHG.CO2.PC.CE.AR5", "EG.ELC.ACCS.ZS",
  "IT.NET.USER.ZS", "SL.UEM.TOTL.ZS", "NV.AGR.TOTL.ZS", "NV.IND.TOTL.ZS", "FP.CPI.TOTL.ZG"
)
indicator_labels <- c(
  "GDP per capita", "Life expectancy", "Tertiary enrolment", "Measles immunisation",
  "Urban population", "CO2 emissions", "Electricity access", "Internet use",
  "Unemployment", "Agriculture share", "Industry share", "Inflation"
)
raw <- read.csv("Data.csv", check.names = FALSE, na.strings = c("..", ""),
                stringsAsFactors = FALSE)
stopifnot(all(c("Time", "Country Name", "Country Code") %in% names(raw)))
columns <- vapply(indicator_codes, function(code) {
  hits <- which(endsWith(names(raw), paste0("[", code, "]")))
  if (length(hits) != 1L) stop("Missing or duplicated indicator: ", code)
  hits
}, integer(1))
# Remove footer notes and blank rows. Retain aggregates as in the original script.
valid <- !is.na(raw$Time) & raw$Time == "2017" & !is.na(raw[["Country Code"]])
data <- raw[valid, , drop = FALSE]
stopifnot(nrow(data) > 12L, !anyDuplicated(data[["Country Code"]]),
          !anyNA(data[["Country Name"]]))
numeric_data <- as.data.frame(lapply(data[, columns], function(x) {
  converted <- suppressWarnings(as.numeric(x))
  if (any(!is.na(x) & is.na(converted))) stop("Unrecognised non-numeric data value.")
  converted
}))
names(numeric_data) <- indicator_codes
missing_count <- rowSums(is.na(numeric_data))
keep <- missing_count <= ncol(numeric_data) * 0.3
sample_audit <- data.frame(name = data[["Country Name"]], code = data[["Country Code"]],
                           missing_indicators = missing_count, retained = keep)
write.csv(sample_audit, "results/sample-audit.csv", row.names = FALSE)
retained <- numeric_data[keep, , drop = FALSE]
stopifnot(nrow(retained) > ncol(retained))
coverage <- data.frame(indicator = indicator_labels, code = indicator_codes,
                       original_column = names(raw)[columns],
                       missing_before_filter = colSums(is.na(numeric_data)),
                       missing_after_filter = colSums(is.na(retained)))
write.csv(coverage, "results/indicator-coverage.csv", row.names = FALSE)

# Preserve the original settings and explicitly select completed dataset 1.
# Other imputations provide a limited variance sensitivity check, not pooled inference.
if (anyNA(retained)) {
  imputed <- mice::mice(retained, m = 5, maxit = 5, method = "pmm", seed = 123, printFlag = FALSE)
  completed <- lapply(seq_len(5), function(i) mice::complete(imputed, action = i))
  events <- imputed$loggedEvents
} else {
  completed <- list(retained)
  events <- NULL
}
if (is.null(events)) events <- data.frame(note = character())
write.csv(events, "results/imputation-events.csv", row.names = FALSE)
fit_pca <- function(x) {
  stopifnot(all(is.finite(as.matrix(x))), all(vapply(x, sd, numeric(1)) > 0))
  fit <- prcomp(x, center = TRUE, scale. = TRUE)
  # Signs are arbitrary. Orient PC1 toward GDP and PC2 toward unemployment.
  anchors <- c("NY.GDP.PCAP.PP.KD", "SL.UEM.TOTL.ZS")
  for (j in seq_along(anchors)) {
    direction <- if (fit$rotation[anchors[j], j] < 0) -1 else 1
    fit$rotation[, j] <- fit$rotation[, j] * direction
    fit$x[, j] <- fit$x[, j] * direction
  }
  fit
}
fits <- lapply(completed, fit_pca)
pca <- fits[[1]]
scaled <- scale(completed[[1]])
eigenvalues <- pca$sdev^2
variance_pct <- eigenvalues / sum(eigenvalues) * 100
variance <- data.frame(component = paste0("PC", seq_along(eigenvalues)),
                       eigenvalue = eigenvalues, variance_pct = variance_pct,
                       cumulative_pct = cumsum(variance_pct))
write.csv(variance, "results/variance-explained.csv", row.names = FALSE)
# Check the SVD against the coursework's manual eigendecomposition.
manual <- eigen(cor(completed[[1]]), symmetric = TRUE)
stopifnot(isTRUE(all.equal(unname(manual$values), unname(eigenvalues), tolerance = 1e-8)))
# For standardised data, correlations = eigenvectors * sqrt(eigenvalues).
correlations <- sweep(pca$rotation, 2, pca$sdev, "*")
write.csv(data.frame(indicator = indicator_labels, code = indicator_codes, pca$rotation),
          "results/loadings.csv", row.names = FALSE)
write.csv(data.frame(indicator = indicator_labels, code = indicator_codes, correlations),
          "results/variable-correlations.csv", row.names = FALSE)
scores <- data.frame(name = data[["Country Name"]][keep],
                     code = data[["Country Code"]][keep], pca$x)
write.csv(scores, "results/scores.csv", row.names = FALSE)
sensitivity <- do.call(rbind, lapply(seq_along(fits), function(i) {
  v <- fits[[i]]$sdev^2 / sum(fits[[i]]$sdev^2) * 100
  data.frame(imputation = i, PC1_pct = v[1], PC2_pct = v[2],
             first_two_pct = sum(v[1:2]), first_three_pct = sum(v[1:3]))
}))
write.csv(sensitivity, "results/imputation-sensitivity.csv", row.names = FALSE)

# Consistent figures for the README and closer inspection.
ink <- "#172F46"
teal <- "#087F8C"
theme_set(theme_minimal(base_size = 12) + theme(
  text = element_text(colour = ink),
  plot.title = element_text(face = "bold", size = 18, margin = margin(b = 8)),
  plot.subtitle = element_text(colour = "#526578", margin = margin(b = 18)),
  plot.caption = element_text(colour = "#526578", hjust = 0, margin = margin(t = 14)),
  panel.grid.minor = element_blank(), plot.margin = margin(20, 24, 18, 20),
  legend.position = "bottom"
))
axis_labels <- sprintf("PC%d (%.1f%% of variance)", 1:2, variance_pct[1:2])
scree <- ggplot(variance, aes(x = seq_along(eigenvalue), y = variance_pct)) +
  geom_col(fill = teal, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", variance_pct)), vjust = -0.6, size = 3.5) +
  scale_x_continuous(breaks = seq_along(eigenvalues), labels = variance$component) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(title = "How much structure do the components retain?",
       subtitle = sprintf("Two components: %.1f%% of variance | Three components: %.1f%%",
                          sum(variance_pct[1:2]), sum(variance_pct[1:3])),
       x = NULL, y = "Variance explained (%)",
       caption = paste("World Development Indicators, 2017 | Standardised PCA |",
                       if (length(completed) > 1) sprintf("Completed dataset 1 of %d", length(completed))
                       else "Complete observed data"))
ggsave("figures/variance-explained.png", scree, width = 10, height = 5.8, dpi = 180, bg = "white")
variables <- data.frame(label = indicator_labels, PC1 = correlations[, 1],
                        PC2 = correlations[, 2], quality = rowSums(correlations[, 1:2]^2))
theta <- seq(0, 2 * pi, length.out = 361)
circle <- data.frame(x = cos(theta), y = sin(theta))
variable_plot <- ggplot(variables, aes(PC1, PC2)) +
  geom_path(data = circle, aes(x, y), inherit.aes = FALSE, colour = "#CAD4DE") +
  geom_hline(yintercept = 0, colour = "#CAD4DE", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "#CAD4DE", linewidth = 0.4) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2, colour = quality),
               arrow = grid::arrow(length = grid::unit(0.14, "cm")), linewidth = 0.7) +
  ggrepel::geom_label_repel(aes(label = label), seed = 123, size = 3.6,
                            box.padding = 0.6, max.overlaps = Inf, label.size = NA,
                            min.segment.length = 0, colour = ink, fill = "white") +
  scale_colour_gradient(low = "#A9C6CD", high = teal, limits = c(0, 1)) +
  coord_fixed(xlim = c(-1.15, 1.2), ylim = c(-1.1, 1.1)) +
  labs(title = "Which indicators shape the first two components?",
       subtitle = "Arrow coordinates are correlations with PC1 and PC2",
       x = axis_labels[1], y = axis_labels[2], colour = "Representation quality (cos2)",
       caption = "Longer arrows indicate better representation in this plane.\nPC signs are oriented toward GDP per capita (PC1) and unemployment (PC2).")
ggsave("figures/variable-correlations.png", variable_plot,
       width = 9, height = 8.5, dpi = 180, bg = "white")
# Label extremes for orientation; every retained observation remains visible.
label_rows <- unique(c(head(order(scores$PC1), 3), tail(order(scores$PC1), 3),
                       head(order(scores$PC2), 2), tail(order(scores$PC2), 2)))
score_plot <- ggplot(scores, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "#CAD4DE", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "#CAD4DE", linewidth = 0.4) +
  geom_point(colour = teal, alpha = 0.65, size = 2.3) +
  ggrepel::geom_text_repel(data = scores[label_rows, ], aes(label = name),
                           seed = 123, size = 3.3, box.padding = 0.5,
                           max.overlaps = Inf, min.segment.length = 0) +
  scale_x_continuous(expand = expansion(mult = 0.16)) +
  scale_y_continuous(expand = expansion(mult = 0.15)) + coord_fixed() +
  labs(title = "A two-dimensional view of development indicators",
       subtitle = sprintf("%d observations: economies and World Bank aggregates | 2017", nrow(scores)),
       x = axis_labels[1], y = axis_labels[2],
       caption = "Each point is a retained data row; selected extremes are labelled.\nScores describe this sample and indicator set. They are not official development rankings.")
ggsave("figures/observation-scores.png", score_plot, width = 10, height = 8, dpi = 180, bg = "white")
saveRDS(list(pca = pca, completed_data = completed[[1]], scaled = scaled,
             scores = scores, correlations = correlations, manual = manual,
             sensitivity = sensitivity, sample_audit = sample_audit), "results/pca-model.rds")
writeLines(capture.output(sessionInfo()), "results/session-info.txt")
versions <- data.frame(package = packages,
                       version = vapply(packages, function(x) as.character(packageVersion(x)), character(1)),
                       R = as.character(getRversion()))
write.csv(versions, "results/package-versions.csv", row.names = FALSE)
cat(sprintf("Retained %d of %d data rows; imputed %d missing cells.\n",
            sum(keep), length(keep), sum(is.na(retained))))
cat(sprintf("PC1: %.2f%% | PC2: %.2f%% | First two: %.2f%%\n",
            variance_pct[1], variance_pct[2], sum(variance_pct[1:2])))
cat("Figures and tables saved to figures/ and results/.\n")
