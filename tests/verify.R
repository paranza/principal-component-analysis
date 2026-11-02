# Run after Stats_Project.R, from the repository root.
if (!file.exists("results/pca-model.rds")) stop("Run Stats_Project.R first.")
model <- readRDS("results/pca-model.rds")
pca <- model$pca
z <- model$scaled
near <- function(actual, expected, tolerance = 1e-8) {
  stopifnot(isTRUE(all.equal(unname(as.matrix(actual)), unname(as.matrix(expected)),
                            tolerance = tolerance, check.attributes = FALSE)))
}
# Independent algebraic checks catch scaling, score, and sign inconsistencies.
near(crossprod(pca$rotation), diag(ncol(z)))
near(pca$x %*% t(pca$rotation), z)
near(cov(pca$x), diag(pca$sdev^2))
near(cor(model$completed_data, pca$x), model$correlations)
near(model$manual$values, pca$sdev^2)
stopifnot(all(rowSums(model$correlations[, 1:2]^2) <= 1 + 1e-10))
stopifnot(pca$rotation["NY.GDP.PCAP.PP.KD", 1] > 0,
          pca$rotation["SL.UEM.TOTL.ZS", 2] > 0)
# Check the known input snapshot and the published tables, not just R objects.
audit <- read.csv("results/sample-audit.csv")
coverage <- read.csv("results/indicator-coverage.csv")
variance <- read.csv("results/variance-explained.csv")
scores <- read.csv("results/scores.csv")
stopifnot(nrow(audit) == 266L, sum(audit$retained) == 236L,
          sum(coverage$missing_after_filter) == 133L,
          all(audit$retained == (audit$missing_indicators <= 3L)),
          nrow(scores) == sum(audit$retained), !anyDuplicated(scores$code),
          identical(scores$code, audit$code[audit$retained]))
near(as.matrix(scores[, paste0("PC", 1:12)]), pca$x)
near(variance$variance_pct, pca$sdev^2 / sum(pca$sdev^2) * 100)
near(variance$cumulative_pct, cumsum(variance$variance_pct))
near(as.matrix(read.csv("results/loadings.csv")[, paste0("PC", 1:12)]), pca$rotation)
near(as.matrix(read.csv("results/variable-correlations.csv")[, paste0("PC", 1:12)]), model$correlations)
stopifnot(nrow(model$sensitivity) == 5L, all(is.finite(as.matrix(model$sensitivity))))
for (path in c("figures/variance-explained.png", "figures/variable-correlations.png",
               "figures/observation-scores.png")) {
  stopifnot(file.exists(path), file.info(path)$size > 1000)
}
cat("PASS: PCA identities, sample audit, exported tables, sensitivity results and figure files.\n")
