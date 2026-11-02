# Run from the repository root: Rscript --vanilla setup.R
# Keep dependencies local to this checkout rather than changing a user library.
dir.create(".R-library", showWarnings = FALSE)
.libPaths(c(normalizePath(".R-library"), .libPaths()))
packages <- c("mice", "ggplot2", "ggrepel")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}
stopifnot(all(vapply(packages, requireNamespace, logical(1), quietly = TRUE)))
cat("Dependencies ready. Run: Rscript --vanilla Stats_Project.R\n")
