# Methodology and revision notes

This page documents the runnable repository version. The [submitted group report](../PCA_report.pdf) and [original repository script](../archive/Stats_Project_original.R) are preserved as historical coursework artifacts. The report includes both a `prcomp()` implementation and a manual eigendecomposition; its appendix code differs from the original standalone script.

## Data and sample

The supplied [Data.csv](../Data.csv) is a World Development Indicators export for **2017**. Its footer gives a database update date of **24 March 2025**. The report records an access date of 13 April 2025. The analysis uses this checked-in snapshot and makes no live data requests.

There are 266 data rows, covering economies and World Bank aggregates, followed by blank rows and source notes. The script removes the footer, converts `..` to missing values, and retains observations with at least 70% of the 12 indicators available. With 12 indicators, this means at most three missing values per row. This leaves **236 observations**, with **133 missing cells** to impute. The [sample audit](../results/sample-audit.csv) records every observation's inclusion status.

**Aggregates remain in the fitted sample**, as in the original standalone script. Regional and income-group averages overlap with individual economies, so the sample is not a collection of independent countries. Every retained row receives equal weight. A country-only analysis would be a separate extension requiring an explicit, documented economy classification.

| Indicator | WDI code | Unit |
| --- | --- | --- |
| GDP per capita, PPP | `NY.GDP.PCAP.PP.KD` | Constant 2021 international dollars |
| Life expectancy at birth | `SP.DYN.LE00.IN` | Years |
| Tertiary school enrolment | `SE.TER.ENRR` | Gross enrolment, % |
| Measles immunisation | `SH.IMM.MEAS` | % of children aged 12 to 23 months |
| Urban population | `SP.URB.TOTL.IN.ZS` | % of total population |
| CO2 emissions excluding LULUCF | `EN.GHG.CO2.PC.CE.AR5` | Tonnes CO2 equivalent per capita |
| Access to electricity | `EG.ELC.ACCS.ZS` | % of population |
| Individuals using the Internet | `IT.NET.USER.ZS` | % of population |
| Unemployment, modelled ILO estimate | `SL.UEM.TOTL.ZS` | % of total labour force |
| Agriculture, forestry and fishing value added | `NV.AGR.TOTL.ZS` | % of GDP |
| Industry, including construction, value added | `NV.IND.TOTL.ZS` | % of GDP |
| Consumer price inflation | `FP.CPI.TOTL.ZG` | Annual % |

LULUCF denotes land use, land-use change and forestry. Original column names and missing-value counts are exported in [indicator-coverage.csv](../results/indicator-coverage.csv).

## Missing values

The original MICE specification is retained: predictive mean matching (`pmm`), five completed datasets, five iterations, and seed 123. The first completed dataset supplies the main PCA and plots. This is made explicit with `complete(imputed, action = 1)`. The [MICE documentation](https://amices.org/mice/reference/complete.mids.html) describes the completed-dataset selection.

PCA is also fitted to the other four completed datasets. The first two components explain **62.36% to 63.03%** across the five imputations in the checked-in run. The full [sensitivity table](../results/imputation-sensitivity.csv) is available for inspection.

This is a limited check of explained variance. It does not pool estimates, quantify uncertainty in scores or loadings, establish imputation convergence, or test alternative missingness assumptions. MICE diagnostic events are exported to [imputation-events.csv](../results/imputation-events.csv); the checked-in run has no logged events, which is not itself evidence of convergence.

## PCA and numerical checks

Each indicator is centred and divided by its sample standard deviation. Let the resulting matrix be $Z$, with $n$ observations. Its correlation matrix is

$$R = \frac{Z^T Z}{n-1}.$$

The manual calculation diagonalises $R = V\Lambda V^T$. Component scores are $T = ZV$, and the variance explained by component $k$ is $\lambda_k / \sum_j \lambda_j$.

The main implementation uses `prcomp()`, which computes PCA by singular value decomposition. Its eigenvalues are checked against `eigen(cor(X))`. [R's documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/prcomp.html) describes the returned eigenvectors, standard deviations, and arbitrary component signs.

For consistent reading, PC1 is oriented to have a positive GDP-per-capita coefficient and PC2 a positive unemployment coefficient. Both scores and eigenvectors are flipped together when needed. This changes orientation only, not variance explained or distances.

The two types of variable coordinates are exported separately:

- **Eigenvector coefficients:** `pca$rotation`, in [loadings.csv](../results/loadings.csv).
- **Variable-component correlations:** $V_{jk}\sqrt{\lambda_k}$ for standardised inputs, in [variable-correlations.csv](../results/variable-correlations.csv).

The correlation circle uses the latter. Its colour scale is the sum of squared correlations with PC1 and PC2, or representation quality (cos2), on a scale from zero to one. It is not a percentage contribution to a component.

Run `Rscript --vanilla tests/verify.R` after the analysis to check orthogonality, full reconstruction, the score covariance, direct variable-score correlations, the manual eigendecomposition, and agreement with exported tables.

## Reading the results

PC1 explains 51.86% of variance. Internet use, life expectancy, electricity access and GDP per capita have positive coefficients, while agriculture's share of GDP has a negative coefficient. This supports a broad development-related interpretation for this indicator set and sample.

PC2 explains 10.86%. Unemployment has a positive coefficient; industry share and CO2 emissions have negative coefficients. It describes a further contrast in economic structure, rather than a second development ranking.

The first two components retain 62.72% of variance, and the first three retain 72.00%. Two dimensions are useful for visual inspection but omit 37.28% of the standardised variation. A 70% threshold is a descriptive choice, not a universal rule for how many components PCA should retain.

The displayed results reproduce the report's main eigenvalues and variance proportions to the precision printed there. The new graphics use corrected correlation coordinates, explicit sign conventions and descriptive observation labels; they are not reproductions of the report's original figures.

## Changes from the archived script

| Original behaviour | Runnable revision |
| --- | --- |
| Required a manually imported R object | Reads the checked-in CSV and validates the indicator codes |
| Broad package imports and an unavailable `geom_circle()` | Three direct dependencies; circle drawn with `geom_path()` |
| Rotation coefficients plotted against a unit circle | Variable-component correlations plotted against the unit circle |
| Normalised cos2 labelled as contribution | Actual cos2 labelled as representation quality |
| PC1 quartiles labelled as levels of development | All observations shown without development categories |
| Biplot used an arbitrary arrow multiplier | Separate score and correlation plots with explicit units |
| Plots printed to an interactive device | Three PNGs and inspectable CSV tables saved automatically |
| Only the first imputation used silently | Explicit main dataset selection and a five-imputation variance comparison |

## Limits and reproducibility

This is an exploratory, cross-sectional coursework analysis. Results depend on the chosen variables, equal weighting of rows, aggregate inclusion, missing-value handling and standardisation. PCA is sensitive to outliers and skewed variables. No transformation or robust alternative is assessed here, and associations do not establish causation.

The checked-in results were generated with R 4.5.2 and the direct dependency versions in [package-versions.csv](../results/package-versions.csv). `setup.R` installs missing packages into a local `.R-library/` directory; it does not pin or downgrade existing packages. A fixed seed supports repeatability within the same software environment, but results can change across package versions. Each run writes full `sessionInfo()` to the locally generated `results/session-info.txt`.

## Sources

- World Bank, [World Development Indicators](https://databank.worldbank.org/source/world-development-indicators). The bundled CSV is the input used here.
- Jolliffe, I. T. (2002). *Principal Component Analysis*. Springer. Cited in the original report.
- Abdi, H. (2010). “Principal Component Analysis.” *WIREs Computational Statistics*, 2(4), 433-459. As cited in the original report.
