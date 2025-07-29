# QIPRNG Discriminant Analysis

This directory contains the comprehensive discriminant analysis framework for evaluating the quality of pseudorandom number generators based on quadratic irrational discriminants.

## Directory Structure

```
analysis/
├── scripts/           # Analysis execution scripts
├── reports/           # Quarto reports and HTML outputs
├── data/             # Input data (discriminants.csv)
├── results/          # Analysis outputs (CSV, plots, etc.)
└── demo/             # Demo results and quick tests
```

## Usage

### Full Analysis (All 750 Discriminants, Parallel)

```bash
cd analysis/scripts
Rscript run_discriminant_analysis.R 1000000
```

This will:
- Test all 750 discriminants with 1,000,000 samples each
- Use parallel processing optimized for M4 Pro
- Generate results in `../results/discriminant_analysis_results/`
- Create comprehensive reports and visualizations

### Generate Quarto Report

```bash
cd analysis/reports
quarto render discriminant_analysis_report.qmd
```

## Key Features

- **Parallel Processing**: Optimized for multi-core systems with chunked processing
- **Robust Error Handling**: Timeout protection and graceful failure handling
- **Comprehensive Testing**: Uniformity, independence, autocorrelation, periodicity, and advanced tests
- **Interactive Reports**: HTML reports with visualizations and interactive tables
- **Empirical Quality Rating**: Data-driven classification based on score distributions
- **Autocorrelation Focus**: Detailed diagnostics for autocorrelation failures

## Test Suite

1. **Uniformity**: Kolmogorov-Smirnov and Chi-squared tests
2. **Independence**: Runs test for sequence independence
3. **Autocorrelation**: Multi-lag correlation analysis (primary focus)
4. **Periodicity**: Spectral analysis with multiple methods
5. **Moments**: Statistical moment validation
6. **Advanced**: Entropy, gap tests, serial correlation, poker test

## Output Files

- `summary_statistics.csv`: Tabular results for all discriminants
- `detailed_report.md`: Comprehensive analysis report
- Various `.png` visualizations including autocorrelation diagnostics
- `raw_results.rds`: Complete R data for further analysis
- Interactive HTML report with empirical quality ratings
