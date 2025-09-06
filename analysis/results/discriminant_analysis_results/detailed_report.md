# Discriminant Analysis Detailed Report

## Executive Summary

Total discriminants tested: 750
Sample size per discriminant: 1e+06

### Overall Performance

- Excellent (score ≥ 0.75): 370 (49.3%)
- Good (score ≥ 0.50): 293 (39.1%)
- Fair (score ≥ 0.25): 50 (6.7%)
- Poor (score < 0.25): 6 (0.8%)
- Errors: 0 (0%)

### Test Pass Rates

- Uniformity (Kolmogorov-Smirnov): 91.5%
- Independence (Runs Test): 95.2%
- Autocorrelation: 56.8%
- Periodicity (Spectral): 97.9%

## Top 5 Performing Discriminants

### Rank 1: Discriminant #2

**Parameters:** a=1, b=9, c=-145, Δ=661
**Overall Score:** 0.963 (Excellent)

**Test Results:**

- Uniformity: Passes both uniformity tests (good) (KS p=0.4543, ChiSq p=0.1343)
- Independence: ✓ PASS (p=0.7505)
- Autocorrelation: ✓ PASS (No significant autocorrelation (max |ACF| = 0.0022 < 0.0026))
- Periodicity: ✓ PASS (No significant periodicity/patterns detected ( 4 / 5  tests passed))
- Mean error: NA
- Variance error: NA

### Rank 2: Discriminant #3

**Parameters:** a=1, b=9, c=-148, Δ=673
**Overall Score:** 0.963 (Excellent)

**Test Results:**

- Uniformity: Passes both uniformity tests (good) (KS p=0.6978, ChiSq p=0.4891)
- Independence: ✓ PASS (p=0.752)
- Autocorrelation: ✓ PASS (No significant autocorrelation (max |ACF| = 0.0022 < 0.0026))
- Periodicity: ✓ PASS (No significant periodicity/patterns detected ( 4 / 5  tests passed))
- Mean error: NA
- Variance error: NA

### Rank 3: Discriminant #8

**Parameters:** a=1, b=13, c=-147, Δ=757
**Overall Score:** 0.963 (Excellent)

**Test Results:**

- Uniformity: Passes both uniformity tests (good) (KS p=0.2449, ChiSq p=0.5723)
- Independence: ✓ PASS (p=0.1579)
- Autocorrelation: ✓ PASS (No significant autocorrelation (max |ACF| = 0.0022 < 0.0026))
- Periodicity: ✓ PASS (No significant periodicity/patterns detected ( 4 / 5  tests passed))
- Mean error: NA
- Variance error: NA

### Rank 4: Discriminant #15

**Parameters:** a=2, b=5, c=-147, Δ=1201
**Overall Score:** 0.963 (Excellent)

**Test Results:**

- Uniformity: Passes both uniformity tests (good) (KS p=0.7405, ChiSq p=0.8625)
- Independence: ✓ PASS (p=0.8274)
- Autocorrelation: ✓ PASS (No significant autocorrelation (max |ACF| = 0.0023 < 0.0026))
- Periodicity: ✓ PASS (No significant periodicity/patterns detected ( 4 / 5  tests passed))
- Mean error: NA
- Variance error: NA

### Rank 5: Discriminant #16

**Parameters:** a=2, b=5, c=-149, Δ=1217
**Overall Score:** 0.963 (Excellent)

**Test Results:**

- Uniformity: Passes both uniformity tests (good) (KS p=0.2543, ChiSq p=0.3302)
- Independence: ✓ PASS (p=0.1465)
- Autocorrelation: ✓ PASS (No significant autocorrelation (max |ACF| = 0.0020 < 0.0026))
- Periodicity: ✓ PASS (No significant periodicity/patterns detected ( 4 / 5  tests passed))
- Mean error: NA
- Variance error: NA

## Bottom 5 Performing Discriminants

### Rank 746 (Bottom 5): Discriminant #342

**Parameters:** a=29, b=7, c=-57, Δ=6661
**Overall Score:** 0.438 (Poor)

**Failed Tests:**

- Independence (p=0.0183)
- Autocorrelation

### Rank 747 (Bottom 4): Discriminant #63

**Parameters:** a=6, b=5, c=-143, Δ=3457
**Overall Score:** 0.275 (Poor)

**Failed Tests:**

- Independence (p=0.037)
- Autocorrelation
- Periodicity

### Rank 748 (Bottom 3): Discriminant #373

**Parameters:** a=32, b=5, c=-44, Δ=5657
**Overall Score:** 0.275 (Poor)

**Failed Tests:**

- Independence (p=0.0392)
- Autocorrelation
- Periodicity

### Rank 749 (Bottom 2): Discriminant #382

**Parameters:** a=33, b=5, c=-57, Δ=7549
**Overall Score:** 0.275 (Poor)

**Failed Tests:**

- Independence (p=0.0468)
- Autocorrelation
- Periodicity

### Rank 750 (Bottom 1): Discriminant #534

**Parameters:** a=46, b=5, c=-79, Δ=14561
**Overall Score:** 0.275 (Poor)

**Failed Tests:**

- Uniformity: Fails uniformity tests (KS: Pass , Chi-Sq: Fail ) (KS p=0.2496, ChiSq p=0.0402)
- Independence (p=0.0394)
- Autocorrelation
