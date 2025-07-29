# Discriminant Analysis Detailed Report

## Executive Summary

Total discriminants tested: 750
Sample size per discriminant: 50000

### Overall Performance:
- Excellent (score ≥ 0.75): 227 (30.3%)
- Good (score ≥ 0.50): 429 (57.2%)
- Fair (score ≥ 0.25): 87 (11.6%)
- Poor (score < 0.25): 7 (0.9%)
- Errors: 0 (0%)

### Test Pass Rates:
- Uniformity (Kolmogorov-Smirnov): 90.7%
- Independence (Runs Test): 94.5%
- Autocorrelation: 33.7%
- Periodicity (Spectral): 97.5%

## Top 5 Performing Discriminants

### Rank 1: Discriminant #13
**Parameters:** a=2, b=5, c=-141, Δ=1153
**Overall Score:** 0.95 (Excellent)

**Test Results:**
- Uniformity: Passes both uniformity tests (good) (KS p=0.7726, ChiSq p=0.25)
- Independence: ✓ PASS (p=0.9929)
- Autocorrelation: ✓ PASS (No significant autocorrelation (good))
- Periodicity: ✓ PASS (No significant periodicity/patterns detected ( 4 / 5  tests passed))
- Mean error: 0.000138
- Variance error: 0

### Rank 2: Discriminant #15
**Parameters:** a=2, b=5, c=-147, Δ=1201
**Overall Score:** 0.95 (Excellent)

**Test Results:**
- Uniformity: Passes both uniformity tests (good) (KS p=0.866, ChiSq p=0.6648)
- Independence: ✓ PASS (p=0.9216)
- Autocorrelation: ✓ PASS (No significant autocorrelation (good))
- Periodicity: ✓ PASS (No significant periodicity/patterns detected ( 4 / 5  tests passed))
- Mean error: 0.000168
- Variance error: 4.9e-05

### Rank 3: Discriminant #16
**Parameters:** a=2, b=5, c=-149, Δ=1217
**Overall Score:** 0.95 (Excellent)

**Test Results:**
- Uniformity: Passes both uniformity tests (good) (KS p=0.8333, ChiSq p=0.3605)
- Independence: ✓ PASS (p=0.6873)
- Autocorrelation: ✓ PASS (No significant autocorrelation (good))
- Periodicity: ✓ PASS (No significant periodicity/patterns detected ( 4 / 5  tests passed))
- Mean error: 0.000194
- Variance error: 0.000103

### Rank 4: Discriminant #19
**Parameters:** a=2, b=7, c=-120, Δ=1009
**Overall Score:** 0.95 (Excellent)

**Test Results:**
- Uniformity: Passes both uniformity tests (good) (KS p=0.9372, ChiSq p=0.6461)
- Independence: ✓ PASS (p=0.83)
- Autocorrelation: ✓ PASS (No significant autocorrelation (good))
- Periodicity: ✓ PASS (No significant periodicity/patterns detected ( 3 / 5  tests passed))
- Mean error: 0.000456
- Variance error: 8.1e-05

### Rank 5: Discriminant #30
**Parameters:** a=3, b=7, c=-129, Δ=1597
**Overall Score:** 0.95 (Excellent)

**Test Results:**
- Uniformity: Passes both uniformity tests (good) (KS p=0.7691, ChiSq p=0.1474)
- Independence: ✓ PASS (p=0.5312)
- Autocorrelation: ✓ PASS (No significant autocorrelation (good))
- Periodicity: ✓ PASS (No significant periodicity/patterns detected ( 4 / 5  tests passed))
- Mean error: 4e-05
- Variance error: 0.000193

## Bottom 5 Performing Discriminants

### Rank 746 (Bottom 5): Discriminant #642
**Parameters:** a=57, b=13, c=-43, Δ=9973
**Overall Score:** 0.35 (Poor)

**Failed Tests:**
- Independence (p=0.0097)
- Autocorrelation
- Periodicity

### Rank 747 (Bottom 4): Discriminant #5
**Parameters:** a=1, b=11, c=-145, Δ=701
**Overall Score:** 0.25 (Poor)

**Failed Tests:**
- Uniformity: Fails uniformity tests (KS: Fail , Chi-Sq: Pass ) (KS p=0.0222, ChiSq p=0.1575)
- Independence (p=0.0297)
- Autocorrelation

### Rank 748 (Bottom 3): Discriminant #10
**Parameters:** a=1, b=13, c=-150, Δ=769
**Overall Score:** 0.25 (Poor)

**Failed Tests:**
- Uniformity: Fails uniformity tests (KS: Fail , Chi-Sq: Pass ) (KS p=0.036, ChiSq p=0.4778)
- Independence (p=0.0187)
- Autocorrelation

### Rank 749 (Bottom 2): Discriminant #400
**Parameters:** a=34, b=7, c=-25, Δ=3449
**Overall Score:** 0.25 (Poor)

**Failed Tests:**
- Uniformity: Fails uniformity tests (KS: Pass , Chi-Sq: Fail ) (KS p=0.6889, ChiSq p=0.0419)
- Independence (p=0.0065)
- Autocorrelation

### Rank 750 (Bottom 1): Discriminant #446
**Parameters:** a=38, b=7, c=-20, Δ=3089
**Overall Score:** 0.25 (Poor)

**Failed Tests:**
- Uniformity: Fails uniformity tests (KS: Fail , Chi-Sq: Pass ) (KS p=0.0284, ChiSq p=0.3262)
- Independence (p=0.0481)
- Autocorrelation


