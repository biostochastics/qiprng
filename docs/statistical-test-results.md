# Statistical Test Results Summary

## Overview

This document presents the comprehensive statistical test results for the qiprng package based on extensive analysis with 100 runs per test and sample sizes up to 5 million.

## Test Methodology

### Configuration
- **Number of runs**: 100 per test (increased from 50 for better statistical power)
- **Sample sizes**: 10,000, 100,000, 1,000,000, and 5,000,000
- **Generators tested**: 18 different PRNGs including QIPRNG variants, dqrng, R built-ins, and OpenSSL
- **Test categories**: Basic distribution, advanced statistical, correlation, and moments analysis

### Key Improvements
1. **Fixed Gap Test**: Properly implements geometric distribution analysis for gaps between value occurrences
2. **Fixed Spectral Test**: Uses Kolmogorov-Smirnov test comparing spectral densities to exponential distribution
3. **Fixed dqrng variants**: Each variant now properly uses its specific algorithm

## Comprehensive Results

### Overall Performance Rankings

| Rank | Generator | Overall Pass Rate | Statistical Moments Deviation |
|------|-----------|------------------|------------------------------|
| 1 | dqrng (Threefry) | 100.0% | 0.009004 |
| 2.5 | dqrng (Xoshiro256+) | 98.8% | 0.002597 |
| 2.5 | dqrng (Xoshiro256++) | 98.8% | 0.004434 |
| 4 | dqrng (Xoroshiro128++) | 98.6% | 0.004670 |
| 5 | dqrng (PCG64) | 97.3% | 0.007339 |
| 6 | **QIPRNG (High Precision)** | **96.8%** | **0.001758** |
| 7 | R L'Ecuyer-CMRG | 96.3% | 0.001169 |
| 8.5 | R Knuth-TAOCP | 96.2% | 0.000586 |
| 8.5 | R Wichmann-Hill | 96.2% | 0.000747 |
| 10 | **QIPRNG (No Crypto)** | **95.9%** | **0.001427** |
| 11 | **QIPRNG (Default Config)** | **95.8%** | **0.003427** |
| 12 | R Marsaglia-Multicarry | 95.7% | 0.002615 |
| 13.5 | R Knuth-TAOCP-2002 | 95.6% | 0.000612 |
| 13.5 | R Mersenne Twister | 95.6% | 0.000804 |
| 15 | Crypto (OpenSSL) | 95.4% | 0.001587 |
| 16 | **QIPRNG (With Crypto)** | **95.2%** | **0.002300** |
| 17 | R Super-Duper | 94.9% | 0.001521 |
| 18 | dqrng (Xoroshiro128+) | 88.4% | 0.004006 |

### Statistical Moments Analysis

Analysis of total deviation from theoretical moments:

| Generator Category | Best | Worst | Range |
|-------------------|------|-------|--------|
| R built-ins (traditional) | 0.000586 (Knuth-TAOCP) | 0.002615 (Marsaglia) | 0.000586-0.002615 |
| **QIPRNG variants** | **0.001427 (No Crypto)** | **0.003427 (Default)** | **0.001427-0.003427** |
| dqrng (fast modern) | 0.002597 (Xoshiro256+) | 0.009004 (Threefry) | 0.002597-0.009004 |
| Cryptographic | 0.001587 (OpenSSL) | - | - |

**Key Finding**: Traditional R generators (especially Knuth-TAOCP) show the best moments adherence, while QIPRNG provides a good balance between statistical quality and cryptographic security.

### Test Category Performance

#### Basic Tests (Average Pass Rate: 95.2%)
- Kolmogorov-Smirnov: 96.9%
- Chi-squared: 95.3%
- Runs Test: 94.9%
- Autocorrelation: 94.6%

#### Advanced Tests (After Fixes)
- **Spectral Test**: 99.1% (was 100% due to bug returning fixed 0.01)
- **Gap Test**: 96.4% (was 0% due to bug returning 0)

### Performance vs Quality Trade-off

| Generator Type | Speed (numbers/sec) | Quality Rating |
|----------------|-------------------|----------------|
| dqrng | 100-111 million | Good |
| R built-ins | 83-100 million | Good |
| OpenSSL | 37 million | Very Good |
| **QIPRNG** | **6 million** | **Excellent** |

## Key Findings

1. **Statistical Quality Hierarchy**: 
   - Best moments adherence: R Knuth-TAOCP (0.000586)
   - QIPRNG shows good statistical quality (0.001427-0.003427)
   - dqrng prioritizes speed over moments accuracy (0.002597-0.009004)

2. **Fixed Test Implementation**: The Gap and Spectral tests now properly evaluate randomness:
   - Gap test correctly analyzes geometric distribution of value occurrence gaps
   - Spectral test properly uses KS test for exponential distribution comparison

3. **dqrng Differentiation**: All dqrng variants now show distinct performance characteristics after fixing the global state issue.

4. **Performance Trade-off**: QIPRNG sacrifices speed (17x slower than dqrng) for cryptographic security, while maintaining good statistical quality.

## Test Implementation Details

### Gap Test
- Analyzes gaps between occurrences of values in range [0.3, 0.7]
- Compares observed gap distribution to expected geometric distribution
- Uses chi-squared goodness-of-fit test
- Average pass rate: 96.4%

### Spectral Test  
- Computes periodogram using FFT
- Normalizes spectral densities by their mean
- Tests if normalized values follow exponential(1) distribution using KS test
- Average pass rate: 99.1%

## Conclusion

The comprehensive testing reveals:

1. **Statistical Moments**: Traditional R generators (Knuth-TAOCP) show the best adherence to theoretical moments, followed by QIPRNG and cryptographic generators, with dqrng showing higher deviations.

2. **Pass Rates**: dqrng generators achieve the highest pass rates but with lower statistical precision. QIPRNG maintains competitive pass rates while providing cryptographic security.

3. **Performance**: Clear trade-off exists - dqrng offers 17x faster generation at the cost of statistical precision, while QIPRNG prioritizes cryptographic security and maintains good (though not best) statistical quality.

4. **Test Accuracy**: The fixes to Gap and Spectral tests now provide accurate assessment of advanced randomness properties, with all generators showing realistic pass rates.

For users requiring cryptographic security, QIPRNG is the clear choice. For pure statistical simulation where speed matters most, dqrng excels. For applications requiring the best statistical moments adherence, traditional R generators like Knuth-TAOCP remain competitive.