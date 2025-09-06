# Comprehensive Random Number Generator Analysis

Generated on: 2025-07-31 21:49:07.928273

## Configuration

- Sample sizes tested:   10000,  100000, 1000000
- Runs per configuration: 50
- Total generators tested: 18

## Generators Tested

- R Mersenne Twister
- R L'Ecuyer-CMRG
- R Wichmann-Hill
- R Marsaglia-Multicarry
- R Super-Duper
- R Knuth-TAOCP
- R Knuth-TAOCP-2002
- QIPRNG (Default Config)
- QIPRNG (No Crypto)
- QIPRNG (With Crypto)
- QIPRNG (High Precision)
- Crypto (OpenSSL)
- dqrng (PCG64)
- dqrng (Xoroshiro128++)
- dqrng (Xoshiro256++)
- dqrng (Threefry)
- dqrng (Xoroshiro128+)
- dqrng (Xoshiro256+)

## Overall Pass Rates

| Generator | Pass Rate |
|-----------|-----------|
| dqrng (Threefry) | 100.00% |
| dqrng (Xoshiro256+) | 98.78% |
| dqrng (Xoshiro256++) | 98.78% |
| dqrng (Xoroshiro128++) | 98.56% |
| dqrng (PCG64) | 97.33% |
| QIPRNG (High Precision) | 96.78% |
| R L'Ecuyer-CMRG | 96.33% |
| R Knuth-TAOCP | 96.22% |
| R Wichmann-Hill | 96.22% |
| QIPRNG (No Crypto) | 95.89% |
| QIPRNG (Default Config) | 95.78% |
| R Marsaglia-Multicarry | 95.67% |
| R Knuth-TAOCP-2002 | 95.56% |
| R Mersenne Twister | 95.56% |
| Crypto (OpenSSL) | 95.44% |
| QIPRNG (With Crypto) | 95.22% |
| R Super-Duper | 94.89% |
| dqrng (Xoroshiro128+) | 88.44% |

## Statistical Moments Analysis

### Deviation from Expected Values

| Generator | Mean Dev | Var Dev | Skew Dev | Kurt Dev | Total Dev |
|-----------|----------|---------|----------|----------|-----------|
| R Knuth-TAOCP | 4.47e-05 | 2.74e-05 | 2.67e-04 | 2.47e-04 | 5.86e-04 |
| R Knuth-TAOCP-2002 | 3.50e-05 | 3.46e-05 | 2.37e-04 | 3.06e-04 | 6.12e-04 |
| R Wichmann-Hill | 8.20e-05 | 3.32e-05 | 2.70e-04 | 3.62e-04 | 7.47e-04 |
| R Mersenne Twister | 1.57e-04 | 2.14e-05 | 3.40e-04 | 2.85e-04 | 8.04e-04 |
| R L'Ecuyer-CMRG | 1.35e-04 | 3.72e-05 | 5.73e-04 | 4.24e-04 | 1.17e-03 |
| QIPRNG (No Crypto) | 1.55e-04 | 1.52e-05 | 5.58e-04 | 6.99e-04 | 1.43e-03 |
| R Super-Duper | 1.54e-04 | 6.05e-05 | 4.22e-04 | 8.83e-04 | 1.52e-03 |
| Crypto (OpenSSL) | 2.26e-05 | 7.54e-05 | 5.25e-04 | 9.64e-04 | 1.59e-03 |
| QIPRNG (High Precision) | 9.21e-05 | 3.32e-05 | 8.63e-04 | 7.70e-04 | 1.76e-03 |
| QIPRNG (With Crypto) | 3.31e-04 | 1.54e-05 | 1.27e-03 | 6.86e-04 | 2.30e-03 |
| dqrng (Xoshiro256+) | 2.35e-04 | 1.35e-05 | 1.42e-03 | 9.33e-04 | 2.60e-03 |
| R Marsaglia-Multicarry | 1.77e-04 | 2.28e-05 | 1.48e-03 | 9.33e-04 | 2.62e-03 |
| QIPRNG (Default Config) | 3.80e-04 | 6.09e-05 | 2.11e-03 | 8.79e-04 | 3.43e-03 |
| dqrng (Xoroshiro128+) | 5.64e-04 | 1.69e-04 | 1.35e-03 | 1.92e-03 | 4.01e-03 |
| dqrng (Xoshiro256++) | 3.77e-04 | 8.20e-05 | 1.61e-03 | 2.37e-03 | 4.43e-03 |
| dqrng (Xoroshiro128++) | 4.72e-04 | 8.51e-05 | 1.45e-03 | 2.66e-03 | 4.67e-03 |
| dqrng (PCG64) | 1.04e-03 | 4.58e-05 | 5.72e-03 | 5.35e-04 | 7.34e-03 |
| dqrng (Threefry) | 1.20e-03 | 1.15e-04 | 5.69e-03 | 2.01e-03 | 9.00e-03 |

### Expected Values

- Mean: 0.5
- Variance: 1/12 ≈ 0.0833
- Skewness: 0
- Kurtosis: -1.2

## Performance Summary

Generation rates shown for largest sample size tested.

| Generator | Rate (numbers/sec) |
|-----------|-------------------|
| dqrng (Xoroshiro128++) | 111,111,111 |
| dqrng (PCG64) | 100,000,000 |
| dqrng (Xoshiro256++) | 100,000,000 |
| dqrng (Xoshiro256+) | 100,000,000 |
| dqrng (Threefry) | 90,909,091 |
| R Knuth-TAOCP | 90,909,091 |
| R Marsaglia-Multicarry | 90,909,091 |
| R Super-Duper | 90,909,091 |
| R Knuth-TAOCP-2002 | 90,909,091 |
| dqrng (Xoroshiro128+) | 90,909,091 |
| R L'Ecuyer-CMRG | 83,333,333 |
| R Wichmann-Hill | 83,333,333 |
| R Mersenne Twister | 76,923,077 |
| Crypto (OpenSSL) | 28,571,429 |
| QIPRNG (With Crypto) | 6,289,308 |
| QIPRNG (High Precision) | 6,134,969 |
| QIPRNG (Default Config) | 6,097,561 |
| QIPRNG (No Crypto) | 6,024,096 |

## Test Categories Performance

### Basic Tests

| Generator | Pass Rate |
|-----------|-----------|
| dqrng (Threefry) | 100.00% |
| dqrng (PCG64) | 98.17% |
| dqrng (Xoshiro256+) | 98.17% |
| dqrng (Xoshiro256++) | 98.17% |
| dqrng (Xoroshiro128++) | 97.83% |

### Advanced Tests

| Generator | Pass Rate |
|-----------|-----------|
| dqrng (Threefry) | 100.00% |
| dqrng (Xoroshiro128++) | 100.00% |
| dqrng (Xoshiro256+) | 100.00% |
| dqrng (Xoshiro256++) | 100.00% |
| QIPRNG (High Precision) | 98.00% |
