# Replication summary

Replication of IES (Charles University) bachelor's and master's meta-analysis theses supervised by Tomáš Havránek or Zuzana Havránková, using `artma`. Each thesis's published dataset is re-analysed and the results compared against the numbers printed in the thesis.

- Theses attempted: **10** (10 replicated, 0 failed)
- Claims checked: **112**
- Verdicts: match **71**, close **13**, mismatch **28**

Generated 2026-08-14 by `scripts/replication/run_replication.R`.

**Verdicts.** `match`: within tolerance (default 0.005 absolute or 5% relative, whichever is looser). `close`: within 3x tolerance and the same sign. `mismatch`: outside that. `no match`/`ambiguous`: the claim's `artma_model`/`artma_term` regexes selected zero or several rows — a manifest bug, not a disagreement with the thesis.

## Where artma agrees and where it does not

Pooling `match` and `close` as agreement, across every resolved claim. These rates are about *this* set of theses, not a general benchmark, but the pattern is consistent enough to be worth reading.

**By term (linear_tests)**

| Term | Claims | Agreeing | Rate |
| --- | ---: | ---: | ---: |
| `effect` | 50 | 41 | 82% |
| `publication_bias` | 50 | 33 | 66% |

**By model (linear_tests)**

| Model | Claims | Agreeing | Rate |
| --- | ---: | ---: | ---: |
| `be` | 18 | 15 | 83% |
| `fe` | 18 | 9 | 50% |
| `ols` | 18 | 18 | 100% |
| `ols_precision_weighted` | 22 | 18 | 82% |
| `ols_study_weighted` | 18 | 11 | 61% |
| `re` | 6 | 3 | 50% |

**By method**

| Method | Claims | Agreeing | Rate |
| --- | ---: | ---: | ---: |
| `effect_summary_stats` | 10 | 10 | 100% |
| `exogeneity_tests` | 2 | 0 | 0% |
| `linear_tests` | 100 | 74 | 74% |

## Per-thesis results

### Hatalová, Lucia Mária (2026) — Women's Empowerment and Economic Development: A Meta-Analysis

- **Degree**: master thesis, supervised by Havránek, Tomáš
- **Repository**: [20.500.11956/210665](https://dspace.cuni.cz/handle/20.500.11956/210665)
- **Dataset**: [120544761.zip](https://dspace.cuni.cz/bitstream/handle/20.500.11956/210665/120544761.zip?sequence=4&isAllowed=y) — code published alongside

- **Sample**: 904 estimates used

| Claim | Source in thesis | Reported | artma | Diff | Verdict |
| --- | --- | ---: | ---: | ---: | --- |
| Unweighted mean PCC, all estimates | Table 3.1, p. 25 | 0.1260 | 0.1274 | 0.0014 | match |
| Effect beyond bias, baseline OLS | Table 4.1, p. 32 (Panel A, full aggregate) | 0.1760 | 0.1764 | 0.0004 | match |
| Effect beyond bias, study-weighted | Table 4.1, p. 32 (Panel A, full aggregate) | 0.0750 | 0.0844 | 0.0094 | close |
| Effect beyond bias, precision-weighted | Table 4.1, p. 32 (Panel A, full aggregate) | 0.1380 | 0.1382 | 0.0002 | match |
| Effect beyond bias, fixed effects | Table 4.1, p. 32 (Panel A, full aggregate) | 0.1380 | 0.2134 | 0.0754 | MISMATCH |
| Effect beyond bias, between effects | Table 4.1, p. 32 (Panel A, full aggregate) | 0.0650 | 0.0646 | -0.0004 | match |
| Effect beyond bias, random effects | Table 4.1, p. 32 (Panel A, full aggregate) | 0.1700 | 0.1773 | 0.0073 | match |
| Publication bias, baseline OLS | Table 4.1, p. 32 (Panel B, full aggregate) | -0.5000 | -0.4996 | 0.0004 | match |
| Publication bias, study-weighted | Table 4.1, p. 32 (Panel B, full aggregate) | 0.6190 | 0.3917 | -0.2273 | MISMATCH |
| Publication bias, precision-weighted | Table 4.1, p. 32 (Panel B, full aggregate) | -0.0520 | -0.0521 | -0.0001 | match |
| Publication bias, fixed effects | Table 4.1, p. 32 (Panel B, full aggregate) | -0.0520 | -0.8778 | -0.8258 | MISMATCH |
| Publication bias, between effects | Table 4.1, p. 32 (Panel B, full aggregate) | 0.5820 | 0.5816 | -0.0004 | match |
| Publication bias, random effects | Table 4.1, p. 32 (Panel B, full aggregate) | -0.4330 | -0.5378 | -0.1048 | MISMATCH |

### Horák, Matyáš (2024) — The Effects of Quantitative Easing: A Meta-Analysis

- **Degree**: bachelor thesis, supervised by Havránek, Tomáš
- **Repository**: [20.500.11956/193473](https://dspace.cuni.cz/handle/20.500.11956/193473)
- **Dataset**: [130404211.zip](https://dspace.cuni.cz/bitstream/handle/20.500.11956/193473/130404211.zip?sequence=4&isAllowed=y) — code published alongside

- **Sample**: 43 estimates used (7 rows dropped: non-finite or non-positive `se`)

| Claim | Source in thesis | Reported | artma | Diff | Verdict |
| --- | --- | ---: | ---: | ---: | --- |
| Sample mean, peak effect on GDP | Table 3.2, p. 12 (All Data) | 0.2390 | 0.2386 | -0.0004 | match |
| Publication bias, OLS | Table 4.1, p. 19 (Peak Effect on GDP) | 1.2830 | 1.2828 | -0.0002 | match |
| Effect beyond bias, OLS | Table 4.1, p. 19 (Peak Effect on GDP) | 0.0120 | 0.0116 | -0.0004 | match |
| Publication bias, fixed effects | Table 4.1, p. 19 (Peak Effect on GDP) | 1.4090 | 1.4086 | -0.0004 | match |
| Effect beyond bias, fixed effects | Table 4.1, p. 19 (Peak Effect on GDP) | -0.0110 | -0.0107 | 0.0003 | match |
| Publication bias, between effects | Table 4.1, p. 19 (Peak Effect on GDP) | 1.1010 | 1.1006 | -0.0004 | match |
| Effect beyond bias, between effects | Table 4.1, p. 19 (Peak Effect on GDP) | 0.0480 | 0.0478 | -0.0002 | match |
| Publication bias, random effects | Table 4.1, p. 19 (Peak Effect on GDP) | 1.3130 | 1.3125 | -0.0005 | match |
| Effect beyond bias, random effects | Table 4.1, p. 19 (Peak Effect on GDP) | 0.0130 | 0.0125 | -0.0005 | match |
| Publication bias, study-weighted | Table 4.1, p. 19 (Peak Effect on GDP) | 1.5530 | 1.1058 | -0.4472 | MISMATCH |
| Effect beyond bias, study-weighted | Table 4.1, p. 19 (Peak Effect on GDP) | -0.0620 | 0.0472 | 0.1092 | MISMATCH |
| Publication bias, precision-weighted | Table 4.1, p. 19 (Peak Effect on GDP) | 1.5900 | 1.5893 | -0.0007 | match |
| Effect beyond bias, precision-weighted | Table 4.1, p. 19 (Peak Effect on GDP) | -0.0040 | -0.0044 | -0.0004 | match |

### Juračková, Martina (2023) — Standing Tall Pays Off: A Meta-Analysis of Height Premium

- **Degree**: master thesis, supervised by Havránková, Zuzana
- **Repository**: [20.500.11956/186222](https://dspace.cuni.cz/handle/20.500.11956/186222)
- **Dataset**: [120458436.zip](https://dspace.cuni.cz/bitstream/handle/20.500.11956/186222/120458436.zip?sequence=4&isAllowed=y) — code published alongside

- **Sample**: 127 estimates used

| Claim | Source in thesis | Reported | artma | Diff | Verdict |
| --- | --- | ---: | ---: | ---: | --- |
| Unweighted mean height premium (causal) | Table 3.2, p. 25 (causal column) | 0.1150 | 0.1111 | -0.0039 | match |
| Median height premium (causal) | Table 3.2, p. 25 (causal column) | 0.0780 | 0.0723 | -0.0057 | close |
| SD of height premium (causal) | Table 3.2, p. 25 (causal column) | 0.1490 | 0.1373 | -0.0117 | close |
| Publication bias, OLS | Table 4.1, p. 33 (Panel A: causal effects) | 0.8740 | 0.8736 | -0.0004 | match |
| Mean beyond bias, OLS | Table 4.1, p. 33 (Panel A: causal effects) | 0.0580 | 0.0583 | 0.0003 | match |
| Publication bias, between effects | Table 4.1, p. 33 (Panel A: causal effects) | 1.5670 | 1.2161 | -0.3509 | MISMATCH |
| Mean beyond bias, between effects | Table 4.1, p. 33 (Panel A: causal effects) | 0.0280 | 0.0192 | -0.0088 | close |
| Publication bias, fixed effects | Table 4.1, p. 33 (Panel A: causal effects) | 2.2420 | -0.7029 | -2.9449 | MISMATCH |
| Mean beyond bias, fixed effects | Table 4.1, p. 33 (Panel A: causal effects) | 0.0050 | 0.1535 | 0.1485 | MISMATCH |
| Publication bias, study-weighted | Table 4.1, p. 33 (Panel A: causal effects) | 1.0270 | 1.0274 | 0.0004 | match |
| Mean beyond bias, study-weighted | Table 4.1, p. 33 (Panel A: causal effects) | 0.0280 | 0.0285 | 0.0005 | match |
| Publication bias, precision-weighted | Table 4.1, p. 33 (Panel A: causal effects) | 1.5290 | 2.2420 | 0.7130 | MISMATCH |
| Mean beyond bias, precision-weighted | Table 4.1, p. 33 (Panel A: causal effects) | 0.0190 | 0.0053 | -0.0137 | close |

### Kozlíková, Kateřina (2024) — Price elasticities of meat, fish and seafood: A meta-analysis

- **Degree**: bachelor thesis, supervised by Havránková, Zuzana
- **Repository**: [20.500.11956/193521](https://dspace.cuni.cz/handle/20.500.11956/193521)
- **Dataset**: [130403547.zip](https://dspace.cuni.cz/bitstream/handle/20.500.11956/193521/130403547.zip?sequence=4&isAllowed=y) — code published alongside

- **Sample**: 202 estimates used

| Claim | Source in thesis | Reported | artma | Diff | Verdict |
| --- | --- | ---: | ---: | ---: | --- |
| Publication bias, OLS | Table 4.1, p. 25 (Marshallian, Meat) | -0.4840 | -0.5070 | -0.0230 | match |
| Mean beyond bias, OLS | Table 4.1, p. 25 (Marshallian, Meat) | -0.8350 | -0.8344 | 0.0006 | match |
| Publication bias, fixed effects | Table 4.1, p. 25 (Marshallian, Meat) | -0.5900 | -0.6251 | -0.0351 | close |
| Mean beyond bias, fixed effects | Table 4.1, p. 25 (Marshallian, Meat) | -0.8100 | -0.8070 | 0.0030 | match |
| Publication bias, between effects | Table 4.1, p. 25 (Marshallian, Meat) | -0.6230 | -0.6640 | -0.0410 | close |
| Mean beyond bias, between effects | Table 4.1, p. 25 (Marshallian, Meat) | -0.7830 | -0.7807 | 0.0023 | match |
| Publication bias, precision-weighted | Table 4.1, p. 25 (Marshallian, Meat) | -16.0950 | -16.3146 | -0.2196 | match |
| Mean beyond bias, precision-weighted | Table 4.1, p. 25 (Marshallian, Meat) | -0.2630 | -0.2545 | 0.0085 | match |
| Publication bias, study-weighted | Table 4.1, p. 25 (Marshallian, Meat) | -0.6610 | -0.7228 | -0.0618 | close |
| Mean beyond bias, study-weighted | Table 4.1, p. 25 (Marshallian, Meat) | -0.7770 | -0.7710 | 0.0060 | match |

### Křenková, Pavlína (2023) — The Price Elasticity of Milk Demand: A Meta-Analysis

- **Degree**: bachelor thesis, supervised by Havránková, Zuzana
- **Repository**: [20.500.11956/181954](https://dspace.cuni.cz/handle/20.500.11956/181954)
- **Dataset**: [130357765.zip](https://dspace.cuni.cz/bitstream/handle/20.500.11956/181954/130357765.zip?sequence=4&isAllowed=y) — code published alongside

- **Sample**: 138 estimates used

| Claim | Source in thesis | Reported | artma | Diff | Verdict |
| --- | --- | ---: | ---: | ---: | --- |
| Mean elasticity (milk composite) | Table 3.1, p. 11 | -0.6000 | -0.6024 | -0.0024 | match |
| Median elasticity | Table 3.1, p. 11 | -0.3800 | -0.3818 | -0.0018 | match |
| SD of elasticity | Table 3.1, p. 11 | 0.6300 | 0.6285 | -0.0015 | match |
| Publication bias, OLS | Table 4.1, p. 21 | -1.0310 | -1.0311 | -0.0001 | match |
| Effect beyond bias, OLS | Table 4.1, p. 21 | -0.4630 | -0.4632 | -0.0002 | match |
| Publication bias, fixed effects | Table 4.1, p. 21 | -0.0910 | -0.0906 | 0.0004 | match |
| Effect beyond bias, fixed effects | Table 4.1, p. 21 | -0.5900 | -0.5902 | -0.0002 | match |
| Publication bias, between effects | Table 4.1, p. 21 | -1.4000 | -1.4004 | -0.0004 | match |
| Effect beyond bias, between effects | Table 4.1, p. 21 | -0.3860 | -0.3863 | -0.0003 | match |
| Publication bias, precision-weighted | Table 4.1, p. 21 | -4.1560 | -4.1561 | -0.0001 | match |
| Effect beyond bias, precision-weighted | Table 4.1, p. 21 | -0.3060 | -0.3056 | 0.0004 | match |
| Publication bias, study-weighted | Table 4.1, p. 21 | -1.1360 | -1.1362 | -0.0002 | match |
| Effect beyond bias, study-weighted | Table 4.1, p. 21 | -0.4210 | -0.4215 | -0.0005 | match |

### Maryško, Karel (2025) — How Does ESG Performance Affect Earnings? A Meta-Analysis

- **Degree**: bachelor thesis, supervised by Havránek, Tomáš
- **Repository**: [20.500.11956/202746](https://dspace.cuni.cz/handle/20.500.11956/202746)
- **Dataset**: [130438350.zip](https://dspace.cuni.cz/bitstream/handle/20.500.11956/202746/130438350.zip?sequence=4&isAllowed=y) — code published alongside

- **Sample**: 108 estimates used

| Claim | Source in thesis | Reported | artma | Diff | Verdict |
| --- | --- | ---: | ---: | ---: | --- |
| Unweighted mean PCC, all studies | Table 3.1, p. 18 | 0.0330 | 0.0327 | -0.0003 | match |
| Publication bias, OLS | Table 4.1, p. 25 (PCC) | 0.9000 | 0.8999 | -0.0001 | match |
| Effect beyond bias, OLS | Table 4.1, p. 25 (PCC) | 0.0030 | 0.0026 | -0.0004 | match |
| Publication bias, fixed effects | Table 4.1, p. 25 (PCC) | 0.3460 | 3.5268 | 3.1808 | MISMATCH |
| Effect beyond bias, fixed effects | Table 4.1, p. 25 (PCC) | 0.0170 | -0.0853 | -0.1023 | MISMATCH |
| Publication bias, random effects | Table 4.1, p. 25 (PCC) | 0.8810 | 1.4753 | 0.5943 | MISMATCH |
| Effect beyond bias, random effects | Table 4.1, p. 25 (PCC) | 0.0030 | -0.0212 | -0.0242 | MISMATCH |
| Publication bias, precision-weighted | Table 4.1, p. 25 (PCC) | 0.3460 | 0.3464 | 0.0004 | match |
| Effect beyond bias, precision-weighted | Table 4.1, p. 25 (PCC) | 0.0170 | 0.0166 | -0.0004 | match |
| Publication bias, study-weighted | Table 4.1, p. 25 (PCC) | 1.4880 | 0.5098 | -0.9782 | MISMATCH |
| Effect beyond bias, study-weighted | Table 4.1, p. 25 (PCC) | -0.0010 | 0.0156 | 0.0166 | MISMATCH |

### Nguyenová, Van Anh (2023) — How much does intelligence predict lifetime income? A Meta-Analysis

- **Degree**: bachelor thesis, supervised by Havránková, Zuzana
- **Repository**: [20.500.11956/185024](https://dspace.cuni.cz/handle/20.500.11956/185024)
- **Dataset**: [130372509.csv](https://dspace.cuni.cz/bitstream/handle/20.500.11956/185024/130372509.csv?sequence=4&isAllowed=y) — data only, no code

- **Sample**: 765 estimates used

| Claim | Source in thesis | Reported | artma | Diff | Verdict |
| --- | --- | ---: | ---: | ---: | --- |
| Unweighted mean effect, all estimates | Table 3.1, p. 20 | 0.0780 | 0.0781 | 0.0001 | match |
| Publication bias, OLS | Table 4.1, p. 26 | 0.3770 | 0.3771 | 0.0001 | match |
| Effect beyond bias, OLS | Table 4.1, p. 26 | 0.0700 | 0.0696 | -0.0004 | match |
| Publication bias, study-weighted | Table 4.1, p. 26 | 0.4700 | 0.3094 | -0.1606 | MISMATCH |
| Effect beyond bias, study-weighted | Table 4.1, p. 26 | 0.0460 | 0.0832 | 0.0372 | MISMATCH |
| Publication bias, precision-weighted | Table 4.1, p. 26 | 4.9600 | 4.9597 | -0.0003 | match |
| Effect beyond bias, precision-weighted | Table 4.1, p. 26 | 0.0070 | 0.0071 | 0.0001 | match |
| Publication bias, fixed effects | Table 4.1, p. 26 | 0.1510 | 0.1514 | 0.0004 | match |
| Effect beyond bias, fixed effects | Table 4.1, p. 26 | 0.0750 | 0.0747 | -0.0003 | match |
| Publication bias, between effects | Table 4.1, p. 26 | 0.4210 | 0.4209 | -0.0001 | match |
| Effect beyond bias, between effects | Table 4.1, p. 26 | 0.0810 | 0.0808 | -0.0002 | match |

### Pokorná, Anastasia (2023) — The Causal Effect of Parents' Schooling on Children's Schooling: A Meta-Analysis

- **Degree**: master thesis, supervised by Havránková, Zuzana
- **Repository**: [20.500.11956/179488](https://dspace.cuni.cz/handle/20.500.11956/179488)
- **Dataset**: [120437903.zip](https://dspace.cuni.cz/bitstream/handle/20.500.11956/179488/120437903.zip?sequence=4&isAllowed=y) — code published alongside

- **Sample**: 387 estimates used (605 rows dropped: non-finite or non-positive `se`)

| Claim | Source in thesis | Reported | artma | Diff | Verdict |
| --- | --- | ---: | ---: | ---: | --- |
| Publication bias, OLS | Table 4.1, p. 27 (Panel A, causal effects) | 1.3500 | 1.3497 | -0.0003 | match |
| Mean beyond bias, OLS | Table 4.1, p. 27 (Panel A, causal effects) | 0.0440 | 0.0442 | 0.0002 | match |
| Publication bias, fixed effects | Table 4.1, p. 27 (Panel A, causal effects) | 0.8940 | 0.6840 | -0.2100 | MISMATCH |
| Mean beyond bias, fixed effects | Table 4.1, p. 27 (Panel A, causal effects) | 0.0810 | 0.1310 | 0.0500 | MISMATCH |
| Publication bias, between effects | Table 4.1, p. 27 (Panel A, causal effects) | 2.0100 | 2.0096 | -0.0004 | match |
| Mean beyond bias, between effects | Table 4.1, p. 27 (Panel A, causal effects) | -0.0420 | -0.0415 | 0.0005 | match |
| Publication bias, precision-weighted | Table 4.1, p. 27 (Panel B, causal effects) | 1.1360 | 0.8935 | -0.2425 | MISMATCH |
| Mean beyond bias, precision-weighted | Table 4.1, p. 27 (Panel B, causal effects) | 0.0720 | 0.0814 | 0.0094 | close |
| Publication bias, study-size weighted | Table 4.1, p. 27 (Panel B, causal effects) | 1.6980 | 1.6977 | -0.0003 | match |
| Mean beyond bias, study-size weighted | Table 4.1, p. 27 (Panel B, causal effects) | 0.0020 | 0.0022 | 0.0002 | match |

### Prokš, Petr (2026) — Temperature and Growth: A Meta-Analysis

- **Degree**: master thesis, supervised by Havránek, Tomáš
- **Repository**: [20.500.11956/206898](https://dspace.cuni.cz/handle/20.500.11956/206898)
- **Dataset**: [120534453.zip](https://dspace.cuni.cz/bitstream/handle/20.500.11956/206898/120534453.zip?sequence=4&isAllowed=y) — code published alongside

- **Sample**: 261 estimates used

| Claim | Source in thesis | Reported | artma | Diff | Verdict |
| --- | --- | ---: | ---: | ---: | --- |
| Publication bias (FAT), OLS | Table 5.1, p. 40 | -0.4440 | -0.4437 | 0.0003 | match |
| Effect beyond bias (PET), OLS | Table 5.1, p. 40 | -0.0150 | -0.0154 | -0.0004 | match |
| Publication bias (FAT), between-study | Table 5.2, p. 41 | -0.3930 | -0.3933 | -0.0003 | match |
| Effect beyond bias (PET), between-study | Table 5.2, p. 41 | -0.0220 | -0.0223 | -0.0003 | match |
| Publication bias (FAT), inverse-variance weighted | Table 5.4, p. 42 | -1.2010 | -1.2009 | 0.0001 | match |
| Effect beyond bias (PET), inverse-variance weighted | Table 5.4, p. 42 | -0.0001 | -0.0001 | -0.0000 | match |
| Publication bias (FAT), thesis's 1/se-weighted OLS | Table 5.3, p. 41 | -0.5730 | -1.2009 | -0.6279 | MISMATCH |
| Effect beyond bias (PET), thesis's 1/se-weighted OLS | Table 5.3, p. 41 | -0.0021 | -0.0001 | 0.0020 | match |

### Simpartl, Josef (2023) — Military expenditure and economic growth: A meta-analysis

- **Degree**: master thesis, supervised by Havránek, Tomáš
- **Repository**: [20.500.11956/179460](https://dspace.cuni.cz/handle/20.500.11956/179460)
- **Dataset**: [120437192.zip](https://dspace.cuni.cz/bitstream/handle/20.500.11956/179460/120437192.zip?sequence=4&isAllowed=y) — code published alongside

- **Sample**: 405 estimates used

| Claim | Source in thesis | Reported | artma | Diff | Verdict |
| --- | --- | ---: | ---: | ---: | --- |
| Publication bias, precision-weighted (WLS) | Table 1, p. 35 | 0.3160 | -0.0648 | -0.3808 | MISMATCH |
| Mean beyond bias, precision-weighted (WLS) | Table 1, p. 35 | -0.0830 | -0.0703 | 0.0127 | close |
| Publication bias, study-weighted | Table 1, p. 35 | 0.6160 | 0.6577 | 0.0417 | close |
| Mean beyond bias, study-weighted | Table 1, p. 35 | -0.1070 | -0.1178 | -0.0108 | close |
| Publication bias, fixed effects | Table 1, p. 35 | -0.1860 | 0.1257 | 0.3117 | MISMATCH |
| Mean beyond bias, fixed effects | Table 1, p. 35 | -0.0590 | -0.0680 | -0.0090 | close |
| Publication bias, between effects | Table 1, p. 35 | 0.4290 | 0.7310 | 0.3020 | MISMATCH |
| Mean beyond bias, between effects | Table 1, p. 35 | -0.0920 | -0.1260 | -0.0340 | MISMATCH |
| Publication bias, instrumental variable | Table 1, p. 35 | 0.3760 | 0.1087 | -0.2673 | MISMATCH |
| Mean beyond bias, instrumental variable | Table 1, p. 35 | -0.0880 | -0.0664 | 0.0216 | MISMATCH |

