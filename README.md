# Jewellery Customer Segmentation (RFM + Clustering)

A behavioural customer segmentation model for an online jewellery retailer, built on Recency, Frequency and Monetary (RFM) features and validated using both hierarchical and K-Means clustering.

## Problem Statement

Most customers in this dataset made only a single purchase, making it easy to treat the entire customer base as one undifferentiated group. This project tests whether RFM-based clustering — a technique built mostly around high-frequency retail — can still recover meaningful behavioural segments in a low-frequency, high-value, occasion-driven category like jewellery, and whether those segments can support differentiated customer relationship strategies.

## Data

- Source: [eCommerce purchase history from jewelry store](https://www.kaggle.com/datasets/mkechinov/ecommerce-purchase-history-from-jewelry-store) (Kaggle)
- 95,911 raw transaction records, 13 fields, covering Dec 2018 – Dec 2021
- Cleaned to 32,188 customer-level RFM profiles after excluding 5,352 records with no user ID or price, and 167 records from the one non-jewellery category present in the data

## Approach

- Audited raw transaction structure first — found that a single `order_id` could span multiple rows (14,729 of 74,760 total orders were multi-row), so **Frequency was built from distinct order counts, not transaction rows**, to avoid inflating purchase frequency
- Engineered Recency, Frequency and Monetary at the customer level; applied `log1p()` transformation and Z-score standardisation to correct heavy right-skew before clustering
- Ran exploratory hierarchical clustering (Ward's linkage, reproducible 3,000-customer sample) to get an early read on cluster structure
- Fit the final K-Means model on the full standardised dataset (32,188 customers), with 50 random initialisations to stabilise the solution
- Selected the final number of clusters using converging evidence from WCSS (elbow method), average silhouette width, and the hierarchical structure — rather than any single metric

**Stack:** R · tidyverse (dplyr, tidyr, stringr, ggplot2) · lubridate · base R (`dist`, `hclust`, `kmeans`, `cutree`) · cluster · corrplot

## Results

| k | WCSS | Avg. Silhouette (K-Means) |
|---|---|---|
| 2 | 66,946.77 | 0.424 |
| **3** | **47,282.65** | **0.431** |
| 4 | 35,614.07 | 0.350 |
| 5 | 29,249.25 | 0.322 |
| 6 | 26,110.06 | 0.301 |

k = 3 gave the strongest silhouette score of any candidate solution and was confirmed by the exploratory hierarchical clustering, which also peaked at three clusters (silhouette width 0.372).

### Final Segments (n = 32,188)

| Segment | Customers | % | Median Recency | Median Frequency | Median Spend | Repeat Rate |
|---|---|---|---|---|---|---|
| **High-Value Repeat** | 3,308 | 10.3% | 204 days | 4 orders | $2,126.50 | 99.8% |
| **Recent Single-Purchase** | 5,603 | 17.4% | 17 days | 1 order | $369.40 | 21.9% |
| **Infrequent** | 23,277 | 72.3% | 280 days | 1 order | $266.99 | 17.6% |

## Key Findings

- 73.2% of all customers had made only one purchase — the customer base looks far more uniform on the surface than it actually behaves
- The High-Value Repeat segment is just 10.3% of customers but generated 35,620 orders — a disproportionate share of total transaction volume for its size
- Product category preference (rings and earrings dominate across every segment) does **not** meaningfully differ between clusters — the segments are driven by purchasing behaviour, not product taste, which rules out "different customers just like different products" as an explanation
- The largest segment (72.3% of customers, long recency, single purchase) shouldn't automatically be read as churned — in an occasion-driven category like jewellery, infrequent purchasing is a structural feature of the category, not necessarily disengagement

## Files

- `RFM_KMeans_Segmentation.R` — full pipeline: data audit, RFM feature engineering, hierarchical clustering, K-Means clustering, validation diagnostics, and segment profiling

## How to Run

1. Install R and the required packages: `tidyverse`, `lubridate`, `cluster`, `corrplot`, `scales`
2. Download `jewelry.csv` from the [Kaggle source](https://www.kaggle.com/datasets/mkechinov/ecommerce-purchase-history-from-jewelry-store) and place it in the same directory as the script
3. Run `RFM_KMeans_Segmentation.R` — it performs the full audit, clustering, and validation pipeline, printing diagnostics and cluster profiles to console
