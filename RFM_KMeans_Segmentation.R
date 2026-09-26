# =========================================================
# Customer Segmentation in the Online Jewellery Industry:
# RFM Analysis, Hierarchical Clustering and K-Means Clustering
# =========================================================
# Dataset: jewelry.csv — online jewellery retail transactions
# Stage: Data Understanding & Preparation (CRISP-DM)
# Pipeline: raw data audit -> RFM feature construction ->
#           hierarchical clustering (exploratory) -> K-Means
#           (final model) -> cluster profiling
# =========================================================


# ===================================================
#          Initial Auditing of the Raw Data 
# ===================================================

# Install Packages
# install.packages("tidyverse", "lubridate")

# Open libraries
library(tidyverse)
library(lubridate)

# Import every field as text first.
# This preserves the full 19-digit IDs exactly as recorded.
jewellery_raw <- read.csv(
  "jewelry.csv",
  header = FALSE,
  colClasses = rep("character", 13),
  na.strings = c("", "NA"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Manually assign the source-data column names
names(jewellery_raw) <- c(
  "order_datetime",
  "order_id",
  "product_id",
  "quantity",
  "category_id",
  "category_alias",
  "brand_id",
  "price_usd",
  "user_id",
  "product_gender",
  "color",
  "metal",
  "gemstone"
)

# Create a typed audit copy.
# Keep IDs as character
# convert only fields that are genuinely numeric/date fields.
jewellery_audit <- jewellery_raw %>%
  mutate(
    order_datetime_parsed = ymd_hms(order_datetime, tz = "UTC", quiet = TRUE),
    quantity_num = suppressWarnings(as.numeric(quantity)),
    price_usd_num = suppressWarnings(as.numeric(price_usd))
  )

# Basic structure
cat("\nDATASET DIMENSIONS\n")
print(dim(jewellery_raw))

cat("\nSTORAGE TYPES AFTER IMPORT\n")
print(tibble(
  variable = names(jewellery_raw),
  storage_type = map_chr(jewellery_raw, typeof)
))

cat("\nMISSING VALUES IN RAW DATA\n")
missing_audit <- jewellery_raw %>%
  summarise(across(
    everything(),
    list(
      missing_n = ~ sum(is.na(.)),
      missing_pct = ~ round(mean(is.na(.)) * 100, 2)
    )
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.+)_(missing_n|missing_pct)"
  ) %>%
  arrange(desc(missing_n))

print(missing_audit)

# Check whether date, quantity, or price conversion failed
cat("\nPARSING CHECKS\n")
parse_audit <- jewellery_audit %>%
  summarise(
    invalid_datetime = sum(!is.na(order_datetime) & is.na(order_datetime_parsed)),
    invalid_quantity = sum(!is.na(quantity) & is.na(quantity_num)),
    invalid_price = sum(!is.na(price_usd) & is.na(price_usd_num))
  )

print(parse_audit)

# Verify that identifier fields contain digits only
cat("\nIDENTIFIER FORMAT CHECKS\n")
id_audit <- jewellery_raw %>%
  summarise(
    invalid_order_id = sum(!is.na(order_id) & !str_detect(order_id, "^\\d+$")),
    invalid_product_id = sum(!is.na(product_id) & !str_detect(product_id, "^\\d+$")),
    invalid_category_id = sum(!is.na(category_id) & !str_detect(category_id, "^\\d+$")),
    invalid_brand_id = sum(!is.na(brand_id) & !str_detect(brand_id, "^\\d+$")),
    invalid_user_id = sum(!is.na(user_id) & !str_detect(user_id, "^\\d+$"))
  )

print(id_audit)

# Define the fields essential for RFM customer analysis
rfm_core_fields <- c(
  "order_datetime",
  "order_id",
  "product_id",
  "quantity",
  "price_usd",
  "user_id"
)

# Establish whether core missingness occurs together
cat("\nMISSINGNESS PATTERNS IN RFM-CORE FIELDS\n")

core_missingness <- jewellery_audit %>%
  mutate(
    missing_datetime = is.na(order_datetime),
    missing_order_id = is.na(order_id),
    missing_product_id = is.na(product_id),
    missing_quantity = is.na(quantity_num),
    missing_price = is.na(price_usd_num),
    missing_user_id = is.na(user_id),
    usable_for_rfm = !if_any(
      all_of(c(
        "order_datetime",
        "order_id",
        "product_id",
        "quantity_num",
        "price_usd_num",
        "user_id"
      )),
      is.na
    )
  ) %>%
  count(
    usable_for_rfm,
    missing_datetime,
    missing_order_id,
    missing_product_id,
    missing_quantity,
    missing_price,
    missing_user_id,
    sort = TRUE
  )

print(core_missingness)

# Examine the incomplete records without deleting anything
cat("\nPROFILE OF RECORDS WITH MISSING PRICE OR USER ID\n")

incomplete_records <- jewellery_audit %>%
  filter(is.na(price_usd_num) | is.na(user_id)) %>%
  summarise(
    records = n(),
    distinct_orders = n_distinct(order_id),
    distinct_products = n_distinct(product_id),
    missing_category_id = sum(is.na(category_id)),
    missing_category_alias = sum(is.na(category_alias)),
    missing_brand_id = sum(is.na(brand_id)),
    missing_metal = sum(is.na(metal)),
    missing_color = sum(is.na(color)),
    missing_gemstone = sum(is.na(gemstone)))

print(incomplete_records)

# Check exact duplicate rows
cat("\nEXACT DUPLICATE RECORDS\n")

duplicate_summary <- jewellery_raw %>%
  summarise(
    total_rows = n(),
    duplicate_rows_excluding_first = sum(duplicated(.)),
    rows_in_duplicate_groups = sum(duplicated(.) | duplicated(., fromLast = TRUE)))

print(duplicate_summary)

# Test whether one order ID represents one row or multiple product lines
cat("\nORDER-LEVEL STRUCTURE\n")

order_level_audit <- jewellery_audit %>%
  group_by(order_id) %>%
  summarise(
    rows_per_order = n(),
    customers_per_order = n_distinct(user_id, na.rm = TRUE),
    timestamps_per_order = n_distinct(order_datetime_parsed, na.rm = TRUE),
    products_per_order = n_distinct(product_id, na.rm = TRUE),
    .groups = "drop"
  )

order_structure_summary <- order_level_audit %>%
  summarise(
    total_orders = n(),
    single_row_orders = sum(rows_per_order == 1),
    multi_row_orders = sum(rows_per_order > 1),
    maximum_rows_in_one_order = max(rows_per_order),
    orders_with_multiple_customers = sum(customers_per_order > 1),
    orders_with_multiple_timestamps = sum(timestamps_per_order > 1),
    orders_with_multiple_products = sum(products_per_order > 1)
  )

print(as.data.frame(order_structure_summary), row.names = FALSE)

# Category audit: identify jewellery vs non-jewellery records
cat("\nCATEGORY ALIAS DISTRIBUTION\n")

category_audit <- jewellery_audit %>%
  mutate(
    category_scope = case_when(
      is.na(category_alias) ~ "Missing category label",
      str_detect(category_alias, "^jewelry\\.") ~ "Jewellery category",
      TRUE ~ "Non-jewellery category"
    )
  ) %>%
  count(category_scope, category_alias, sort = TRUE)

print(as.data.frame(category_audit), row.names = FALSE)

# Quantity audit
cat("\nINVALID QUANTITY CHECK\n")

print(jewellery_audit %>%
    summarise(
      zero_quantity = sum(quantity_num == 0, na.rm = TRUE),
      negative_quantity = sum(quantity_num < 0, na.rm = TRUE),
      non_integer_quantity = sum(
        quantity_num != floor(quantity_num),
        na.rm = TRUE)) %>%
    as.data.frame(), row.names = FALSE)

# Price audit: invalid values and distribution
cat("\nPRICE VALIDITY CHECK\n")

print(
  jewellery_audit %>%
    summarise(
      missing_price = sum(is.na(price_usd_num)),
      zero_price = sum(price_usd_num == 0, na.rm = TRUE),
      negative_price = sum(price_usd_num < 0, na.rm = TRUE),
      minimum_positive_price = min(price_usd_num[price_usd_num > 0], na.rm = TRUE),
      maximum_price = max(price_usd_num, na.rm = TRUE)) %>%
    as.data.frame(), row.names = FALSE)

cat("\nPRICE DISTRIBUTION (NON-MISSING PRICES)\n")

print(
  jewellery_audit %>%
    summarise(
      mean = mean(price_usd_num, na.rm = TRUE),
      median = median(price_usd_num, na.rm = TRUE),
      sd = sd(price_usd_num, na.rm = TRUE),
      p01 = quantile(price_usd_num, 0.01, na.rm = TRUE),
      p05 = quantile(price_usd_num, 0.05, na.rm = TRUE),
      p25 = quantile(price_usd_num, 0.25, na.rm = TRUE),
      p75 = quantile(price_usd_num, 0.75, na.rm = TRUE),
      p95 = quantile(price_usd_num, 0.95, na.rm = TRUE),
      p99 = quantile(price_usd_num, 0.99, na.rm = TRUE)) %>%
    as.data.frame(), row.names = FALSE)

cat("\n10 HIGHEST-PRICED PRODUCT LINES\n")

print(
  jewellery_audit %>%
    filter(!is.na(price_usd_num)) %>%
    arrange(desc(price_usd_num)) %>%
    select(
      order_datetime, order_id, product_id,
      category_alias, quantity_num, price_usd_num, user_id) %>%
    slice_head(n = 10) %>%
    as.data.frame(), row.names = FALSE)

# Confirm date range and coverage by year
cat("\nDATE RANGE\n")

print(jewellery_audit %>%
    summarise(
      earliest_transaction = min(order_datetime_parsed, na.rm = TRUE),
      latest_transaction = max(order_datetime_parsed, na.rm = TRUE)
    ) %>%
    as.data.frame(), row.names = FALSE)

cat("\nRECORDS BY YEAR\n")

print(jewellery_audit %>%
    mutate(year = year(order_datetime_parsed)) %>%
    count(year) %>%
    arrange(year) %>%
    as.data.frame(), row.names = FALSE)

# =========================================================
#             BUILD CUSTOMER-LEVEL RFM DATASET
# =========================================================

# Keep records usable for RFM and exclude electronics
# since electronics is the one non-jewellery category
# and falls outside the analysis scope
rfm_transactions <- jewellery_audit %>%
  filter(
    !is.na(user_id),
    !is.na(price_usd_num),
    !is.na(order_datetime_parsed),
    category_alias != "electronics.clocks" | is.na(category_alias)
  )

# One day after the final recorded transaction:
# +1 day chosen so the most recent customers gets recency = 1
# rather than 0, avoiding a zero value that log transforms awkward
analysis_date <- max(rfm_transactions$order_datetime_parsed) + days(1)

# Create one RFM profile per customer
rfm <- rfm_transactions %>%
  group_by(user_id) %>%
  summarise(
    recency = as.integer(analysis_date - max(order_datetime_parsed)),
    frequency = n_distinct(order_id),
    monetary = sum(price_usd_num),
    .groups = "drop")

# Inspect the resulting dataset
cat("\nRFM DATASET SIZE\n")
print(dim(rfm))

cat("\nRFM SUMMARY\n")
print(summary(rfm))

cat("\nRFM QUANTILES\n")

print(round(sapply(rfm[c("recency", "frequency", "monetary")],
      quantile,
      probs = c(0.01, 0.25, 0.50, 0.75, 0.95, 0.99)),2))

cat("\nONE-TIME BUYERS\n")
print(rfm %>%
    summarise(
      customers = n(),
      one_time_buyers = sum(frequency == 1),
      one_time_buyer_pct = round(mean(frequency == 1) * 100, 2)))

print(rfm %>% 
        summarise(across(c(recency, frequency, monetary), sd)))

# color palette
pastel_blue <- "#A8DADC"
pastel_pink <- "#F4A6B8"
pastel_lilac <- "#CDB4DB"

# Recency distribution
ggplot(rfm, aes(recency)) +
  geom_histogram(bins = 40, fill = pastel_blue, color = "white") +
  labs(
    title = "Customer Recency Distribution",
    subtitle = "Days since each customer's most recent purchase",
    x = "Recency (days)",
    y = "Number of customers"
  ) +
  theme_minimal(base_size = 13) 

# Frequency distribution: log scale makes one-time and repeat buyers visible
ggplot(rfm, aes(frequency)) +
  geom_histogram(bins = 30, fill = pastel_pink, color = "white") +
  scale_x_log10() +
  labs(
    title = "Customer Purchase Frequency Distribution",
    subtitle = "Logarithmic x-axis used because purchase frequency is skewed",
    x = "Distinct orders (log scale)",
    y = "Number of customers"
  ) +
  theme_minimal(base_size = 13)

# Monetary distribution: log scale makes the long right tail readable
ggplot(rfm, aes(monetary)) +
  geom_histogram(bins = 40, fill = pastel_lilac, color = "white") +
  scale_x_log10(labels = scales::dollar) +
  labs(
    title = "Customer Monetary Value Distribution",
    subtitle = "Logarithmic x-axis used because customer spending is skewed",
    x = "Total historical spending (USD, log scale)",
    y = "Number of customers"
  ) +
  theme_minimal(base_size = 13)

# =====================================================
#               PREPROCESSING DIAGNOSTIC 
# =====================================================

# Simple skewness measure: values above +1 indicate notable right-skew
# motivating the log transform below.
skewness_value <- function(x) {
  mean((x - mean(x))^3) / sd(x)^3
}

preprocessing_check <- rfm %>%
  summarise(across(c(recency, frequency, monetary),
      list(
        mean = mean,
        median = median,
        skewness = skewness_value,
        minimum = min,
        maximum = max
      )))

print(as.data.frame(preprocessing_check), row.names = FALSE)

# Excess kurtosis: 0 = normal-like tails, positive = heavier tails / more
# extreme values than a normal distribution, which is expected here given
# the right-skew already observed above.
kurtosis_value <- function(x) {
  mean((x - mean(x))^4) / sd(x)^4 - 3
}

# Standard error of the mean: sd divided by sqrt
se_value <- function(x) {
  sd(x) / sqrt(length(x))
}

descriptive_stats <- rfm %>%
  summarise(
    across(
      c(recency, frequency, monetary),
      list(
        minimum = min,
        maximum = max,
        median = median,
        se = se_value,
        sd = sd,
        kurtosis = kurtosis_value,
        skewness = skewness_value,
        n = ~n()
      )
    )
  )

print(as.data.frame(descriptive_stats), row.names = FALSE)

# Log transform and standardise RFM variables
# log1p means log(1 + x), safe for positive values and any future zero values
rfm_log <- rfm %>%
  mutate(
    across(
      c(recency, frequency, monetary),
      log1p,
      .names = "log_{.col}"
    )
  )

# Standardise only the transformed RFM variables for clustering
rfm_scaled <- rfm_log %>%
  select(log_recency, log_frequency, log_monetary) %>%
  scale() %>%
  as.data.frame()

# Verify standardisation: means should be ~0 and SDs ~1
cat("\n STANDARDISATION CHECK \n")
print(round(colMeans(rfm_scaled), 3))
print(round(sapply(rfm_scaled, sd), 3))

# =========================================================
#            EXPLORATORY HIERARCHICAL CLUSTERING 
# =========================================================

# A reproducible, manageable customer sample
# because hierarchical clustering requires a full pairwise distance matrix, 
# which is computationally infeasible at ~32000 customers
set.seed(247)
hierarchical_sample <- rfm_scaled %>%
  slice_sample(n = 3000)

# Ward's method is suitable for Euclidean-distance clustering
sample_distance <- dist(hierarchical_sample)
hc <- hclust(sample_distance, method = "ward.D2")

# Dendrogram: labels removed because there are 3,000 customers
plot(hc, labels = FALSE, hang = -1, xlab = "", ylab = "Height",
     main = "Hierarchical Clustering Dendrogram",
     sub = "Ward's method on a random sample of 3,000 customers")
rect.hclust(hc, k = 3, border = c("blue4", "hotpink2", "purple"))

# Compare plausible solutions using average silhouette width
candidate_k <- 2:6

silhouette_results <- data.frame(
  clusters = candidate_k,
  average_silhouette = sapply(candidate_k, function(k) {
    groups <- cutree(hc, k = k)
    mean(cluster::silhouette(groups, sample_distance)[, "sil_width"])
  })
)

print(silhouette_results)

# diagnostic chart
ggplot(silhouette_results, aes(clusters, average_silhouette)) +
  geom_line(color = "#9B8AC4", linewidth = 1) +
  geom_point(color = "#9B8AC4", size = 3) +
  scale_x_continuous(breaks = candidate_k) +
  labs(
    title = "Hierarchical Clustering Diagnostic",
    subtitle = "Average silhouette width across candidate cluster solutions",
    x = "Number of clusters",
    y = "Average silhouette width"
  ) +
  theme_minimal(base_size = 13)

# RFM CORRELATION DIAGNOSTIC

rfm_correlation <- cor(
  rfm[c("recency", "frequency", "monetary")],
  method = "spearman"
)

print(round(rfm_correlation, 2))

corrplot::corrplot(
  rfm_correlation,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  col = colorRampPalette(c("#A8DADC", "white", "#CDB4DB"))(200)
)
# =========================================================
#            COMPARE CANDIDATE K-MEANS SOLUTIONS 
# =========================================================

set.seed(247)
k_values <- 2:6

# Full-data WCSS: measures within-cluster compactness
wcss <- sapply(k_values, function(k) {
  kmeans(rfm_scaled, centers = k, nstart = 25, iter.max = 100)$tot.withinss
})

# Sample-based silhouette: avoids a full 32,188-customer distance matrix
kmeans_silhouette <- sapply(k_values, function(k) {
  model <- kmeans(hierarchical_sample, centers = k, nstart = 25, iter.max = 100)
  mean(cluster::silhouette(model$cluster, sample_distance)[, "sil_width"])
})

k_diagnostics <- data.frame(
  clusters = k_values,
  wcss = wcss,
  average_silhouette = kmeans_silhouette
)

print(k_diagnostics)

# Elbow chart
ggplot(k_diagnostics, aes(clusters, wcss)) +
  geom_line(color = "#8EC5B3", linewidth = 1) +
  geom_point(color = "#8EC5B3", size = 3) +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Elbow Method for K-Means",
    subtitle = "Within-cluster sum of squares across candidate solutions",
    x = "Number of clusters",
    y = "WCSS"
  ) +
  theme_minimal(base_size = 13)

# Silhouette chart
ggplot(k_diagnostics, aes(clusters, average_silhouette)) +
  geom_line(color = "#E9A3B2", linewidth = 1) +
  geom_point(color = "#E9A3B2", size = 3) +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Silhouette Diagnostic for K-Means",
    subtitle = "Calculated using the 3,000-customer exploratory sample",
    x = "Number of clusters",
    y = "Average silhouette width"
  ) +
  theme_minimal(base_size = 13)

# =========================================================
#         FINAL K-MEANS MODEL AND CLUSTER PROFILES 
# =========================================================

set.seed(247)

# Final three-cluster solution on all customers
final_kmeans <- kmeans(
  rfm_scaled,
  centers = 3,
  nstart = 50,
  iter.max = 100,
)

# Attach cluster membership to the original RFM measures
rfm_results <- rfm %>%
  mutate(cluster = factor(final_kmeans$cluster))

# Cluster sizes and original-scale RFM profiles
cluster_summary <- rfm_results %>%
  group_by(cluster) %>%
  summarise(
    customers = n(),
    customer_pct = round(n() / nrow(rfm_results) * 100, 2),
    median_recency = median(recency),
    median_frequency = median(frequency),
    median_monetary = round(median(monetary), 2),
    mean_recency = round(mean(recency), 1),
    mean_frequency = round(mean(frequency), 2),
    mean_monetary = round(mean(monetary), 2),
    .groups = "drop"
  )

print(cluster_summary)

# Standardised cluster centres: shows relative behavioural differences
cluster_centres <- as.data.frame(final_kmeans$centers) %>%
  mutate(cluster = factor(row_number())) %>%
  pivot_longer(
    cols = -cluster,
    names_to = "rfm_measure",
    values_to = "standardised_value"
  )
print(as.data.frame(cluster_summary), row.names = FALSE)

ggplot(
  cluster_centres,
  aes(rfm_measure, standardised_value, fill = cluster)
) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#A8DADC", "#F4A6B8", "#CDB4DB")) +
  labs(
    title = "Behavioural Profile of Final Customer Clusters",
    subtitle = "Values are standardised: above zero indicates above-average behaviour",
    x = "",
    y = "Standardised cluster centre",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 13)

# =========================================================
#          CLUSTER SPENDING, REPEAT PURCHASE,
#          ORDER VALUE, AND CATEGORY PROFILE
# =========================================================

# Attach final cluster membership to usable transaction records
# because it re-attaches the cluster labels to transaction-level
# (not just customer-level) data so behaviour, spend, and category
# profiles can be computed per cluster.
cluster_transactions <- rfm_transactions %>%
  inner_join(
    rfm_results %>% select(user_id, cluster),
    by = "user_id"
  )

# Aggregate product lines into orders for order-value analysis
order_values <- cluster_transactions %>%
  group_by(cluster, user_id, order_id) %>%
  summarise(order_value = sum(price_usd_num), .groups = "drop")

# Spending, repeat-purchase behaviour, and order value
cluster_behaviour <- rfm_results %>%
  group_by(cluster) %>%
  summarise(
    customers = n(),
    repeat_customer_pct = round(mean(frequency > 1) * 100, 2),
    median_customer_spend = round(median(monetary), 2),
    .groups = "drop"
  ) %>%
  left_join(
    order_values %>%
      group_by(cluster) %>%
      summarise(
        orders = n(),
        median_order_value = round(median(order_value), 2),
        mean_order_value = round(mean(order_value), 2),
        .groups = "drop"
      ),
    by = "cluster"
  )

print(as.data.frame(cluster_behaviour), row.names = FALSE)

# Category preferences: percentages are among labelled product lines only
category_profile <- cluster_transactions %>%
  filter(!is.na(category_alias)) %>%
  count(cluster, category_alias, name = "product_lines") %>%
  group_by(cluster) %>%
  mutate(category_share_pct = round(product_lines / sum(product_lines) * 100, 2)) %>%
  ungroup() %>%
  arrange(cluster, desc(category_share_pct))

print(as.data.frame(category_profile), row.names = FALSE)

# category-preference chart
ggplot(category_profile, aes(category_alias, category_share_pct, fill = cluster)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#A8DADC", "#F4A6B8", "#CDB4DB")) +
  labs(
    title = "Product Category Preferences by Customer Cluster",
    subtitle = "Share of labelled product lines within each cluster",
    x = "",
    y = "Share of product lines (%)",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# =========================================================
#        MATERIAL AND PRODUCT-ORIENTATION PROFILES
# =========================================================

# Shows how much usable information exists for each attribute
attribute_coverage <- cluster_transactions %>%
  group_by(cluster) %>%
  summarise(
    metal_available_pct = round(mean(!is.na(metal)) * 100, 2),
    gemstone_available_pct = round(mean(!is.na(gemstone)) * 100, 2),
    gender_available_pct = round(mean(!is.na(product_gender)) * 100, 2),
    .groups = "drop"
  )

print(as.data.frame(attribute_coverage), row.names = FALSE)

# Reusable profile table for a product attribute
preference_profile <- function(variable) {
  cluster_transactions %>%
    filter(!is.na(.data[[variable]])) %>%
    count(cluster, preference = .data[[variable]], name = "product_lines") %>%
    group_by(cluster) %>%
    mutate(share_pct = round(product_lines / sum(product_lines) * 100, 2)) %>%
    ungroup() %>%
    arrange(cluster, desc(share_pct))
}

cluster_transactions <- cluster_transactions %>%
  mutate(
    product_orientation = case_when(
      product_gender == "f" ~ "Female-oriented",
      product_gender == "m" ~ "Male-oriented",
      TRUE ~ "Unisex"
    )
  )

metal_profile <- preference_profile("metal")
gemstone_profile <- preference_profile("gemstone")
orientation_profile <- preference_profile("product_orientation")

cat("\n METAL PREFERENCES \n")
print(as.data.frame(metal_profile), row.names = FALSE)

cat("\n GEMSTONE PREFERENCES \n")
print(as.data.frame(gemstone_profile), row.names = FALSE)

cat("\n PRODUCT-GENDER LABELS \n")
print(as.data.frame(orientation_profile), row.names = FALSE)

# =========================================================
# End of clustering pipeline.
# Outputs: rfm_results (customer-level RFM + cluster labels),
# cluster_summary, cluster_behaviour, category/metal/gemstone/
# orientation profiles — used for cluster interpretation and
# CRM recommendations in the Discussion.
# =========================================================

