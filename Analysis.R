# Step 1: Create a subset of the data (without extra_info)
df_subset <- data_raw[, c("Transaction_ID", "User_ID", "Transaction_Date",
                        "Amount", "Category", "Payment_Method",
                        "Merchant_Name", "Location", "Income_Monthly",
                        "Savings_Contribution", "Debt_or_Credit_Usage",
                        "Balance_After_Transaction", "Transaction_Notes")]

# Step 2: Display entries where Merchant_Name is blank
blank_merchants <- df_subset[df_subset$Merchant_Name == "" | is.na(df_subset$Merchant_Name), ]
print("Entries with blank Merchant_Name:")
print(blank_merchants)
cat("\nTotal entries with blank Merchant_Name:", nrow(blank_merchants), "\n")

# Step 3: Remove entries with blank Merchant_Name
df_cleaned <- df_subset[df_subset$Merchant_Name != "" & !is.na(df_subset$Merchant_Name), ]
cat("\nAfter removing blank Merchant_Name:")
cat("\nOriginal rows:", nrow(df_subset), "\n")
cat("Rows after cleaning:", nrow(df_cleaned), "\n")

# Step 4: Display entries where Location is blank
blank_location <- df_cleaned[df_cleaned$Location == "" | is.na(df_cleaned$Location), ]
print("\nEntries with blank Location:")
print(blank_location)
cat("\nTotal entries with blank Location:", nrow(blank_location), "\n")

# Step 5: Remove entries with blank Location
df_final <- df_cleaned[df_cleaned$Location != "" & !is.na(df_cleaned$Location), ]

# Step 6: Change Yes/No/y/n to True/False in Debt_or_Credit_Usage
df_final$Debt_or_Credit_Usage <- ifelse(df_final$Debt_or_Credit_Usage == "Yes", TRUE,
                                         ifelse(df_final$Debt_or_Credit_Usage == "No", FALSE,
                                                ifelse(tolower(trimws(df_final$Debt_or_Credit_Usage)) == "y", TRUE,
                                                       ifelse(tolower(trimws(df_final$Debt_or_Credit_Usage)) == "n", FALSE,
                                                              df_final$Debt_or_Credit_Usage))))

# Step 7: Display entries that aren't TRUE or FALSE
invalid_debt <- df_final[!(df_final$Debt_or_Credit_Usage %in% c(TRUE, FALSE)) |
                           is.na(df_final$Debt_or_Credit_Usage), ]
print("\n=== Entries with invalid Debt_or_Credit_Usage (not TRUE/FALSE) ===")
print(invalid_debt)
cat("\nTotal invalid entries:", nrow(invalid_debt), "\n")

# Step 8: Remove entries that aren't TRUE or FALSE
df_final <- df_final[df_final$Debt_or_Credit_Usage %in% c(TRUE, FALSE) &
                       !is.na(df_final$Debt_or_Credit_Usage), ]

# Show the final values
cat("\n=== Debt_or_Credit_Usage values after cleaning ===\n")
print(table(df_final$Debt_or_Credit_Usage))

# Final Summary
cat("\n=== FINAL SUMMARY ===\n")
cat("Original rows:", nrow(df_subset), "\n")
cat("After removing blank Merchant_Name:", nrow(df_cleaned), "\n")
cat("After removing blank Location & invalid Debt_or_Credit_Usage:", nrow(df_final), "\n")
cat("Total rows removed:", nrow(df_subset) - nrow(df_final), "\n")

# View the cleaned data
head(df_final)

# Check how many rows were removed
cat("Original rows:", nrow(data_raw), "\n")
cat("Cleaned rows:", nrow(df_final), "\n")
cat("Rows removed:", nrow(data_raw) - nrow(df_final), "\n")

# ===== ANALYSIS: TOTAL SPENDINGS =====
cat("\n\n========================================\n")
cat("        SPENDING ANALYSIS\n")
cat("========================================\n\n")

# Calculate Total Spendings
total_spending <- sum(df_final$Amount, na.rm = TRUE)

cat("Total Spendings: $", format(total_spending, big.mark = ",", nsmall = 2), "\n\n")

# Additional useful metrics
cat("--- Additional Spending Metrics ---\n")
cat("Average Transaction Amount: $", format(mean(df_final$Amount, na.rm = TRUE), nsmall = 2), "\n")
cat("Median Transaction Amount: $", format(median(df_final$Amount, na.rm = TRUE), nsmall = 2), "\n")
cat("Minimum Transaction: $", format(min(df_final$Amount, na.rm = TRUE), nsmall = 2), "\n")
cat("Maximum Transaction: $", format(max(df_final$Amount, na.rm = TRUE), nsmall = 2), "\n")
cat("Total Number of Transactions:", nrow(df_final), "\n")

# ===== ANALYSIS: TRANSACTIONS BY CATEGORY =====
cat("\n\n========================================\n")
cat("    TRANSACTIONS BY CATEGORY\n")
cat("========================================\n\n")

# Aggregate spending by category
library(dplyr)

category_analysis <- df_final %>%
  group_by(Category) %>%
  summarise(
    Total_Spending = sum(Amount, na.rm = TRUE),
    Number_of_Transactions = n(),
    Average_Transaction = mean(Amount, na.rm = TRUE),
    Min_Transaction = min(Amount, na.rm = TRUE),
    Max_Transaction = max(Amount, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Spending))

# Calculate percentage of total spending
category_analysis$Percentage <- (category_analysis$Total_Spending / total_spending) * 100

# Display the results
cat("Summary by Category:\n")
cat("-------------------\n\n")

for(i in 1:nrow(category_analysis)) {
  cat(sprintf("Category: %s\n", category_analysis$Category[i]))
  cat(sprintf("  Total Spending: $%s (%.1f%% of total)\n", 
              format(category_analysis$Total_Spending[i], big.mark = ",", nsmall = 2),
              category_analysis$Percentage[i]))
  cat(sprintf("  Number of Transactions: %d\n", category_analysis$Number_of_Transactions[i]))
  cat(sprintf("  Average Transaction: $%s\n", 
              format(category_analysis$Average_Transaction[i], nsmall = 2)))
  cat(sprintf("  Min Transaction: $%s\n", 
              format(category_analysis$Min_Transaction[i], nsmall = 2)))
  cat(sprintf("  Max Transaction: $%s\n\n", 
              format(category_analysis$Max_Transaction[i], nsmall = 2)))
}

# Display as a formatted table
cat("\n--- Category Summary Table ---\n\n")
print(category_analysis, n = Inf)

# Create a simple bar chart visualization
cat("\n--- Spending Distribution by Category (Bar Chart) ---\n\n")
barplot(category_analysis$Total_Spending, 
        names.arg = category_analysis$Category,
        las = 2,  # Rotate labels
        col = rainbow(nrow(category_analysis)),
        main = "Total Spending by Category",
        ylab = "Total Spending ($)",
        cex.names = 0.8)

# Pie chart for category distribution
cat("\n--- Category Distribution (Pie Chart) ---\n\n")
pie(category_analysis$Total_Spending,
    labels = paste(category_analysis$Category, 
                   "\n$", format(category_analysis$Total_Spending, big.mark = ","),
                   "\n(", round(category_analysis$Percentage, 1), "%)", sep = ""),
    main = "Spending Distribution by Category",
    col = rainbow(nrow(category_analysis)))

# ===== ANALYSIS: PAYMENT METHOD TRENDS =====
cat("\n\n========================================\n")
cat("    PAYMENT METHOD ANALYSIS\n")
cat("========================================\n\n")

# First, let's check the current format of Transaction_Date
cat("Sample Transaction_Date values:\n")
print(head(df_final$Transaction_Date, 10))
cat("\nTransaction_Date class:", class(df_final$Transaction_Date), "\n\n")

# Try to convert Transaction_Date to Date format with multiple format attempts
library(lubridate)

# Function to parse dates with multiple format attempts
parse_date_flexible <- function(date_vector) {
  # Try common date formats
  result <- suppressWarnings(as.Date(date_vector, format = "%Y-%m-%d"))
  if(all(is.na(result))) {
    result <- suppressWarnings(as.Date(date_vector, format = "%m/%d/%Y"))
  }
  if(all(is.na(result))) {
    result <- suppressWarnings(as.Date(date_vector, format = "%d/%m/%Y"))
  }
  if(all(is.na(result))) {
    result <- suppressWarnings(mdy(date_vector))
  }
  if(all(is.na(result))) {
    result <- suppressWarnings(dmy(date_vector))
  }
  if(all(is.na(result))) {
    result <- suppressWarnings(ymd(date_vector))
  }
  return(result)
}

# Convert Transaction_Date
df_final$Transaction_Date <- parse_date_flexible(df_final$Transaction_Date)

# Check for any dates that couldn't be parsed
cat("Dates successfully parsed:", sum(!is.na(df_final$Transaction_Date)), "\n")
cat("Dates failed to parse:", sum(is.na(df_final$Transaction_Date)), "\n\n")

# Remove rows with unparseable dates
df_final <- df_final[!is.na(df_final$Transaction_Date), ]

cat("Rows after removing invalid dates:", nrow(df_final), "\n\n")

# Extract month and year for trend analysis
df_final$Month <- format(df_final$Transaction_Date, "%Y-%m")
df_final$Year <- format(df_final$Transaction_Date, "%Y")

# Overall Payment Method Summary
payment_summary <- df_final %>%
  group_by(Payment_Method) %>%
  summarise(
    Total_Spending = sum(Amount, na.rm = TRUE),
    Number_of_Transactions = n(),
    Average_Transaction = mean(Amount, na.rm = TRUE),
    Percentage_of_Total = (sum(Amount, na.rm = TRUE) / total_spending) * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Spending))

cat("--- Overall Payment Method Summary ---\n\n")
for(i in 1:nrow(payment_summary)) {
  cat(sprintf("Payment Method: %s\n", payment_summary$Payment_Method[i]))
  cat(sprintf("  Total Spending: $%s (%.1f%% of total)\n", 
              format(payment_summary$Total_Spending[i], big.mark = ",", nsmall = 2),
              payment_summary$Percentage_of_Total[i]))
  cat(sprintf("  Number of Transactions: %d (%.1f%% of all transactions)\n", 
              payment_summary$Number_of_Transactions[i],
              (payment_summary$Number_of_Transactions[i] / nrow(df_final)) * 100))
  cat(sprintf("  Average Transaction: $%s\n\n", 
              format(payment_summary$Average_Transaction[i], nsmall = 2)))
}

# Payment Method Trend Over Time (Monthly)
payment_trend_monthly <- df_final %>%
  group_by(Month, Payment_Method) %>%
  summarise(
    Total_Spending = sum(Amount, na.rm = TRUE),
    Number_of_Transactions = n(),
    .groups = 'drop'
  ) %>%
  arrange(Month, desc(Total_Spending))

cat("\n--- Monthly Payment Method Trend ---\n\n")
print(payment_trend_monthly, n = Inf)

# Payment Method Usage Frequency Over Time
payment_frequency <- df_final %>%
  group_by(Month, Payment_Method) %>%
  summarise(Transaction_Count = n(), .groups = 'drop') %>%
  arrange(Month, desc(Transaction_Count))

# Visualizations

# 1. Bar Chart: Overall Payment Method Distribution
cat("\n--- Payment Method Distribution (Bar Chart) ---\n\n")
par(mar = c(8, 5, 4, 2))
barplot(payment_summary$Number_of_Transactions, 
        names.arg = payment_summary$Payment_Method,
        las = 2,
        col = rainbow(nrow(payment_summary)),
        main = "Number of Transactions by Payment Method",
        ylab = "Number of Transactions",
        cex.names = 0.9)

# 2. Pie Chart: Payment Method by Spending
cat("\n--- Payment Method Spending Distribution (Pie Chart) ---\n\n")
pie(payment_summary$Total_Spending,
    labels = paste(payment_summary$Payment_Method, 
                   "\n$", format(payment_summary$Total_Spending, big.mark = ","),
                   "\n(", round(payment_summary$Percentage_of_Total, 1), "%)", sep = ""),
    main = "Spending Distribution by Payment Method",
    col = rainbow(nrow(payment_summary)))

# 3. Line Chart: Payment Method Trend Over Time
cat("\n--- Payment Method Usage Trend Over Time (Line Chart) ---\n\n")
library(reshape2)

# Prepare data for line chart
trend_wide <- dcast(payment_frequency, Month ~ Payment_Method, value.var = "Transaction_Count", fill = 0)

# Plot
months <- trend_wide$Month
payment_methods <- names(trend_wide)[-1]

if(length(payment_methods) > 0 && nrow(trend_wide) > 0) {
  plot(1:length(months), trend_wide[[payment_methods[1]]], 
       type = "l", col = 1, lwd = 2,
       xlab = "Month", ylab = "Number of Transactions",
       main = "Payment Method Usage Trend Over Time",
       xaxt = "n", ylim = c(0, max(trend_wide[,-1], na.rm = TRUE) * 1.1))
  
  axis(1, at = 1:length(months), labels = months, las = 2, cex.axis = 0.7)
  
  if(length(payment_methods) > 1) {
    for(i in 2:length(payment_methods)) {
      lines(1:length(months), trend_wide[[payment_methods[i]]], col = i, lwd = 2)
    }
  }
  
  legend("topleft", legend = payment_methods, col = 1:length(payment_methods), 
         lwd = 2, cex = 0.8)
}

# 4. Growth/Decline Analysis
cat("\n\n--- Payment Method Growth Analysis ---\n\n")

# Compare first month vs last month
first_month <- min(df_final$Month)
last_month <- max(df_final$Month)

first_month_data <- df_final %>%
  filter(Month == first_month) %>%
  group_by(Payment_Method) %>%
  summarise(Count_First = n(), .groups = 'drop')

last_month_data <- df_final %>%
  filter(Month == last_month) %>%
  group_by(Payment_Method) %>%
  summarise(Count_Last = n(), .groups = 'drop')

growth_analysis <- merge(first_month_data, last_month_data, by = "Payment_Method", all = TRUE)
growth_analysis[is.na(growth_analysis)] <- 0
growth_analysis$Growth <- growth_analysis$Count_Last - growth_analysis$Count_First
growth_analysis$Growth_Percentage <- ifelse(growth_analysis$Count_First > 0,
                                            (growth_analysis$Growth / growth_analysis$Count_First) * 100,
                                            NA)

cat(sprintf("Period: %s to %s\n\n", first_month, last_month))
for(i in 1:nrow(growth_analysis)) {
  cat(sprintf("Payment Method: %s\n", growth_analysis$Payment_Method[i]))
  cat(sprintf("  First Month (%s): %d transactions\n", first_month, growth_analysis$Count_First[i]))
  cat(sprintf("  Last Month (%s): %d transactions\n", last_month, growth_analysis$Count_Last[i]))
  cat(sprintf("  Change: %+d transactions", growth_analysis$Growth[i]))
  if(!is.na(growth_analysis$Growth_Percentage[i])) {
    cat(sprintf(" (%+.1f%%)\n\n", growth_analysis$Growth_Percentage[i]))
  } else {
    cat(" (New payment method)\n\n")
  }
}

cat("\n========================================\n")

# ===== ANALYSIS: LOCATION-BASED SPENDING =====
cat("\n\n========================================\n")
cat("    LOCATION-BASED SPENDING ANALYSIS\n")
cat("========================================\n\n")

# Overall Location Summary
location_summary <- df_final %>%
  group_by(Location) %>%
  summarise(
    Total_Spending = sum(Amount, na.rm = TRUE),
    Number_of_Transactions = n(),
    Average_Transaction = mean(Amount, na.rm = TRUE),
    Median_Transaction = median(Amount, na.rm = TRUE),
    Min_Transaction = min(Amount, na.rm = TRUE),
    Max_Transaction = max(Amount, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Spending))

# Calculate percentage of total spending
location_summary$Percentage_of_Total <- (location_summary$Total_Spending / total_spending) * 100
location_summary$Percentage_of_Transactions <- (location_summary$Number_of_Transactions / nrow(df_final)) * 100

cat("--- Overall Location Summary ---\n\n")
cat(sprintf("Total Unique Locations: %d\n\n", nrow(location_summary)))

for(i in 1:nrow(location_summary)) {
  cat(sprintf("Location: %s\n", location_summary$Location[i]))
  cat(sprintf("  Total Spending: $%s (%.1f%% of total)\n", 
              format(location_summary$Total_Spending[i], big.mark = ",", nsmall = 2),
              location_summary$Percentage_of_Total[i]))
  cat(sprintf("  Number of Transactions: %d (%.1f%% of all transactions)\n", 
              location_summary$Number_of_Transactions[i],
              location_summary$Percentage_of_Transactions[i]))
  cat(sprintf("  Average Transaction: $%s\n", 
              format(location_summary$Average_Transaction[i], nsmall = 2)))
  cat(sprintf("  Median Transaction: $%s\n", 
              format(location_summary$Median_Transaction[i], nsmall = 2)))
  cat(sprintf("  Min Transaction: $%s\n", 
              format(location_summary$Min_Transaction[i], nsmall = 2)))
  cat(sprintf("  Max Transaction: $%s\n\n", 
              format(location_summary$Max_Transaction[i], nsmall = 2)))
}

# Top 10 Locations by Spending
cat("\n--- Top 10 Locations by Total Spending ---\n\n")
top_10_locations <- head(location_summary, 10)
print(top_10_locations, n = 10)

# Location by Category Analysis
cat("\n\n--- Spending by Location and Category ---\n\n")
location_category <- df_final %>%
  group_by(Location, Category) %>%
  summarise(
    Total_Spending = sum(Amount, na.rm = TRUE),
    Number_of_Transactions = n(),
    .groups = 'drop'
  ) %>%
  arrange(Location, desc(Total_Spending))

print(location_category, n = Inf)

# Most Popular Category per Location
cat("\n\n--- Most Popular Category by Location ---\n\n")
top_category_by_location <- location_category %>%
  group_by(Location) %>%
  slice_max(Total_Spending, n = 1) %>%
  ungroup() %>%
  arrange(desc(Total_Spending))

for(i in 1:nrow(top_category_by_location)) {
  cat(sprintf("Location: %s\n", top_category_by_location$Location[i]))
  cat(sprintf("  Top Category: %s\n", top_category_by_location$Category[i]))
  cat(sprintf("  Spending in this category: $%s\n", 
              format(top_category_by_location$Total_Spending[i], big.mark = ",", nsmall = 2)))
  cat(sprintf("  Transactions: %d\n\n", top_category_by_location$Number_of_Transactions[i]))
}

# Location Spending Over Time
cat("\n--- Location Spending Trend Over Time ---\n\n")
location_time_trend <- df_final %>%
  group_by(Month, Location) %>%
  summarise(
    Total_Spending = sum(Amount, na.rm = TRUE),
    Number_of_Transactions = n(),
    .groups = 'drop'
  ) %>%
  arrange(Month, desc(Total_Spending))

print(location_time_trend, n = Inf)

# Visualizations

# 1. Bar Chart: Top 15 Locations by Spending
cat("\n--- Top 15 Locations by Spending (Bar Chart) ---\n\n")
top_15_locations <- head(location_summary, 15)
par(mar = c(10, 5, 4, 2))
barplot(top_15_locations$Total_Spending, 
        names.arg = top_15_locations$Location,
        las = 2,
        col = rainbow(nrow(top_15_locations)),
        main = "Top 15 Locations by Total Spending",
        ylab = "Total Spending ($)",
        cex.names = 0.7)

# 2. Bar Chart: Top 15 Locations by Transaction Count
cat("\n--- Top 15 Locations by Transaction Count (Bar Chart) ---\n\n")
par(mar = c(10, 5, 4, 2))
barplot(top_15_locations$Number_of_Transactions, 
        names.arg = top_15_locations$Location,
        las = 2,
        col = heat.colors(nrow(top_15_locations)),
        main = "Top 15 Locations by Number of Transactions",
        ylab = "Number of Transactions",
        cex.names = 0.7)

# 3. Pie Chart: Top 10 Locations Spending Distribution
cat("\n--- Top 10 Locations Spending Distribution (Pie Chart) ---\n\n")
top_10 <- head(location_summary, 10)
# Calculate "Others" category
others_spending <- sum(location_summary$Total_Spending) - sum(top_10$Total_Spending)
if(others_spending > 0) {
  pie_data <- c(top_10$Total_Spending, others_spending)
  pie_labels <- c(paste(top_10$Location, "\n$", format(top_10$Total_Spending, big.mark = ","), 
                        "\n(", round(top_10$Percentage_of_Total, 1), "%)", sep = ""),
                  paste("Others\n$", format(others_spending, big.mark = ","), sep = ""))
  pie(pie_data, labels = pie_labels, 
      main = "Spending Distribution by Location (Top 10 + Others)",
      col = rainbow(length(pie_data)),
      cex = 0.7)
} else {
  pie(top_10$Total_Spending,
      labels = paste(top_10$Location, "\n$", format(top_10$Total_Spending, big.mark = ","),
                     "\n(", round(top_10$Percentage_of_Total, 1), "%)", sep = ""),
      main = "Spending Distribution by Location (Top 10)",
      col = rainbow(nrow(top_10)),
      cex = 0.7)
}

# 4. Comparison: Average Transaction Size by Location (Top 15)
cat("\n--- Average Transaction Size by Location (Top 15) ---\n\n")
par(mar = c(10, 5, 4, 2))
barplot(top_15_locations$Average_Transaction, 
        names.arg = top_15_locations$Location,
        las = 2,
        col = terrain.colors(nrow(top_15_locations)),
        main = "Average Transaction Size by Location (Top 15)",
        ylab = "Average Transaction Amount ($)",
        cex.names = 0.7)

# 5. Location Growth Analysis
cat("\n\n--- Location Spending Growth Analysis ---\n\n")

first_month_loc <- df_final %>%
  filter(Month == first_month) %>%
  group_by(Location) %>%
  summarise(Spending_First = sum(Amount, na.rm = TRUE), 
            Count_First = n(),
            .groups = 'drop')

last_month_loc <- df_final %>%
  filter(Month == last_month) %>%
  group_by(Location) %>%
  summarise(Spending_Last = sum(Amount, na.rm = TRUE),
            Count_Last = n(),
            .groups = 'drop')

location_growth <- merge(first_month_loc, last_month_loc, by = "Location", all = TRUE)
location_growth[is.na(location_growth)] <- 0
location_growth$Spending_Growth <- location_growth$Spending_Last - location_growth$Spending_First
location_growth$Spending_Growth_Pct <- ifelse(location_growth$Spending_First > 0,
                                               (location_growth$Spending_Growth / location_growth$Spending_First) * 100,
                                               NA)
location_growth$Transaction_Growth <- location_growth$Count_Last - location_growth$Count_First

location_growth <- location_growth %>%
  arrange(desc(abs(Spending_Growth)))

cat(sprintf("Period: %s to %s\n\n", first_month, last_month))
cat("Top 10 Locations with Biggest Changes:\n\n")

top_growth <- head(location_growth, 10)
for(i in 1:nrow(top_growth)) {
  cat(sprintf("Location: %s\n", top_growth$Location[i]))
  cat(sprintf("  First Month Spending: $%s (%d transactions)\n", 
              format(top_growth$Spending_First[i], big.mark = ",", nsmall = 2),
              top_growth$Count_First[i]))
  cat(sprintf("  Last Month Spending: $%s (%d transactions)\n", 
              format(top_growth$Spending_Last[i], big.mark = ",", nsmall = 2),
              top_growth$Count_Last[i]))
  cat(sprintf("  Spending Change: $%+.2f", top_growth$Spending_Growth[i]))
  if(!is.na(top_growth$Spending_Growth_Pct[i])) {
    cat(sprintf(" (%+.1f%%)\n", top_growth$Spending_Growth_Pct[i]))
  } else {
    cat(" (New location)\n")
  }
  cat(sprintf("  Transaction Change: %+d\n\n", top_growth$Transaction_Growth[i]))
}

cat("\n========================================\n")
