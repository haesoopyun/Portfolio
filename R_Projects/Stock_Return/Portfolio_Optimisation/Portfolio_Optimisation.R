packages <- c("ROI", "ROI.plugin.quadprog", "ROI.plugin.glpk", 
              "ROI.plugin.symphony", "dplyr", "PerformanceAnalytics", 
              "broom", "ggplot2", "tidyr", "lubridate", "tibble", 
              "tidyquant", "xts", "zoo", "PortfolioAnalytics")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

if(!("MyFunctions" %in% installed.packages()[, "Package"])) {
  devtools::install_github("thezeming/MyFunctions", upgrade = "never")
}
library(MyFunctions)

DATA <- CRSP.MERGE(
  freq = 'm',  # Monthly data
  START_date = '20150101',  # Start from January 2015
  END_date = format(Sys.Date(), '20200101'),  # Fetch up to 01/01/2020
  vars = c(
    'permno', # Unique stock identifier
    'date', # Monthly date of the record
    'prc', # Stock price (to analyse price trends)
    'ret', # Stock return (to measure performance)
    'vol', # Trading volume (to analyse liquidity)
    'shrout', # Shares outstanding (for market capitalisation calculation)
    'exchcd', # Exchange code (to identify listing exchange)
    'shrcd', # Security type (to filter common shares)
    'siccd', # Industry classification (for sector analysis)
    'comnam' # Company name (to identify stock issuer)
  ),
  usnm = "",  # WRDS username 
  pwd = "",  # WRDS password
  pgpass = FALSE
) |> 
  drop_na(prc, ret, vol, shrout) |>  # Ensure critical fields have no missing values
  filter(exchcd %in% c(1, 2, 3), shrcd %in% c(10, 11))  # Keep only listed common shares

# Define assigned PERMNOs
assigned_permnos <- c(89393, 14358, 17034)

# Filter the CRSP data to keep only assigned stocks
CRSP_Assigned <- DATA |> filter(permno %in% assigned_permnos)

# Save Assigned Stocks Data
write.csv(CRSP_Assigned, "CRSP_Assigned.csv", row.names = FALSE)

# Task 1 Performance Overview

# 1.1 Stock Exchange

# Ensure `company_name` is present
if (!"company_name" %in% colnames(CRSP_Assigned)) {
  CRSP_Assigned <- CRSP_Assigned |> mutate(company_name = comnam)  # Copy `comnam` as `company_name`
}

# Identify stock exchange for each assigned stock
exchange_mapping <- c("1" = "NYSE", "2" = "AMEX", "3" = "NASDAQ")

CRSP_Assigned <- CRSP_Assigned |> 
  mutate(exchange = recode(as.character(exchcd), !!!exchange_mapping))

# Print stock exchange information
print("Assigned Stocks and Their Exchange Listings:")
print(CRSP_Assigned |> select(permno, company_name, exchange) |> distinct())

# Save stock exchange data to CSV
write.csv(CRSP_Assigned |> select(permno, company_name, exchange) |> distinct(), 
          "Stock_Exchange_Info.csv", row.names = FALSE)

# 1.2 Sector Information 

# Print SIC codes in dataset
print(CRSP_Assigned |> select(permno, company_name, siccd) |> distinct())

# Manually assign industry sectors based on actual SIC Codes in dataset
CRSP_Assigned <- CRSP_Assigned |> 
  mutate(Industry_Sector = case_when(
    siccd == 7841  ~ "Motion Picture Distribution",  # Netflix (Correct)
    siccd == 7997  ~ "Oil & Gas Drilling & Exploration",  # Torchlight Energy (Reclassified)
    siccd == 9999  ~ "Software & Technology Services",  # Altair Engineering (Reclassified)
    TRUE ~ "Unknown"
  ))

# Print Updated Data
print("Updated Assigned Stocks with Corrected Industry Sectors:")
print(CRSP_Assigned |> select(permno, company_name, siccd, Industry_Sector) |> distinct())

# Save to CSV
write.csv(CRSP_Assigned |> select(permno, company_name, siccd, Industry_Sector) |> distinct(), 
          "Stock_Sector_Info.csv", row.names = FALSE)

# 1.3 Company Identification

# Ensure `company_name` exists in dataset
if (!"company_name" %in% colnames(CRSP_Assigned)) {
  CRSP_Assigned <- CRSP_Assigned |> mutate(company_name = comnam)  # Copy `comnam` to `company_name`
}

# Print Assigned Companies Using 'company_name' Field
print("Company Identification:")
print(CRSP_Assigned |> select(permno, company_name) |> distinct())

# 1.4 Performance Metrics

# Compute market capitalisation and organise data
CRSP_Assigned <- CRSP_Assigned |>
  mutate(
    MktCap = abs(prc) * shrout / 1000,  # Market Cap in millions
    Year = year(date),
    Month = month(date)
  ) |>
  arrange(permno, date) |>  # Ensure sorting
  group_by(permno) |>  
  mutate(MktCap_lag = lag(MktCap, default = first(MktCap))) |>  # Handle missing values
  ungroup()

# Display summaries
print("Head of Processed CRSP Data:")
print(head(CRSP_Assigned))

print("Summary of Market Capitalisation:")
summary(CRSP_Assigned$MktCap)

print("Summary of Lagged Market Capitalisation:")
summary(CRSP_Assigned$MktCap_lag)

# Save processed data
write.csv(CRSP_Assigned, "Processed_CRSP_Data.csv", row.names = FALSE)

# Save summary statistics separately
summary_stats <- CRSP_Assigned |>
  summarise(
    Min_MktCap = min(MktCap, na.rm = TRUE),
    Max_MktCap = max(MktCap, na.rm = TRUE),
    Mean_MktCap = mean(MktCap, na.rm = TRUE),
    Min_Lagged_MktCap = min(MktCap_lag, na.rm = TRUE),
    Max_Lagged_MktCap = max(MktCap_lag, na.rm = TRUE),
    Mean_Lagged_MktCap = mean(MktCap_lag, na.rm = TRUE)
  )

write.csv(summary_stats, "MarketCap_Summary.csv", row.names = FALSE)

# Market capitalisation visualisation

# Line Chart: Market Capitalisation Over Time
market_cap_plot <- ggplot(CRSP_Assigned, aes(x = date, y = MktCap, color = factor(permno))) +
  geom_line(size = 1) +
  ggtitle("Market Capitalisation Over Time") +
  xlab("Date") +
  ylab("Market Cap (in millions)") +
  theme_minimal() +
  scale_color_discrete(name = "Stock PERMNO")

# Save and show market capitalization trend
ggsave("MarketCap_Trend.png", plot = market_cap_plot, width = 8, height = 6)
print(market_cap_plot)  # Show in RStudio Viewer

# Histogram: Market Capitalisation Distribution
market_cap_hist <- ggplot(CRSP_Assigned, aes(x = MktCap)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  ggtitle("Market Capitalisation Distribution") +
  xlab("Market Cap (in millions)") +
  ylab("Frequency") +
  theme_minimal()

# Save and show histogram
ggsave("MarketCap_Histogram.png", plot = market_cap_hist, width = 8, height = 6)
print(market_cap_hist)  # Show in RStudio Viewer

# Compute performance metrics
Performance_Metrics <- CRSP_Assigned |> 
  group_by(permno) |> 
  summarise(
    Avg_Return = mean(ret, na.rm = TRUE),   # Average monthly return
    Volatility = sd(ret, na.rm = TRUE),     # Standard deviation of returns
    Avg_Volume = mean(vol, na.rm = TRUE),   # Average trading volume
    Market_Cap_Avg = mean(MktCap, na.rm = TRUE),  # Average market cap in millions
    Sharpe_Ratio = ifelse(Volatility > 0, Avg_Return / Volatility, NA)  # Risk-adjusted return
  )

# Save and display performance metrics
write.csv(Performance_Metrics, "Performance_Metrics.csv", row.names = FALSE)
print(Performance_Metrics)

# Visualisation: Stock Performance Metrics

# Bar Plot for Average Returns
avg_return_plot <- ggplot(Performance_Metrics, aes(x = factor(permno), y = Avg_Return, fill = factor(permno))) +
  geom_bar(stat = "identity") +
  ggtitle("Average Monthly Return per Stock") +
  xlab("Stock PERMNO") +
  ylab("Average Monthly Return") +
  theme_minimal() +
  theme(legend.position = "none")

# Save and display
ggsave("Avg_Returns_BarPlot.png", plot = avg_return_plot, width = 8, height = 6)
print(avg_return_plot)

# Bar Plot for Volatility
volatility_plot <- ggplot(Performance_Metrics, aes(x = factor(permno), y = Volatility, fill = factor(permno))) +
  geom_bar(stat = "identity") +
  ggtitle("Stock Return Volatility") +
  xlab("Stock PERMNO") +
  ylab("Standard Deviation of Returns") +
  theme_minimal() +
  theme(legend.position = "none")

# Save and display
ggsave("Volatility_BarPlot.png", plot = volatility_plot, width = 8, height = 6)
print(volatility_plot)

# Bar Plot for Sharpe Ratio
sharpe_plot <- ggplot(Performance_Metrics, aes(x = factor(permno), y = Sharpe_Ratio, fill = factor(permno))) +
  geom_bar(stat = "identity") +
  ggtitle("Sharpe Ratio of Stocks") +
  xlab("Stock PERMNO") +
  ylab("Sharpe Ratio") +
  theme_minimal() +
  theme(legend.position = "none")

# Save and display
ggsave("Sharpe_Ratio_BarPlot.png", plot = sharpe_plot, width = 8, height = 6)
print(sharpe_plot)

# Task 2

# 2.1 Performance Portfolio

# Compute equally weighted portfolio returns
EW_Portfolio <- CRSP_Assigned |> 
  group_by(date) |> 
  summarise(Portfolio_Return = mean(ret, na.rm = TRUE)) |> 
  mutate(Cumulative_Return = cumprod(1 + Portfolio_Return) - 1)  # Total Return

# Compute value-weighted portfolio Returns
VW_Portfolio <- CRSP_Assigned |> 
  group_by(date) |> 
  summarise(Portfolio_Return = weighted.mean(ret, MktCap_lag, na.rm = TRUE)) |> 
  filter(!is.na(Portfolio_Return)) |> 
  mutate(Cumulative_Return = cumprod(1 + Portfolio_Return) - 1)  # Total Return

# Compute performance metrics for each portfolio
Portfolio_Performance <- tibble(
  Portfolio = c("Equal-Weighted", "Value-Weighted"),
  Mean_Return = c(mean(EW_Portfolio$Portfolio_Return, na.rm = TRUE),
                  mean(VW_Portfolio$Portfolio_Return, na.rm = TRUE)),
  Volatility = c(sd(EW_Portfolio$Portfolio_Return, na.rm = TRUE),
                 sd(VW_Portfolio$Portfolio_Return, na.rm = TRUE))
)

# Compute sharpe ratio (Assume risk-free rate = 0 for simplicity)
Portfolio_Performance <- Portfolio_Performance |>  
  mutate(Sharpe_Ratio = Mean_Return / Volatility)  # Risk-adjusted return

# Print and save performance metrics
print(Portfolio_Performance)
write.csv(Portfolio_Performance, "Portfolio_Performance.csv", row.names = FALSE)

# Visualisation: Portfolio Performance

# Compute Cumulative Returns
EW_CumRet <- EW_Portfolio |> mutate(CumRet = cumprod(1 + Portfolio_Return))
VW_CumRet <- VW_Portfolio |> mutate(CumRet = cumprod(1 + Portfolio_Return))

# Equally Weighted Portfolio Plot
ew_plot <- ggplot(EW_CumRet, aes(x = date, y = CumRet)) +
  geom_line(color = "blue", linewidth = 1) + 
  ggtitle("Equally Weighted Portfolio Performance") +
  xlab("Date") + ylab("Cumulative Return") +
  theme_minimal()

ggsave("EW_Portfolio_Performance.png", plot = ew_plot, width = 8, height = 6)
print(ew_plot)

# Value Weighted Portfolio Plot
vw_plot <- ggplot(VW_CumRet, aes(x = date, y = CumRet)) +
  geom_line(color = "red", linewidth = 1) + 
  ggtitle("Value-Weighted Portfolio Performance") +
  xlab("Date") + ylab("Cumulative Return") +
  theme_minimal()

ggsave("VW_Portfolio_Performance.png", plot = vw_plot, width = 8, height = 6)
print(vw_plot)

# Combined Portfolio Performance Plot
combined_plot <- ggplot() +
  geom_line(data = EW_CumRet, aes(x = date, y = CumRet), color = "blue", linewidth = 1, linetype = "dashed") +
  geom_line(data = VW_CumRet, aes(x = date, y = CumRet), color = "red", linewidth = 1, linetype = "solid") +
  ggtitle("Comparison of Portfolio Performance") +
  xlab("Date") + ylab("Cumulative Return") +
  theme_minimal() +
  labs(caption = "Blue (Dashed): EW Portfolio | Red (Solid): VW Portfolio")

ggsave("Portfolio_Comparison.png", plot = combined_plot, width = 8, height = 6)
print(combined_plot)

# Adding Market Benchmarking (S&P 500 & Sector ETFs)

# Retrieve S&P 500 (GSPC) and Sector ETF data from Yahoo Finance
benchmark_data <- tq_get(c("^GSPC", "XLK", "XLE", "XLF"),  # S&P 500, Tech, Energy, Finance ETFs
                         from = "2015-01-01", to = "2020-01-01", 
                         periodicity = "monthly")

# Calculate monthly returns
benchmark_data <- benchmark_data %>%
  group_by(symbol) %>%
  mutate(Return = adjusted / lag(adjusted) - 1) %>%
  na.omit()

# Plot Portfolio vs. S&P 500 Performance
portfolio_vs_sp500_plot <- ggplot() +
  geom_line(data = EW_Portfolio, aes(x = date, y = Cumulative_Return, color = "Portfolio"), size = 1) +
  geom_line(data = filter(benchmark_data, symbol == "^GSPC"), aes(x = date, y = Return, color = "S&P 500"), size = 1, linetype = "dashed") +
  ggtitle("Portfolio vs. S&P 500 Performance") +
  xlab("Date") + ylab("Cumulative Return") +
  theme_minimal()

# Save graph
ggsave("Portfolio_vs_SP500.png", plot = portfolio_vs_sp500_plot, width = 8, height = 6)

# Rolling Volatility Analysis

# Convert returns to xts format
returns_xts <- xts(EW_Portfolio$Portfolio_Return, order.by = EW_Portfolio$date)

# Compute rolling 12-month volatility
rolling_volatility <- rollapply(returns_xts, width = 12, FUN = sd, by.column = TRUE, align = "right") * sqrt(12)

# Plot Rolling Volatility
plot(rolling_volatility, main = "Rolling 12-Month Portfolio Volatility", col = "blue", lwd = 2, ylab = "Volatility")

# Save graph
png("Rolling_Volatility_Analysis.png", width = 800, height = 600)
plot(rolling_volatility, main = "Rolling 12-Month Portfolio Volatility", col = "blue", lwd = 2, ylab = "Volatility")
dev.off()

# CAPM & Beta Calculation

# Adjust EW_Portfolio dates to first day of the month
EW_Portfolio <- EW_Portfolio %>%
  mutate(date = as.Date(format(date, "%Y-%m-01")))

# Check the new date range
range(EW_Portfolio$date)

# Merge Portfolio Returns with Market Returns (S&P 500)
capm_data <- left_join(
  EW_Portfolio %>% select(date, Portfolio_Return),
  benchmark_data %>% filter(symbol == "^GSPC") %>% select(date, Return),
  by = "date"
)

# Remove NA values after merging
capm_data <- na.omit(capm_data)

# Save and print CAPM regression output
if(nrow(capm_data) > 0) {
  capm_model <- lm(Portfolio_Return ~ Return, data = capm_data)
  print(summary(capm_model))
  capture.output(summary(capm_model), file = "CAPM_Regression_Results.txt")
} else {
  print("No matching data available for regression. Check date formatting.")
}

# Task 2.2: Alternative Strategy (Mean-Variance Portfolio Optimisation)

# Reshape for data optimisation
Portfolio_Data <- CRSP_Assigned |> 
  select(date, permno, ret) |>  # Selecting relevant columns: date, PERMNO, and returns (ret)
  pivot_wider(names_from = permno, values_from = ret) |>  # Pivoting the data to create a wide format with PERMNO as columns
  mutate(date = as.Date(date)) |>  # Convert `date` column to Date format
  column_to_rownames(var = "date") |>  # Move `date` to row names (needed for time-series conversion)
  as.xts() |>  # Convert the dataframe into an xts object for time-series analysis
  na.omit()  # Remove any rows with missing values to ensure clean data

# Check dimensions and preview dataset
print(dim(Portfolio_Data))  
print(head(Portfolio_Data))

# Define portfolio optimisation model
pspec <- portfolio.spec(assets = colnames(Portfolio_Data)) |>  
  add.constraint(type = "full_investment") |>  # Ensure that all funds are invested
  add.constraint(type = "box", min = 0, max = 1) |>  # No short selling constraint (weights must be between 0 and 1)
  add.objective(type = "risk", name = "StdDev") |>  # Minimise portfolio risk (Standard Deviation)
  add.objective(type = "return", name = "mean")  # Maximise portfolio return

# Optimise portfolio
optimised_portfolio <- optimize.portfolio(
  R = Portfolio_Data,  # Input return data
  portfolio = pspec,  # Portfolio specification
  optimize_method = "ROI"  # Use the ROI method for optimisation
)

# Ensure portfolio weights sum to 1
print(sum(optimised_portfolio$weights))

# Save Optimised Portfolio Weights
write.csv(optimised_portfolio$weights, "Optimised_Portfolio_Weights.csv", row.names = FALSE)

# Compute optimised portfolio returns
Optimised_Returns <- Return.portfolio(
  Portfolio_Data,  # Apply optimised weights to the portfolio
  weights = optimised_portfolio$weights
)

# Convert optimised returns into a tibble format for plotting
Optimised_Returns_Tibble <- as_tibble(Optimised_Returns, rownames = "date") |>  
  mutate(date = as.Date(date), CumRet = cumprod(1 + portfolio.returns))  # Compute cumulative return

# Visualisation: Optimised Portfolio Performance
optimized_plot <- ggplot(Optimised_Returns_Tibble, aes(x = date, y = CumRet)) +
  geom_line(color = "green") +  # Green line represents the optimised portfolio performance
  ggtitle("Optimised Portfolio Performance") +
  xlab("Date") + ylab("Cumulative Return") +
  theme_minimal()

# Save the plot as a PNG file
ggsave("Optimised_Portfolio_Performance.png", plot = optimized_plot, width = 8, height = 6)


# Compare Portfolio Performance

# Compute cumulative returns for different portfolio types
EW_CumRet <- EW_Portfolio |> mutate(CumRet = cumprod(1 + Portfolio_Return))  # Equal-weighted portfolio
VW_CumRet <- VW_Portfolio |> mutate(CumRet = cumprod(1 + Portfolio_Return))  # Value-weighted portfolio
Optimised_CumRet <- Optimised_Returns_Tibble |> mutate(CumRet = cumprod(1 + portfolio.returns))  # Optimised portfolio

# Plot all three portfolio performances together for comparison
comparison_plot <- ggplot() +
  geom_line(data = EW_CumRet, aes(x = date, y = CumRet), color = "blue", linewidth = 1, linetype = "dashed") +  # Equal-weighted portfolio (dashed blue line)
  geom_line(data = VW_CumRet, aes(x = date, y = CumRet), color = "red", linewidth = 1, linetype = "dotted") +  # Value-weighted portfolio (dotted red line)
  geom_line(data = Optimised_CumRet, aes(x = date, y = CumRet), color = "green", linewidth = 1) +  # Optimised portfolio (solid green line)
  ggtitle("Comparison of Portfolio Performance") +
  xlab("Date") + ylab("Cumulative Return") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(caption = "Blue: EW Portfolio, Red: VW Portfolio, Green: Optimised Portfolio")

# Display the plot in RStudio Viewer
print(comparison_plot)

# Save the plot as a PNG file
ggsave("3_Portfolios_Comparison.png", plot = comparison_plot, width = 8, height = 6)