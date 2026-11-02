# NYSE Stock Price Analysis

A financial time-series analysis project built in R, covering NYSE-listed stocks. The project processes raw historical price data, computes return metrics, evaluates volatility, and presents findings through an interactive Shiny dashboard and a reproducible R Markdown report.

The raw dataset is included as `stocks.csv`. No external API calls are needed to run the analysis.

---

## Background

Stock markets generate enormous amounts of price data every trading day. Raw prices alone tell you very little — what matters is how prices *change* over time, how much they swing, and how different stocks behave relative to each other. This project applies standard quantitative finance techniques to answer those questions using only open-source R tools.

The dataset covers historical daily OHLCV data (Open, High, Low, Close, Volume) for a selection of NYSE-listed companies. All analysis is done on the **adjusted closing price**, which accounts for stock splits and dividend payouts, making comparisons across time and across companies fair.

---

## Financial Concepts Used

### Daily Returns
The most basic measure of how a stock performs from one day to the next. Calculated as:

```
Daily Return = (Price_today - Price_yesterday) / Price_yesterday
```

This converts raw price into a percentage change, which is what you actually compare across stocks with different price levels. A stock trading at $10 and a stock trading at $500 both become directly comparable once you work in returns.

### Cumulative Returns
Compounding daily returns forward from a starting date gives cumulative return — how much a $1 investment would be worth at any point in time. This is the standard way to show long-run performance on a single chart regardless of where each stock started.

```
Cumulative Return = (1 + r1) * (1 + r2) * ... * (1 + rn) - 1
```

### Volatility
Volatility is the standard deviation of daily returns over a rolling window. High volatility means the stock swings around a lot — which means more risk and potentially more reward. It is a core input in virtually every risk model used in finance (Black-Scholes, VaR, portfolio optimization, etc.).

A 30-day rolling window is used here so you can see how a stock's risk profile changes across different market conditions rather than collapsing everything into a single number.

### Risk vs. Return
No analysis of returns is complete without pairing it with risk. This project plots each stock on a risk-return scatter (annualized return vs. annualized volatility) so you can see at a glance which stocks gave you more return per unit of risk taken — the basic idea behind the Sharpe ratio.

---

## Project Structure

```
NYSE-Stock-Price-Analysis/
│
├── stocks.csv          # Raw historical OHLCV data for NYSE stocks
├── stocks.Rmd          # Full analysis in R Markdown (narrative + code + output)
├── stocks.nb.html      # Rendered R Notebook output (open in browser)
├── stocks.html         # Standalone knitted HTML report
├── stocks.docx         # Word export of the report
├── app.R               # Interactive Shiny dashboard
├── .gitignore          # Ignores .RData, .Rhistory, .Rproj.user
└── README.md           # This file
```

---

## What the Analysis Covers

- **Data loading and cleaning** — reading the CSV, parsing dates, handling missing values, filtering to valid trading days
- **Daily return calculation** — per-stock percentage change using `dplyr` and `lag()`
- **Cumulative return calculation** — compounded growth from the start date for each stock
- **Rolling 30-day volatility** — standard deviation over a sliding window using `zoo::rollapply`
- **Trend visualization** — closing price over time, per stock, using `ggplot2`
- **Return distribution** — histogram of daily returns to see if they are normally distributed or have fat tails
- **Risk-return scatter** — annualized return vs. annualized volatility for all stocks side by side
- **Cross-stock comparison** — cumulative return lines for all stocks on one chart to compare performance

---

## Interactive Shiny Dashboard (`app.R`)

The Shiny app lets you explore the data without touching any code. It includes:

- A **stock selector** — pick one or more companies to display
- A **date range slider** — zoom into any time period in the dataset
- **Price trend chart** — adjusted closing price over time
- **Cumulative return chart** — compounded growth from your chosen start date
- **Volatility chart** — rolling 30-day standard deviation of returns
- **Summary statistics table** — mean return, volatility, min/max for the selected stocks and period

To run the dashboard locally:

```r
# Install dependencies if not already installed
install.packages(c("shiny", "ggplot2", "dplyr", "readr", "tidyr", "lubridate", "zoo"))

# Then run
shiny::runApp("app.R")
```

---

## R Libraries Used

| Library | Purpose |
|---|---|
| `readr` | Fast CSV reading |
| `dplyr` | Data manipulation (filter, mutate, group_by, summarise) |
| `tidyr` | Reshaping data between wide and long format |
| `ggplot2` | All static charts and visualizations |
| `zoo` | Rolling window calculations for volatility |
| `lubridate` | Date parsing and manipulation |
| `shiny` | Interactive web dashboard |

---

## How to Run the R Markdown Report

Open `stocks.Rmd` in RStudio and click **Knit**, or run from the console:

```r
rmarkdown::render("stocks.Rmd")
```

This will regenerate `stocks.html` with all charts and output. The pre-rendered versions (`stocks.nb.html`, `stocks.html`) are also included in the repository so you can read the report without running anything.

---

## Dataset

`stocks.csv` contains daily historical price data with the following columns:

| Column | Description |
|---|---|
| `Date` | Trading date (YYYY-MM-DD) |
| `Symbol` | Stock ticker (e.g. AAPL, MSFT) |
| `Open` | Opening price |
| `High` | Intraday high |
| `Low` | Intraday low |
| `Close` | Raw closing price |
| `Adj Close` | Dividend and split adjusted closing price |
| `Volume` | Number of shares traded |

All return and volatility calculations are based on `Adj Close`.

---

## Requirements

- R version 4.0 or higher
- RStudio (recommended for running `.Rmd` and `app.R`)
- Internet connection not required — all data is local in `stocks.csv`
