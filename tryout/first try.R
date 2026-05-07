library(readxl)
library(dplyr)

# ── 1. DOWNLOAD APPLE DATA ─────────────────────────────────────────────────────────────────────────────────────────────────

# just set the working directory
getwd()
setwd("C:/Users/willi/OneDrive/Uni St.Gallen/Lectures/2nd Year Bachelor/2nd Semester/Stochasitc Method in Finance/Assignment")
data <- read_excel("AAPL.xlsx")

# simply clean the environment so everyone get a clean slate
rm(list = ls())

# ── 2. CLEAN & FORMAT ──────────────────────────────────────────────────────────────────────────────────────────────

# Convert date to Date format
data$Date <- as.Date(data$Date)

# Rename AAPL.US to Price
colnames(data)[colnames(data) == "AAPL.US"] <- "Price"

# Convert Price to numeric (in case it was read as character)
data$Price <- as.numeric(data$Price)

# Sort ascending (oldest first)
data <- data[order(data$Date), ]

# Filter to 2020 onwards
data <- data[data$Date >= as.Date("2020-01-01"), ]

# Drop unnecessary columns (keep only Date and Price)
data <- data[, c("Date", "Price")]

# Remove any rows with NA
print(count(is.na))
data <- na.omit(data)

# ── 3. VERIFY ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
cat("Date range:  ", format(min(data$Date)), "to", format(max(data$Date)), "\n")
cat("Observations:", nrow(data), "\n")
cat("Date class:  ", class(data$Date), "\n")
cat("Price class: ", class(data$Price), "\n")
cat("Any NAs:     ", anyNA(data), "\n")

head(data)
tail(data)


# ── Given Parameters ────────────────────────────────────────────────────────────────────────────────
n <- 25           #number of periods
r <- 0.01         #interest rate per annum
S0 <- 267.6100    #Close value on the 27th April 2026 
p <- 0.5          #probability of a up or down
T <- 6/12         #anualise the maturity
delta_t <- T/n    #go from discrete to continuous Random walk to Brownian motion



# ── Computations of parameters ──────────────────────────────────────────────────────────────────────────────────────────────────────────

#Daily log returns 
returns <- diff(log(data$Price)) #Calculate the daily log returns, slide 

sigma_daily <- sd(returns, na.rm = TRUE) #Calculate the standard deviation, remove na values

sigma <- sigma_daily * sqrt(250) #calculate the annualized volatility, given in assignment

#Up and down factors 
u <- exp(sigma * sqrt(delta_t))  #page 2
d <- exp(-sigma * sqrt(delta_t))
q <- (1+r-d)/(u-d)


# ── The binomial tree ────────────────────────────────────────────────────────────────────────────────────────────────────────────

# Initialize price tree
stock_tree <- matrix(0, nrow = n + 1, ncol = n + 1)

for (i in 0:n) {        # i = time step
  for (j in 0:i) {      # j = number of up moves
    stock_tree[i - j + 1, i + 1] <- S0 * u^j * d^(i - j)
  }
}

print(stock_tree)
#middle node at maturity [13;25] = S0

# All non-zero terminal nodes
terminal <- stock_tree[stock_tree[, n + 1] != 0, n + 1]

cat("# of terminal nodes:", length(terminal), "\n")
cat("Maximum price:           ", max(terminal), "\n")
cat("Minimum price:           ", min(terminal), "\n")
cat("Middle node (~S0):       ", median(terminal), "\n")

# All terminal prices
cat("\nAll terminal prices:\n")
print(round(terminal, 2))



# ── Compute the average stock price for each terminal node of the tree ────────────────────────────────────────────────────────────────────────

compute_terminal_node_averages <- function(S0, u, d) {
  num_nodes <- n + 1
  
  # Initialize vectors to accumulate sums and counts
  path_sums <- numeric(num_nodes)
  path_counts <- numeric(num_nodes)
  
  # Total number of paths = 2^25
  total_paths <- 2^n
  
  for (i in 0:(total_paths - 1)) {
    bits <- as.integer(intToBits(i))[1:n]
    up_moves <- sum(bits)
    
    prices <- numeric(num_nodes)
    prices[1] <- S0
    
    # Traverse the paths
    for (j in 1:n) {
      prices[j + 1] <- if (bits[j] == 1) prices[j] * u else prices[j] * d
    }
    
    avg_price <- mean(prices)
    path_sums[up_moves + 1] <- path_sums[up_moves + 1] + avg_price
    path_counts[up_moves + 1] <- path_counts[up_moves + 1] + 1
  }
  
  # Compute the final averages
  result <- path_sums / path_counts
  return(result)
}

# We should then compute the vector: 
averages <- compute_terminal_node_averages(S0, u, d)


#Compute the payoff of the Asian option for each terminal node
