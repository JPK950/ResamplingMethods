# Bootstrap and Jackknife Estimations
set.seed(123)
data <- rnorm(30, mean = 5, sd = 2)

######### Part 3 #########

# Jackknife bias for mean
jack_means <- sapply(1:length(data), function(i) mean(data[-i]))
jack_means
jack_bias <- (length(data) - 1) * (mean(jack_means) - mean(data))
jack_bias

# Bootstrap bias for mean
B <- 1000
boot_means <- replicate(B, mean(sample(data, replace = TRUE)))
boot_means
boot_bias <- mean(boot_means) - mean(data)
boot_bias

n <- length(data)
jackknife_var <- (n - 1) / n * sum((jack_means - mean(jack_means))^2)
jackknife_var  

boot_means <- replicate(1000, mean(sample(data, replace = TRUE)))
bootstrap_var <- var(boot_means)
bootstrap_var


hist(boot_means, col = "skyblue", main = "Bootstrap Distribution")
hist(jack_means, col = "pink", main = "Jackknife Distribution")

######### Part 4 ###########

###4.2
# Simulate normal data
set.seed(123)
data <- rnorm(30, mean = 0, sd = 1)

# Bootstrap for mean
boot_means <- replicate(1000, mean(sample(data, replace = TRUE)))
boot_var <- var(boot_means)

# Jackknife for mean
jack_means <- sapply(1:30, function(i) mean(data[-i]))
jack_var <- (29/30) * sum((jack_means - mean(jack_means))^2)

mean(data)          # true sample mean
mean(boot_means)    # bootstrap estimate of the mean
boot_var            # bootstrap variance estimate
mean(jack_means)    # jackknife estimate of the mean
jack_var            # jackknife variance estimate

hist(boot_means, col = "skyblue", main = "Bootstrap Sample Means")


###4.3

# Simulate exponential data
set.seed(456)
data <- rexp(30, rate = 1)

# Bootstrap for median
boot_medians <- replicate(1000, median(sample(data, replace = TRUE)))
boot_var_median <- var(boot_medians)
boot_ci_median <- quantile(boot_medians, probs = c(0.025, 0.975))

# Jackknife for median
jack_medians <- sapply(1:30, function(i) median(data[-i]))
jack_var_median <- (29/30) * sum((jack_medians - mean(jack_medians))^2)
jack_bias_median <- (29) * (mean(jack_medians) - median(data))

# Print results
median(data)                         # True sample median
mean(boot_medians)                  # Bootstrap estimate of the median
boot_var_median                     # Bootstrap variance
boot_ci_median                      # Bootstrap 95% CI

mean(jack_medians)                  # Jackknife estimate of the median
jack_var_median                     # Jackknife variance
jack_bias_median                    # Jackknife bias

### 4.4 Regression

# Simulate data
set.seed(789)
n <- 100
X <- runif(n, 0, 10)

# Homoscedastic case
epsilon_homo <- rnorm(n, mean = 0, sd = 1)
Y_homo <- 2 + 0.5 * X + epsilon_homo  # True β1 = 0.5

# Heteroscedastic case
epsilon_hetero <- rnorm(n, mean = 0, sd = sqrt(1 + 0.5 * X^2))
Y_hetero <- 2 + 0.5 * X + epsilon_hetero

# Function to estimate β1 from linear regression
get_beta1 <- function(Y, X) {
  coef(lm(Y ~ X))[2]
}

# Jackknife estimate for β1
jackknife_beta1 <- function(Y, X) {
  n <- length(Y)
  estimates <- sapply(1:n, function(i) get_beta1(Y[-i], X[-i]))
  beta_jack <- mean(estimates)
  var_jack <- (n - 1) / n * sum((estimates - beta_jack)^2)
  list(estimate = beta_jack, variance = var_jack)
}

# Bootstrap estimate for β1
bootstrap_beta1 <- function(Y, X, B = 1000) {
  n <- length(Y)
  estimates <- replicate(B, {
    idx <- sample(1:n, replace = TRUE)
    get_beta1(Y[idx], X[idx])
  })
  list(estimate = mean(estimates), variance = var(estimates))
}

# Run for homoscedastic
jack_homo <- jackknife_beta1(Y_homo, X)
boot_homo <- bootstrap_beta1(Y_homo, X)

# Run for heteroscedastic
jack_hetero <- jackknife_beta1(Y_hetero, X)
boot_hetero <- bootstrap_beta1(Y_hetero, X)

# Output results
cat("Homoscedastic Model:\n")
cat("  Jackknife β1:", round(jack_homo$estimate, 4), "| Variance:", round(jack_homo$variance, 5), "\n")
cat("  Bootstrap β1:", round(boot_homo$estimate, 4), "| Variance:", round(boot_homo$variance, 5), "\n\n")

cat("Heteroscedastic Model:\n")
cat("  Jackknife β1:", round(jack_hetero$estimate, 4), "| Variance:", round(jack_hetero$variance, 5), "\n")
cat("  Bootstrap β1:", round(boot_hetero$estimate, 4), "| Variance:", round(boot_hetero$variance, 5), "\n")



