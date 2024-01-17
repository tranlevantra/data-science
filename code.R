
library(tidyr)
library(readr)
library(dplyr)
library(magrittr)
library(tidyr)
library(lubridate)
library(mvn)

# Define the custom validator function
validate_prev <- function(x) {
  lag_values <- lag(x)
  is_valid <- is.na(lag_values) | x > lag_values
  return(is_valid)
}

impute_difference <- function(data) {
  for (i in 2:length(data)) {
    if (is.na(data[i])) {
      data[i] <- data[i-1]
    } else {
      data[i] <- data[i] - data[i-1]
    }
  }
  return(data)
}
dataset_imputed <- dataset %>%
  mutate(var2 = impute_difference(var2),
         var3 = impute_difference(var3))

check_next_greater_or_equal <- function(data) {
  is_greater_or_equal <- all(data[-1] >= data[-length(data)])
  return(is_greater_or_equal)
}

check_next_greater <- function(data) {
  is_greater <- c(data[-1] > data[-length(data)])
  return(is_greater)
}

check_max_preceding <- function(data) {
  logical_vector <- logical(length(data))  # Initialize logical vector with all elements as FALSE
  
  max_observation <- data[1]  # Initialize the maximum observation as the first element
  
  logical_vector[1] <- TRUE  # The first observation is always considered TRUE
  
  for (i in 2:length(data)) {
    if (data[i] < max_observation) {
      logical_vector[i] <- FALSE
    } else {
      logical_vector[i] <- TRUE
    }
    
    max_observation <- max(max_observation, data[i])  # Update the maximum observation to the maximum of all preceding observations
  }
  
  return(logical_vector)
}

impute_with_difference <- function(vec) {
  imputed <- vec
  total <- 0
  for (i in 1:length(vec)) {
    if (is.na(imputed[i])) {
      imputed[i] <- NA
    } else {
      imputed[i] <- imputed[i] - total
      total <- total + ifelse(is.na(imputed[i]), 0, imputed[i])
    }
  }
  return(imputed)
}


# Load the example dataset (mtcars)
data(mtcars)
# Apply the validation rule to specific variables using pipes
result <- mtcars %>%
  mutate(valid_mpg = validate_prev(mpg),
         false_count = sum(!valid_mpg, na.rm = TRUE))

# Print the validation result

# Apply the validator rule to specific variables using pipes
cases_deaths %>%
  select(cases, deaths) %>%
  validate(validator(~ validator_prev(.))) %>% View()

data_df <- data.frame(x1 = c("S", "S", "S", "M", "M", "M"), 
                      x2 = c(7, 7, 7, 1, 1, 2),
                      x3 = c(NA, NA, NA, 1, 1, 2),
                      y = c(4, 3, 2, 5, 5, 1))

data_df$x1 <- as.integer(as.factor(data_df$x1))
data_df <- na.omit(data_df)



# Read data ---------------------------------------------------------------
cases_deaths <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv") %>% rename_all(tolower)


#Check structure
as_tibble(cases_deaths)
head(us)
str(us)


# Data understanding ------------------------------------------------------

# Dataset 1: US
# location will be dropped,
# date is already in date format
# source_url will be dropped
# total_vaccinations is unecessary dropped
# people_vacciated will be dropped
# people_fully vaccinated is the key --> stay
# total boostered 
us %<>% select(date, people_fully_vaccinated)
as_tibble(us)
# This dataset is tidy becuase we wnat 


# Dataset 2: cases, deaths
# date should be removed from the data 
cases_deaths %<>% filter(date >= dmy("13-12-2020"))
# This dataset is untidy
cases_deaths$deaths %>% check_next_greater_or_equal()





false_deaths <- cases_deaths$deaths %>% check_max_preceding() %>% {which(!.)}
# Check length of those who violates the rules
false_deaths %>% length()
cases_deaths$deaths[false_deaths] <- NA
 # Now that we have imputed, these NA values will checked again after we merge two datasets
sum(is.na(cases_deaths$deaths))

false_cases <- cases_deaths$cases %>% check_max_preceding() %>% {which(!.)}
# Check length of those who violates the rules
false_cases %>% length()
cases_deaths$cases[false_cases] <- NA


# the imputation logic has been adjusted to meet both conditions. 
# If the current element is NA, it is imputed as NA. 
# If the current element is not NA, it is imputed by subtracting the total sum of all preceding non-NA values, na.rm = TRUE
temporary_cases_deaths <- cases_deaths %>% 
  mutate(deaths = impute_with_difference(deaths),
         cases = impute_with_difference(cases)) 
colSums(is.na(temporary_cases_deaths))

tidied_cases_deaths <-temporary_cases_deaths %>% 
  pivot_longer(names_to = "effect",
               values_to = "value",
               cols = 2:3)

# Merging two datasets ----------------------------------------------------

# I use left join to preserve all the values in the 
temp_join <- us %>% left_join(temporary_cases_deaths, by = 'date')


# Undestanding dataset ----------------------------------------------------

# Data description
# Explation goes here

# factorise

temp_join_factor <- temp_join %>% mutate(month = month(date, label = TRUE))
         
         
# %>% mutate(season = factor(season, levels = c("Influenza Season","Non-influenza Season" )))

# Check str/tibble and rearrange using select()
temp_join_factor %>% as.tibble()

# Searching for missing values --------------------------------------------
# complete cases (missing values and conversion)
colSums(is.na(temp_join_factor))
# since the dimension is 878 * 5 and the number of NA is roughly 5% of total dataset, I am going to eliminate them
temp_join_factor <- temp_join_factor[complete.cases(temp_join_factor),] 

temp_join_factor %>%
  filter(people_fully_vaccinated < 0 |  cases
         < 0 | deaths < 0) %>%
  summarize(
    variable2_negative = any(people_fully_vaccinated < 0),
    variable3_negative = any(cases < 0),
    variable4_negative = any(deaths < 0)
  ) %>% tibble::as.tibble()

# Check the dimension, datatype 
temp_join_factor %>% tibble::as.tibble()



# Detecting outliers ------------------------------------------------------




# Check the shape using hist of each numerical var
par(mfrow = c(3, 2))
par(mar = c(1, 1, 1, 1))
hist(candy$production, main = "Histogram of Candy Production (default breaks)")
hist(candy$production, breaks = 5, main = "Histogram of Candy Production (5 breaks)")
hist(candy$production, breaks = 10, main = "Histogram of Candy Production (10 breaks)", 
     xlab = "Candy Production", ylab = "Frequency")
hist(candy$production, breaks = 30, main = "Histogram of Candy Production (30 breaks)", 
     xlab = "Candy Production", ylab = "Frequency")
hist(candy$production, breaks = 50, main = "Histogram of Candy Production (50 breaks)", 
     xlab = "Candy Production", ylab = "Frequency")
hist(candy$production, breaks = 100, main = "Histogram of Candy Production (100 breaks)", 
     xlab = "Candy Production", ylab = "Frequency")
par(mfrow = c(1, 1))

shapiro.test()
# Conclude they re all abnormally distributed and using non-parametric method will be better (ref)

# Apply summary function to numerical variables using pipe operator
df %>% sapply(summary)


# Boxplot
car::qqPlot(temp_join_factor$people_fully_vaccinated, main = "Q-Q Plot of Mean Green")
car::qqPlot(temp_join_factor$cases, main = "Cases")
car::qqPlot(temp_join_factor$deaths, main = "deaths")


# Checking on outliers with boxplot * 3
test <- boxplot(wilt$Mean_Green)
test$out
mean_green_out2 <- boxplot(wilt$Mean_Green)[[4]]



# Look at different multivariate

#cases and seasons
biovariate <- plot(temp_join_factor$deaths, temp_join_factor$people_fully_vaccinated, xlab = "Variable X", ylab = "Variable Y", main = "Bivariate Scatter Plot")
# Conclude that we should not remove the outliers for 


# Plot for cases and vaccinated
par(mfrow = c(2, 2))  # Divide the plot area into 2x2 grid for marginal plots

# Marginal density plot for Variable X
hist(temp_join_factor$deaths, main = "Marginal Density of X", xlab = "Variable X")
rug(temp_join_factor$deaths)

# Marginal density plot for Variable Y
hist(temp_join_factor$people_fully_vaccinated, main = "Marginal Density of Y", xlab = "Variable Y")
rug(temp_join_factor$people_fully_vaccinated)

# Plot for deaths and vaccinated

# Marginal density plot for Variable X
hist(temp_join_factor$deaths, main = "Marginal Density of X", xlab = "Variable X")
rug(temp_join_factor$deaths)

# Marginal density plot for Variable Y
hist(temp_join_factor$people_fully_vaccinated, main = "Marginal Density of Y", xlab = "Variable Y")
rug(temp_join_factor$people_fully_vaccinated)
par(mfrow = c(1, 1))

# Using mmv to remove the 
# First Approach 
results <- mvn(data = ozone_sub, 
               multivariateOutlierMethod = "quan", 
               showOutliers = TRUE)

# number of 
length(results$multivariateOutliers$Outlier)


clean_dat <- mvn(ozone_sub, 
                 multivariateOutlierMethod = "quan", 
                 showNewData = TRUE)$newData

dim(original_subset)
dim(clean_subset)

#' Using capping
ozone_sub <- ozone %>% select(ozone_reading, Month, Wind_speed)
ozone_cap <- as.data.frame(ozone_sub, FUN = cap)

summary(ozone_sub)
summary(ozone_cap)

ozone_sub <- ozone[ , c(1, 4, 6)]
summary(ozone_sub)

ozone_sub_clean2 <- mvn(data = ozone_sub, 
                        multivariateOutlierMethod = "quan", 
                        showOutliers = TRUE, 
                        showNewData = TRUE)

summary(ozone_sub_clean2$newData)

# As capping will change the scale and distribution, while i want to study the relationship, hence in this case I want to  I prefer using mvn()
ozone_sub <- ozone %>% select(ozone_reading, Month, Wind_speed)
ozone_cap <- as.data.frame(ozone_sub, FUN = cap)

summary(ozone_sub)
summary(ozone_cap)

ozone_sub <- ozone[ , c(1, 4, 6)]
summary(ozone_sub)

ozone_sub_clean2 <- mvn(data = ozone_sub, 
                        multivariateOutlierMethod = "quan", 
                        showOutliers = TRUE, 
                        showNewData = TRUE)

summary(ozone_sub_clean2$newData)



# transform data --------------------------------------------------------------
# Histogram of mathematical transformation
ozone_sub <- ozone %>% 
  rename_all(tolower) %>% 
  select(ozone_reading, pressure_height, pressure_gradient, 
         visibility, inversion_temperature) 

vars <- colnames(ozone_sub)
vars <- ozone_sub %>% names()

par(mfrow = c(3, 2)) 
for(i in 1:ncol(ozone_sub)) {
  hist(unlist(ozone_sub[ , i]), main = vars[i])
  
}
par(mfrow = c(1, 1))

# Base 10 Log Transformation 
ozone_log <- sapply(ozone_sub, log10) 
par(mfrow = c(3, 2)) 
for(i in 1:ncol(ozone_log)) {
  hist(unlist(ozone_log[ , i]), main = vars[i])
  
}
par(mfrow = c(1, 1))

# Natural Log Transformation 
ozone_ln <- sapply(ozone_sub, log) 
par(mfrow = c(3, 2)) 
for(i in 1:ncol(ozone_ln)) {
  hist(unlist(ozone_ln[ , i]), main = vars[i])
  
}
par(mfrow = c(1, 1))

# Square Root Transformation 
ozone_sqrt <- sapply(ozone_sub, sqrt)
par(mfrow = c(3, 2)) 
for(i in 1:ncol(ozone_sqrt)) {
  hist(unlist(ozone_sqrt[ , i]), main = vars[i])
  
}
par(mfrow = c(1, 1))

# Box-Cox Transformation 
ozone_boxcox <- sapply(ozone_sub, BoxCox, lambda = "auto")
par(mfrow = c(3, 2)) 
for(i in 1:ncol(ozone_boxcox)) {
  hist(ozone_boxcox[ , i], main = vars[i])
  
}
par(mfrow = c(1, 1))

par(mfrow = c(3, 3)) 
hist(candy$production, breaks = 30, main = "Histogram of Candy Production")
hist(sqr_candy, main = "Histogram of Square-Transformed Candy Production", breaks = 30)
hist(cube_candy, main = "Histogram of Cube-Transformed Candy Production", breaks = 30)
hist(box_cox_candy, main = "Histogram of Box-Cox Transformation \nof Candy Production", breaks = 30)
hist(log_candy, main = "Histogram of Log Transformation \nof Candy Production", breaks = 30)
hist(ln_candy, main = "Histogram of Natural Log \nTransformation of Candy Production", breaks = 30)
hist(sqrt_candy, main = "Histogram of Square Root \nTransformation of Candy Production", breaks = 30)
hist(cubrt_candy, main = "Histogram of Cube Root \nTransformation of Candy Production", breaks = 30)
hist(rec_candy, main = "Histogram of Recipricol \nTransformation of Candy Production", breaks = 30)
par(mfrow = c(1, 1))










boxplot_vaccinate <-boxplot(temp_join_factor$people_fully_vaccinated)
# I will keep them because the amount is so large 
boxplot_vaccinate$out
boxplot_deaths <- boxplot(temp_join_factor$deaths)
boxplot_peopl <- boxplot(temp_join_factor$deaths)


par(mfrow = c(2, 2))  # Divide the plot area into 2x2 grid for marginal plots

# Marginal density plot for Variable X
hist(temp_join_factor$deaths, main = "Marginal Density of X", xlab = "Variable X")
rug(temp_join_factor$deaths)

# Marginal density plot for Variable Y
hist(temp_join_factor$people_fully_vaccinated, main = "Marginal Density of Y", xlab = "Variable Y")
rug(temp_join_factor$people_fully_vaccinated)



candy70 <- candy %>% 
  filter(observation_date >= as.Date("1970-01-01") & 
           observation_date < as.Date("1980-01-01")) %>% 
  pull(production) %>% 
  scale(center = TRUE, scale = TRUE) 

candy80 <- candy %>% 
  filter(observation_date >= as.Date("1980-01-01") & 
           observation_date < as.Date("1990-01-01")) %>% 
  pull(production) %>% 
  scale(center = TRUE, scale = FALSE) 

candy90 <- candy %>% 
  filter(observation_date >= as.Date("1990-01-01") & 
           observation_date < as.Date("2000-01-01")) %>% 
  pull(production) %>% 
  scale(center = TRUE, scale = FALSE) 

candy00 <- candy %>% 
  filter(observation_date >= as.Date("2000-01-01") & 
           observation_date < as.Date("2010-01-01")) %>% 
  pull(production) %>% 
  scale(center = TRUE, scale = FALSE) 

candy10 <- candy %>% 
  filter(observation_date >= as.Date("2010-01-01") & 
           observation_date < as.Date("2020-01-01")) %>% 
  pull(production) %>% 
  scale(center = TRUE, scale = FALSE) 

par(mfrow = c(5, 1)) 
par(mar = c(1, 1, 1, 1)) 
hist(candy70, main = "Centred Candy Production in the 1970s", xlim = c(50, 35), breaks = 10)
hist(candy80, main = "Centred Candy Production in the 1980s", xlim = c(-35, 35), breaks = 10)
hist(candy90, main = "Centred Candy Production in the 1990s", xlim = c(-35, 35), breaks = 10)
hist(candy00, main = "Centred Candy Production in the 2000s", xlim = c(-35, 35), breaks = 10)
hist(candy10, main = "Centred Candy Production in the 2010s", xlim = c(-35, 35), breaks = 10)
par(mfrow = c(1, 1)) 
par(mar = c(1, 1, 1, 1))



library(MVN)

biovariate_boxplot <- boxplot(temp_join_factor$cases ~ temp_join_factor$season)
biovariate_boxplot <- boxplot(temp_join_factor$deaths ~ temp_join_factor$people_fully_vaccinated)
biovariate_boxplot <- boxplot(temp_join_factor$deaths ~ temp_join_factor$people_fully_vaccinated)

# Subset the dataframe based on the conditions and count the rows
count <- nrow(subset(df, ID %in% c(1, 2, 3) & Married == FALSE))
# Remove observations based on the conditions
df <- subset(df, !(ID %in% c(1, 2, 3) & Married == FALSE))
