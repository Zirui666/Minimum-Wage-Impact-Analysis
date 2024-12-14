##################################################
# ECON 418-518 Exam 3
# Zirui Yan
# The University of Arizona
# ziruiyan@arizona.edu 
# 13 December 2024
###################################################

#####################
# Preliminaries
#####################

# Clear environment, console, and plot pane
rm(list = ls())
cat("\014")
graphics.off()

# Turn off scientific notation
options(scipen = 999)

# Load required libraries
if (!require(pacman)) install.packages("pacman")
pacman::p_load(caret, dplyr,  glmnet, data.table, ISLR2, randomForest)

# Set sead
set.seed(418518)



#####################
# Problem 3
#####################

# Load the dataset
data <- read.csv("~/Econ 418/ECON_418-518_Exam_3_Data.csv")
data <- as.data.table(data)
#Overview the data set
glimpse(data)

#################
# Question (ii)
#################

# Create indicators
data[, is_nov := ifelse(time_period == "Nov", 1, 0)]
data[, is_nj := ifelse(state == 1, 1, 0)]

# Check the updated data structure
head(data)

# Compute the mean total employment for each state and time period
mean_employment <- data[, .(mean_total_emp = mean(total_emp, na.rm = TRUE)), by = .(state, time_period)]
mean_employment


#################
# Question (iii)
#################

# Calculate differences for New Jersey and Pennsylvania 
diff_nj <- mean_employment[state == 1 & time_period == "Nov", mean_total_emp] - 
  mean_employment[state == 1 & time_period == "Feb", mean_total_emp]

diff_pa <- mean_employment[state == 0 & time_period == "Nov", mean_total_emp] - 
  mean_employment[state == 0 & time_period == "Feb", mean_total_emp]

# Calculate the Difference-in-Differences
did_estimate <- diff_nj - diff_pa
print(paste("Difference-in-Differences Estimate:", did_estimate))


# Create a summary dataset for plotting
summary_data <- data.frame(
  state = c("Pennsylvania", "Pennsylvania", "New Jersey", "New Jersey"),
  time_period = c("Feb", "Nov", "Feb", "Nov"),
  mean_total_emp = c(
    mean_employment[state == 0 & time_period == "Feb", mean_total_emp],
    mean_employment[state == 0 & time_period == "Nov", mean_total_emp],
    mean_employment[state == 1 & time_period == "Feb", mean_total_emp],
    mean_employment[state == 1 & time_period == "Nov", mean_total_emp]
  )
)

# Plot the Difference-in-Differences
library(ggplot2)
ggplot(summary_data, aes(x = time_period, y = mean_total_emp, group = state, color = state)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Difference-in-Differences Plot",
    x = "Time Period",
    y = "Mean Total Employment",
    color = "State"
  ) +
  theme_minimal()

#################
# Question (iv)
#################

# Create a new indicator variable for whether the time period is November
data[, is_nov := as.integer(time_period == "Nov")]

# Estimate the model using lm()
did_model <- lm(total_emp ~ state + is_nov + state*is_nov, data = data)

# Display the summary of the model
summary(did_model)

# Extract ATT estimate and standard error from the model
att_coef <- coef(summary(did_model))["state:is_nov", "Estimate"]
att_se <- coef(summary(did_model))["state:is_nov", "Std. Error"]

#Compute Critical Value (t)
t_critical <- qt(0.975, df = 764)
t_critical

# Compute the confidence interval
lower_bound <- att_coef - t_critical * att_se
upper_bound <- att_coef + t_critical * att_se
lower_bound
upper_bound

#################
# Question (iv)
#################

# Estimate the model with restaurant fixed effects
did_model_fe <- lm(total_emp ~ state + is_nov + state * is_nov + restaurant_id, data = data)

# Display the summary of the new model
summary(did_model_fe)


