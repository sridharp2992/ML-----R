# Select out data of interest:
d <- mtcars %>% select(mpg, hp, wt, disp)

## Check out 6 sigma library in R
## Check out https://www.r-bloggers.com/visualizing-residuals 

## https://www.r-bloggers.com/visualizing-residuals/
  
## Truly decouple data from visualization - D3

fit <- lm(mpg ~ hp, data = mtcars)  # Fit the model
summary(fit)  # Report the results

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section

d <- mtcars
fit <- lm(mpg ~ hp, data = d)
d$predicted <- predict(fit)   # Save the predicted values
d$residuals <- residuals(fit) # Save the residual values
# Quick look at the actual, predicted, and residual values
library(dplyr)
d %>% select(mpg, predicted, residuals) %>% head()

library(ggplot2)
ggplot(d, aes(x = hp, y = mpg)) +  # Set up canvas with outcome variable on y-axis
  geom_point()  # Plot the actual points

ggplot(d, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)  # Add the predicted values

ggplot(d, aes(x = hp, y = mpg)) +
  geom_segment(aes(xend = hp, yend = predicted)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)

library(ggplot2)
ggplot(d, aes(x = hp, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = hp, yend = predicted), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()  # Add theme for cleaner look


# ALPHA
# Changing alpha of actual values based on absolute value of residuals
ggplot(d, aes(x = hp, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = hp, yend = predicted), alpha = .2) +
  
  # > Alpha adjustments made here...
  geom_point(aes(alpha = abs(residuals))) +  # Alpha mapped to abs(residuals)
  guides(alpha = FALSE) +  # Alpha legend removed
  # <
  
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

# COLOR
# High residuals (in abolsute terms) made more red on actual values.
ggplot(d, aes(x = hp, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = hp, yend = predicted), alpha = .2) +
  
  # > Color adjustments made here...
  geom_point(aes(color = abs(residuals))) + # Color mapped to abs(residuals)
  scale_color_continuous(low = "black", high = "red") +  # Colors to use here
  guides(color = FALSE) +  # Color legend removed
  # <
  
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

# SIZE AND COLOR
# Same coloring as above, size corresponding as well
ggplot(d, aes(x = hp, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = hp, yend = predicted), alpha = .2) +
  
  # > Color AND size adjustments made here...
  geom_point(aes(color = abs(residuals), size = abs(residuals))) + # size also mapped
  scale_color_continuous(low = "black", high = "red") +
  guides(color = FALSE, size = FALSE) +  # Size legend also removed
  # <
  
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

# COLOR UNDER/OVER
# Color mapped to residual with sign taken into account.
# i.e., whether actual value is greater or less than predicted
ggplot(d, aes(x = hp, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = hp, yend = predicted), alpha = .2) +
  
  # > Color adjustments made here...
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +  # Colors to use here
  guides(color = FALSE) +
  # <
  
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

