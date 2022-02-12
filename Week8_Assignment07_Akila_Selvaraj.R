# Assignment: ASSIGNMENT 7
# Name: Selvaraj, Akila
# Date: 2010-02-14

## Set the working directory to the root of your DSC 520 directory
setwd("G:/Users/a162940/Akila/Work/R/Projects/dsc520-master")

## Load the `data/r4ds/heights.csv` to
heights_df <- read.csv("data/r4ds/heights.csv")

# Fit a linear model
earn_lm <-  lm(earn ~ height + sex + ed + age + race, data=heights_df)

# plot predicted values and actual values

plot(predict(earn_lm), heights_df$earn,
     xlab = "Predicted Values",
     ylab = "Earn") +
  abline(a = 0, b = 1, lwd=2,
         col = "green")

# View the summary of your model
summary(earn_lm)

#get the residuals
resid(earn_lm)

new_df = data.frame(age=c(40,45,50,55,60,65,70),
                    ed=c(7,8,9,10,11,13,11), race=c('black','white','hispanic','other','black','white','other'), height=c(59,60,61,62,63,64,70),
                    age=c(40,45,50,55,60,65,70), sex=c('male','female','female','male','male','female','female')
)

predicted_df <- data.frame(
  earn = predict(earn_lm, newdata = new_df),
  ed=c(7,8,9,10,11,13,11), race=c('black','white','hispanic','other','black','white','other'), height=c(59,60,61,62,63,64,70),
  age=c(40,45,50,55,60,65,70), sex=c('male','female','female','male','male','female','female')
)


## Compute deviation (i.e. residuals)
mean_earn <- mean(heights_df$earn)
## Corrected Sum of Squares Total
sst <- sum((mean_earn - heights_df$earn)^2)

## Corrected Sum of Squares for Model
ssm <-  sum((mean_earn - predicted_df$earn)^2)
## Residuals
residuals <- heights_df$earn - predicted_df$earn
## Sum of Squares for Error
sse <- sum(residuals^2)
## R Squared
r_squared <- ssm/sst

## Number of observations
n <- 1192
## Number of regression paramaters
p <- 8
## Corrected Degrees of Freedom for Model
dfm <- p - 1
## Degrees of Freedom for Error
dfe <- n-p
## Corrected Degrees of Freedom Total:   DFT = n - 1
dft <- n - 1

## Mean of Squares for Model:   MSM = SSM / DFM
msm <- ssm/dfm
## Mean of Squares for Error:   MSE = SSE / DFE
mse <- sse/dfe
## Mean of Squares Total:   MST = SST / DFT
mst <- sst/dft
## F Statistic
f_score <- msm/mse

## Adjusted R Squared R2 = 1 - (1 - R2)(n - 1) / (n - p)
adjusted_r_squared <- 1 - (1 - r_squared)*(n - 1) / (n - p)