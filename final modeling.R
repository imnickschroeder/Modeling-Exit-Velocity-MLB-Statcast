# This script will build and test 5 types of statistical models: full linear model, forward selection lm,
# backward selection lm, ridge regression, and the lasso

# We will build the models using 65% of the data and then test the model performance on the other 35% of the data

# Question of interest: How well can we predict how fast the ball is hit with new pitching variables from Statcast data?

# Data, run the clean trout for modeling script and then write it over
Trout_model <- Trout_model
summary(Trout_model)

# Let's look at our y-variable, hit speed
hist(Trout_model$hit_speed,breaks=25)
library(MASS)
boxcox(lm(hit_speed~.,data=Trout_model)) # I think this is okay!

# Set up test and train data
set.seed(1738)
library(dplyr)
train.trout <- sample_frac(Trout_model,0.65)
heeyy <- as.numeric(rownames(train.trout))
test.trout <- Trout_model[-heeyy,]

#### Full linear model ####
# fit on train
full.lm <- lm(hit_speed~.,data=train.trout)
summary(full.lm)
# predict on test
full.lm.predictions <- predict.lm(full.lm,newdata=test.trout)

#### Forward selection ####
fwd.step <- step(full.lm,direction="forward",data=train.trout)
summary(fwd.step)
# predict on test
fwd.step.predictions <- predict.lm(fwd.step,newdata=test.trout)

#### Backward selection ####
bwd.step <- step(full.lm,direction="backward",data=train.trout)
summary(bwd.step)
# predict on test
bwd.step.predictions <- predict.lm(bwd.step,newdata=test.trout)

#### Ridge regression ####
# set up a grid of lambda values
grid=10^seq(-2,10,length=100)
# Set up x and y for glmnet function
x=model.matrix(hit_speed~.,data=train.trout)[,-23]
y=train.trout$hit_speed

# Fit model
library(glmnet)
ridge.mod <- glmnet(x,y,alpha=0,lambda=grid)

# Use 10-fold cv to select the best value of lambda
cv.out <- cv.glmnet(x,y,alpha=0,lambda = grid)
plot(cv.out)
bestlam <- cv.out$lambda.min

# make predictions on test data
x.test=model.matrix(hit_speed~.,data=test.trout)[,-23]
y.test=test.trout$hit_speed
ridge.predictions <- predict.glmnet(ridge.mod,newx=x.test,s=bestlam)

test.trout$ridge.predictions <- ridge.predictions


#### Lasso ####
# Fit model
lasso.mod <- glmnet(x,y,alpha=1,lambda=grid)

cv.out.lasso <- cv.glmnet(x,y,alpha=1,lambda=grid)
plot(cv.out.lasso)
bestlam.lasso <- cv.out.lasso$lambda.min

#predict
lasso.predictions <- predict.glmnet(lasso.mod,newx=x.test,s=bestlam.lasso)

test.trout$lasso.predictions <- lasso.predictions

# add lm, fwd, bwd predictions to test data
test.trout$full.lm.predictions <- full.lm.predictions
test.trout$fwd.step.predictions <- fwd.step.predictions
test.trout$bwd.step.predictions <- bwd.step.predictions

# calculate residuals for each
test.trout$full.lm.resid <- test.trout$hit_speed-test.trout$full.lm.predictions
test.trout$fwd.step.resid <- test.trout$hit_speed-test.trout$fwd.step.predictions
test.trout$bwd.step.resid <- test.trout$hit_speed-test.trout$bwd.step.predictions
test.trout$ridge.resid <- test.trout$hit_speed-test.trout$ridge.predictions
test.trout$lasso.resid <- test.trout$hit_speed-test.trout$lasso.predictions

# Residual plots
par(mfrow=c(2,3))
plot(full.lm.resid~full.lm.predictions,data=test.trout)
plot(fwd.step.resid~fwd.step.predictions,data=test.trout)
plot(bwd.step.resid~bwd.step.predictions,data=test.trout)
plot(ridge.resid~ridge.predictions,data=test.trout)
plot(lasso.resid~lasso.predictions,data=test.trout)

# Actual v predicted
plot(hit_speed~full.lm.predictions,data=test.trout,xlab='Predicted',ylab='Actual',main='Full linear model (actual v. predicted)',
     xlim=c(55,110),ylim=c(55,110))
abline(0,1)
plot(hit_speed~fwd.step.predictions,data=test.trout,xlab='Predicted',ylab='Actual',main='Forward selection (actual v. predicted)',
     xlim=c(55,110),ylim=c(55,110))
abline(0,1)
plot(hit_speed~bwd.step.predictions,data=test.trout,xlab='Predicted',ylab='Actual',main='Backward selection (actual v. predicted)',
     xlim=c(55,110),ylim=c(55,110))
abline(0,1)
plot(hit_speed~ridge.predictions,data=test.trout,xlab='Predicted',ylab='Actual',main='Ridge regression (actual v. predicted)',
     xlim=c(55,110),ylim=c(55,110))
abline(0,1)
plot(hit_speed~lasso.predictions,data=test.trout,xlab='Predicted',ylab='Actual',main='The lasso (actual v. predicted)',
     xlim=c(55,110),ylim=c(55,110))
abline(0,1)

# Modeling comparisons
test.trout$naiive.model <- mean(test.trout$hit_speed)
test.trout$naiive.resid <- test.trout$hit_speed-test.trout$naiive.model
rsquare.full <- (sum((test.trout$naiive.resid)^2)-sum((test.trout$full.lm.resid)^2))/sum((test.trout$naiive.resid)^2)
rsquare.fwd <- (sum((test.trout$naiive.resid)^2)-sum((test.trout$fwd.step.resid)^2))/sum((test.trout$naiive.resid)^2)
rsquare.bwd <- (sum((test.trout$naiive.resid)^2)-sum((test.trout$bwd.step.resid)^2))/sum((test.trout$naiive.resid)^2)
rsquare.ridge <- (sum((test.trout$naiive.resid)^2)-sum((test.trout$ridge.resid)^2))/sum((test.trout$naiive.resid)^2)
rsquare.lasso <- (sum((test.trout$naiive.resid)^2)-sum((test.trout$lasso.resid)^2))/sum((test.trout$naiive.resid)^2)

#trout.rsquare.results <- data.frame(
#  full.lm <- rsquare.full,
#  fwd <- rsquare.fwd,
#  bwd <- rsquare.bwd,
#  ridge <- rsquare.ridge,
#  lasso <- rsquare.lasso
#)
trout.rsquare.results <- data.frame(
  full.lm <- rsquare.full,
  fwd <- rsquare.fwd,
  bwd <- rsquare.bwd,
  ridge <- rsquare.ridge,
  lasso <- rsquare.lasso,
  name <- 'Mike Trout'
)
