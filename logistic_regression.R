#http://astrostatistics.psu.edu/datasets/2006tutorial/html/boot/html/cv.glm.html


require(glmnet)
require(Hmisc)
require(boot)
require(glmnet)











smp_size <- floor(0.75 * nrow(glm.data))


## set the seed to make your partition reproductible
set.seed(123)

train_ind <- sample(seq_len(nrow(glm.data)), size = smp_size)
train <- glm.data[train_ind, ]
test <- glm.data[-train_ind, ]


fit <- glm(cand_party ~ contb_receipt_amt + estimated_salary + 
  predicted_gender + gender + cbus + cincy + cleveland   + elect_delta + election_tp +  
  population + multiple_contb, data=train, family="binomial")



fit <- glm(cand_party ~contb_receipt_amt + log(estimated_salary+1) + 
  predicted_gender   + cbus + cincy + cleveland   + log(elect_delta + 1) + general_election +  
  log(population+1) + multiple_contb, data=train, family="binomial")


predpr <- predict(fit, newdata=test,type="response")
coefplot(fit)
glm.roc <- roc(test$cand_party ~ predpr)
plot(glm.roc)


predpr <- predict(glm.fit, newx=test.x,type="response")
coefplot(fit)
glm.roc <- roc(test.y~ predpr)
plot(glm.roc)

# 10 fold cross validation
cv.fit <- cv.glm(test, fit, K=10)
cv.error <- cv.fit$delta


# delta is the cross-validated prediction error where:
# 
# The first number is the raw leave-one-out, or lieu cross-validation result.
# The second one is a bias-corrected version of it.
# The bias correction has to do with the fact that the data set that we train it on is slightly smaller than the one that we actually would like to get the error for, which is the full data set of size n. It turns out that has more of an effect for k-fold cross-validation.







