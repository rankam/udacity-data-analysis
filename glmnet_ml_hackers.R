
set.seed(1)

glm.data <- data[c(11, 19, 20, 21, 22, 26, 28, 29, 30, 31, 32, 33)]
# glm.data <- data[c(10, 19, 20, 25, 26, 29, 30, 31, 32,33,21)]
glm.data$cand_party <- ifelse(glm.data$cand_party == 'D', 1, 0)
glm.data$predicted_gender <- ifelse(glm.data$predicted_gender == 'male',1,0)
glm.data$election_tp <- ifelse(glm.data$election_tp == 'G2012',1,0)


performance <- data.frame()
for (i in 1:10)
{
  smp_size <- floor(0.75 * nrow(glm.data))
  indices <- sample(seq_len(nrow(glm.data)), size = smp_size)
  training.x <- as.matrix(glm.data[indices,c(1:4,6:12) ])
  training.y <- as.matrix(glm.data[indices,c(5)])


  test.x <- as.matrix(glm.data[-indices,c(1:4,6:12)  ])
  test.y <- as.matrix(glm.data[-indices,c(5)])

  for (lambda in c(0.00001, 0.0001, 0.001, 0.01))
  {
    glm.fit <- glmnet(training.x, training.y, family = 'binomial')
    predicted.y <- ifelse(predict(glm.fit, test.x, s = lambda) > 0, 1, 0)
    error.rate <- mean(predicted.y != test.y)

    performance <- rbind(performance,
                         data.frame(Lambda = lambda,
                                    Iteration = i,
                                    ErrorRate = error.rate))
  }
}


ggplot(performance, aes(x = Lambda, y = ErrorRate)) +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'point') +
    scale_x_log10()

cv.class<-cv.glmnet(test.x, test.y, family="binomial", type.measure='class')