library(caret)

# leave-one-out cross-validation -----------------------------------------------
train_control <- trainControl(method = "LOOCV") 

# gbg --------------------------------------------------------------------------
gbg$sex <- as.factor(gbg$Sex)

cv_gbg <- train(
  Sex ~
    scale(fWHR),
  data = gbg,
  trControl = trainControl(method = "LOOCV"),
  method = "glm",
  family = binomial()
  )

# summary
summary(cv_gbg)
print(cv_gbg$results)

# evaluate
pred_values <- cv_gbg$pred$pred 
actual_values <- gbg$sex 

# confusion matrix
conf_matrix <- confusionMatrix(dat = pred_values,
                               reference = actual_values,
                               positive = "Male")
conf_matrix

# ggg --------------------------------------------------------------------------
ggg$sex <- as.factor(ggg$Sex)

cv_ggg <- train(
  Sex ~
    scale(fWHR),
  data = ggg,
  trControl = trainControl(method = "LOOCV"),
  method = "glm",
  family = binomial()
  )

# summary
summary(cv_ggg)
print(cv_ggg$results)

# evaluate
pred_values <- cv_ggg$pred$pred 
actual_values <- ggg$sex 

# confusion matrix
conf_matrix <- confusionMatrix(dat = pred_values,
                               reference = actual_values,
                               positive = "Male")
conf_matrix

# ptt --------------------------------------------------------------------------
ptt$sex <- as.factor(ptt$Sex)

cv_ptt <- train(
  Sex ~
    scale(fWHR),
  data = ptt,
  trControl = trainControl(method = "LOOCV"),
  method = "glm",
  family = binomial()
  )

# summary
summary(cv_ptt)
print(cv_ptt$results)

# evaluate
pred_values <- cv_ptt$pred$pred 
actual_values <- ptt$sex 

# confusion matrix
conf_matrix <- confusionMatrix(dat = pred_values,
                               reference = actual_values,
                               positive = "Male")
conf_matrix

# pts --------------------------------------------------------------------------
pts$sex <- as.factor(pts$Sex)

cv_pts <- train(
  Sex ~
    scale(fWHR),
  data = pts,
  trControl = trainControl(method = "LOOCV"),
  method = "glm",
  family = binomial()
  )

# summary
summary(cv_pts)
print(cv_pts$results)

# evaluate
pred_values <- cv_pts$pred$pred 
actual_values <- pts$sex 

# confusion matrix
conf_matrix <- confusionMatrix(dat = pred_values,
                               reference = actual_values,
                               positive = "Male")
conf_matrix

# ppn --------------------------------------------------------------------------
ppn$sex <- as.factor(ppn$Sex)

cv_ppn <- train(
  Sex ~
    scale(fWHR),
  data = ppn,
  trControl = trainControl(method = "LOOCV"),
  method = "glm",
  family = binomial()
  )

# summary
summary(cv_ppn)
print(cv_ppn$results)

# evaluate
pred_values <- cv_ppn$pred$pred 
actual_values <- ppn$sex 

# confusion matrix
conf_matrix <- confusionMatrix(dat = pred_values,
                               reference = actual_values,
                               positive = "Male")
conf_matrix
