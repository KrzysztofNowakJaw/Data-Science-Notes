
### create an automated model
### to predict monthly alcohol sales
### with the Holt-Winters method


### when we create an automated model
### the program builds several models and chooses the one with the lowest AIC

### (AIC - Akaike Information Criterion)

### we will use the value "ZZZ" for the model parameter
### and let the program select the model for us

model_auto <- ets(sales_train_ts, model = "ZZZ", alpha = 0.2)

model_auto

pred_auto <- forecast(model_auto, h = ntest, level = 0)


### compute the prediction accuracy

accuracy(pred_auto, alcohol_test$sales)

### drawback of automation:
### it relies one one indicator only: AIC



