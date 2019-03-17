woonModelData

library(dplyr)
library(xgboost)
library(Matrix)
library(ggplot2)

ABT = woonModelData %>% 
  filter(
    Transactieprijs > 0, 
    Transactieprijs < 1150000,
    Woonoppervlak > 10, Woonoppervlak < 250 
  )
inputMatrix = sparse.model.matrix(  ~ Woonoppervlak, data = ABT)

## zonder monotonicity constraints
xgboost.out = xgboost(data = inputMatrix, label = ABT$Transactieprijs, nrounds = 25)

## met monotonicity contraints
xgboost.out2 = xgboost(
  params = list(monotone_constraints = c(0,1)),
  data = inputMatrix,
  label = ABT$Transactieprijs, 
  nrounds = 50
)

lm1 = lm(Transactieprijs ~ Woonoppervlak, data = ABT)
lm2 = lm(Transactieprijs ~ splines::ns(Woonoppervlak,9), data = ABT)

newdata = data.frame( Woonoppervlak = 20:250)
predmatrix = sparse.model.matrix(  ~ Woonoppervlak, data = newdata)
pred_xgboost = predict(xgboost.out, newdata = predmatrix)
pred_xgboost2 = predict(xgboost.out2, newdata = predmatrix)
pred_lm = predict(lm1, newdata = newdata)
pred_lm2 = predict(lm2, newdata = newdata)

newdata = newdata %>% 
  mutate(
    xgboost_pred = pred_xgboost,
    xgboost_pred_mono = pred_xgboost2,
    lm_pred = pred_lm,
    lm_pred2 = pred_lm2
  )


ggplot(data = ABT, aes(Woonoppervlak, Transactieprijs)) +
  geom_point(alpha = .5) 
  
  ggplot(data = newdata, aes(Woonoppervlak, xgboost_pred)) +
  geom_line(data = newdata, aes(Woonoppervlak, xgboost_pred), col=2, size = 1)  +
  geom_line(data = newdata, aes(Woonoppervlak, xgboost_pred_mono), col=4, size = 1.5)  +
  labs(title = "Voorspelde huisprijs versus woonoppervlak \nXGboost met en zonder monotonicity constrained", y="huisprijs") +
    scale_x_continuous(breaks = 25*(0:10)) + 
    scale_y_continuous(breaks = 100000*(0:8), labels = scales::dollar_format(suffix = "€", prefix = "") )
  


  
  
  
  
  
  
  
  
  
  