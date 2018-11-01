library(dplyr)
library(rsample)
library(reticulate)

postocdes_NL <- readRDS("postocdes_NL.RDs")
jaap <- readRDS("JaapResults.RDs")

jaap = jaap %>% 
  left_join(postocdes_NL, by = c("PC6"= "Postcode_spatie")) %>% 
  select(
    prijs,  Lat_Postcode6P, Long_Postcode6P
  ) %>% 
  filter(
    Lat_Postcode6P > 50.7,
    !is.na(prijs),
    prijs > 75000,
    prijs < 2500000
  )

tmp = rsample::initial_split(jaap)
train = training(tmp)
train_matrix = as.matrix(train[,2:3])
test = testing(tmp)
test_matrix = as.matrix(test[,2:3])
prijs_test = test$prijs
prijs = train$prijs


########### K-NN regressie op long lat via Scikit ##############################################

reticulate::use_condaenv("my_py36")

KNN_reg = import("sklearn.neighbors")$KNeighborsRegressor
rsq = numeri(15)
for(i in 1L:15L){
  neigh = KNN_reg(n_neighbors = i)
  neigh$fit(X = train_matrix, y=prijs)

  test$predictie = neigh$predict(test_matrix)
  test$resid = test$prijs - test$predictie
  rss <- sum((test$pred - test$prijs) ^ 2)  ## residual sum of squares
  tss <- sum((test$prijs - mean(test$prijs)) ^ 2)  ## total sum of squares
  rsq[i] = 1 - rss/tss
}

k_bepaling = data.frame(k=1:15, rsq)
ggplot(k_bepaling, aes(k,rsq)) +
  geom_point(size=3) +
  geom_line(size=2, col="blue") +
  labs(title="verschillende k in KNN, R-squared op test")

ggplot(test, aes(resid)) +geom_histogram(col="black")








#from sklearn.neighbors import NearestNeighbors
KNN_reg = import("sklearn.neighbors")$KNeighborsRegressor
neigh = KNN_reg(n_neighbors = 3L)

X = matrix(runif(100), ncol=5)
y = rnorm(20) +100
neigh$fit(X,y)

buren = neigh$kneighbors(X[1:5,])

testdata = matrix(runif(20), ncol=5)

neigh$predict(testdata)
