library(dplyr)
library(rsample)
library(reticulate)
library(ggmap)
library(osmdata)
library(sf)
library(feather)
#################################################################################################

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
  labs(title="verschillende k in KNN, R-squared op test") +
  scale_x_continuous(breaks = 1:15)

ggplot(test, aes(resid)) +geom_histogram(col="black")

######## gebruik beste k om model te maken en uit te scoren op postcode data
KNN_reg = import("sklearn.neighbors")$KNeighborsRegressor
bestkmodel = KNN_reg(n_neighbors = 5L)
bestkmodel$fit(X = train_matrix, y=prijs)
NL_postcode = as.matrix(postocdes_NL[,8:9])

huispredicties = bestkmodel$predict(NL_postcode)
postocdes_NL$predictie = huispredicties

sample_huizen = postocdes_NL %>% 
   filter(
     Lat_Postcode6P > 52.3,
     city == "Amsterdam"
    ) 

sample_huizen = postocdes_NL %>% 
  filter(
    province == "Groningen"
  ) 


sample_huizen = postocdes_NL %>% 
  filter(
    city == "Baarn"
  ) 

amsterdam =  c(4.9036,52.3680)
groningen = c(6.576000607, 53.2168049)
baarn = c(5.26, 52.2107)

############# ggmap met google maps werkt niet meer, je moet je nu registreren

ggmap(
  get_googlemap(center = amsterdam, scale=2, zoom=13)
) + 
geom_point(
  data=sample_huizen, aes(y=Lat_Postcode6P,x=Long_Postcode6P, color = predictie),
  size = 1.2, alpha = 0.6
) +
scale_colour_gradientn(colours = colorRamps::green2red(20))

######## gebruik stamen of osm maps met de volgende code

amsterdam_map <- get_map(
  getbb("Amsterdam"), 
  maptype = "toner-background", 
  source = "stamen"
)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
amsterdam <- st_as_sf(
  x = sample_huizen,                         
  coords = c("Long_Postcode6P", "Lat_Postcode6P"),
  crs = projcrs
)

#duurt paar minuten voor amsterdam.....
ggmap(amsterdam_map) +
  geom_sf(
    data = amsterdam,
    inherit.aes = FALSE,
    aes(colour = predictie),
    size = 0.5
  ) +
  labs(title = "Huisprijzen in Amsterdam", x="",y="")+ 
  scale_color_gradient(low="blue", high="red")

  

feather::write_feather(postocdes_NL[, c(8,9,16)], "postcode_nl.feather")
