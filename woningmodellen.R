######### *** WONINGTRANSACTIES **** ###################################################
##
## Wat analyzes em eem voorspel model op woningtransacties
##

#### benodigde libraries
library(tidyverse)
library(sp)
library(maptools)
library(leaflet)
library(colorRamps)
library(plotly)
library(h2o)
library(lime)

## initialiseer h2o
h2o.init()

### IMPORT en overzicht data ##################################################

### Lees woning transacties in 
transacties_2018 <- read_csv(
  "2018-woontransacties.csv",
  col_types = cols(
    `Datum akte` = col_date(format = "%d-%m-%Y"),
    Invoerdatum = col_date(format = "%d-%m-%Y")
  )
)

### overzichten van paar variablen 
parkeer = transacties_2018 %>%
  group_by(Garage) %>%  
  summarise(n=n())

type = transacties_2018 %>% 
  group_by(`Type woning`) %>% 
  summarise(n=n())

Soort = transacties_2018 %>% 
  group_by(`Soort woning`) %>% 
  summarise(n=n())

##### FILTER Alleen woningen  ################################################

woningen = transacties_2018 %>% 
  filter(
    Garage == "GeenGarage"
  )

ggplot(woningen, aes(Transactieprijs)) + 
  geom_histogram(col="black", bins = 50) +
  labs("Transactieprijs verdeling")

#### OUTLIERS weghalen #########################################################

### outliers er uithalen die de boel verstoren 
### op basis van plots
woningen = woningen %>% 
  filter(Transactieprijs < 2000000) %>% 
  mutate(
    aantalkamers = `Aantal kamers`,
  )

ggplot(woningen, aes(Woonoppervlak, aantalkamers)) +
  geom_point(alpha = 0.5) 

woningen = woningen %>% 
  filter(
    Woonoppervlak < 700,
    aantalkamers < 10
  )

woningen = woningen %>% 
  mutate(
    aantalkamers = `Aantal kamers`,
    PC2 = str_sub(Postcode,1,2),
    ouderdom = 2019 - Bouwjaar,
    woningBeschrijving = paste0(
      ifelse(is.na(`Soort woning`), "", `Soort woning`),
      ifelse(is.na(`Type woning`),  "",  `Type woning`),
      ifelse(is.na(`Soort appartement`), "",`Soort appartement`)
    )
  ) %>% 
  filter(
    ouderdom < 200,
    ouderdom >= 0,
    Perceel < 750,
    Inhoud < 1000
  ) %>% 
  select(-`Aantal kamers`) %>% 
  filter(
    woningBeschrijving != "BenedenBovenwoning"
  ) 
  

ggplot(woningen, aes(ouderdom)) + 
  geom_histogram(col="black", binwidth = 1) +
  labs(title = "ouderdom in jaren")

type = woningen %>% group_by(woningBeschrijving) %>%  summarise(n=n())


### nog wat visuals inhoud en perceel 

ggplot(woningen, aes(Perceel)) + 
  geom_histogram(col="black", bins=50) +
  labs("Perceel")

ggplot(woningen, aes(Inhoud)) + 
  geom_histogram(col="black", bins=70) +
  labs("Inhoud")

ggplot(woningen, aes(aantalkamers)) + 
  geom_histogram(col="black", bins=70) +
  labs("Inhoud")


######### PREDICTIVE MODEL ########################################################

## voorspelmodel voor prijs 

woonModelData  = woningen %>% 
  select(
    Transactieprijs,
    PC2,
    KoopConditie,
    ouderdom,
    Woonoppervlak,
    aantalkamers,
    Perceel,
    Inhoud,
    KoopConditie,
    woningBeschrijving
  )

#### simpel lineare regressie modellen 
out1 = lm(Transactieprijs ~ Woonoppervlak, data = woonModelData)
summary(out1)

out2 = lm(Transactieprijs ~ aantalkamers , data = woonModelData)
summary(out2)

### nu negatief coefficient voor aantal kamers.....
out2 = lm(Transactieprijs ~ aantalkamers + Woonoppervlak, data = woonModelData)
summary(out2)

out2 = lm(Transactieprijs ~ aantalkamers * Woonoppervlak, data = woonModelData)
summary(out2)

out2 = lm(Transactieprijs ~ PC2 + aantalkamers* Woonoppervlak, data = woonModelData)
summary(out2)

outall = lm(Transactieprijs ~ ., data = woonModelData)
summary(outall)


###### Teveel levels in Woningbeschrijving 

woonModelData = woonModelData %>%
  mutate(
    woningBeschrijving = fct_lump(woningBeschrijving, n = 25)
  ) 
 
table(woonModelData$woningBeschrijving)

outall = lm(Transactieprijs ~ ., data = woonModelData)
summary(outall)


##### Visuele beoordeling model
## plaatje van waargenomen prijs tov voorspelde prijs

pred = predict(outall, woonModelData)

woonModelData2 = woonModelData
woonModelData2$predictie = pred

ggplot(woonModelData2, aes(Transactieprijs, predictie)) +
  geom_point(alpha = 0.2) + 
  geom_smooth() +
  labs(title = "waargenomen prijs vs linear regressie model voorspelde prijs")


##### PLAATJES van parameters ###############################################

## woningbeschrijving de level Benedenwoning is weggelaten
coefnamen = names(outall$coefficients)

HuisType = sort(outall$coefficients [ str_detect(coefnamen, "woningBeschrijving")])
HuisType_naam = names(HuisType) %>% str_replace("woningBeschrijving", "")

plot_ly(x=~HuisType_naam, y=~HuisType, type = 'bar')


## Postcode 2 op een leaflet  
# Note PC2 10 is weggelaten (Amsterdam)
PC2coef = outall$coefficients [ str_detect(coefnamen, "PC2")]
PC2 = names(PC2coef) %>% str_replace("PC2", "")

#### Stel dat we een huis hebben van 1 miljoen in PC2 = 10
PC2data = tibble(PC2 = PC2, waarde = 1000000 + PC2coef)
PC2data = bind_rows(tibble(PC2="10", waarde = 1000000 ), PC2data)

## definieer een kleuren schema
colpal <- colorQuantile(
  palette = green2red(9), n=9,
  domain = PC2data$waarde
)

## elk polygoontje krijgt nu op basis van zijn restaurant aantal het 'juiste' kleurtje

### teken per polygon, dat is het makkelijkst
pc2_lf = leaflet() %>%  addTiles() 
for (i in 1:length(plg)){
  ptxt = paste(
    "PC2: ", PC2data$PC2[i],
    "<br>",
    "Waarde",
    as.character(round(PC2data$waarde[i]))
  )
  pc2_lf = pc2_lf %>%
    addPolygons(
      data = plg[[i]],
      weight = 2,
      smoothFactor = 1,
      fillOpacity = 0.75,
      fillColor= colpal(PC2data$waarde[i]),
      popup = ptxt
    )
}

pc2_lf



###########  ADVANCED H2O MODELS  ##########################################


# character data needs to be factor in h2o
woonModelData = woonModelData %>% mutate_if(is.character, as.factor)

### maak data beschikbaar in h20
huizen.h2o = as.h2o(woonModelData)

### splits in train test
TT = h2o.splitFrame(huizen.h2o)

### ** Random forest ################
outRF =  h2o.randomForest(
  x = 2:9,
  y = 1,
  training_frame = TT[[1]],
  validation_frame = TT[[2]],
  ntrees = 30
)

h2o.varimp_plot(outRF)

###  R2 op de TEST set
rsq <- function (x, y) cor(x, y) ^ 2

TEST = h2o.cbind(
  TT[2], 
  h2o.predict(outRF,TT[[2]])
) %>%
  as.data.frame()

ggplot(TEST, aes(Transactieprijs, predict)) +
  geom_point() + 
  geom_smooth() +
  labs(title = "waargenomen prijs vs Random Forest model voorspelde prijs")

rsq(TEST$Transactieprijs, TEST$predict)


### ** Neuraal netwerk ###############

outNN =  h2o.deeplearning(
  x = 2:9,
  y = 1,
  hidden = c(15,15),
  epochs = 150,
  training_frame = TT[[1]],
  validation_frame = TT[[2]]
)

### saving model to disk for later usage
h2omodel = h2o.saveModel(outRF, "huismodel_30.h2o")


### VOORSPEL waarde van mijn huis #######################

huismodel = h2o.loadModel(h2omodel)

mijnhuis = data.frame(
  PC2 = "11", 
  KoopConditie = "kosten koper", 
  ouderdom = 8,
  Woonoppervlak = 125,
  aantalkamers = 6,
  Perceel = 100,
  Inhoud = 140,
  woningBeschrijving = "EengezinswoningTussenwoning" 
) %>% 
  as.h2o

out = predict(huismodel, mijnhuis)
out


##### LIME: verklaring voor model voorspelling ########################

huizen.h2o = as.h2o(woonModelData)

### splits in train test
TT = h2o.splitFrame(huizen.h2o)

outRF =  h2o.randomForest(
  x = 2:9,
  y = 1,
  training_frame = TT[[1]],
  validation_frame = TT[[2]],
  ntrees = 30
)

h2otraindata = TT[[1]] %>% as.data.frame()

explainer = lime(
  h2otraindata[,2:9],
  bin_continuous = TRUE,
  outNN, nbins = 25
)
summary(explainer)



mijnTeVerklarenHuizen = data.frame(
  PC2 = c("16", "10", "99", "35"),  
  KoopConditie = c("kosten koper","kosten koper", "kosten koper", "vrij op naam"), 
  ouderdom = c(8, 10, 25, 2),
  Woonoppervlak = c(125, 100, 250, 100),
  aantalkamers = c(3, 5, 6, 6),
  Perceel = c(100,120,120,130),
  Inhoud = c(140, 150, 160, 150),
  woningBeschrijving = rep("EengezinswoningTussenwoning" ,4)
) 
  
explanations = lime::explain(
  x = mijnTeVerklarenHuizen, 
  explainer,
  n_features = 100,
  feature_select = "none",
  n_permutations = 10000
  
)

plot_features(explanations)

######## AUTOML met H2O #########################################

out = h2o.automl(
  x = 2:9,
  y = 1,
  training_frame  = TT[[1]],
  validation_frame = TT[[2]],
  max_runtime_secs = 1800
)

## out is nu een zgn H2OAutoML object met alle resultaten
out

## iets overzichtelijker output is de leaderboard
out@leaderboard

WINNER = out@leader
WINNER

# De winner is een GBM met 
# RMSE:  53596.61
# random forest was RMSE:  54643.54


TEST2 = h2o.cbind(
  TT[2], 
  h2o.predict(WINNER,TT[[2]])
) %>%
  as.data.frame()

ggplot(TEST2, aes(Transactieprijs, predict)) +
  geom_point() + 
  geom_smooth() +
  labs(title = "waargenomen prijs vs Random Forest model voorspelde prijs")

rsq(TEST2$Transactieprijs, TEST2$predict)





#################### OVERBIEDEN?? ###################################################################

## hier is de interactieve plot
MM


## R code om get te maken. Is iets uitgebreider omdat we moeten zoeken waar in welke buurt 
## Een huis is in amsterdam mbv CBS buurten van Amsterdam

#######  buurten CBS 
CBSbuurten <- readShapeSpatial("Uitvoer_shape/buurt2018.shp")
#### Zet coordinatensysteem
proj4string(CBSbuurten) <- CRS("+init=epsg:28992")
#### transformeer naar long /lat
CBSbuurten = spTransform(CBSbuurten, CRS("+proj=longlat +datum=WGS84"))

# focus op amsterdam
amsterdamPC <- CBSbuurten[str_sub(CBSbuurten$POSTCODE,1,2) == "10" ,]
plot(amsterdamPC)

amsterdam = woningen %>% 
  mutate(
    plaats = stringr::str_to_lower(Plaats),
    surplus = (Transactieprijs - Vraagprijs)/Vraagprijs
  ) %>% 
  filter(
    plaats == "amsterdam", 
    !is.na(Longitude),
    !is.na(Latitude),
    Vraagprijs > 150000,
    Garage == "GeenGarage"
  )
coordinates(amsterdam) <- ~Longitude + Latitude
proj4string(amsterdam) = CRS("+proj=longlat +datum=WGS84")

tmp = sp::over(amsterdam, amsterdamPC)

#combineer nu 

amsterdam = bind_cols(
  amsterdam@data,
  amsterdam@coords %>% as.data.frame,
  tmp
)

############ nu kan je op CBS BU_NAAM aggregegeren
adampvakkenBU = amsterdam %>% 
  group_by(BU_NAAM) %>% 
  summarise(
    ntrx = n(),
    overbieden = median(surplus, na.rm = TRUE)
  )

### join met amsterdamPC zodat je het op een leaflet kan zetten

tmp = amsterdamPC@data
tmp2 = tmp %>% left_join(adampvakkenBU)

amsterdamPC@data = tmp2

# op een leaflet
pal <- colorQuantile(
  palette = "inferno",
  domain = amsterdamPC$overbieden, n=9)

### op leaflet maar dit is net te veel op buurt niveau
ptekst = sprintf(
  "<strong> %s </stong> </br>
  mediane overbieding %g procent</br>
  aantal trx %g </br>
  ", 
  amsterdamPC$BU_NAAM,
  round(amsterdamPC$overbieden*100,1),
  amsterdamPC$ntrx
)%>% lapply(htmltools::HTML)


MM = leaflet(amsterdamPC) %>%
  addTiles() %>%
  addPolygons(
    stroke = TRUE, weight = 1, fillOpacity = 0.55, smoothFactor = 0.15,
    popup = ptekst,
    color = ~pal(overbieden),
    highlightOptions = highlightOptions(
      color = "white", weight = 2,
      bringToFront = TRUE),
    label = ptekst,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  )

MM







