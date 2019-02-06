#######################################################

## Wat analyzes op woning transacties

#### libraries
library(tidyverse)
library(sp)
library(maptools)
library(leaflet)
library(colorRamps)
library(plotly)
library(h2o)


### Lees woning transacties in ##########################
transacties_2018 <- read_csv(
  "2018-woontransacties.csv",
  col_types = cols(
    `Datum akte` = col_date(format = "%d-%m-%Y"),
    Invoerdatum = col_date(format = "%d-%m-%Y")
  )
)

#### overzichten van paar variablen ############################

parkeer = transacties_2018 %>%
  group_by(Garage) %>%  
  summarise(n=n())

Type = transacties_2018 %>% 
  group_by(`Type woning`) %>% 
  summarise(n=n())

Soort = transacties_2018 %>% 
  group_by(`Soort woning`) %>% 
  summarise(n=n())

##### Alleen woningen bekijken ################################

woningen = transacties_2018 %>% 
  filter(
    Garage == "GeenGarage"
  )

ggplot(woningen, aes(Transactieprijs)) + 
  geom_histogram(col="black", bins=50) +
  labs("Transactieprijs verdeling")


#### extremen er uithalen die de boel verstoren ###################

## op basis van plots
woningen = woningen %>% filter(Transactieprijs < 2000000)

ggplot(woningen, aes(Woonoppervlak, `Aantal kamers`)) +
  geom_point(alpha = 0.5) 

woningen = woningen %>% 
  filter(
    Woonoppervlak < 700,
    `Aantal kamers` < 10
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
  select(-`Aantal kamers`)

ggplot(woningen, aes(ouderdom)) + 
  geom_histogram(col="black", binwidth = 1) +
  labs(title = "ouderdom in jaren")

Type = woningen %>% group_by(woningBeschrijving) %>%  summarise(n=n())


### check inhoud en perceel

ggplot(woningen, aes(Perceel)) + 
  geom_histogram(col="black", bins=50) +
  labs("Perceel")

ggplot(woningen, aes(Inhoud)) + 
  geom_histogram(col="black", bins=70) +
  labs("Inhoud")

ggplot(woningen, aes(aantalkamers)) + 
  geom_histogram(col="black", bins=70) +
  labs("Inhoud")


#########  voorspelmodel voor prijs ###############################################

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

# simpel lineare regressie modellen ####################################

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


###### Teveel levels in Woningbeschrijving ############################################

woonModelData = woonModelData %>%
  filter(
    woningBeschrijving != "BenedenBovenwoning"
  ) %>% 
  mutate(
    woningBeschrijving = fct_lump(woningBeschrijving, n = 25)
  ) 
 
table(woonModelData$woningBeschrijving)

outall = lm(Transactieprijs ~ ., data = woonModelData)
summary(outall)


# visuele beoordeling model ###########################################
# plaatje van waargenomen prijs tov voorspelde prijs

pred = predict(outall, woonModelData)

woonModelData2 = woonModelData
woonModelData2$predictie = pred

ggplot(woonModelData2, aes(Transactieprijs, predictie)) +
  geom_point(alpha = 0.2) + 
  geom_smooth() +
  labs(title = "waargenomen prijs vs linear regressie model voorspelde prijs")

##### plaatje van parameters ###############################################

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



###########  advanced models met h2o ##########################################

h2o.init()

# character data needs to be factor in h2o
woonModelData = woonModelData %>% mutate_if(is.character, as.factor)

### maak data beschikbaar in h20
huizen.h2o = as.h2o(woonModelData)

### splits in train test
TT = h2o.splitFrame(huizen.h2o)

outRF =  h2o.randomForest(
  x = 2:9,
  y = 1,
  training_frame = TT[[1]],
  validation_frame = TT[[2]]
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

### saving model to disk ###################################
h2omodel = h2o.saveModel(outRF, "huismodel.h2o")


### Voorspel waarde van mijn huis #######################

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

predict(huismodel, mijnhuis)



######## h2o auto ml #########################################

out = h2o.automl(
  x = 2:9,
  y = 1,
  training_frame  = TT[[1]],
  validation_frame = TT[[2]],
  max_runtime_secs = 180
)

## out is nu een zgn H2OAutoML object met alle resultaten
out

## iets overzichtelijker output is de leaderboard
out@leaderboard

WINNER = out@leader
WINNER

# De winner is een GBM met 
# RMSE:  53432.65











