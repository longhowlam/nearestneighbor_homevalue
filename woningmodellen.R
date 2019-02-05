#######################################################

## Wat woningen modellen

#### libraries
library(tidyverse)
library(sp)
library(maptools)
library(leaflet)
library(colorRamps)
library(plotly)
library(h2o)


### Lees transacties in
transacties_2018 <- read_csv(
  "2018-woontransacties.csv",
  col_types = cols(
    `Datum akte` = col_date(format = "%d-%m-%Y"),
    Invoerdatum = col_date(format = "%d-%m-%Y")
  )
)

#### overzichten paar variablen ############################

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
  )

ggplot(woningen, aes(ouderdom)) + 
  geom_histogram(col="black", binwidth = 1) +
  labs(title = "ouderdom in jaren")

type = woningen %>% group_by(woningBeschrijving) %>%  summarise(n=n())

### check inhoud en perceel

ggplot(woningen, aes(Perceel)) + 
  geom_histogram(col="black", bins=50) +
  labs("Perceel")

ggplot(woningen, aes(Inhoud)) + 
  geom_histogram(col="black", bins=70) +
  labs("Inhoud")

ggplot(woningen, aes(`Aantal kamers`)) + 
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
    `Aantal kamers`,
    Perceel,
    Inhoud,
    KoopConditie,
    woningBeschrijving
  )

# simpel lineare modellen

out1 = lm(Transactieprijs ~ Woonoppervlak, data = woonModelData)
summary(out1)

out2 = lm(Transactieprijs ~ `Aantal kamers` , data = woonModelData)
summary(out2)

out2 = lm(Transactieprijs ~ `Aantal kamers` + Woonoppervlak, data = woonModelData)
summary(out2)

out2 = lm(Transactieprijs ~ `Aantal kamers` * Woonoppervlak, data = woonModelData)
summary(out2)

out2 = lm(Transactieprijs ~ PC2 + `Aantal kamers` * Woonoppervlak, data = woonModelData)
summary(out2)

outall = lm(Transactieprijs ~ ., data = woonModelData)
summary(outall)

###### Teveel levels
woonModelData = woonModelData %>% 
  mutate(
    woningBeschrijving = fct_lump(woningBeschrijving, n = 25)
  )

outall = lm(Transactieprijs ~ ., data = woonModelData)
summary(outall)

##### plaatje van waargenomen prijs tov voorspelde prijs

pred = predict(outall, woonModelData)

woonModelData2 = woonModelData
woonModelData2$predictie = pred

ggplot(woonModelData2, aes(Transactieprijs, predictie)) +
  geom_point() + 
  geom_smooth() +
  labs(title = "waargenomen prijs vs linear model voorspelde prijs")

##### plaatje van parameters ###############################################

## woningbeschrijving de level BenedenBovenwoning is weggelaten
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


### Voorspel mijn huis

mijnhuis = data.frame(
  PC2 = "10", 
  KoopConditie = "kosten koper", 
  ouderdom = 26,
  Woonoppervlak = 113,
  `Aantal kamers` = 5,
  Perceel = 50,
  Inhoud = 180,
  woningBeschrijving = "EengezinswoningTussenwoning" 
)

%>% 
  as.h2o

predict(outRF, mijnhuis)