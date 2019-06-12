###########################################################
#################   Project src   #########################
###########################################################


library(car)
library(MASS)

# De veronderstellingen van een general lineair model:
# 1) het gemiddelde van de fouten/residuals is gelijk aan 0
# 2) de residuals hebben een constante variantie, ze liggen ongeveer overal in hetzelfde gebied rond het model
# 3) de residuals zijn niet gecorreleerd, ze liggen bijvoorbeeld niet allemaal onde de curve links en erboven rechts of een bananevorm

# de residuals mogen ook niet gecorelleerd zijn met de x'en

# voor alles wat we gezien hebben van regressie, zijn deze veronderstellingen gebruikt. Dus gelijk welke metriek je wil gebruiken moet
# je dit checken.

# eenmaal je aan inferentie begint doen, moet je ook checken of de residuals normaal verdeeld zijn


###########################################################
###############   Load and format data   ##################
###########################################################

# set wd
wd = dirname(rstudioapi::getActiveDocumentContext()$path); wd
setwd(wd)

# TODO als je kijkt op see data vissualy hebben we te veel rows...

# eerst alle data apart inlezen van Gent, Antwerpen en Brussel
gent = read.csv("listingsGhent16_04_2019.csv", header = TRUE, sep = ",", dec = ".")
gent$city = "Gent"
antwerpen = read.csv("listingsAntwerp20_04_2019.csv", header = TRUE, sep = ",", dec = ".")
antwerpen$city = "Antwerpen"
brussel = read.csv("listingsBrussels13_04_2019.csv", header = TRUE, sep = ",", dec = ".")
brussel$city = "Brussel"

# dataframes samenvoegen
airbnb = rbind(gent, antwerpen, brussel)
# test of dat alle rows van de drie data frames in de nieuwe dataframe zitten
#   naive test, enkel op aantal rijen gebaseerd
nrow(airbnb) == (nrow(gent) + nrow(antwerpen) + nrow(brussel))
names(airbnb[airbnb$city == "Gent",]) ; names(gent)
nrow(airbnb[airbnb$city == "Gent",]) ; nrow(gent)
comp = airbnb[airbnb$city == "Gent",]

head(comp); head(gent)

attributes(airbnb)$names


# price == 0 -> NA
airbnb$price[airbnb$price == 0] = NA
# minimum_nights > 365 -> NA
airbnb$minimum_nights[airbnb$minimum_nights > 365] = NA

# Full == verhuurd == 0 dagen vrij
airbnb$full = FALSE
airbnb$full[airbnb$availability_365 == 0] = TRUE

airbnb$last_review[airbnb$last_review == ""] = NA
airbnb$last_review = as.Date(airbnb$last_review)

# last_review aanpassen brussel
airbnb$date_compiled = as.Date("2019-04-13")

datum = as.Date("2019-04-13");
airbnb$date_compiled[airbnb$city == "Brussel"] = as.Date(datum)


# last_review aanpassen Gent
datum = "2019-04-16"
airbnb$date_compiled[airbnb$city == "Gent"] = as.Date(datum)

# last_review aanpassen Antwerpen
datum = "2019-04-20";
airbnb$date_compiled[airbnb$city == "Antwerpen"] = as.Date(datum)


airbnb$last_review =  as.integer(airbnb$date_compiled) - as.integer(airbnb$last_review)
# airbnb$last_review[is.na(airbnb$last_review)] = 

airbnb$date_compiled = NULL

allNA = TRUE
for (i in 1:nrow(airbnb)) {
  if(!is.na(airbnb$neighbourhood_group)){
    allNA = FALSE
  }
}
allNA
# neighbourhood is all NA, so we drop it

airbnb$neighbourhood_group = NULL

attach(airbnb)

###########################################################
###############   Model voor de huurprijs   ###############
###########################################################
ab=na.omit(airbnb)
detach(airbnb)
attach(ab)
# 1. variabele selectie


plot(price~longitude)
ab$tmp[latitude<50.95] = "<50.95"
ab$tmp[latitude>50.95 & latitude<51.13] = "<51.13"
ab$tmp[latitude>51.13] = ">51.1"
boxplot(price~ab$tmp, outline=FALSE)
# duidelijke dat latitude en city zelfde data weergeven
# longitude zondert Gent af van Brussel en Antwerpen
ab$tmp = NULL



boxplot(price~room_type)
boxplot(price~room_type, outline = FALSE)
# room_type is een relevante variabele voor het model

plot(price~minimum_nights)
# minimum_nights kan ook een relevante variabele zijn,
#   in deze plot is te zien dat hoe kleiner minimum_nights, dan is er meer kans op een hogere prijs

plot(price~number_of_reviews)
# idem als met minimum_nights, een minder aantal reviews, grotere kans op hogere prijs
plot(price~last_review)
# idem als number_reviews, maar minder duidelijk, number_of_reviews geef een duidelijker verschil weer
#   beiden hebben te maken met reviews
plot(price~reviews_per_month)
# idem, een significanter verschil te merken dan met number_of_reviews
# we bekijken de correlatiecoeff
cor(reviews_per_month,last_review)
cor(number_of_reviews,last_review)
cor(reviews_per_month, number_of_reviews)
# hieruit blijkt dat reviews_per_month het sterks gecorreleerd is met de ander twee variabelen over de reviews
# daarom gaan we hier enkel verder nog reviews_per_month gebruiken in ons model.


plot(price~calculated_host_listings_count)
ab$tmp[calculated_host_listings_count<=12] = 0
ab$tmp[calculated_host_listings_count>12 & calculated_host_listings_count<=30] = 1
ab$tmp[calculated_host_listings_count>30] = 2
boxplot(price~ab$tmp, outline=FALSE)
ab$tmp = NULL
# bij het opdelen van deze data zien we dat hoe minder calculated_host_listing_count is, 
#   hoe lager de prijs is 

plot(price~availability_365)
mean(price[availability_365>100])
mean(price[availability_365<100])
boxplot(price~full,outline=FALSE)
# als we naar de prijs tov de geabstraheerde variabele full van availability_365 kijken,
#   dan zien we dat een verschil is tussen prijs als het volboekt is en als het niet volboekt is
#   aangezien availability_365 meer informatie bevat, gaan we hiermee verder

boxplot(price~city)
boxplot(price~city, outline=FALSE)
# uit de boxplot blijkt dat er een verschil is in prijs tussen Brussel en de andere twee steden
# Er is ook een klein verschil tussen Gent en Antwerpen, maar minder significant.

# conclusie:
# verder gaan met room_type, minimum_nights, reviews_per_month, 
#                   calculated_host_listings_count, availability_365 en city
#   Er zal ook eens gekeken worden naar het model met alle variabelen voor de reviews

# 2. kijken voor eventuele transformaties
plotBoxQQHist = function(X) {
  boxplot(X)
  qqnorm(X); qqline(X)
  hist(X, breaks="Sturges")
}
summary(powerTransform(price))
testTransform(powerTransform(price), -1/4)
par(mfrow=c(2,3))
plotBoxQQHist(price)
plotBoxQQHist(log10(price))
plotBoxQQHist(((price)**(-1/4)-1)/(-1/4)) # veel beter


summary(powerTransform(minimum_nights))
par(mfrow=c(2,3)) # Hoofdwet van de statistiek: kijk naar de data
plotBoxQQHist(minimum_nights)
plotBoxQQHist(log10(minimum_nights)) # geen verbetering
testTransform(powerTransform(minimum_nights), -2/3)
plotBoxQQHist(minimum_nights)
plotBoxQQHist(((minimum_nights)**(-2/3)-1)/(-2/3))  # iets beter dan de identieke of log10




summary(powerTransform(number_of_reviews))
plotBoxQQHist(number_of_reviews)
plotBoxQQHist(log10(number_of_reviews)) # stuk beter

summary(powerTransform(last_review+1))
plotBoxQQHist(last_review)
plotBoxQQHist(log10(last_review+1)) # stuk beter

summary(powerTransform(reviews_per_month))
plotBoxQQHist(reviews_per_month)
plotBoxQQHist(log10(reviews_per_month)) #stuk beter

summary(powerTransform(calculated_host_listings_count))
plotBoxQQHist(calculated_host_listings_count)
plotBoxQQHist(calculated_host_listings_count**(-1)) # beste
testTransform(powerTransform(calculated_host_listings_count), -1)
plotBoxQQHist(log10(calculated_host_listings_count))

summary(powerTransform(availability_365+1))
testTransform(powerTransform(availability_365+1), 1/4)
plotBoxQQHist(availability_365)
plotBoxQQHist((availability_365+1)**(1/4)) # niet zodanig veel beter
plotBoxQQHist(availability_365)
plotBoxQQHist(log10(availability_365+1)) # niet zodanig veel beter


par(mfrow=c(1,1))

# conclusie:
# variabele           transformatie
# price               **(-1/4)
# minimum_nights      **(-2/3)
# number_of_reviews   log10
# (last_review + 1)   log10
# reviews_per_month   log10
# calc_host_list      **(-1)
# availability_365    Identieke

#TODO model maken van variabelen zonder transformaties, met en zonder alle reviews
#     model maken van variabelen met transformaties, met en zonder alle reviews
#     multicolineariteit checken, vif command, oefneing12
#     outliers checken
#     plots pg 216 maken en het vergelijken van de modellen

{
  data = ab[,-c(1:7,17)] # data met alle relevante variabelen
  data$room_type = as.factor(data$room_type)
  data$city = as.factor(data$city)
  data2 = data[,-c(4,5)] # zelfde data zonder number_of_reviews en last_review
  
  data.transformed = data
  data.transformed$price = ((data.transformed$price)**(-1/4)-1)/(-1/4)
  data.transformed$minimum_nights = ((data.transformed$minimum_nights)**(-2/3)-1)/(-2/3)
  data.transformed$last_review = log10(data.transformed$last_review + 1)
  data.transformed$number_of_reviews = log10(data.transformed$number_of_reviews)
  data.transformed$reviews_per_month = log10(data.transformed$reviews_per_month)
  data.transformed$calculated_host_listings_count = (data.transformed$calculated_host_listings_count)**(-1) # TODO juiste boxcox transformatie
  data2.transformed = data.transformed[,-c(4,5)]
}
detach(ab)
attach(data)
# eerst beste model voor data zoeken, daarna beste model voor data2 zoeken
#   hiervan de beste van de twee nemen
{
  model1.null = lm(price~1, data=data)
  model1.full = lm(price~., data=data)
  stepAIC(model1.full, scope=list(upper=model1.full, lower=model1.null) ,direction = "both")
  
  model1.step = stepAIC(model1.full)
  summary(model1.step) # kleine Rsquared: 0.031
  anova(model1.step)
  # price ~ room_type + reviews_per_month + calculated_host_listings_count + 
  #   availability_365 + city
  library(leaps)
  model1.all = regsubsets(price~., data=data, nvmax=15)
  plot(model1.all)
  which.min(summary(model1.all)$bic) # vierde model heeft laagste BIC-score
  
  p = rowSums(summary(model1.all)$which)
  bic = summary(model1.all)$bic
  plot(p,bic)
  which.min(bic)
  attributes(summary(model1.all))
  summary(model1.all)$which[which.min(bic),]
  coef(model1.all,which.min(bic)) # coefficienten van het beste model
  summary(model1.all)$rsq[which.min(bic)] # 0.03
  # price ~ room_type + reviews_per_month + calculated_host_listings_count + 
  #   availability_365 + city
  
  # regsubsets geeft zelfde terug als stepAIC
  
  model1 = lm(price ~ room_type + reviews_per_month + calculated_host_listings_count + 
                availability_365 + city, data=data)
  summary(model1)
  
  # met interactietermen
  model1.full2 = lm(price~(minimum_nights+number_of_reviews+last_review+reviews_per_month+calculated_host_listings_count+availability_365)*city*room_type)
  model1.step2 = stepAIC(model1.full2)
  summary(model1.step2) # betere Rsquared = 0.056
  anova(model1.step2)
  
  model1.all2 = regsubsets(price~(minimum_nights+number_of_reviews+last_review+reviews_per_month+calculated_host_listings_count+availability_365+city)*room_type, nvmax=15, data=data)
  plot(model1.all2)
  summary(model1.all2)$rsq[which.min(bic)]
  summary(model1.all2)$which[which.min(bic),]
  coef(model1.all2,which.min(bic)) 
  
  model1.all2 = regsubsets(price~(minimum_nights+number_of_reviews+last_review+reviews_per_month+calculated_host_listings_count+availability_365+room_type)*city, nvmax=30, data=data)
  plot(model1.all2)
  summary(model1.all2)$rsq[which.min(bic)]
  summary(model1.all2)$which[which.min(bic),]
  coef(model1.all2,which.min(bic)) 
  # beiden minder goede Rsquare, niet mogelijk van zowel city als room_type samen te doen
  
  model1.2 = lm(price ~ minimum_nights + last_review + reviews_per_month + 
                  calculated_host_listings_count + availability_365 + city + 
                  room_type + minimum_nights:city + reviews_per_month:city + 
                  calculated_host_listings_count:city + minimum_nights:room_type + 
                  last_review:room_type + calculated_host_listings_count:room_type + 
                  city:room_type + minimum_nights:city:room_type + calculated_host_listings_count:city:room_type, data=data)
  summary(model1.2)
  # als we kijken naar de p-values dan zien we dat de meesten boven 0.05 liggen, we vinden dus eigenlijk een minder goed model
  
  detach(data)
}
# naar de getransformeerde data kijken
  
{
    attach(data.transformed)
    model2.null = lm(price~1, data=data.transformed)
    model2.full = lm(price~., data=data.transformed)
    model2.step = stepAIC(model2.full)
    summary(model2.step) # direct al een veel betere Rsquared (0.37) dan de niet-getransformeerde data
    anova(model2.step)
    
    
    model2.all = regsubsets(price~., data=data.transformed, nvmax=15)
    plot(model2.all)
    which.min(summary(model2.all)$bic) # negende model heeft laagste BIC-score
    
    p = rowSums(summary(model2.all)$which)
    bic = summary(model2.all)$bic
    plot(p,bic)
    which.min(bic)
    attributes(summary(model2.all))
    summary(model2.all)$which[which.min(bic),]
    coef(model2.all,which.min(bic)) # coefficienten van het beste model
    summary(model2.all)$rsq[which.min(bic)] # 0.37
    
    model2 = lm(price ~ room_type + minimum_nights + last_review + 
                  reviews_per_month + calculated_host_listings_count + availability_365 + 
                  city, data = data.transformed)
    summary(model2)
    
    # met interactietermen
    model2.full2 = lm(price~(minimum_nights+number_of_reviews+last_review+reviews_per_month+calculated_host_listings_count+availability_365)*city*room_type)
    model2.step2 = stepAIC(model2.full2)
    summary(model2.step2) # Rsquared = 0.38
    anova(model2.step2)
    
    model2.all2 = regsubsets(price~(minimum_nights+number_of_reviews+last_review+reviews_per_month+calculated_host_listings_count+availability_365+city)*room_type, nvmax=15, data=data.transformed)
    plot(model2.all2)
    summary(model2.all2)$rsq[which.min(bic)] # Rsq 0.37
    summary(model2.all2)$which[which.min(bic),]
    coef(model2.all2,which.min(bic)) 
    
    model2.all2 = regsubsets(price~(minimum_nights+number_of_reviews+last_review+reviews_per_month+calculated_host_listings_count+availability_365+room_type)*city, nvmax=30, data=data.transformed)
    plot(model2.all2)
    summary(model2.all2)$rsq[which.min(bic)] # Rsq 0.37
    summary(model2.all2)$which[which.min(bic),]
    coef(model2.all2,which.min(bic)) 
    # beiden minder goede Rsquare, niet mogelijk van zowel city als room_type samen te doen
    
    model2.2 = model2.step2
    summary(model2.2)
    detach(data.transformed)
}
# als we kijken naar de p-values dan zien we dat de meesten boven 0.05 liggen, we vinden dus eigenlijk een minder goed model
# de Rsq is hier wel beter iets beter (0.01), maar dit is niet zo'n groot significant verschil
#   We gaan hier verder met model1 en model2

# multicolineariteit al eens bekijken

vif(model1)
vif(model2)
# in beide gevallen geen multicolineariteit

{
  attach(data2)  
  
  model3.null = lm(price~1, data=data2)
  model3.full = lm(price~., data=data2)
  model3.step = stepAIC(model3.full)
  summary(model3.step) # kleine Rsquared: 0.032
  anova(model3.step)
  
  # zelfde model als model1, in model1 werden de last_review en number_of_reviews
  #   uit het model gehaald.
  
  # nu met de interactietermen
  model3.full2 = lm(price~(minimum_nights+reviews_per_month+calculated_host_listings_count+availability_365)*city*room_type, data=data2)
  model3.step2 = stepAIC(model3.full2)
  summary(model3.step2) # Rsquared = 0.06
  anova(model3.step2)
  
  model3.all2 = regsubsets(price~(minimum_nights+reviews_per_month+calculated_host_listings_count+availability_365+city)*room_type, nvmax=30, data=data2)
  plot(model3.all2)
  summary(model3.all2)$rsq[which.min(bic)] # Rsq 0.045
  summary(model3.all2)$which[which.min(bic),]
  coef(model3.all2,which.min(bic))
  attributes(summary(model3.all2))
  
  model3.all2 = regsubsets(price~(minimum_nights+reviews_per_month+calculated_host_listings_count+availability_365+room_type)*city, nvmax=30, data=data2)
  plot(model3.all2)
  summary(model3.all2)$rsq[which.min(bic)] # Rsq 0.036
  summary(model3.all2)$which[which.min(bic),]
  coef(model3.all2,which.min(bic))
  
  # allemaal vrij slecht, we hebben al een beter model gevonden, nl model2
  detach(data2)
}

{
  attach(data2.transformed)
  model4.null = lm(price~1, data=data2.transformed)
  model4.full = lm(price~., data=data2.transformed)
  model4.step = stepAIC(model4.full)
  summary(model4.step) # direct al een veel betere Rsquared (0.37) dan de niet-getransformeerde data
  anova(model4.step)
  #verschil met model2 is dat hier last_review er niet bij zit, Rsq is niet significant veranderd
  
  model4.all = regsubsets(price~., data=data2.transformed, nvmax=15)
  plot(model4.all)
  which.min(summary(model4.all)$bic) # 8e heeft laagste bic
  
  p = rowSums(summary(model4.all)$which)
  bic = summary(model4.all)$bic
  plot(p,bic)
  which.min(bic)
  attributes(summary(model4.all))
  summary(model4.all)$which[which.min(bic),]
  coef(model4.all,which.min(bic)) # coefficienten van het beste model
  summary(model4.all)$rsq[which.min(bic)] # 0.37
  
  model4 = model4.step
  
  # met interactietermen
  model4.full2 = lm(price~(minimum_nights+reviews_per_month+calculated_host_listings_count+availability_365)*city*room_type, data=data2.transformed)
  model4.step2 = stepAIC(model4.full2)
  summary(model4.step2) # Rsquared = 0.38
  anova(model4.step2)
  
  model4.all2 = regsubsets(price~(minimum_nights+reviews_per_month+calculated_host_listings_count+availability_365+city)*room_type, nvmax=15, data=data2.transformed)
  plot(model4.all2)
  summary(model4.all2)$rsq[which.min(bic)] # Rsq 0.37
  summary(model4.all2)$which[which.min(bic),]
  coef(model4.all2,which.min(bic)) 
  
  model4.all2 = regsubsets(price~(minimum_nights+reviews_per_month+calculated_host_listings_count+availability_365+room_type)*city, nvmax=30, data=data2.transformed)
  plot(model4.all2)
  summary(model4.all2)$rsq[which.min(bic)] # Rsq 0.37
  summary(model4.all2)$which[which.min(bic),]
  coef(model4.all2,which.min(bic)) 
  # beiden zelfde Rsquare, niet mogelijk van zowel city als room_type samen te doen
  
  model4.2 = model4.step2
  summary(model4.2)
  
  detach(data2.transformed)
}
vif(model4)
vif(model4.2)
# meer multicollineariteit in model4.2 dan in model4
# model4 is hier die de beste

anova(model4,model2) # last_review heeft geen significante bijdrage
anova(model2, model1)
library(robustbase)

# outliers eens eruit filteren voor model1 en model4
model1
model1.lts = ltsReg(price ~ room_type + reviews_per_month + calculated_host_listings_count + 
                      availability_365 + city, data = data)
summary(model1.lts)
rbind(
  ols = model1$coefficients,
  lts = model1.lts$coefficients
)
cov(model1$coefficients, model1.lts$coefficients)
# significant verschil tussen de coefficienten

summary(model4)
model4.lts = ltsReg(price ~ room_type + minimum_nights + reviews_per_month + 
                      calculated_host_listings_count + availability_365 + city, 
                    data = data2.transformed)
summary(model4.lts)
coeff4 = rbind(
  ols = model4$coefficients,
  lts = model4.lts$coefficients
)
cov(model4$coefficients, model4.lts$coefficients)
# geen significant verschil tussen de coefficienten.

par(mfrow=c(2,2))
plot(model1)
plot(model4)
plot(model1.lts)

plot(model4$fitted.values, model4$residuals)
plot(model1$fitted.values[model1$residuals<1500], model1$residuals[model1$residuals<1500])
