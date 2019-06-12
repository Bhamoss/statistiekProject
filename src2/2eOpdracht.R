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
testTransform(powerTransform(price), -1/5)
par(mfrow=c(2,3))
plotBoxQQHist(price)
plotBoxQQHist((price)**(-1/5)) # veel beter

summary(powerTransform(minimum_nights))
par(mfrow=c(2,3)) # Hoofdwet van de statistiek: kijk naar de data
plotBoxQQHist(minimum_nights)
plotBoxQQHist(log10(minimum_nights)) # geen verbetering
testTransform(powerTransform(minimum_nights), -2/3)
plotBoxQQHist(minimum_nights)
plotBoxQQHist((minimum_nights)**(-2/3)) # iets beter dan de identieke of log10

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
# price               **(-1/5)
# minimum_nights      **(-2/3)
# number_of_reviews   log10
# (last_review + 1)   log10
# reviews_per_month   log10
# calc_host_list      **(-1)
# availability_365    Identieke
