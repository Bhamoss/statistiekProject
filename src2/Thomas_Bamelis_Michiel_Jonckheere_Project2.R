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
###############   Data Visualisation  #####################
###########################################################

names(airbnb)
for (i in 1:ncol(airbnb)) {
  print(typeof(airbnb[1,i]))
}


svg(filename="visual.svg", 
    width=3*8, 
    height=6*8, 
    pointsize=12)
par(mfrow=c(6,3))
for (i in 1:ncol(airbnb)) {
  if (names(airbnb)[i] != "name" && names(airbnb)[i] != "host_name")
  {
    if (is.numeric(airbnb[1,i]))
    {
      hist(airbnb[!is.na(airbnb[,i]) ,i], breaks =  "Sturges", main = names(airbnb)[i], xlab = names(airbnb)[i] )
    }
    else
    {
      barplot(table(airbnb[,i]), main = names(airbnb)[i], xlab = names(airbnb)[i])
    }
    
  }
  else{
    plot(1,1)
  }
}
plot(1,1)
dev.off()

par(mfrow=c(1,1))


###########################################################
###############   Ligging en type verblijf   ##############
###########################################################
priceNoNa = price[!is.na(price)]

hist(price[price < 200], breaks = "Sturges")
# zeer zware rechterstaart
summary(powerTransform(price))
powerTransform(price)

### Type verblijf

table(airbnb$room_type) # bevat geen NA's
barplot(table(room_type))
boxplot(price[price <200]~room_type[price<200])
boxplot(price~room_type)

# shapiro test waardeloos omdat er teveel samples zijn, waardoor er te makkelijk
# de null hypothese verworpen wordt
qqnorm(price)
# price helemaal niet normaal
qqnorm(log10(price))
qqline(log10(price))
qqnorm(log10(log10(price)))
qqline(log10(log10(price)))
# dit valt al wat mee maar toch

leveneTest(price~room_type)
#verschil in varianties



# hoofdstuk 14 & 15 zijn hier belangrijk voor
# gemiddelde huurprijs, dus waarschijnlijk enkel H15

# wat shit van H14 erop toegepast
rt.lm = lm(price~room_type)
summary(rt.lm)
# estimated mean price for roomtype
# suggested by this model:
# entire home = 88,544
# private = 52.921
# shared = 48.895
mean(rt.lm$residuals)
# very small/ almost zere mean for residuals, maar dit is altijd zo voor LSE blijkbaar
# de normale quantile plot van de residuals maak je altijd van de gestandardiseerde waarden
e = residuals(rt.lm)
es = stdres(rt.lm)
qqnorm(es,ylab="Standardized residuals")
qqline(es)
# very heavy right tail, not normal in its current form, skewed right
# TODO: hoe moet je dit oplossen
qqnorm(residuals(lm(sqrt(price)~room_type)))
qqline(residuals(lm(sqrt(price)~room_type)))
qqnorm(residuals(lm(log10(price)~room_type)))
qqline(residuals(lm(log10(price)~room_type)))
qqnorm(residuals(lm(log10(log10(price))~room_type)))
qqline(residuals(lm(log10(log10(price))~room_type)))
summary(powerTransform(e - min(e) + 0.0000001))

lambda = 0.09
qqnorm(residuals(lm(((price**lambda - 1)/lambda)~room_type)))
qqline(residuals(lm(((price**lambda - 1)/lambda)~room_type)))

# log 10 transformation shows best results
rt.lm = lm(log10(price)~room_type)
summary(rt.lm)
# 3th var more relevant
e = residuals(rt.lm)
es = stdres(rt.lm)
qqnorm(es,ylab="Standardized residuals")
qqline(es)

plot(e,xlab="Index",ylab="Residuals")
# there does not seem to be correlation in time, some heavy outliners though 


plot(rt.lm$fitted.values,e)
# why do we only have 3 fitted values?
# ah, probably because we regres the means out of the room_type
# should not show any correlation
# it does not really show any, excpet for the first mean, but this is because it has fewer samples which makes it less black  e bit higher
# there seems to be a difference in variance though

# plot of residuals vs variables not possible because categorical regressor

plot(es,xlab="Index",ylab="Standardized Residuals")
abline(h=-2.5,lty=2)
abline(h=2.5,lty=2)
# 3 very heavy outliers
# considerably more outliers on the uppper side

leveneTest(log10(price)~room_type)
leveneTest(rt.lm)

# TODO: probleem: heteroscedasticiteit
e=rt.lm$residuals
yhat=rt.lm$fitted.values
e.lm = lm(abs(e)~yhat); summary(e.lm)
w = 1/e.lm$fitted.values**2

rt.lm2 = lm(price[!is.na(price)]~room_type[!is.na(price)], weights=w); summary(rt.lm2)
leveneTest(rt.lm2)
# geen verbetering

# dus niet normaal (zware rechtste staart) en heteroscidasticiteit die we niet kunnen verhelpen

qqnorm(rt.lm$residuals)

rt.an = aov(log10(price)~room_type)
summary(rt.an)
# iets aan rt is zeker signigicant

# we will perform the inference analysis with these problems, so they have to be taken with a grain of salt

TukeyHSD(rt.an)
# significant verschill russen alles

boxplot((price~room_type))
boxplot(price[price <155]~room_type[price<155])
boxplot((log10(price)~room_type))
boxplot(log10(price[log10(price) <2.5])~room_type[log10(price)<2.5])


# COnclusie, rekendehouden met de niet voldane veronderstellingen besluiten we dat het room_type inderdaad een siginificante invloed heeft op de
# prijs en de prijzen ondeling significant verschillen

# cities

table(airbnb$city) # bevat geen NA's
barplot(table(city))
boxplot(price~city)
boxplot(price[price <155]~city[price<155])

TukeyHSD(aov(price~city))
# Antwerpen verschilt sterk van de rest, Gent en brussel niet verworpen
model.tables(aov(price~city), type="means")
model.tables(aov(price~city), type="effects")


TukeyHSD(aov(price~neighbourhood))
# bijna allemaal niet significant

summary(lm(price~neighbourhood))
# heel veel die niet belangrijk zijn
summary(aov(price~neighbourhood))
# signigicant verschil, maar de modelveronderstellingen zijn niet voldaan

# het lijkt erop dat dit niet er toe doet, en city meer dan genoeg is qua opsplitsing
# maar de vraagstelling wijst erop dat het waarschijnlijk wel zo is, help...

summary(lm(price~neighbourhood + room_type))
summary(aov(price~neighbourhood + room_type))
summary(aov(price~room_type + neighbourhood))

###########################################################
###############   Model voor de huurprijs   ###############
###########################################################

attributes(airbnb)$names
# relevante gegevens:
#   room_type:                      Entire home, Private room, Shared room
#   minimum_nights:                 minimum aantal nachten dat je moet boeken
#   number_of_reviews:              aantal reviews die de airbnb al kreeg
#   last_review:                    aantal dagen sinds de laatste review
#   reviews_per_month:              aantal review per maand
#   calculated_host_listings_count: 
#   availability_365:               hoeveel dagen vrij op een jaar
#   city:                           Brussel, Gent, Antwerpen
#   full:                           True or False

# eerst enkel de numerieke waarden bekijken, daarna eventueel kijken om city, full of room_type toe te voegn
detach(data)
attach(airbnb)
data = as.data.frame(cbind(price, minimum_nights,number_of_reviews,last_review,reviews_per_month,
                calculated_host_listings_count))
removeNA <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
data = removeNA(data, names(data))
detach(airbnb)
attach(data)
data.numeric = data[,0:6]
model.null = lm(price~1,data=data.numeric)
model.full = lm(price~., data=data.numeric)
stepAIC(model.null, direction = "forward",scope=list(upper=model.full,lower=model.null))
stepAIC(model.full, direction = "backward")
stepAIC(model.null, direction = "both",scope=list(upper=model.full,lower=model.null))
price.step = stepAIC(model.full)
summary(price.step) # summary geeft dus de klassieke tabel bij dit model
anova(price.step)   # analoog geeft anovamodel van onderliggende modellen
price.step$anova    # $anova-attribuut geeft anovatabel van doorlopen modellen


