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





# ############     cities

table(airbnb$city) # bevat geen NA's
barplot(table(city))
boxplot(price~city)
boxplot(price[price <155]~city[price<155])

cit.lmn = lm(price~city)
qqnorm(cit.lmn$residuals)
qqline(cit.lmn$residuals)

summary(powerTransform(cit.lmn$residuals - min(cit.lmn$residuals) + 0.0001))
# indicates a log transformation is in place

cit.lms = lm(sqrt(price)~city)
qqnorm(cit.lms$residuals)
qqline(cit.lms$residuals)

cit.lml = lm(log10(price[!is.na(price)])~city[!is.na(price)])
qqnorm(cit.lml$residuals)
qqline(cit.lml$residuals)
# not normal, but is de meest gepaste geziene transformatie




e = residuals(cit.lml)
es = stdres(cit.lml)
qqnorm(es,ylab="Standardized residuals")
qqline(es)

plot(e,xlab="Index",ylab="Residuals")
# there does not seem to be correlation in time, wel meer outliers met positieve waarde


plot(cit.lml$fitted.values,e)
# ziet er redelijk aanvaardbaar uit, zelfs de variantie, behalve de outliers van de ereste

# plot of residuals vs variables not possible because categorical regressor

plot(es,xlab="Index",ylab="Standardized Residuals")
abline(h=-2.5,lty=2)
abline(h=2.5,lty=2)
# 3 very heavy outliers
# considerably more outliers on the uppper side

summary(cit.lml)
cit.aov = aov(log10(price[!is.na(price)])~city[!is.na(price)])
summary(cit.aov)

leveneTest(cit.aov)
leveneTest(cit.lml)

# verschillende variantie...
e=cit.lml$residuals
yhat=cit.lml$fitted.values
e.lm = lm(abs(e)~yhat); summary(e.lm)
w = 1/e.lm$fitted.values**2

rt.lm2 = lm(log10(price[!is.na(price)])~city[!is.na(price)], weights=w); summary(rt.lm2)
leveneTest(rt.lm2)
# geen verbetering

# verder doen met niet normaliteit

TukeyHSD(cit.aov)
# ze verschillen al van elkaar, behalve antwerpen en gent waar het wat zwakker is maar nog altijd significant
model.tables(cit.aov, type="means")
model.tables(cit.aov, type="effects")


ne.lm = lm(log10(price[!is.na(price)])~neighbourhood[!is.na(price)])
ne.aov = aov(log10(price[!is.na(price)])~neighbourhood[!is.na(price)])

summary(ne.lm)
summary(ne.aov)

nes.lm = lm(log10(price[!is.na(price)])~city[!is.na(price)] + neighbourhood[!is.na(price)])
nes.aov = aov(log10(price[!is.na(price)])~city[!is.na(price)] + neighbourhood[!is.na(price)])

summary(nes.lm)
summary(nes.aov)


pvals = summary(nes.lm)$coefficients[,4]
length(pvals[pvals<0.05])
pvals = summary(ne.lm)$coefficients[,4]
length(pvals[pvals<0.05])

# Brussel en antwerpen zijn nog steeds significant als je de neighboorhood er bijvoegd
# maar gent helemaal niet meer. 
# dit kan erop wijzen dat per wijk in gent er zeer veel verschil is, omdat als je de wijk weet het er niet ote doet dat het gent is
# door het aantal mogelijkheden zijn ze natuurlijk niet allemaal significant maar wel ongeveer de helft.
# 4 keer grotere R waarde dan enkel city
TukeyHSD(ne.aov)
# bijna allemaal niet significant
TukeyHSD(nes.aov)


head(sort(mean(price~neighbourhood)))
mean(price~neighbourhood)


neig = neighbourhood[!is.na(price)]
l = c()
k = unique(neig)
for (i in 1:length(unique(neig))) {
  l = c(l, mean(priceNoNa[neig == (k[i])]))
}
plot(l)

x = tail(sort(l, index.return=T)$ix)
k[x]
l[x]
mean(l)

for (i in 1:length(k[x])) {
  print(unique(city[neighbourhood == k[x[i]]]))
  print(k[x[i]])
}

# dus hoogste is in brussel, 2de hoogste in Gent, en de rest van de top 5 in Antwerpen
# vooral de top3 zijn veel hoger dan de rest

pl = T
for (i in 1:length(neighbourhood)) {
  if(is.na(neighbourhood[i])){
    pl = F
  }
}
pl

# dus hoogste is in brussel, 2de hoogste in Gent, en de rest van de top 5 in Antwerpen

summary(lm(price~neighbourhood))
# heel veel die niet belangrijk zijn
summary(aov(price~neighbourhood))
# signigicant verschil, maar de modelveronderstellingen zijn niet voldaan

# het lijkt erop dat dit niet er toe doet, en city meer dan genoeg is qua opsplitsing
# maar de vraagstelling wijst erop dat het waarschijnlijk wel zo is, help...


summary(lm(log10(price)~neighbourhood + room_type))
summary(aov(log10(price)~neighbourhood + room_type))
summary(aov(log10(price)~room_type + neighbourhood))

# de aov zegt dat het er nog toe doet


###########################################################
###############   Model voor de huurprijs   ###############
###########################################################

attributes(airbnb)$names
# relevante gegevens:
#   room_type:                      Entire home, Private room, Shared room
#   minimum_nights:                 minimum aantal nachten dat je moet boeken
#   city:                           Brussel, Gent, Antwerpen
#   full:                           True or False

# eerst kijken welke transformatie toegepast moeten worden
plotBoxQQHist = function(X) {
  boxplot(X)
  qqnorm(X); qqline(X)
  hist(X)
}
summary(powerTransform(price))
testTransform(powerTransform(price), -1/4)
par(mfrow=c(2,3))
plotBoxQQHist(price)
plotBoxQQHist((price)**(-1/4)) # veel beter

summary(powerTransform(minimum_nights))
par(mfrow=c(2,3)) # Hoofdwet van de statistiek: kijk naar de data
plotBoxQQHist(minimum_nights)
plotBoxQQHist(log10(minimum_nights)) # geen verbetering
testTransform(powerTransform(minimum_nights), -2/3)
plotBoxQQHist(minimum_nights)
plotBoxQQHist((minimum_nights)**(-2/3)) # iets beter dan de identieke of log10

par(mfrow=c(1,1))

# summary:
#   VARIABLE                        TRANSFORMATION
#   price:                          lambda = -1/4
#   minimum_nights:                 lambda = -2/3



# eerst enkel de numerieke waarden bekijken, daarna eventueel kijken om city, full of room_type toe te voegn

# selecteer enkel de numerieke relevante waarden van airbnb
?na.omit
airbnb = na.omit(airbnb)


data <- airbnb[,-(1:7)]; #View(data)
data<- data[,-(4:8)]; #View(data)
data$city = as.factor(data$city)
data$full = as.factor(data$full)
detach(airbnb)
# eerst kijken naar het regressie model zonder de bovenstaande transformaties
attach(data)
model.null = lm(price~1, data=data)
model.full = lm(price~., data=data)
stepAIC(model.full,direction="backward")
stepAIC(model.full,direction="both")
stepAIC(model.null,direction="forward",scope=list(upper=model.full,lower=model.null))
price.step = stepAIC(model.full)
summary(price.step) # zeer lage Rsquared, geen nuttige regressie dus, 0.02366 
anova(price.step)


detach(data)

# pas hierop de transformaties toe van hierboven
data$price = data$price**(-1/4)
data$minimum_nights = data$minimum_nights**(-2/3)
#View(data)

attach(data)
model.null = lm(price~1, data=data)
model.full = lm(price~., data=data)
stepAIC(model.full,direction="backward")
stepAIC(model.full,direction="both")
stepAIC(model.null,direction="forward",scope=list(upper=model.full,lower=model.null))
price.step = stepAIC(model.full)
summary(price.step) # groot verschil in Rsq met vorig model, maar nog altijd niet super, 0.3529 
anova(price.step)


library(leaps); ?regsubsets
price.all = regsubsets(price~., data=data) # vergelijkt meer regressies van stepAIC, die met kleinste BIC is beste
plot(price.all) # zie je welke variabelen het meest doorslaggevend zijn
summary(price.all)
summary(price.all)$outmat
summary(price.all)$which

which.min(summary(price.all)$bic)
p = rowSums(summary(price.all)$which)
bic = summary(price.all)$bic
plot(p, bic)
summary(price.all)$which[which.min(bic),]
coef(price.all, which.min(bic)) # coef van de beste regressie
summary(price.all)$adjr2[which.min(bic)] # adjsq van beste regressie, 0.3529302, basicly zelfde uitkomst als vorig model
detach(data)
# ik heb ook nog het model model.full = lm(price~.^2, data=data) bekeken (waar hij ook bv X1*X2 zal gaan toevoegen)
#   dit kwam iets ingewikkelder uit, met eenzelfde Rsq, dus ik zou voor het normale model gaan
#   
# In het verslag: misschien met die categorische variabelen een table maken bv als city=Gent dan is het dit model
#   zoals op pg 235 in de cursus
#   als we het niet zo doen, weet ik niet goed hoe we best dit model voorstellen, ik vind dit moeilijk te zien met de categorische


###########################################################
###############   beschikbaarheid van een bedrijf   #######
###########################################################
attach(airbnb)
# DIt moet met logistische regressie, dat dient voor binaire responsvariabelen.
# DUs doe alles aan de hand van dat laatste hoofdstuk over logistische regressie
attributes(airbnb)$names

# relevante data:
#   room_type
#   number_of_reviews
#   last_review
#   reviews_per_month
#   city --> als multiplicatieve variabele mss

# TODO: waarom price, de andere 2 reviews, listings, minimum nights, neighbourhood niet?

#TODO: waarom kijk je hier transormaties van de variabelen? Dat is niet nodig want dat moet niet normaal zijn
# en het maakt het enkel onmogelijk om de coefficienten te interpreteren

# eventuele transformqties bekijken
summary(powerTransform((number_of_reviews+1))) # plus 1 want moet strikt positief zijn
plotBoxQQHist(number_of_reviews+1)
plotBoxQQHist(log10(number_of_reviews+1)) # nog steeds rechtsscheef, maar iets beter al

summary(powerTransform(last_review+1))
plotBoxQQHist(last_review+1)
plotBoxQQHist(log10(last_review+1)) # ook een verbetering

summary(powerTransform(reviews_per_month))
plotBoxQQHist(reviews_per_month)
plotBoxQQHist(log10(reviews_per_month)) # verbetering
# summary:
#   VARIABLE                        TRANSFORMATION
#   number_of_reviews:              (..+1) log10
#   last_review:                    (..+1) log10
#   reviews_per_month:              log10

data <- airbnb[,-(1:7)]; #View(data)
data<- data[,-(7:8)] #View(data)
data<- data[,-(2:3)] #View(data)
data$city = as.factor(data$city)
data$full = as.factor(data$full)
detach(airbnb)
attach(data)

full.null = glm(full~1, family = binomial)
full.glm = glm(full~(log10(last_review+1)+log10(number_of_reviews+1)+log10(reviews_per_month)+room_type+city), family = binomial, data=data)
summary(full.glm)
anova(full.glm, test="LRT")



# toont grafisch hoe goed het model is, mbv predicted values
mooiFiguurtje = function(X) {
  predicted.data = data.frame(probability.of.full=X$fitted.values, full=data$full)
  predicted.data = predicted.data[order(predicted.data$probability.of.full, decreasing = FALSE),]
  predicted.data$rank <- 1:nrow(predicted.data)
  library(ggplot2)
  library(cowplot)
  ggplot(data=predicted.data, aes(x=predicted.data$rank, y = predicted.data$probability.of.full)) +
    geom_point(aes(color=full), alpha=1,shape=4,stroke=2) +
    xlab("Index") +
    ylab("predicted prob")
}
mooiFiguurtje(full.glm)
