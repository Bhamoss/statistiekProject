###########################################################
#################   Project src   #########################
###########################################################


library(car)



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

# TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
qqnorm(rt.lm$residuals)
qqnorm(lm(log10(price)~room_type)$residuals)

rt.an = aov(price~room_type)
summary(rt.an)
# iets aan rt is zeker signigicant

TukeyHSD(rt.an)
# significant verschill russen home/apt en de andere twee, maar geen significant verschil tussen private en shared

# cities

table(airbnb$city) # bevat geen NA's
barplot(table(city))
boxplot(price~city)
boxplot(price[price <200]~city[price<200])

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
