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
{
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
}
###########################################################
###############   Data Visualisation  #####################
###########################################################
{
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
}

###########################################################
###############   Ligging en type verblijf   ##############
###########################################################
{ 
  priceNoNa = price[!is.na(price)]
  
  
  jpeg("prijsVis.jpg")
  hist(price[price < 200], breaks = "Sturges")
  dev.off()
  # zeer zware rechterstaart
  summary(powerTransform(price))
  powerTransform(price)
  
  ### Type verblijf
  
  table(airbnb$room_type) # bevat geen NA's
  barplot(table(room_type))
  
  
  jpeg("boxplotPrijsCity.jpg")
  boxplot(price[price <200]~room_type[price<200])
  dev.off()
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # Todo: bovenste figuur in verslag
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  boxplot(price~room_type)
  length(price)
  length(price[price < 1500 ]) -   length(price[price < 150 ])
  length(price[price < 150 ])
  length(price[price > 1500 ])
  # shapiro test waardeloos omdat er teveel samples zijn, waardoor er te makkelijk
  # de null hypothese verworpen wordt
  jpeg("qqp.jpg")
  qqnorm(price)
  qqline(price)
  dev.off()
  
  jpeg("qqlp.jpg")
  qqnorm(log10(price))
  qqline(log10(price))
  dev.off()
  
  summary(powerTransform(price))
  powerTransform(price)
  jpeg("qqbp.jpg")
  lambda = -0.25
  qqnorm((price**lambda - 1)/lambda)
  qqline((price**lambda - 1)/lambda)
  dev.off()
  
  
  
  jpeg("prijsB.jpg")
  hist((price**lambda - 1)/lambda, breaks = "Sturges")
  dev.off()
  
  
  # price helemaal niet normaal
  qqnorm(log10(price))
  qqline(log10(price))
  hist(log10(price))
  qqnorm(log10(log10(price)))
  qqline(log10(log10(price)))
  hist(log10(log10(price)))
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
  
  lambda = -0.25
  qqnorm(residuals(lm(((price**lambda - 1)/lambda)~room_type)))
  qqline(residuals(lm(((price**lambda - 1)/lambda)~room_type)))
  
  ###############################################################
  # gedaan met spelen
  
  lambda = -0.25
  boxprice = ((price**lambda - 1)/lambda)
  shapiro.test(na.omit(boxprice))
   # box cox -1/4 shows best results
  rt.lm = lm(boxprice~room_type)
  summary(rt.lm)
  # 3th var more relevant
  
  leveneTest(aov(rt.lm))
  
  summary(aov(rt.lm))
  
  
  jpeg("pcqq.jpeg")
  e = residuals(rt.lm)
  es = stdres(rt.lm)
  qqnorm(es,ylab="Standardized residuals")
  qqline(es)
  dev.off()
  
  
  
  plot(e,xlab="Index",ylab="Residuals")
  # there does not seem to be correlation in time, some heavy outliners though 
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # Todo: bovenste figuur in verslag
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  plot(rt.lm$fitted.values,e)
  # why do we only have 3 fitted values?
  # ah, probably because we regres the means out of the room_type
  # should not show any correlation
  # it does not really show any, excpet for the first mean, but this is because it has fewer samples which makes it less black  e bit higher
  # there seems to be a difference in variance though
  
  # plot of residuals vs variables not possible because categorical regressor
  jpeg("pcisr.jpeg")
  plot(es,xlab="Index",ylab="Standardized Residuals")
  abline(h=-2.5,lty=2, col = "red", lwd = 4)
  abline(h=2.5,lty=2, col = "red", lwd = 4)
  dev.off()
  # 3 very heavy outliers
  # considerably more outliers on the uppper side
  
  
  e=rt.lm$residuals
  yhat=rt.lm$fitted.values
  e.lm = lm(abs(e)~yhat); summary(e.lm)
  w = 1/e.lm$fitted.values**2
  
  rt.lm2 = lm(price[!is.na(price)]~room_type[!is.na(price)], weights=w); summary(rt.lm2)
  leveneTest(rt.lm2)
  # geen verbetering
  
  # dus niet normaal (zware rechtste staart) en heteroscidasticiteit die we niet kunnen verhelpen
  
  qqnorm(rt.lm$residuals)
  
  rt.an = aov(boxprice~room_type)
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
  jpeg("boxprijscity.jpg")
  boxplot(price[price <150]~city[price<150])
  dev.off()
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # Todo: bovenste figuur in verslag
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
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
  
  cit.box = lm(boxprice[!is.na(price)]~city[!is.na(price)])
  
  qqnorm(cit.box$residuals)
  qqline(cit.box$residuals)
  
  
  e = residuals(cit.box)
  es = stdres(cit.box)
  jpeg("qqboxcit.jpg")
  qqnorm(es,ylab="Standardized residuals")
  qqline(es)
  dev.off()
  
  jpeg("indc.jpg")
  plot(es,xlab="Index",ylab="Residuals")
  abline(h=-2.5,lty=2, col = "red", lwd = 4)
  abline(h=2.5,lty=2, col = "red", lwd = 4)
  dev.off()
  # there does not seem to be correlation in time, wel meer outliers met positieve waarde
  
  
  plot(cit.box$fitted.values,e)
  # ziet er redelijk aanvaardbaar uit, zelfs de variantie, behalve de outliers van de ereste
  
  # plot of residuals vs variables not possible because categorical regressor
  
  plot(es,xlab="Index",ylab="Standardized Residuals")
  abline(h=-2.5,lty=2)
  abline(h=2.5,lty=2)
  # 3 very heavy outliers
  # considerably more outliers on the uppper side
  
  summary(cit.box)
  summary(aov(cit.box))
  

  leveneTest(cit.box)
  
  
  
  # verder doen met niet normaliteit
  
  TukeyHSD(aov(cit.box))
  # ze verschillen al van elkaar, behalve antwerpen en gent waar het wat zwakker is maar nog altijd significant
  model.tables(aov(cit.box), type="means")
  model.tables(aov(cit.box), type="effects")
  
  ####################### neighbourhood
  
  ne.lm = lm(boxprice[!is.na(price)]~neighbourhood[!is.na(price)])
  ne.aov = aov(ne.lm)
  
  jpeg("boxne.jpg")
  boxplot(boxprice~neighbourhood)
  dev.off()
  
  which(is.infinite(stdres(ne.lm)))
  which(is.na(stdres(ne.lm)))
  jpeg("qqne.jpg")
  qqnorm(stdres(ne.lm)[!is.infinite(stdres(ne.lm))], ylab="Standardized residuals")
  qqline(stdres(ne.lm))
  dev.off()
  
  jpeg("stdne.jpg")
  plot(stdres(ne.lm),xlab="Index",ylab="Standardized Residuals")
  abline(h=-2.5,lty=2, col = "red", lwd = 4)
  abline(h=2.5,lty=2, col = "red", lwd = 4)
  dev.off()
  
  summary(ne.lm)
  summary(ne.aov)
  leveneTest(ne.aov)
  k = TukeyHSD(ne.aov)
  length(k$`neighbourhood[!is.na(price)]`[k$`neighbourhood[!is.na(price)]` < 0.05]) / length(k$`neighbourhood[!is.na(price)]`)
  
  
  # city en neigborhood
  
  nes.lm = lm(boxprice[!is.na(price)]~city[!is.na(price)] + neighbourhood[!is.na(price)])
  nes.aov = aov(nes.lm)
  
  summary(nes.lm)
  summary(nes.aov)
  
  
  which(is.infinite(stdres(nes.lm)))
  which(is.na(stdres(nes.lm)))
  jpeg("qqnes.jpg")
  qqnorm(stdres(nes.lm)[!is.infinite(stdres(nes.lm))], ylab="Standardized residuals")
  qqline(stdres(nes.lm))
  dev.off()
  
  jpeg("stdnes.jpg")
  plot(stdres(nes.lm),xlab="Index",ylab="Standardized Residuals")
  abline(h=-2.5,lty=2, col = "red", lwd = 4)
  abline(h=2.5,lty=2, col = "red", lwd = 4)
  dev.off()
  
  summary(nes.lm)
  summary(nes.aov)
  leveneTest(nes.aov)
  k = TukeyHSD(nes.aov)
  k
  length(k$`neighbourhood[!is.na(price)]`[k$`neighbourhood[!is.na(price)]` < 0.05]) / length(k$`neighbourhood[!is.na(price)]`)
  
  
  pvals = summary(nes.lm)$coefficients[,4]
  length(pvals[pvals<0.05])
  length(pvals[pvals>=0.05])
  length(pvals)
  # woluwe en zorvel zijn NA? wtf?
  pvals = summary(ne.lm)$coefficients[,4]
  length(pvals[pvals<0.05])
  length(pvals[pvals>=0.05])
  length(pvals)
  
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
  jpeg("pn.jpg")
  plot(l)
  dev.off()
  
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # Todo: bovenste figuur in verslag
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  x = head(sort(l, index.return=T)$ix)[1:5]
  k[x]
  l[x]
  mean(l)
  
  x = tail(sort(l, index.return=T)$ix)[1:5]
  k[x]
  l[x]
  mean(l)
  
  
  #[1] Woluwe-Saint-Pierre Sint Denijs Westrem Oud - Berchem       Eilandje            Polder              Stadspark          
  #96 Levels: Binnenstad Bloemekenswijk Brugse Poort - Rooigem Dampoort Drongen Elisabethbegijnhof - Papegaai Gentbrugge Ledeberg ... Woluwe-Saint-Pierre
  #> l[x]
  #[1] 103.1416 127.3333 146.8571 200.8485 217.5000 239.7847
  #> mean(l)
  #[1] 72.20993
  
  # dus hoogste is in brussel, 2de hoogste in Gent, en de rest van de top 5 in Antwerpen
  # vooral de top3 zijn veel hoger dan de rest
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # Todo: bovenste data in verslag voor top 5
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  
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
  nes.lm = lm(boxprice[!is.na(price)]~room_type[!is.na(price)] + neighbourhood[!is.na(price)])
  nes.aov = aov(nes.lm)
  
  summary(nes.lm)
  summary(nes.aov)
  
  
  which(is.infinite(stdres(nes.lm)))
  which(is.na(stdres(nes.lm)))
  jpeg("qqnes.jpg")
  qqnorm(stdres(nes.lm)[!is.infinite(stdres(nes.lm))], ylab="Standardized residuals")
  qqline(stdres(nes.lm))
  dev.off()
  
  jpeg("stdnes.jpg")
  plot(stdres(nes.lm),xlab="Index",ylab="Standardized Residuals")
  abline(h=-2.5,lty=2, col = "red", lwd = 4)
  abline(h=2.5,lty=2, col = "red", lwd = 4)
  dev.off()
  
  summary(nes.lm)
  summary(nes.aov)
  leveneTest(nes.aov)
  k = TukeyHSD(nes.aov)
  k
  length(k$`neighbourhood[!is.na(price)]`[k$`neighbourhood[!is.na(price)]` < 0.05]) / length(k$`neighbourhood[!is.na(price)]`)
  
  
  pvals = summary(nes.lm)$coefficients[,4]
  length(pvals[pvals<0.05])
  length(pvals[pvals>=0.05])
  length(pvals)
}
###########################################################
###############   Model voor de huurprijs   ###############
###########################################################
{  ab=na.omit(airbnb)
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
  plotBoxQQHist((calculated_host_listings_count**(-1)-1)/(-1)) # beste
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
    data.transformed$calculated_host_listings_count = ((data.transformed$calculated_host_listings_count)**(-1)-1)/(-1)
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
    summary(model1.step) # kleine Rsquared: 0.032
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
    summary(model1.step2) # betere Rsquared = 0.059
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
  summary(model2)
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
    summary(model3.step2) # Rsquared = 0.059
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
  attach(data)
  model1.lts = ltsReg(price ~ reviews_per_month + calculated_host_listings_count + 
                        availability_365+city+room_type, data = data, alpha=0.75 )
  summary(model1.lts) # rsq 0.45
  qqnorm(model1.lts$residuals)
  rbind(
    ols = model1$coefficients,
    lts = model1.lts$coefficients
  )
  sd(model1$coefficients)
  sd(model1.lts$coefficients)
  # significant verschil tussen de coefficienten
  
  y = scale(model1.lts$residuals)
  Mr = covMcd(model1.lts$model[,-c(1,5,6)], alpha = 1)$center; Mr
  Cr = covMcd(model1.lts$model[,-c(1,5,6)], alpha = 1)$cov; Cr
  x = mahalanobis(model1.lts$model[,-c(1,5,6)], Mr, Cr)
  plot(x,y, xlab="Mahanalobis afstand", ylab="Standardized LTS residuals")
  y0=qnorm(.975)
  abline(h=c(-y0,y0),col='red') # geen echte outliers, behalve het verwachte aantal overschrijdingen
  data$price[abs(y)>y0] # high residual points
  x0=qchisq(.975,2) 
  abline(v=x0,col='red') # behoorlijk wat punten met hoge leverage
  data$price[x>x0 & abs(y)>y0] # high leverage points
  bad = ab[which(x>x0 & abs(y)>y0),]; bad 
  nrow(ab[which(abs(y)>y0),]) # 77
  nrow(ab[which(x>x0 & abs(y)>y0),]) # 9
  nrow(ab[which(x>x0),]) # 1079
  ab[which(abs(y)>20),]
  
  y = scale(model1.lts$residuals[scale(model1.lts$residuals)<20])
  Mr = covMcd(model1.lts$model[scale(model1.lts$residuals)<20,-c(1,5,6)], alpha = 1)$center; Mr
  Cr = covMcd(model1.lts$model[scale(model1.lts$residuals)<20,-c(1,5,6)], alpha = 1)$cov; Cr
  x = mahalanobis(model1.lts$model[scale(model1.lts$residuals)<20,-c(1,5,6)], Mr, Cr)
  plot(x,y, xlab="Mahanalobis afstand", ylab="Standardized LTS residuals")
  y0=qnorm(.975)
  abline(h=c(-y0,y0),col='red') # geen echte outliers, behalve het verwachte aantal overschrijdingen
  data$price[abs(y)>y0] # high residual points
  x0=qchisq(.975,2) # 2.5% dus bij chikwadraatverdeling: 2 a 3-tal elementen buiten deze grenzen  
  abline(v=x0,col='red') # behoorlijk wat punten met hoge leverage
  
  summary(model4)
  model4.lts = ltsReg(price ~ room_type + minimum_nights + reviews_per_month + 
                        calculated_host_listings_count + availability_365 + city, 
                      data = data2.transformed)
  summary(model4.lts)
  coeff4 = rbind(
    ols = model4$coefficients,
    lts = model4.lts$coefficients
  )
  sd(model4$coefficients)
  sd(model4.lts$coefficients)
  # geen significant verschil tussen de coefficienten.
  
  model2
  model2.lts = ltsReg(price ~ room_type + minimum_nights + last_review + 
                        reviews_per_month + calculated_host_listings_count + availability_365 + 
                        city, data = data.transformed)
  
  summary(model2.lts)
  rbind(
    ols = model2$coefficients,
    lts = model2.lts$coefficients
  )
  
  rbind(
    ols = model2$coefficients[-c(5)],
    lts = model4$coefficients
  )
  par(mfrow=c(2,2))
  plot(model1)
  plot(model4)
  plot(model2)
  plot(model1.lts)
  plot(model1.lts$fitted.values[model1.lts$residuals<1000],model1.lts$residuals[model1.lts$residuals<1000])
  
  plot(model4$fitted.values, model4$residuals)
  plot(model1$fitted.values[model1$residuals<1500], model1$residuals[model1$residuals<1500])
  
 
}
###########################################################
###############   beschikbaarheid van een bedrijf   #######
###########################################################
{
  detach(airbnb)
  attach(airbnb)
  nrow(na.omit(airbnb))
  table(airbnb$city)
  table(na.omit(airbnb)$city)
  table(airbnb$full)
  table(na.omit(airbnb)$full)
  # evenredige verwijdering, dus we doen hier verder zonder omits
  ab = na.omit(airbnb)
  ab$availability_365 = NULL
  detach(airbnb)
  attach(ab)
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
  
  # id achterwege gelaten, net zoals de naam en de hostnaam omdat deze te uniek zijn.
  # host_id achterwege omdat idd bepaalde hosts bepaalde soort apartementen verkopen,
  # maar aangezien deze volledig los staat van de id van de host zal dit gewoon random zijn.
  # dit verduidelijk ik hieronder
  n = c()
  p = c()
  for (i in 1:length(host_id)) {
    k = full[host_id == host_id[i]]
    k = k[k == F]
    n = c(n, length(k))
    k = full[host_id == host_id[i]]
    k = k[k == T]
    p = c(p, length(k))
  }
  barplot(n)
  barplot(p)
  # alhoewel er wel een positief verband lijkt te zijn
  
  cor(last_review,reviews_per_month)
  cor(last_review, number_of_reviews)
  cor(reviews_per_month, number_of_reviews)
  boxplot(full, reviews_per_month)
  boxplot(full, last_review)
  boxplot(full, number_of_reviews)
  plot(reviews_per_month, last_review)
  plot(reviews_per_month, number_of_reviews)

  boxplot(full, calculated_host_listings_count)
  # calculated host listings count lijkt toch verschillend te zijn voor het al dan niet volzitten van het ding, dus nemen we het mee
  
  boxplot(full, minimum_nights)
  mean(minimum_nights[full])
  mean(minimum_nights[!full])
  # zelfde in mindere mate voor minimum nachten
  
  
  
  
  
  
  
  
  
   # variabelen selecteren
  
  model.null =  glm(full ~ 1, family = binomial)
  model.full =  glm(as.factor(full)~price + longitude + latitude + last_review + number_of_reviews + reviews_per_month + as.factor(room_type) + as.factor(neighbourhood)  + minimum_nights +  calculated_host_listings_count + as.factor(city),  family = binomial)
  stepAIC(model.full, list(upper = ~price + longitude + latitude + last_review + number_of_reviews + reviews_per_month + as.factor(room_type) + as.factor(neighbourhood)  + minimum_nights +  calculated_host_listings_count + as.factor(city), lower = ~ 1) , direction="backward")
  #price + longitude + latitude + last_review + 
  #  number_of_reviews + reviews_per_month + as.factor(room_type) + 
  #  minimum_nights + calculated_host_listings_count
  stepAIC(model.full, list(upper = ~price + longitude + latitude + last_review + number_of_reviews + reviews_per_month + as.factor(room_type) + as.factor(neighbourhood)  + minimum_nights +  calculated_host_listings_count + as.factor(city), lower = ~ 1) , direction="both")
  #price + longitude + latitude + last_review + 
  #  number_of_reviews + reviews_per_month + as.factor(room_type) + 
  #  minimum_nights + calculated_host_listings_count
  # same as above
  
  stepAIC(model.null,direction="forward",scope=list(upper=model.full,lower=model.null))
  #last_review + calculated_host_listings_count + reviews_per_month + 
  #  minimum_nights + price + number_of_reviews + as.factor(city) + 
  #  as.factor(room_type)
  # longtitude and latitude weg, last review weg, city erbij
  
  summary(model.full)
  # neighbourhood weg, long en lat weg, alle 3 de reviews significant, room type niet significant, kan door correlatie zijn
  
  # omgekeerde volgorde
  
  
  # as.factor(city) + calculated_host_listings_count + minimum_nights + as.factor(neighbourhood)  +  as.factor(room_type) + reviews_per_month +   number_of_reviews + last_review + latitude + longitude + price 
  
  model.null =  glm(full ~ 1, family = binomial)
  model.full =  glm(as.factor(full)~as.factor(city) + calculated_host_listings_count + minimum_nights + as.factor(neighbourhood)  +  as.factor(room_type) + reviews_per_month +   number_of_reviews + last_review + latitude + longitude + price ,  family = binomial)
  stepAIC(model.full, list(upper = ~as.factor(city) + calculated_host_listings_count + minimum_nights + as.factor(neighbourhood)  +  as.factor(room_type) + reviews_per_month +   number_of_reviews + last_review + latitude + longitude + price , lower = ~ 1) , direction="backward")
  #calculated_host_listings_count + minimum_nights + 
  #  as.factor(room_type) + reviews_per_month + number_of_reviews + 
  #  last_review + latitude + longitude + price
  stepAIC(model.full, list(upper = ~as.factor(city) + calculated_host_listings_count + minimum_nights + as.factor(neighbourhood)  +  as.factor(room_type) + reviews_per_month +   number_of_reviews + last_review + latitude + longitude + price , lower = ~ 1) , direction="both")
  #calculated_host_listings_count + minimum_nights + 
  #  as.factor(room_type) + reviews_per_month + number_of_reviews + 
  #  last_review + latitude + longitude + price
  
  stepAIC(model.null, list(upper = ~as.factor(city) + calculated_host_listings_count + minimum_nights + as.factor(neighbourhood)  +  as.factor(room_type) + reviews_per_month +   number_of_reviews + last_review + latitude + longitude + price , lower = ~ 1) , direction="forward")
  
  #last_review + calculated_host_listings_count + reviews_per_month + 
  #minimum_nights + price + number_of_reviews + as.factor(city) + 
  #  as.factor(room_type)
  
  summary(model.full)
  # door correlatie is dit allesbehalve ideaal, maar bevestigt wel wat we al wisten
  # longitude, latitude, city, neighbourhood zijn sterk gecoreleerd
  # reviews zijn ook gecoreleerd, maar ze kunnen niet door 1 vervangen worden
  
  # volgende zitten er altijd in:
  # price + as.factor(room_type) + 
  #  minimum_nights + calculated_host_listings_count 
  # + last_review + number of reviews + reviews_per_month
  
  # 4 longitutde, 4 latitude, 0 neighbourhood, 2 keer city
  # dus longitude en latitude vs city
  
  #TODO: interactietermen en regsubsets
  
  model.step = stepAIC(model.full)
  #steunt latitude
  #- longitude                       1   7461.5 7481.5
  #- as.factor(room_type)            2   7465.3 7483.3
  #- latitude                        1   7475.9 7495.9
  #- number_of_reviews               1   7488.4 7508.4
  #- price                           1   7527.5 7547.5
  #- reviews_per_month               1   7538.1 7558.1
  #- calculated_host_listings_count  1   7580.4 7600.4
  #- minimum_nights                  1   7591.8 7611.8
  #- last_review                     1   8687.5 8707.5
  
  model.ll = glm(as.factor(full)~price + longitude + latitude + last_review + number_of_reviews + reviews_per_month + as.factor(room_type) + minimum_nights +  calculated_host_listings_count,  family = binomial)
  model.cit = glm(as.factor(full)~price  + last_review + number_of_reviews + reviews_per_month + as.factor(room_type)  + minimum_nights +  calculated_host_listings_count + as.factor(city),  family = binomial)
  
  summary(model.ll) # wald test test of de coef 0 is
  # verwerpt room type en zwakke longitutde
  #aic 7475 en dev 7453
  anova(model.ll,test="LRT") # Lrt heeft als 0 hypothese dat een groep coef 0 zijn.
  # verwerpt longitutde en room type
  1-pchisq(model.ll$deviance, model.ll$df.residual)
  1-pchisq(
    model.ll$null.deviance-model.ll$deviance,
    model.ll$df.null-model.ll$df.residual)
  plot(residuals(model.ll), ylab="Deviance residuals ipv andere") # 1 hele zware oultier
  
  cor(longitude, latitude)
  
  summary(model.cit) # wald test test of de coef 0 is
  # verwerpt room type 
  # 7475 aic en 7453 dev
  anova(model.cit,test="LRT") # Lrt heeft als 0 hypothese dat een groep coef 0 zijn.
  # verwerpt room type 
  1-pchisq(model.cit$deviance, model.cit$df.residual)
  1-pchisq(
    model.cit$null.deviance-model.cit$deviance,
    model.cit$df.null-model.cit$df.residual)
  plot(residuals(model.cit), ylab="Deviance residuals ipv andere") # 1 hele zware oultier
  
  # beiden verwerpen room type
  
  # longitude en room type weglaten
  model.ll = glm(as.factor(full)~price +  latitude + last_review + number_of_reviews + reviews_per_month + minimum_nights +  calculated_host_listings_count,  family = binomial)
  model.cit = glm(as.factor(full)~price  + last_review + number_of_reviews + reviews_per_month   + minimum_nights +  calculated_host_listings_count + as.factor(city),  family = binomial)
  
  summary(model.ll) # wald test test of de coef 0 is
  #aic 7488 en dev 7472
  anova(model.ll,test="LRT") # Lrt heeft als 0 hypothese dat een groep coef 0 zijn.
  1-pchisq(model.ll$deviance, model.ll$df.residual)
  plot(residuals(model.ll), ylab="Deviance residuals ipv andere") # 1 hele zware oultier
  
  cor(longitude, latitude)
  
  summary(model.cit) # wald test test of de coef 0 is
  # 7483 aic en 7465 dev
  anova(model.cit,test="LRT") # Lrt heeft als 0 hypothese dat een groep coef 0 zijn.
  1-pchisq(model.cit$deviance, model.cit$df.residual)
  plot(residuals(model.cit), ylab="Deviance residuals ipv andere") # 1 hele zware oultier
  
  # we kiezen dus voor het eerste cit omdat de aic daar het laagst was, ookal is roomtype niet verworpen
  
  data = ab[,6:16]
  library(leaps)
  model.regs = regsubsets(full~., data=data, nvmax=12)
  plot(model.regs)
  summary(model.regs)
  # latitude en longitude eerder geselecteerd dan steden :/
  
  
  
  model.f = glm(as.factor(full)~ price  + last_review + number_of_reviews + reviews_per_month + as.factor(room_type)  + minimum_nights +  calculated_host_listings_count + as.factor(city),  family = binomial)
  # als je price wegdoet krijg je die warning niet
  plot(price)
  which(price > 2000)
  ab[3887,]
  ab[5419,]
  boxplot(price~full)
  # ik laat ze weg, we zien nog wat we er mee doen
  
  
  
  #TODO: michiel haalt betere AIC na transorfmaties te doen, dus we kunnen dit opnieuw doen maar met de getransformeerde shit
  
  
  
    ab$price = ((ab$price)**(-1/4)-1)/(-1/4)
    ab$minimum_nights = ((ab$minimum_nights)**(-2/3)-1)/(-2/3)
    ab$last_review = log10(ab$last_review + 1)
    ab$number_of_reviews = log10(ab$number_of_reviews)
    ab$reviews_per_month = log10(ab$reviews_per_month)
    ab$calculated_host_listings_count = ((ab$calculated_host_listings_count)**(-1)/(-1)) # TODO juiste boxcox transformatie
  

  attach(ab)
  
    #commentaar is van voor transformatie
    # de price selecteerd nu ook alles door de transformatie
  model.fp = glm(as.factor(full[price < 2000])~ price[price < 2000]  + last_review[price < 2000] + number_of_reviews[price < 2000] + reviews_per_month[price < 2000] + as.factor(room_type)[price < 2000]  + minimum_nights[price < 2000] +  calculated_host_listings_count[price < 2000] + as.factor(city)[price < 2000],  family = binomial)
  # het is door die outliers, wat doe je dan?
  
  # kwaliteit
  summary(model.fp) # wald test test of de coef 0 is
  # room type nu niet verworpen lol
  # alles slaagt, dus goed, AIC van 7343.6
  anova(model.fp,test="LRT") # Lrt heeft als 0 hypothese dat een groep coef 0 zijn.
  # alles slaagt opnieuw
  plot(residuals(model.fp), ylab="Deviance residuals ipv andere") # vreemde driehoeken
  # zeer vreemde driehoek. 
  # TODO: bespreek
  
  
  bgof = function(model){
    1-pchisq(model$null.deviance - model$deviance, model$df.null - model$df.residual)
  }
  
  bgof(model.fp)
  # goodness of fit geslaagd.
  
  
  
  
  
  
  # problemen
  
  plot(residuals(model.fp), ylab="Deviance residuals ipv andere") # vreemde driehoeken
  # zeer vreemde driehoek. 
  # TODO: bespreek
  
  #########################################
  # de commentaar was de waarde van de coefficienten zonder transformaties
  # dus neem dit van het model zonder om te kunnen reproduceren
  
  # coefficienten bespreken (odds-ratio)
  coef = model.fp$coefficients
  coef
  #  (Intercept)                                    -0.5378595
  #  price[price < 2000]                            -0.0122283  
  #  last_review[price < 2000]                       0.0035141  
  #  number_of_reviews[price < 2000]                -0.0068606  
  #  reviews_per_month[price < 2000]                -0.2496038 
  #  as.factor(room_type)[price < 2000]Private room -0.3347014 
  #  as.factor(room_type)[price < 2000]Shared room  -0.7143295  
  #  minimum_nights[price < 2000]                   -0.0298897 
  #  calculated_host_listings_count[price < 2000]   -0.0411858 
  #  as.factor(city)[price < 2000]Brussel            0.3947290 
  #  as.factor(city)[price < 2000]Gent               0.4652026 
  
  exp(coef)
  
  
  
  # iemand in brussel heeft
  1.4839819/0.5839970 -1
  # 54 procent meer odds dat het vol zit dan in antwerpen
  # iemand in gent  
  1.5923367/0.5839970 -1
  # 73% meer kans dat het vol zit dan in antwerpen
  
  #iemand die een private room zoekt
  0.7155517/0.5839970 -1
  # heeft 22.5 procent meer kans dat het vol zit dan bij een entrier home app
  # iemand die een shared room zoekt 
  0.4895202/0.5839970 -1
  # heeft 16.2 procent MInder kans dat het vol zit dan bij een entire home/app  
  
  exp(coef) - 1
  # dussssss (zie voorbeeld p 369)
  # TODO: zoek de juiste terminologie op tussen kans en odds
  # TODO: bespreek standard case = in Antwerpenen entire home/appartement
  # als de prijs 1 euro meer is heb je 1.2% minder kans dat het apartement vol zit
  # als de laatste review 1 dag langer geleden is dan heb je 0.35% meer kans dat het appartement vol zit
  # als er 1 review meer is (overall) dan is er 0.7% minder kans dat het appartement vol zit
  # als er 1 review meer is geweest in de laatste maand heb je 22% minder kanns dat het vol zit
  # ...
  # meer kans dat het appartement vol zit als:
  # price -     (1.2)
  # last_review  + (0.3)
  # num_rev - (0.7)
  # rev_mon - (22.1)
  #min_nights - (3)
  # cal_list - (4)
  
  # Cleaning
  detach(ab)
  attach(airbnb)
  
  ################################################################################################################
  #####################################        Michiel
  ################################################################################################################
  
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
}
