### Transformaties

## Prestige
library(car)
attach(Prestige)

summary(powerTransform(education)) # log10 best, maar identieke wordt niet sterk verworpen (vergelijk met andere voorbeelden)
par(mfrow=c(2,3)) # Hoofdwet van de statistiek: kijk naar de data
boxplot(education) 
qqnorm(education); qqline(education)
hist(education) # symmetrisch, eerder uniform, misschien twee groepen?
boxplot(log10(education))
qqnorm(log10(education)); qqline(log10(education))
hist(log10(education)) # geen verbetering

summary(powerTransform(income)) # log10 zeer uitgesproken boven identieke
par(mfrow=c(2,3))
boxplot(income) 
qqnorm(income); qqline(income)
hist(income)
boxplot(log10(income))
qqnorm(log10(income)); qqline(log10(income))
hist(log10(income)) # duidelijke verbetering

summary(powerTransform(prestige)) # log10 en identieke niet zo veel verschillend
par(mfrow=c(2,3))
boxplot(prestige) 
qqnorm(prestige); qqline(prestige)
hist(prestige)
boxplot(log10(prestige))
qqnorm(log10(prestige)); qqline(log10(prestige))
hist(log10(prestige)) # geen duidelijke verbetering

summary(powerTransform(women+1)) # women is eigenlijk proportie: eerder logit dan boxcox
par(mfrow=c(2,3))
boxplot(women) 
qqnorm(women); qqline(women)
hist(women) # scheef (proporties vaak piek bij zowel 0 als 1)
boxplot(log10(women+1))
qqnorm(log10(women+1)); qqline(log10(women+1))
hist(log10(women+1)) # totaal geen verbetering

par(mfrow=c(2,3))
boxplot(women) 
qqnorm(women); qqline(women)
hist(women) # scheef (proporties vaak piek bij zowel 0 als 1 of eerder uniform, waardoor log10 niet geschikt is)
boxplot(car::logit(women)) # car::logit transformeert automatisch (warning)
qqnorm(car::logit(women)); qqline(car::logit(women))
hist(car::logit(women)) # lichte verbetering (logit vooral conceptueel belangrijk)

par(mfrow=c(2,3))
boxplot(women) 
qqnorm(women); qqline(women)
hist(women)
boxplot(qnorm((women+1)/100)) # manueel transformeren!
qqnorm(qnorm((women+1)/100)); qqline(qnorm((women+1)/100))
hist(qnorm((women+1)/100)) # zeer vergelijkbaar met vorige

par(mfrow=c(2,3))
boxplot(women) 
qqnorm(women); qqline(women)
hist(women)
boxplot(asin(sqrt(women/100))) # nul en 1 mogen meedoen + variantie stabiliserend
qqnorm(asin(sqrt(women/100))); qqline(asin(sqrt(women/100)))
hist(asin(sqrt(women/100)))  # zeer vergelijkbaar met vorige

par(mfrow=c(2,2))
model.lin = lm(prestige~education+women+income,data=Prestige)
model.trans = lm(prestige~education+car::logit(women)+log10(income),data=Prestige)
plot(model.lin)
plot(model.trans)
# zonder transfo lichte ophoping in linkerhelft residuplot, betere spreiding na transformaties, minder belangrijke outliers?
summary(powerTransform(model.trans))
# residuen vragen geen verdere transformatie

# Extra:
Prestige.c = cbind(prestige,education,women+1,income)
pT = powerTransform(Prestige.c)
summary(pT) # simultaan MLE-parameters fitten (multivariate normaliteit)
testTransform(pT, c(1,1,0,0) ) # eigen lamda-waarden testen
bcPower(Prestige.c, pT$roundlam ) # simultaan transformeren

## UScereal
library(MASS)
attach(UScereal)

summary(powerTransform(calories)) # log10 best
par(mfrow=c(2,3))
boxplot(calories) 
qqnorm(calories); qqline(calories)
hist(calories) # eerder rechtsscheef
boxplot(log10(calories))
qqnorm(log10(calories)); qqline(log10(calories))
hist(log10(calories)) # behoorlijke verbetering, zie vooral histogram

summary(powerTransform(protein)) # log10 best
par(mfrow=c(2,3))
boxplot(protein) 
qqnorm(protein); qqline(protein)
hist(protein) # eerder rechtsscheef
boxplot(log10(protein))
qqnorm(log10(protein)); qqline(log10(protein))
hist(log10(protein)) # behoorlijke verbetering

summary(powerTransform(fat+1)) # log10 best
par(mfrow=c(2,3))
boxplot(fat+1) 
qqnorm(fat+1); qqline(fat+1)
hist(fat+1) 
boxplot(log10(fat+1))
qqnorm(log10(fat+1)); qqline(log10(fat+1))
hist(log10(fat+1)) # groot aantal met precies 0 fat: transformaties kunnen dit niet oplossen

summary(powerTransform(sodium+1)) # eerder identieke dan log, 2/3?
testTransform(powerTransform(sodium+1),2/3)
par(mfrow=c(2,3))
boxplot(sodium) 
qqnorm(sodium); qqline(sodium)
hist(sodium) # eerder rechtsscheef
boxplot((sodium+1)**(2/3))
qqnorm((sodium+1)**(2/3)); qqline((sodium+1)**(2/3))
hist((sodium+1)**(2/3)) # helpt een klein beetje (zie boxplot), identieke minimumwaarden zijn lastig
testTransform(powerTransform(sodium+1),1/2)
par(mfrow=c(2,3))
boxplot(sodium) 
qqnorm(sodium); qqline(sodium)
hist(sodium)
boxplot((sodium+1)**(1/2))
qqnorm((sodium+1)**(1/2)); qqline((sodium+1)**(1/2))
hist((sodium+1)**(1/2)) # wortel is waarschijnlijk even goed (normaal wordt het toch niet, wel beter dan identieke en eenvoudiger)
par(mfrow=c(2,3))
boxplot(sodium) 
qqnorm(sodium); qqline(sodium)
hist(sodium) 
boxplot(log10(sodium+1))
qqnorm(log10(sodium+1)); qqline(log10(sodium+1))
hist(log10(sodium+1)) # 2/3 of 1/2 doet het wel degelijk beter dan log10

summary(powerTransform(fibre+1)) # log10 best
par(mfrow=c(2,3))
boxplot(fibre+1) 
qqnorm(fibre+1); qqline(fibre+1)
hist(fibre+1) # eerder rechtsscheef
boxplot(log10(fibre+1))
qqnorm(log10(fibre+1)); qqline(log10(fibre+1))
hist(log10(fibre+1)) # behoorlijke verbetering, weer probleem met identieke waarden

summary(powerTransform(carbo)) # log10 licht verworpen, suggestie voor inverteren
par(mfrow=c(2,3))
boxplot(carbo) 
qqnorm(carbo); qqline(carbo)
hist(carbo) # eerder rechtsscheef
boxplot(1/carbo)
qqnorm(1/carbo); qqline(1/carbo)
hist(1/carbo) # behoorlijke verbetering
par(mfrow=c(2,3))
boxplot(carbo) 
qqnorm(carbo); qqline(carbo)
hist(carbo) # eerder rechtsscheef
boxplot(log10(carbo))
qqnorm(log10(carbo)); qqline(log10(carbo))
hist(log10(carbo)) # niet veel slechter, vaak beter interpreteerbaar

summary(powerTransform(sugars+1)) # sugereert identieke transformatie
par(mfrow=c(2,3))
boxplot(sugars) 
qqnorm(sugars); qqline(sugars)
hist(sugars) # niet perfect: 2 groepen? lost box cox niet op (splitsen indien mogelijk)

summary(powerTransform(potassium)) # log10 best
par(mfrow=c(2,3))
boxplot(potassium) 
qqnorm(potassium); qqline(potassium)
hist(potassium) # eerder rechtsscheef
boxplot(log10(potassium))
qqnorm(log10(potassium)); qqline(log10(potassium))
hist(log10(potassium)) # behoorlijke verbetering

model.lin = lm(calories~fat+carbo+protein+sugars)
model.trans = lm(log10(calories)~log10(fat+1)+log10(carbo)+log10(protein)+sugars)
par(mfrow=c(2,2))
plot(model.lin)
plot(model.trans)
# residuplot lijkt wat homogener maar niet fundamenteel beter
# tranformaties in deze dataset raar omdat de formule van FDA lineair is
summary(powerTransform(model.trans))
# residuen vragen geen verdere transformatie

### Gewogen regressie
#install.packages('faraway')
library(faraway) # faraway::logit andere logit-definitie, transformeert niet automatisch maar heeft wel inverse faraway::ilogit()
attach(strongx); head(strongx)
cross.lm = lm(crossx~energy)
par(mfrow=c(2,2))
plot(cross.lm)
yhat = cross.lm$fitted.values
e = cross.lm$residuals
es = rstandard(cross.lm)
shapiro.test(es) # Er zijn maar tien meetwaarden, louche om test uit te voeren...

cross.w = lm(crossx~energy,weights=sd^(-2))
par(mfrow=c(2,2))
plot(cross.w)
# de standaardvoorstelling laat niet toe de resultaten te vergelijken
esw = rstandard(cross.w)
shapiro.test(esw) # ...maar verschil is duidelijk, wegen komt normaliteit hier sterk ten goede
yhatw = cross.w$fitted.values
ew = cross.w$residuals/sd # betekenis van residu verandert, er moet rekening gehouden worden met het gewicht!
par(mfrow=c(1,1))
plot(yhat,e); abline(h=0,col='red'); points(yhatw,ew,col='red')
# De residuplot in het gewogen model ziet er beter uit als het vergeleken wordt met het gewone model (hoewel nog niet goed, maar de dataset is natuurlijk ook te beperkt om een echt gedetailleerde studie van de residuen te maken).

plot(crossx~energy,cex=10/sd)
abline(cross.lm,col='red') # model zonder weging, grootste waarden wegen door ondanks de grotere fout
abline(cross.w,col='blue') # model met weging, sluit beter aan bij de punten met kleine fout
# De punten met lage energie-waarden hebben groter gewicht en hebben dus een grotere invloed op de regressielijn. De punten met hoge energie-waarde wegen veel minder sterk door omdat de fout op die meetwarden veel groter is.
detach(strongx)

### Gewogen regressie: geschatte fouten
attach(cars); head(cars)
x = speed; boxplot(x)
y = dist; boxplot(y)
speed.lm = lm(y~x); summary(speed.lm)
plot(y~x); abline(speed.lm,col='red')
es = rstandard(speed.lm)
shapiro.test(es)
par(mfrow=c(2,2))
plot(speed.lm)
# Model zeer significant, residuen wijken licht af van normaliteit en vertonen lichte vorm van heteroscedasticiteit.

e=speed.lm$residuals
yhat=speed.lm$fitted.values
e.lm = lm(abs(e)~yhat); summary(e.lm)
par(mfrow=c(1,1))
plot(abs(e)~yhat); abline(e.lm,col='red')
w = 1/e.lm$fitted.values**2
# Heteroscedasticiteit wordt (net) bevestigd door de significantie van dit tweede model.

lm.w = lm(y~x, weights=w); summary(lm.w)
plot(y ~ x, cex=70*w); abline(speed.lm,col='red'); abline(lm.w,col='blue')
lines(x,predict(speed.lm,interval='prediction')[,2],col='red',lty='dotted')
lines(x,predict(speed.lm,interval='prediction')[,3],col='red',lty='dotted')
lines(x,predict(lm.w,interval='prediction')[,2],col='blue',lty='dotted')
lines(x,predict(lm.w,interval='prediction')[,3],col='blue',lty='dotted')
es = rstandard(lm.w)
shapiro.test(es)
yhat.w = lm.w$fitted.values
e.w = lm.w$residuals*sqrt(w)
summary(speed.lm)$sigma
summary(lm.w)$sigma
plot(yhat,e); abline(h=0,col='red'); points(yhat.w,e.w,col='blue')
plot(yhat,scale(e)); abline(h=0,col='red'); points(yhat.w,scale(e.w),col='blue')
# Weging heeft niet per se een grote invloed op het model zelf, de punten met het grootste gewicht (speed<5) dempen het effect enigszins. Significantie en normaliteit veranderen nauwelijks maar de gewogen residuen zijn een pak kleiner dan de oorspronkelijke residuen. Het belangrijkste (en relevante) effect is echter dat de residuen minder heteroscedasticiteit vertonen: de kleinere variantie links is relatief vergroot ten opzichte van de grotere variantie rechts die wat is verkleind. Het effect daarvan is dat de predictiebanden niet meer "parallel" lopen met de regressielijn, maar smaller zijn waar de gewichten groter zijn, breder waar die lager zijn.

e.lm2 = lm(abs(e.w)~yhat.w)
summary(e.lm2)
plot(abs(e.w)~x); abline(e.lm2,col='red')
# Zoals eerder vermoed is er inderdaad geen trend meer zichtbaar in de absolute waarden van de gewogen residuen, het regressiemodel is niet meer significant.
detach(cars)
