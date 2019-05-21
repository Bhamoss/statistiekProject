library(MASS)
data(UScereal)
model = lm(calories~fat+carbo+sugars+protein+fibre+potassium)
summary(model)

e = model$residuals
s = summary ( model ) $sigma
es = rstandard ( model )

par(mfrow =c(2 ,2)) # grafieken in twee rijen en twee kolommen zetten
plot(model)
# Gemiddelde residuen niet nul, eerst dalende trend daarna grote outliers
# Normalitiet op outliers na redelijk goed benaderd
# Variantie behoorlijk goed, op outliers na
# Twee invloedrijke punten met zowel groot residu als veel leverage
# Ondanks de hoge significantie en Adjusted R-squared is dit eigenlijk geen goed model, mogelijk dringen zich transformaties op en de outliers zijn ook een probleem
par( mfrow =c(3 ,2)) # grafiek opnieuw schermvullend
termplot(model, partial.resid = TRUE )
# Quasi perfect lineaire termplots, geen enkele suggestie voor transformatie van de verklarende veranderlijken. Misshcien ligt de redding in het uitsluiten van outliers?
par( mfrow =c(1 ,1))
UScereal[c(7,31,32,48),]
model.out = lm(calories~fat+carbo+sugars+protein+fibre+potassium,data=UScereal[-c(7,31,32,48),])
par(mfrow =c(2 ,2))
plot(model.out)
# Niet fundamenteel beter. Nieuwe outliers. Heteroscedasticiteit?
par( mfrow =c(1 ,1))

data(Prestige)
model = lm(prestige~.^2,data=Prestige[,-5])
model = stepAIC(model,direction="both")
summary(model)

par(mfrow =c(2 ,2)) # grafieken in twee rijen en twee kolommen zetten
plot(model)
# Zeer homogene puntenwolk, geen zichtbare afwijkingen van de modelveronderstellingen
par( mfrow =c(2 ,2)) # grafiek opnieuw schermvullend
termplot(model, partial.resid = TRUE )
# Warning: partiële residuplots houden geen rekening met interactie
# Ophoping bij income en women suggereert transformatie, volgend hoofdstuk
par( mfrow =c(1 ,1))
