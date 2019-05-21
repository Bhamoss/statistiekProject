library(MASS)
data(UScereal)

model.null =  lm(calories ~ 1, data=UScereal)
model.full =  lm(calories ~ ., data=UScereal)
stepAIC(model.full,direction="backward")
# Achterwaartse regressie:
# - een aantal veranderlijken weglaten levert een betere (lagere) AIC op
# - <none> correspondeert met het huidige model
# - voor een aantal andere veranderlijken resulteert het weglaten in een stijgende AIC
# De veranderlijke die grootste AIC-reductie veroorzaakt wordt weggelaten
# Wanneer <none> de laatste AIC heeft, stopt het proces
stepAIC(model.full,direction="both")
# Bidirectionele selectie:
# - Ook de AIC bij het toevoegen van termen wordt bekeken
# - In de eerste stappen zal dat niet gebeuren omdat de overeenkomstige modellen al beschouwd werden
# - In de derde stap is het mogelijk dat het toevoegen van een term een lagere AIC oplevert. In dit voorbeeld gebeurt het niet.
stepAIC(model.null,direction="forward",scope=list(upper=model.full,lower=model.null))
# Voorwaartse regressie:
# - De eerste stappen tonen wat (volgens het gekozen criterium) de interessantste veranderlijke is: hier carbo
# In het algemeen geven deze methoden hoogstens een leidraad voor modelselectie. Toepassen zonder meer is zelden voldoende.

calories.step = stepAIC(model.full)
class(calories.step)   # output is enkel het finale model
summary(calories.step) # summary geeft dus de klassieke tabel bij dit model
anova(calories.step)   # analoog geeft anovamodel van onderliggende modellen
calories.step$anova    # $anova-attribuut geeft anovatabel van doorlopen modellen
# merk op dat stepAIC categorische veranderlijken (mfr, vitamins) als geheel behandelt

###

library(leaps)
calories.all = regsubsets(calories~.,data=UScereal,nvmax=15) # de attributen van het regsubsets-object zijn niet interessant
plot(calories.all)
summary(calories.all) # de interessante resultaten zitten in het summary.regsubsets-object
# carbo, sugars en fat - in die volgorde - kunnen het aantal calorieën het best voorspellen (ha ja, natuurlijk...)
attributes(summary(calories.all))
summary(calories.all)$outmat # schematische voorstellling
summary(calories.all)$which # logische voorstelling
# indicatoren bij mfr en vitamins worden los van 
which.min(summary(calories.all)$bic) # zesde model heeft laagste BIC-score
coef(calories.all,1) # coëfficiënten van het eerste model

p = rowSums(summary(calories.all)$which)
bic = summary(calories.all)$bic
plot(p,bic)
which.min(bic)
summary(calories.all)$which[which.min(bic),]
coef(calories.all,which.min(bic)) # coëfficiënten van het beste model
# dat is er dus zeker eentje te veel: potassium heeft niets met calorieën te maken terwijl alle info in de andere variabelen zou moeten zitten volgens de formule van FDA

cereal.five = regsubsets(calories~.,data=UScereal, nvmax=15, nbest=5)
p = rowSums(summary(cereal.five)$which)
bic = summary(cereal.five)$bic
plot(p,bic)
# "beste" modellen liggen niet noodzakelijk ver uit elkaar, dus ook kijken wat er gebeurt als er termen worden toegevoegd
# eerste paar termen hebben een duidelijk voordeel ten opzichte van de andere, daarna is de winst veel kleiner en speelt het ook niet zo'n rol meer
p[which.min(bic)]; abline(h=min(bic),col='red'); abline(v=p[which.min(bic)],col='red')
# De laagste bic wordt bereikt bij het 6e model met 7 termen
summary(cereal.five)$which[which.min(bic),]
coef(cereal.five,which.min(bic)) # idem vorige uiteraard

library(gmodels)
model = lm(calories~fat+carbo+sugars+protein+fibre+potassium)
summary(model)
# automatisch model uit regsubsets selecteren is jammer genoeg moeilijk, zeker met indicatoren en interacties
test.glh = glh.test(model, cm=diag(rep(1,7)), d=c(0,9,4,4,4,2,0)); test.glh
# De coëfficiënten liggen weliswaar in de buurt maar verschillen desalniettemin significant van het voorgestelde model

data(Prestige)
names(Prestige)
model = lm(prestige~.^2,data=Prestige[,-5])
stepAIC(model,direction="both") # interacties met type blijven, andere niet
model.all = regsubsets(prestige~.^2,data=Prestige[,-5],nvmax=15)
bic = summary(model.all)$bic
plot(model.all)
coef(model.all,which.min(bic)) # regsubsets gebruikt indicatoren als afzonderlijke veranderlijken
# dit is niet zeer zinvol bij meerdere niveaus omdat verschillen met de referentiegroep WEL maar tussen referentiegroepen NIET door het model kunnen worden in(uit)gesloten

