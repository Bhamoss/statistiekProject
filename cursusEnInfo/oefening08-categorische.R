##### Oefenzitting 9 - Categorische voorspellers
library(car)
attach(Prestige)

head(Prestige)

###********************************************************************
### One-way anova: model met 1 categorische veranderlijke 
###********************************************************************

###--------------------------------------------------------------------
### 1. 
###--------------------------------------------------------------------

### Lineair model
## output van een aov-model
lm1 = aov(prestige~type)
summary(lm1)

summary(lm(lm1))
tapply(prestige,type,mean) # 68 = 36+32 ; 42 = 36+7
# lm en aov maken identieke modellen maar geven andere output 
## modelveronderstellingen (zie later)
shapiro.test(rstandard(lm1))
plot(lm1)
plot(lm1$residuals~lm1$fitted.values); abline(h=0,col='red') #residuplot
leveneTest(lm1) # geen significant verschil in varianties
plot(sort(abs(rstandard(lm1)))); abline(h=2.5,col='red')
Prestige[which(abs(rstandard(lm1))>2.5),]
# normale residuen met constante variantie en geen opvallende uitschieters
model.tables(lm1, type="means") 
model.tables(lm1, type="effects") # verschil met gemiddelde
# Modelveronderstellingen: model lijkt correct, residuen normaal, gemiddelde constant nul, variantie constant.
# Globale F-test: type verklaart significant deel van prestige.
# Interpretatie: gemiddelde prestige is 47, prof scoort hoger, beide andere groepen lager

###--------------------------------------------------------------------
### 2. Gemiddelde prestige
###--------------------------------------------------------------------
TukeyHSD(lm1)
# de gemiddelde prestige verschilt tussen elke twee groepen onderling, het verschil wc-bc is het kleinst maar ook na Tukey-correctie nog significant.

###--------------------------------------------------------------------
### 3. Betrouwbaarheidsintervallen arbeiders
###--------------------------------------------------------------------
table(type); boxplot(prestige[type=='bc'])
tapply(prestige,type,mean)
tapply(prestige,type,sd)
t.test(prestige[type=='bc'])
# BI = [32.5,38.6], geldig onder CLS (n=44, redelijk symmetrische verdeling)
predict(lm1,data.frame(type='bc'),interval='confidence')
# BI = [32.7,38.4], geldig dankzij modelveronderstellingen, smaller omdat de variantie homogeen verondersteld wordt over de drie groepen en geschat wordt op de ganse dataset
## Predictie-intervallen
predict(lm1,data.frame(type='wc'),interval='prediction')
# PI = [23.0,61.5], dankzij normaliteitsassumptie voor residuen kunnen predictie-intervallen voor individuele respons worden gemaakt.


###********************************************************************
### Analyse van Covariantie: modellen met continue en categorische veranderlijken
###********************************************************************

###--------------------------------------------------------------------
### 4. Model met interactietermen
###--------------------------------------------------------------------
lm3 = lm(prestige~log10(income)+education+women)
lm3.type = lm(prestige~(log10(income)+education+women)*type)
summary(lm3.type)
# Gewone lm-output geeft zeer onvolledig beeld: enerzijds staan er enkel vergelijkingen met de referentiegroep (hier bc) anderzijds staat er een amalgaam van significante en niet-significante resultaten) - Is de bijdrage significant? Welke zijn de concrete verschillen?
anova(lm3.type)
# anova(lm3,lm3.type) # NA's in type, wreed ambetant
lm3.NA = lm(prestige~log10(income)+education+women,data=na.omit(Prestige)) # beetje lomp als er nog veel andere veranderlijken zijn met eigen NA's
anova(lm3.NA,lm3.type) # type geeft een zeer significante bijdrage bovenop de andere veranderlijken, ook al zijn niet alle interacties significant
# De ANOVA-tabel toont een duidelijker beeld: toevoegen van de interactieterm met inkomen en women is wel significant, de interactieterm met education niet
lm3.type = update(lm3.type,.~.-education:type) # optimaliseren van model komt in principe later aan bod
anova(lm3.type)
shapiro.test(rstandard(lm3.type))
plot(lm3.type$residuals~lm3.type$fitted.values); abline(h=0,col='red')
plot(sort(abs(rstandard(lm3.type)))); abline(h=2.5,col='red')
# De interactieterm met inkomen is significant, die met education niet. Hoewel women in het basismodel niet significant leek blijkt women*type dat wel: het percentage vrouwen op zich heeft geen invloed op de prestige, maar het verschil in invloed tussen de drie groepen is dat wel! Het bekomen model voldoet vrij goed aan de modelveronderstellingen.
# Noteer dat achterwaartse regressie niet noodzakelijk resulteert in het beste model: de initiele volgorde van de termen speelt immers een rol! In een volgende sectie komt dit meer uitgebreid aan bod.

###--------------------------------------------------------------------
### 5. Parti?le F-test
###--------------------------------------------------------------------
anova(lm3,lm3.type)
# er ontbreken waarden voor type, waardoor de SST niet hetzelfde is in beide modellen: onvergelijkbaar!
lm3.type_na.rm = lm(prestige~log10(income)*type+education+women*type,data=na.omit(Prestige))
lm3_na.rm = lm(prestige~log10(income)+education+women,data=na.omit(Prestige))
anova(lm3_na.rm,lm3.type_na.rm)
# Op de beperkte dataset waaruit de cases met type=NA zijn geweerd, blijkt een extreem significant verschil tussen beide modellen: type verklaart een significant deel van de prestige nadat income, education en women al in rekening zijn gebracht.

###--------------------------------------------------------------------
### 6. Vergelijkingen en eerste conclusies
###--------------------------------------------------------------------
summary(lm3.type)
rbind(
  BC =   lm3.type$coefficients[c(1,2,4,3)], # bc
  WC =   lm3.type$coefficients[c(1,2,4,3)] + c(lm3.type$coefficients[c(6,8,10)],0), # wc
  prof = lm3.type$coefficients[c(1,2,4,3)] + c(lm3.type$coefficients[c(5,7,9)],0)) # prof  
# Het effect van scholing wordt in elke groep even groot ingeschat: gemiddeld +2.8 extra prestige per extra jaar scholing.
# Het effect van inkomen en percentage vrouwen verschilt significant tussen de groepen. De t-testen geven enkel vergelijkingen met de referentiegroep (het inkomen speelt een veel minder grote rol bij professionals dan bij arbeiders, tussen arbeiders en bedienden is er geen significant verschil)

###--------------------------------------------------------------------
### 7. Bijkomende test
###--------------------------------------------------------------------
## Eerste methode: algemene lineaire hypothese
library(gmodels)
test.glh = glh.test(lm3.type, cm=c(0,0,0,0,0,0,1,-1,0,0), d=0); test.glh
# De gevraagde test heeft p-waarde 0.9%: inkomenseffect bij professionals en bedienden is significant verschillend
## Tweede methode: volgorde van de levels veranderen om ander referentieniveau te hebben
type2 = factor(type, levels(type)[c(2,1,3)])
summary(lm(prestige~log10(income)*type2+education+women*type2))
# Zelfde resultaat: de p-waarde van de glh-test en de t-test zijn identiek zijn en de F-score is het kwadraat van de t-score - dit is uiteraard geen toeval, de reden wordt later duidelijk.
sqrt(test.glh$statistic) ## dit is de t-statistiek!
## Simultane uitspraken
type3 = factor(type, levels(type)[c(3,1,2)])
summary(lm(prestige~log10(income)*type3+education+women*type3))
# Om post-hoc te testen welke effecten significant zijn moeten de drie mogelijke modellen worden vergeleken (of een hele resem glh-testen worden uitgevoerd)
# Het inkomenseffect 13.6 bij prof is niet significant (p=6%) zie model met type2
# Het inkomenseffect 49.1 bij bc is zeer significant (p=10**(-10)) zie model met type
# Het inkomenseffect 55.5 bij wc is zeer significant (p=0.02%) zie model met type3
# Het verschil in inkomenseffect wc-bc 6.4 is niet significant (p=69%) zie model met type of type3 (op teken na)
# Het verschil in inkomenseffect bc-prof 35.5 is zeer significant (p=0.05%) zie model met type3 of type (op teken na)
# Het verschil in inkomenseffect wc-prof 41.9 is zeer significant (p=0.9%) zie model met type2 of type3 (op teken na)
# Deze p-waarden zijn allemaal zonder post-hoc correctie! Hou er rekening mee dat er bij het systematisch zoeken naar significante effecten in principe (Bonferroni-)correctie is aangewezen. Bij het beantwoorden van 1 op zichzelf staande, a priori geformuleerde vraag, is dat niet nodig.
p.adjust(c(0.062188,4.51e-10,0.000246,0.000458,0.685668,0.009252), method = "bonferroni")
# Er tekenen zich dus twee groepen af: prof (zonder inkomenseffect) en bc+wc (met een significant maar niet onderling verschillend inkomenseffect)
# Er bestaan meer gespecialiseerde post-hoc tests die gebaseerd zijn op bijvoorbeeld het Tukey-principe maar die buiten bestek van de cursus vallen

### Toelichting bij de synax:
# - de notatie X^n voegt NIET het kwadraat van een veranderlijke aan een model toe, daarvoor is I(X^n)
# - de notatie (X_1+...+X_p)^n voegt interacties tot en met orde n toe
# - n is dus pas zinvol indien kleiner of gelijk aan p, omdat voor n=p alles met elkaar interageert
# de meest courante interacties zijn er met categorische veranderlijken
# het bestuderen van interacties hoger dan 2e orde is zelden relevant
#
lm(prestige~income+education+women+type)
lm(prestige~income^2+education+women+type) # idem vorige (geen functionele syntax)
lm(prestige~I(income^2)+education+women+type) # gebruikt kwadraat van income ipv income
#
lm(prestige~(income+education+women+type)^2) # voegt interacties tot orde twee toe (alle producten van maximaal 2 factoren)
lm(prestige~(income+education+women+type)^3) # voegt interacties tot orde drie toe (alle producten van maximaal 3 factoren)
lm(prestige~(income+education+women+type)^4) # voegt interacties tot orde vier toe (alle mogelijke interacties, inclusief die van alle vier de termen)
lm(prestige~(income+education+women+type)^5) # identiek aan vorige aangezien er maar vier termen zijn
###

detach(Prestige)

###********************************************************************
### Lack of fit
###********************************************************************

###--------------------------------------------------------------------
### 8. Regressiemodel
###--------------------------------------------------------------------
#install.packages('Sleuth3')
library(Sleuth3)
attach(case0802)
?case0802
plot(Time~Voltage)
model.lm = lm(Time~Voltage)
summary(model.lm)
# Er is een significante trend, hoe hoger het voltage hoe korter de tijd tot breakdown

###--------------------------------------------------------------------
### 9. One-way anova
###--------------------------------------------------------------------
model.aov = aov(Time~as.factor(Voltage))
summary(model.aov)
model.tables(model.aov,type='means')
# Wanneer factorieel beschouwd, verklaart het voltage eveneens een significant deel van de totale som van kwadraten (ondanks het feit dat er meer coefficientenschattingen nodig zijn). Er is een systematisch dalende trend te zien in de groepsgemiddelden.

###--------------------------------------------------------------------
### 10. Grafische voorstelling
###--------------------------------------------------------------------
anova(model.lm,model.aov)
plot(Time~Voltage)
abline(model.lm,col='blue',lwd=3)
points(Voltage,predict(model.aov),col='red',pch='-',cex=4)
# Conceptueel: Regressie fit 1 trendlijn (2 schattingen). One-way anova schat een gemiddelde in elke groep, los van de andere (7 schattingen) en zal dus per definitie meer verklaren. De extra som van kwadraten is zeer significant, het regressiemodel is dus een te grote vereenvoudiging: het is beter om afzonderlijke schattingen voor het gemiddelde te maken.
# Grafisch ziet het model er helemaal niet goed uit: er is vermoedelijk verbetering mogelijk!
shapiro.test(rstandard(model.lm))
plot(model.lm$residuals~model.lm$fitted.values); abline(h=0,col='red')
plot(sort(abs(rstandard(model.lm)))); abline(h=2.5,col='red')
shapiro.test(rstandard(model.aov))
plot(model.aov$residuals~model.aov$fitted.values); abline(h=0,col='red'); leveneTest(model.aov)
plot(sort(abs(rstandard(model.aov)))); abline(h=2.5,col='red')
# Geen van beide modellen voldoet aan de modelveronderstellingen: er dringen zich modelaanpassingen op.

###--------------------------------------------------------------------
### 11. Getransformeerd model
###--------------------------------------------------------------------
logTime = log10(Time)
logVolt = log10(Voltage)
log.lm = lm(logTime~logVolt)
summary(log.lm)
# Er is een significante trend, hoe hoger het voltage hoe korter de tijd tot breakdown, R^2 is erg verhoogd.
log.aov = aov(logTime~as.factor(Voltage))
summary(log.aov)
model.tables(log.aov,type='means')
# Nog steeds een zeer significant model en dalende breakdown-tijden
anova(log.lm,log.aov)
# Er is nu helemaal geen significant verschil meer tussen beide modellen: met minder schattingen slaagt het regressiemodel er in even veel (niet significant minder) te verklaren. Dit levert dus het betere model: het model is eenvoudiger (minder parameters) en  de trendlijn is beter te interpreteren dan de afzonderlijke schattingen.
plot(logTime~logVolt)
abline(log.lm,col='blue',lwd=3)
points(log10(Voltage),predict(log.aov),col='red',pch='-',cex=4)
plot(Time~Voltage)
x = seq(26,38,length=100)
y = 10**predict(log.lm,data.frame(logVolt=log10(x)))
lines(x,y,col='blue',lwd=3)
points(Voltage,10**predict(log.aov),col='red',pch='-',cex=2)
# Grafisch stemmen beide modellen inderdaad vrij goed overeen
shapiro.test(rstandard(log.lm))
plot(log.lm$residuals~log.lm$fitted.values); abline(h=0,col='red')
plot(sort(abs(rstandard(log.lm)))); abline(h=2.5,col='red')
shapiro.test(rstandard(log.aov))
plot(log.aov$residuals~log.aov$fitted.values); abline(h=0,col='red'); leveneTest(log.aov)
plot(sort(abs(rstandard(log.aov)))); abline(h=2.5,col='red')
# De modelveronderstellingen zijn enorm veel beter dan in het niet-getransformeerde model, niet perfect in het regressiemodel maar zeker aanvaardbaar:
# logTime = 26 - 16 logVolt
# <=> Time = 10^26 * V^(-16)
detach(case0802)

###********************************************************************
### Two-way anova
###********************************************************************
attach(ex1320)
head(ex1320)

###--------------------------------------------------------------------
### 12. additieve en multiplicatieve model
###--------------------------------------------------------------------
model.add = aov(Score~Background+Sex)
summary(model.add)
model.tables(model.add,type='effects') # m*n verschillende cellen zijn samen te vatten in m+n schattingen, effecten zijn informatiever bij een additief model
# Beide effecten zijn significant:
# Uiteraard is voorkennis bepalend voor de score (<10**-16) dit resultaat is eigenlijk geen deel van de onderzoeksvraag maar om na te gaan of vrouwen en mannen verschillend scoren wil je dit effect wegfilteren, anders scoren mannen misschien louter beter omdat ze gemiddeld een betere voorkennis hebben
# Daarbovenop scoren vrouwen lager dan mannen (wat niet te verklaren is door verschil in voorkennis, want we kijken hier enkel naar de extra som van kwadraten)
shapiro.test(rstandard(model.add))
plot(model.add$residuals~model.add$fitted.values); abline(h=0,col='red')
plot(sort(abs(rstandard(model.add)))); abline(h=2.5,col='red')
2*pnorm(-2.5)*length(Score) # verwachte waarde van aantal outliers (binomaalverdeling)
length(which(rstandard(model.add)>2.5))
# Modelveronderstellingen voldaan. Veel grote residuen, maar niet meer dan verwacht in een dataset van deze omvang

model.full = aov(Score~Background*Sex)
summary(model.full)
model.tables(model.full,type="means") # 
# model.tables(model.full,type="effects") # resultaten bij interactie gelden cel per cel, kolom- en rij-effecten worden dus teniet gedaan. hier zijn gemiddelden dus relevanter
shapiro.test(rstandard(model.full))
plot(model.full$residuals~model.full$fitted.values); abline(h=0,col='red')
plot(sort(abs(rstandard(model.full)))); abline(h=2.5,col='red')
# Het interactie-effect is niet significant, de additieve effecten verklaren de score dus even goed (niet significant minder). Modelveronderstellingen blijven geldig.
# Als er rekening wordt gehouden met voorkennis, scoren vrouwen systematisch lager
# Het effect van voorkennis is echter hetzelfde voor mannen en vrouwen 

###--------------------------------------------------------------------
### 13. Verschil tussen de modellen en de t-test
###--------------------------------------------------------------------
x = Score[Sex=='male']; length(x)
y = Score[Sex=='female']; length(y)
shapiro.test(x)
shapiro.test(y)
t.test(x,y)
chisq.test(Sex,Background)
chisq.test(Sex,Background)$residuals
# Test voor gemiddelden: Vrouwen scoren significant lager dan mannen. Dit kan bijvoorbeeld aan een verschillende voorkennis te wijten zijn, daar houdt de t-test geen rekening mee.
# Test voor afhankelijkheid: Vrouwen hebben minder vaak sterke en eerder een gemiddelde vooropleiding, in tegenstelling tot mannen. Het verschil in score kan dus hieraan te wijten zijn.
# Additief model: Ook rekening houdend met voorkennis, scoren vrouwen nog significant lager dan mannen.
# Multiplicatief model: Het verschil in score tussen mannen en vrouwen verschilt niet over de verschillende voorkennis-groepen. Het zou bijvoorbeeld kunnen dat meer voorkennis minder baat heeft bij een van beide groepen, waardoor het effect tussen de geslachten vergroot. Dat wordt hier niet waargenomen. Vanwaar het verschil tussen beide groepen komt, is verder niet op te maken uit deze dataset.

detach(ex1320)
