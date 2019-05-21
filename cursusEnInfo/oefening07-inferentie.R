### Voorbeeld
#install.packages("car")
library(car)
attach(Prestige); head(Prestige)
?Prestige

### Modellen
prestige.0 = lm(prestige~1)
prestige.1 = lm(prestige~education)
prestige.2 = lm(prestige~log10(income))
prestige.12 = lm(prestige~education+log10(income))
prestige.123 = lm(prestige~education+log10(income)+women)
prestige.321 = lm(prestige~women+log10(income)+education)
prestige.213 = lm(prestige~log10(income)+education+women)

### Individuele testen
###----------------------
### 1. t-statistiek en p-waarde bij coef van X2 in prestige.123
###----------------------
summary.123 = summary(prestige.123); summary.123
beta2 = summary.123$coefficients[3,1]; beta2
sb2 = summary.123$coefficients[3,2]; sb2
t = beta2/sb2; t
2*(1-pt(abs(t),98))
# De coefficient bij log10(income) verschilt significant van nul. De formule schrijft voor dat een beroepsgroep die 10x hogere inkomens kent, 31 punten hoger scoort op de prestige-schaal.

###----------------------
### 2. test de hypothesen dat H0: beta_2 = 25 in prestige.2 en in prestige.123
###----------------------
# in model prestige.123:
t2 = (beta2-25)/sb2; t2
2*(1-pt(abs(t2),98))
# Deze gegevens wijzen er niet op dat de coefficient bij log10(income) significant afwijkt van 25.

# in model prestige.2:
summary.2 = summary(prestige.2); summary.2
beta2 = summary.2$coefficients[2,1]; beta2
sb2 = summary.2$coefficients[2,2]; sb2
t = beta2/sb2; t
2*(1-pt(t,100))
# De coefficient bij log10(income) verschilt ook in dit model significant van nul.

t2 = (beta2-25)/sb2; t2
2*(1-pt(t2,100))
# Deze gegevens verwerpen de hypothese dat de coefficient bij log10(income) gelijk is aan 25.
# De waarde van een coefficient in een model hangt dus af van de andere termen in dat model. Een t-test zegt NIET of een veranderlijke invloed heeft op de respons maar hoogstens er significante bijdrage is in het model waarin ook alle andere veranderlijken zitten.

### Simultaan testen uitvoeren

###----------------------
#3. hypothese (beta1,beta3) = (4.5,0.1) testen (zelfde resultaat voor prestige.123 en prestige.321)
###----------------------

summary.123
beta = summary.123$coefficients[c(2,4),1]; beta
sb = summary.123$coefficients[c(2,4),2]; sb
beta0 = c(4.5,0.1)
t = (beta-beta0)/sb; t
p = 2*(1-pt(abs(t),98)); p
p.adjust(p,method='bonferroni')
# ook al lijkt een resultaat significant en een ander randsignificant, na p-waarde-correctie is dat niet meer zo: de kans dat er in twee simultane uitspraken op 5% significantie een hypothese wordt verworpen terwijl H_0 waar is, is groter dan 5%!

### Grafische voorstelling van betrouwbaarheidsgebieden
plot(c(2.5,5),c(-.05,.15),type='n',xlab='beta1',ylab='beta3')
points(c(beta[1],beta0[1]),c(beta[2],beta0[2]),col=c('red','blue'),pch=16)
BI = confint(prestige.123)
abline(v=BI[2,],col='red',lty="dashed")
abline(h=BI[4,],col='red',lty="dashed")
# univariate betrouwbaarheidsintervallen: de simultane uitspraak "het koppel (beta1,beta3) verschilt significant van (4.5,0.1)" op basis van deze grenzen, resulteert in een lagere betrouwbaarheid dan de beoogde 95%!
BIcor = confint(prestige.123, level=1-.05/2) # Bonferroni-correctie
abline(v=BIcor[2,],col='red')
abline(h=BIcor[4,],col='red')
# betrouwbaarheidsintervallen na Bonferroni-correctie: deze garanderen betrouwbaarheid 95% in tegenstelling tot het simultaan interpreteren van de klassieke intervallen
###

### Kwadratensommen en determinatiecoefficient
###----------------------
### 4. Bereken de (aangepaste) determinatieco?ffici?nt op basis van anova op prestige.123
###----------------------
summary.123
anova(prestige.123)
attributes(anova(prestige.123))
SSE = anova(prestige.123)$"Sum Sq"[4]; SSE
SST = sum(anova(prestige.123)$"Sum Sq"); SST
rsq = 1-SSE/SST; rsq
abs(rsq-summary.123$r.squared)
dim(prestige.123$model)
rsqadj = 1-(SSE/(102-4))/(SST/(102-1)); rsqadj
abs(rsqadj-summary.123$adj.r.squared)

###----------------------
### 5. Vergelijk anovatabellen en benoem elke kwadratensom voor alle prestiges.
###----------------------
### Extra kwadratensommen
prestige.0 = lm(prestige~1); anova(prestige.0)
# SST = 29895
# De totale som van kwadraten is 29895
anova(prestige.1)
# SSR(X1) = 21608
# SSE(X1) =  8287
# education verklaart 21608 van de totale kwadratensom, 8287 blijft onverklaard
# SSR verschilt significant van nul
anova(prestige.2)
# SSR(X2) = 16418
# SSE(X2) = 13478
# income verklaart slechts 16418 van de totale kwadratensom (en dus minder dan education)
# ook al is SSR(X2) lager, het verschilt significant van nul
anova(prestige.12)
# SSR(X1) = 21608
# identieke waarde als hierboven
# SSR(X2|X1) = 3233
# extra som van kwadraten als de term X2 wordt toegevoegd aan een model dat al term X1 bevat.
# dit is lager dan SSR(X2) omdat een deel van de variatie dat verklaard wordt door X2 ook al verklaard wordt door X1 (dit zal enkel niet zo zijn als X1 en X2 perfect onafhankelijk zijn).
# bijdrage nog steeds significant: income toevoegen na education verklaart nog een bijkomend, significant van nul verschillend deel van de totale variantie
# SSE(X1,X2) = 5054
# samen verklaren X1 en X2 wel meer dan de twee afzonderlijke modellen want de fout SSE is kleiner (dit zal enkel niet zo zijn als X1 en X2 perfect afhankelijk zijn)
anova(prestige.123)
# SSR(X1) = 21608
# SSR(X2|X1) = 3233
# identieke waarde als hierboven
# SSR(X3|X1,X2) = 124
# extra som van kwadraten als de term X3 wordt toegevoegd aan een model dat al term X1 en X2 bevat is futiel in vergelijking met de vorige termen en inderdaad niet significant
# X3 geeft dus geen significante hoeveelheid bijkomende informatie meer bovenop X1 en X2
# SSE(X1,X2,X3) = 4930
# de fout van dit model is dan ook nauwelijks kleiner dan bij het vorige model
anova(prestige.321)
# SSR(X3) = 419
# model met enkel X3 verklaart wel significant deel van de variantie!
# SSR(X2|X3) = 18973
# SSR(X1|X2,X3) = 5574
# achtereenvolgens X2 en X1 toevoegen geeft telkens significante extra bijdrage!
# Let op: MOGELIJK is prestige niet lager OMDAT er veel vrouwen zijn maar bijvoorbeeld omdat lonen en studieduur lager zijn in beroepsgroepen waar vrouwen sterker vertegenwoordigd zijn

###
# LET OP: de modellen prestige.123 en prestige.321 zijn IDENTIEK!!!
# zelfde t- en globale F-testen, zelfde residuen en residu-analyse
# enige verschil is output in ANOVA-tabel die ??n specifieke volgorde van modellen opbouwt en vergelijkt!
# HOE dan beste model zoeken?
# - conceptueel: ALLE mogelijke volgordes overlopen en het beste zoeken
# - praktisch: zie latere les... (selectie van veranderlijken)
###

###----------------------
### 6. 
###----------------------
### Partiele F-test
anova(lm(prestige~education+women+log10(income)))
# de extra som van kwadraten is 2480 en is zeer significant
# andere werkwijze:
prestige.13 = lm(prestige~education+women)
anova(prestige.13,prestige.123)

###----------------------
### 7. 
###----------------------
prestige.12333 = lm(prestige~education+log10(income)+women+I(women**2)+I(women**3))
summary(prestige.12333)
anova(prestige.12,prestige.12333)
# de extra som van kwadraten is 379, een rand-significante bijdrage
# in principe is het niet onmogelijk dat meerdere (t-test insignificante) termen simultaan toch een significante bijdrage leveren
# (dit soort transformaties wordt weliswaar doorgaans niet gemotiveerd door significantie (overfitten) maar door residu-analyse (er blijkt een kromlijnig verband te zijn) of relevantie (de theorie schrijft een derdegraadsmodel voor))

###----------------------
### 8. 
###----------------------

### General linear hypothesis
#install.packages('gmodels')
library(gmodels)
C = rbind(c(0,1,0,0),
          c(0,0,0,1))
beta0 = c(4.5,0.1)
model.glh = glh.test(prestige.123,C,beta0)
summary(model.glh)
# De F-test is zeer significant, de coefficienten wijken af van de hypothetische waarden.
# Deze testprocedure blijkt veel sterker dan de afzonderlijke t-testen, zelfs voor Bonferroni-correctie
# F-test gebruikt immers correlatie-informatie (analoog aan bijvoorbeeld Hotelling-test in Deel 1)
# Blijkbaar is de combinatie van de hypothetische coefficienten (4.5,0.1) atypisch gezien de correlatie tussen de steekproefwaarden

### vervolg grafische voorstelling
# install.packages('ellipse')
library(ellipse)
lines(ellipse::ellipse(prestige.123, which=c(2,4)),col='green',lwd=2) # argument which geeft aan welke beta's geplot worden (beta0 is eerste beta)
# Beide coefficienten zijn negatief gecorrelleerd: twee (univariaat niet significant) positief afwijkende waarden zijn zeer onwaarschijnlijk (terwijl de combinatie van even grote afwijkingen maar ??n in positieve zin en ??n in negatieve zin niet tot verwerping van de nulhypothese zou leiden) Zie ook "Sigma_hatbeta" vorige les 
###

###----------------------
### 9. 
###----------------------
summary(prestige.123)
C = rbind(c(0,10,-1,0))
beta0 = 0
model.glh = glh.test(prestige.123,C,beta0)
summary(model.glh)
# nulhypothese wordt aanvaard: coefficienten verschillen niet significant van elkaar

