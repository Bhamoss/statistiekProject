#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
# Oefenzitting 12: Ridge & robust regression
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#install.packages('MASS')
#install.packages('Sleuth3')
#install.packages('leaps')
library(car)
library(Sleuth3)
library(MASS)

###*******************************************************
### Multicollineariteit constateren
###*******************************************************

###*******************************************************
### Remedies tegen multicollineariteit
###*******************************************************

###--------------------------------------------------------------------
### 1. 
###--------------------------------------------------------------------
prestige.lm = lm(prestige~education+log10(income)+logit(women/100),data=Prestige)
summary(prestige.lm)
vif(prestige.lm)
# geen al te grote waarden, geen dramatische multicollineariteit

###--------------------------------------------------------------------
### 2. 
###--------------------------------------------------------------------
litter.lm1 = lm(Litter~Gestation+log10(Brain)+log10(Body),data=case0902)
summary(litter.lm1)
pairs(litter.lm1$model)
vif(litter.lm1)
# problematische vif bij brain en body: grote hersenen en groot lijf hangen samen
# merk dalende trend tussen Litter en de rest (en ook dat de andere veranderlijken sterk positief gecorreleerd zijn)
# nochtans staat er positief teken bij body

litter.lm2 = lm(Litter~Gestation+log10(Body),data=case0902)
summary(litter.lm2)
vif(litter.lm2)
# veranderlijke weg: model verklaart wat minder maar multicol is weg en teken van Body is logisch

###*******************************************************
### Veranderlijken centreren
###*******************************************************
x1 = rnorm(20,5)
x2 = rnorm(20,5)
x3 = rnorm(20,mean=x1,sd=.01)

y1 = rnorm(20,mean=3+x1+x1**2,5)
y2 = rnorm(20,mean=3+x1+x3)

x1c = x1-mean(x1)
x2c = x2-mean(x2)
x3c = x3-mean(x3)

###--------------------------------------------------------------------
### 3. 
###--------------------------------------------------------------------
vif(lm(y1~x1+I(x1**2)))
vif(lm(y1~x1c+I(x1c**2)))
# multicol verdwijnt

###--------------------------------------------------------------------
### 4. 
###--------------------------------------------------------------------
vif(lm(y1~x1*x2))
vif(lm(y1~x1c*x2c))
# multicol verdwijnt

vif(lm(y1~x1*x3))
vif(lm(y1~x1c*x3c))
# enkel vif van interactieterm verdwijnt omdat x1 en x3 nog steeds afhankelijk zijn (wegens definitie x3)

###*******************************************************
### Ridge regression
###*******************************************************

###--------------------------------------------------------------------
### 5. 
###--------------------------------------------------------------------
X = data.frame(y2,x1,x3)
# exacte vergelijking: y = 3 + x1 + x3
# lm hoort dus coefficientenschattingen (1,1) te vinden

lm.ols = lm(y2~.,data=X)
summary(lm.ols)
# schattingen fout, verkeerd teken, enorme standaardfouten
# (correcte waarden worden dus weliswaar niet verworpen)
vif(lm.ols)

###--------------------------------------------------------------------
### 6. 
###--------------------------------------------------------------------
lm.ridge(y2~.,data=X,lambda=0) # geeft bij lambda=0 identieke schattingen (ridge standardized regression estimators terug omgezet naar de oorspr variabelen)
lm.ridge(y2~.,data=X,lambda=0)$coef # schattingen voor ridge standardized regression estimators beta*

###--------------------------------------------------------------------
### 7. 
###--------------------------------------------------------------------
lm.r = lm.ridge(y2~.,data=X,lambda=10**(-5:0))
lm.r # modellen bij verschillende waarden voor lambda
plot(lm.r)
# coefficientenschattingen stabiliseren voor grotere waarden van lambda, maar wel duidelijk vertekend 
# optimale lambda is waar curves stabiliseren. Er bestaan verschillende methodes om een optimale waarde voor lambda formeel te schatten, buiten bestek cursus.
lm.ridge(y2~.,data=X,lambda=.1)

###--------------------------------------------------------------------
### 8. Ridge regression op model voor Litter
###--------------------------------------------------------------------
litter.r = lm.ridge(Litter~Gestation+log10(Brain)+log10(Body),data=case0902,lambda=seq(0,100,length=100))
plot(litter.r); abline(h=0)
# Coefficienten stabiliseren inderdaad bij negatieve waarden, zoals aanvankelijk verwacht. Kwalitatief idee is hier belangrijkst.

###*******************************************************
### Outlier-onderzoek
###*******************************************************
#install.packages('robustbase')
library(robustbase)

###--------------------------------------------------------------------
### 9a. Prestige 
###--------------------------------------------------------------------
prestige.lm = lm(prestige~education+log10(income)+logit(women/100),data=Prestige[,1:4])
y = rstudent(prestige.lm) #?rstudent
Mr = covMcd(prestige.lm$model[,-1])$center; Mr
Cr = covMcd(prestige.lm$model[,-1])$cov; Cr
x = mahalanobis(prestige.lm$model[,-1], Mr, Cr)

plot(x,y)
y0=2.5 # eventueel: y0=qnorm(.975)
abline(h=c(-y0,y0),col='red') # geen echte outliers, behalve het verwachte aantal overschrijdingen
row.names(Prestige)[abs(y)>y0] # high residual points
x0=qchisq(.975,2) # 2.5% dus bij chikwadraatverdeling: 2 a 3-tal elementen buiten deze grenzen  
abline(v=x0,col='red') # behoorlijk wat punten met hoge leverage
row.names(Prestige)[x>x0] # high leverage points
bad = which(x>x0 & abs(y)>y0); bad # geen bad leverage points
# indien gewerkt wordt met y0=qnorm(.975) dan wel bad leverage  point
text(x[bad],y[bad],row.names(Prestige)[bad])
# enkel het beroep minister is invloedrijke outlier
sort(cooks.distance(prestige.lm))

prestige.lts = ltsReg(prestige~education+log10(income)+logit(women/100), data=Prestige)
plot(prestige.lts) # laatste grafiek is de diagnostische plot

summary(prestige.lm)
summary(prestige.lts)
rbind(
  ols = prestige.lm$coefficients,
  lts = prestige.lts$coefficients
)
# Niet heel verschillende coefficienten (kijk ook naar de standaarddeviaties vooraleer dit te beslissen)!

# De outlier-plot in de standaard-output toont de studentized residuals in functie van de hat-diagnostic en tekent daarbij contourlijnen voor de Cook's afstand: deze afstand kijkt niet naar horizontale of verticale uitwijking, maar naar het concrete effect van een punt op het model. Punten worden vaak als invloedrijk beschouwd vanaf D>1.
par(mfrow=c(2,2)) # meerdere grafieken tegelijk tonen
plot(prestige.lm)
par(mfrow=c(1,1)) # terug naar standaard output
h = hatvalues(prestige.lm) # formule 8.5
plot(y~h)
sort(cooks.distance(prestige.lm)) # het meest invloedrijke punt heeft Cooks's afstand 0.15: niet afwijkend!

###--------------------------------------------------------------------
### 9b. Litter 
###--------------------------------------------------------------------
litter.lm = lm(Litter~Gestation+log10(Body),data=case0902)
y = rstudent(litter.lm)
Mr = covMcd(litter.lm$model[,-1])$center; Mr
Cr = covMcd(litter.lm$model[,-1])$cov; Cr
x = mahalanobis(litter.lm$model[,-1], Mr, Cr)

plot(x,y)
y0 = 2.5 #y0=qnorm(.975)
abline(h=c(-y0,y0),col='red') # geen echte outliers, behalve het verwachte aantal overschrijdingen
case0902[abs(y)>y0,1] # high residual points
x0=qchisq(.975,1) # 2.5% dus bij chikwadraatverdeling: 2 a 3-tal elementen buiten deze grenzen 
abline(v=x0,col='red') # behoorlijk wat punten met hoge leverage
case0902[x>x0,1] # high leverage points
bad = which(x>x0 & abs(y)>y0); bad # bad leverage points, de meeste hiervan hebben weinig invloed op/voldoen redelijk goed aan de regressievergelijking
text(x[bad],y[bad],case0902[bad,1])
# Rat en olifant voldoen niet aan het model

litter.lm = lm(Litter~Gestation+log10(Body),data=case0902)
plot(litter.lm)
summary(litter.lm)
litter.lts = ltsReg(Litter~Gestation+log10(Body), data=case0902)
plot(litter.lts)
summary(litter.lts)
rbind(
  ols = litter.lm$coefficients,
  lts = litter.lts$coefficients
)
# model totaal niet geschikt, outliers filteren lost de zaak helemaal niet op - lineaire regressie is hier gewoon fundamnteel ongeschikt gezien de specifieke vorm van de respons: glm nodig!
