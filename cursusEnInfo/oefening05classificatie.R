####  Classificatiemethodes
library(MASS)
attach(crabs)
names(crabs)

### Lineaire discriminant analyse
X = crabs[,4:8]
y = sex
## Voorwaarden: Multivariate normaliteit werd bestudeerd in sectie 3. Zeker in de afzonderlijke groepen lijken de gegevens normaliteit te bevestigen.
lda.sex = lda(X,y); lda.sex
#of lda(y~.,X) 
#The . in the formula argument means that we use all the remaining variables in data as covariates
#of vb enkel op RW en FL: lda(y~FL+RW,X) 
# Coef. of ld: zie cursus blz. 145-146 (2017)/blz. 135 (2018)
lda.pred = predict(lda.sex)
lda.pred
# Vb. voor case 1: sum(c(8.1-mean(crabs$FL),6.7-mean(crabs$RW),16.1-mean(crabs$CL),19.0-mean(crabs$CW),7-mean(crabs$BD))*c(-0.17509926,-1.61455655,0.90033985,-0.27294518,0.08261892)) = 0.82...
attributes(lda.pred)
lda.pred$class # door methode toegekende klasse

head(lda.pred$posterior) # a posteriori kans om tot bepaalde groep te behoren
table(lda.pred$posterior[,1]>lda.pred$posterior[,2],lda.pred$class) # classificatie naargelang grootste a posteriori-kans
table(sex,lda.pred$class) # acht van 200 krabben worden verkeerd geclassificeerd
#install.packages('klaR')
library(klaR)
partimat(sex~FL+RW,method="lda",imageplot=FALSE) # twee veranderlijken
partimat(sex~FL+RW+CL+CW+BD,method='lda',plot.matrix=TRUE,imageplot=FALSE) # meerdere veranderlijken: langere rekentijd!
## APER
lda.APER=table(sex,lda.pred$class); lda.APER
sum(lda.APER-diag(diag(lda.APER)))/sum(lda.APER) # mogelijke onderschatting want classificatie gebaseerd op observaties zelf
## AER via hold-out cross-validation:
lda.CV = lda(X,y, CV=TRUE)
lda.AER = table(sex,lda.CV$class); lda.AER
sum(lda.AER-diag(diag(lda.AER)))/sum(lda.AER) 
table(lda.pred$class,lda.CV$class) # CV-classificatie verschilt hier niet

### Quadratic discriminant analysis
qda.sex = qda(X,y); qda.sex
qda.pred = predict(qda.sex)
qda.pred$class
head(qda.pred$posterior)
table(qda.pred$class,lda.pred$class) # 1 krab wordt anders ingedeeld
partimat(sex~FL+RW,method='qda',imageplot=FALSE) # nauwelijke merkbaar verschillende curve
# partimat(sex~FL+RW+CL+CW+BD,method='qda',plot.matrix=TRUE,imageplot=FALSE) # projecties geven slechts partieel beeld en suggereren slechtere indeling dan in werkelijkheid (met een globaal model) het geval is - er is geen reden om met qda te werken in dit geval.
## APER
qda.APER=table(sex,qda.pred$class); qda.APER
sum(qda.APER-diag(diag(qda.APER)))/sum(qda.APER)
## AER via hold-out cross-validation:
qda.CV = qda(X,y, CV=TRUE)
qda.AER = table(sex,qda.CV$class); qda.AER
sum(qda.AER-diag(diag(qda.AER)))/sum(qda.AER) 
table(qda.pred$class,qda.CV$class) # CV-classificatie deelt 1 observatie extra verkeerd in
# Gek dat een hogere orde systeem een slechtere verdeling geeft: covarianties niet robuust en outliers wegen zwaarder door in kleine groepen

### Classificatie van een specifieke krab (FL=10 en RW=10)
predict(lda(sex~FL+RW),data.frame(FL=10,RW=10)) # Kans vrouwelijk: 98%
partimat(sex~FL+RW,method='lda',imageplot=FALSE); points(10,10,col='green',pch=16)

## Classificatie met kostenfunctie: manuele berekeningen
X1 = X[y=="F",] # gegevensmatrix vrouwtjes
X2 = X[y=="M",] # gegevensmatrix mannetjes
n1 = dim(X1)[1] # aantal F
n2 = dim(X2)[1] # aantal M
p1 = n1/(n1+n2) # a priori kans F
p2 = n2/(n1+n2) # a priori kans M
mu1 = colMeans(X1) # gemiddeldes F
mu2 = colMeans(X2) # gemiddeldes M
S1 = cov(X1)
S2 = cov(X2)
Sp = ( (n1-1)*S1+(n2-1)*S2 )/(n1+n2-2) # gepoolde schatter

## Gelijke misclassificatiekosten:
crit0 = t(mu1-mu2)%*%solve(Sp)%*%t(X) - as.numeric(t(mu1-mu2)%*%solve(Sp)%*%(mu1+mu2)/2 + log(p2/p1) )
table(crit0>0)
table(crit0>0,sex)
table(crit0>0,lda.pred$class)
## Identieke indeling als via a posteriori-kansen in geval van gelijke kost, er zijn zowel F als M verkeerd ingedeeld

## Verschillende kostenfuncties
crit1 = t(mu1-mu2)%*%solve(Sp)%*%t(X) - as.numeric(
  t(mu1-mu2)%*%solve(Sp)%*%(mu1+mu2)/2 + log((1/10)*p2/p1) )
## Het teken van deze grootheid vervangt de rol van de a posteriori-kansen   
table(crit1>0) # TRUE is vrouw; door penalisatie nu meer F-classificaties
table(crit0>0,crit1>0) # er worden 17 elementen extra bij F ingedeeld (en uiteraard geen enkel bij M)
table(crit1>0,sex) # waardoor er nu helemaal geen verkeerd geklasseerde F'en zijn maar wel meer verkeerd geklasseerde M

detach(crabs)

### Meerdere groepen & knn
library(car)
attach(Prestige)

## Onderzoek van normaliteit, zie sectie 3
pairs(cbind(income,women,prestige,education),col=as.numeric(type))
pairs(cbind(log10(income),logit(women),prestige,education),col=as.numeric(type))
# Gegevens niet multivariaat normaal, na herschaling wordt benaderde verdeling iets beter. (meer elliptisch)
# Onduidelijk of covarianties homogeen zijn (kijk naar grootte en orientatie van omhullende ellipsen per kleur) 
# al zijn er geen extreem grote afwijkingen

## lda voor originele data
#Bekijk eerst de data
type=Prestige$type
i = which(is.na(type)) # 4 NA-waarden in variabele type
X = cbind(income,women,prestige,education)[-i,]#laat die 4 weg
y = type[-i]
orig.lda = predict(lda(X,y))$posterior
orig.lda.CV = lda(X,y,CV=TRUE)$class
orig.lda.AER = table(y,orig.lda.CV); orig.lda.AER
sum(orig.lda.AER-diag(diag(orig.lda.AER)))/sum(orig.lda.AER) 
# Indeling zeker niet slecht, gebrek aan normaliteit maakt a posteriori-kansen zinloos

## lda voor getransformeerde data
Xt = cbind(log10(income),logit(women),prestige,education)[-i,]
trans.lda = predict(lda(Xt,y))$posterior
trans.lda.CV = lda(Xt,y,CV=TRUE)$class
trans.lda.AER = table(y,trans.lda.CV); trans.lda.AER
sum(trans.lda.AER-diag(diag(trans.lda.AER)))/sum(trans.lda.AER) 
# Indeling quasi gelijk
table(trans.lda.CV,orig.lda.CV)
#       bc prof wc
# bc   45    0  1
# prof  0   31  0
# wc    0    0 21
which(trans.lda.CV=="bc"&orig.lda.CV=="wc")
Prestige[57,] # Buyers worden anders ingedeeld na transformatie
# slechts 1 observatie wordt anders ingedeeld
verschil = abs(trans.lda[,1]-orig.lda[,1]);verschil
head(sort(verschil,decreasing=TRUE))
# ... maar kansen lopen tot 20% uit elkaar! 
#(de verdeling is nog steeds niet normaal en dus zijn de a posteriori kansen nog steeds weinig betrouwbaar!)
which.max(verschil)
Prestige[52,] # sales clerks ondergaan grootste effect van transformatie

## qda voor originele data
orig.qda = predict(qda(X,y))$posterior
orig.qda.CV = qda(X,y,CV=TRUE)$class
orig.qda.AER = table(y,orig.qda.CV); orig.qda.AER
sum(orig.qda.AER-diag(diag(orig.qda.AER)))/sum(orig.qda.AER) 
verschil = abs(orig.qda[,1]-orig.lda[,1]); verschil #geen al te grote verschillen
table(orig.qda.CV,orig.lda.CV)


## qda voor getransformeerde data
trans.qda = predict(qda(Xt,y))$posterior
trans.qda.CV = qda(Xt,y,CV=TRUE)$class
trans.qda.AER = table(y,trans.qda.CV); trans.qda.AER
sum(trans.qda.AER-diag(diag(trans.qda.AER)))/sum(trans.qda.AER) 
# Indeling qda wat minder goed dan lda
table(trans.qda.CV,trans.lda.CV)
# 3 observaties worden anders ingedeeld
verschil = abs(trans.qda[,1]-trans.lda[,1])
head(sort(verschil,decreasing=TRUE))
# opnieuw enorme verschillen in kans...

table(trans.qda.CV,orig.qda.CV)


# knn-methode op originele data
library(class)
orig.knn = knn(train=X,test=X,cl=y,k=5); orig.knn
# gebruik knn-object enkel om nieuwe (of test-)data X2 te classificeren (test=X2)
# DOE HET VOLGENDE NIET OM DE KWALITEIT VAN KNN TE EVALUEREN:
orig.knn.APER = table(orig.knn,y); orig.knn.APER
sum(orig.knn.APER-diag(diag(orig.knn.APER)))/sum(orig.knn.APER)
# met train=test heeft deze methode niet zoveel zin, 
# het is de bedoeling nieuwe observaties waarvan de klasse ongekend is (test), 
# te classificeren op basis van de trainingsset train met indeling cl
# Is train=test dan is er sowieso vertekenening: observatie zit de facto bij k dichtste punten
# In het bijzonder is k=1 op die manier altijd "perfect"
# ZORG DAT JE BEGRIJPT WAAROM APER bij KNN GEEN GOED IDEE IS!!!
FOUT.knn = knn(train=X,test=X,cl=y,k=1)
FOUT.knn.APER = table(FOUT.knn,y); FOUT.knn.APER
sum(FOUT.knn.APER-diag(diag(FOUT.knn.APER)))/sum(FOUT.knn.APER)

# MAAR WEL DIT:
orig.knn.cv = knn.cv(train=X,cl=y,k=5); orig.knn.cv
orig.knn.AER = table(orig.knn.cv,y); orig.knn.AER
sum(orig.knn.AER-diag(diag(orig.knn.AER)))/sum(orig.knn.AER)
#knn.cv is bedoeld om schatter voor de AER te bepalen. 
#In dat geval is er logischerwijze geen test-argument. 
# knn gebruikt geen informatie over verdeling en is dus bruikbaar als normaliteit 
# niet geldt. Door geen gebruik te maken van informatie over enige 
# verdeling (maar van Euclidische afstand) is de methode in het algemeen wel 
# minder krachtig. Mogelijk te verbeteren door data te schalen!

## knn-methode op geschaalde data, aanbovelen wegens verschillende eenheden en gebruik van euclidische afstand
Xs = scale(X)
scale.knn.cv = knn.cv(Xs,y,k=5); scale.knn.cv  
scale.knn.AER = table(scale.knn.cv,y); scale.knn.AER
#       bc prof wc
# bc   42    1  4
# prof  0   29  2
# wc    2    1 17
sum(scale.knn.AER-diag(diag(scale.knn.AER)))/sum(scale.knn.AER)
#0.5306122
# herschaling maakt de methode in dit geval even krachtig als discriminantmethode

## knn-methode op getransformeerde data
trans.knn.cv = knn.cv(Xt,y,k=5); trans.knn.cv
trans.knn.AER = table(trans.knn.cv,y); trans.knn.AER
#      bc prof wc
# bc   41    0  6
# prof  1   31  2
# wc    2    0 15
sum(trans.knn.AER-diag(diag(trans.knn.AER)))/sum(trans.knn.AER)
#0.1122449

## knn-methode op geschaalde data en getransformeerde data
Xst = scale(Xt)
trans.scale.knn.cv = knn.cv(Xst,y,k=5); trans.scale.knn.cv
trans.scale.knn.AER = table(trans.scale.knn.cv,y); trans.scale.knn.AER
#       bc prof wc
# bc   41    0  4
# prof  0   29  2
# wc    3    2 17
sum(trans.scale.knn.AER-diag(diag(trans.scale.knn.AER)))/sum(trans.scale.knn.AER)
#0.1122449
detach(Prestige)

