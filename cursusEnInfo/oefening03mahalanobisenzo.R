### Voorbeeld
x = c(1,2,3)
y = c(4,5,6)
A = cbind(x,y);A
t(A)
B = t(A) %*% A; B
eigen(B)
C = solve(B); C
B %*% C
diag(3)
xx = x %*% x; xx
xx*A
as.numeric(xx)*A
D = data.frame(x,y)
t(D)%*%D
t(as.matrix(D))%*%as.matrix(D)
mean(A)
colMeans(A)
colMeans(D)

### Oefeningen Beschrijvende multivariate statistiek
x = rnorm(100); y = rnorm(100); z = x-2*y
X = cbind(x,y,z)

head(X)
class(X)
n = dim(X)[1]; n
p = dim(X)[2]; p

one = matrix(rep(1,n))
dim(one) # controleer steeds dimensies!
xbar = t(one)%*%X/n; xbar
colMeans(X)

W = t(X - one%*%xbar)%*%(X - one%*%xbar)
S = W/(n-1); S
cov(X)

D = diag(diag(S)); D
R = sqrt(solve(D))%*%S%*%sqrt(solve(D));R
cor(X)

pairs(X)

### Oefeningen Mahalanobisafstand
m = as.matrix(c(75,1.75))
a = as.matrix(c(75,1.85))
b = as.matrix(c(85,1.75))
c = as.matrix(c(85,1.85))
d = as.matrix(c(65,1.85))

D = as.matrix(diag(c(100,0.01)))
Dinv = solve(D); Dinv
S = as.matrix(cbind( c(100,0.9), c(0.9,0.01) ))
Sinv = solve(S)

sqrt(t(m-a) %*% (m-a))
sqrt(t(m-b) %*% (m-b))
sqrt(t(m-c) %*% (m-c))
sqrt(t(m-d) %*% (m-d))

sqrt(t(m-a) %*% Dinv %*% (m-a))
sqrt(t(m-b) %*% Dinv %*% (m-b))
sqrt(t(m-c) %*% Dinv %*% (m-c))
sqrt(t(m-d) %*% Dinv %*% (m-d))

(t(m-a) %*% Sinv %*% (m-a))
(t(m-b) %*% Sinv %*% (m-b))
(t(m-c) %*% Sinv %*% (m-c))
(t(m-d) %*% Sinv %*% (m-d))

mahalanobis(t(a),m,S)

### Voorbeeld
#install.packages('ellipse')
library(ellipse)
x = rnorm(100); y = rnorm(100)
A = cbind(x,y)
plot(x,y,main="Twee ongecorreleerde veranderlijken")
points(mean(x),mean(y),pch=8,col='red')
lines(ellipse(cov(A),centre = colMeans(A),level=.95),col='red')

### Oefeningen Meetkundige interpretatie
P = matrix(c(mean(x)+sd(x),mean(y)+sd(y)),1,2)
points(P,pch=8,col="blue")
dMsq = mahalanobis(P,colMeans(A),cov(A)); dMsq
pchi = pchisq(dMsq,2); pchi
lines(ellipse(cov(A),centre=colMeans(A),level=pchi),col='blue')
# Het punt wijkt af in x- noch y-richting en onderscheidt zich evenmin van de puntenwolk.

z = y-2*x
A = cbind(x,z)
plot(x,z,main="Twee negatief gecorreleerde veranderlijken")
points(mean(x),mean(z),pch=8,col="red")
lines(ellipse(cov(A),centre=colMeans(A),level=.9),col="red")
P = matrix(c(mean(x)+sd(x),mean(y)+sd(y)),1,2)
points(P,pch=8,col='blue')
dMsq = mahalanobis(P,colMeans(A),cov(A)); dMsq
pchi = pchisq(dMsq,2); pchi
lines(ellipse(cov(A),centre=colMeans(A),level=pchi),col="blue")
# Het punt wijkt af in x- noch y-richting maar onderscheidt zich toch van de puntenwolk: waar grotere x-waarden een gemiddeld kleinere z-waarde heeft, is dat bij het punt P net omgekeerd.


#install.packages('MASS')
#install.packages('ellipse')
#install.packages('car')
library(MASS)
library(car)
library(ellipse)

### DATASET CRABS
crabs = crabs
help(crabs)
names(crabs)#Opvragen van de variabelen (koppen)
####VOOR CL EN CW
X = crabs[,6:7]
n = dim(X)[1]
p = dim(X)[2]
#1. univariate marginalen
#Maak qqplots om visueel beeld te krijgen
qqnorm(X[,1])
qqnorm(X[,2])
#De data lijken normaal. De shapiro-wilktest berekent correlatie tussen theoretische en empirische kwantielen. Met de p-waarde wordt de nulhypothese (data zijn normaal) al an niet verworpen
shapiro.test(X[,1]) #0.3527, p-waarde >0.05, dus nulhypothese niet verwerpen
shapiro.test(X[,2]) #0.2542, p-waarde >0.05, dus nulhypothese niet verwerpen
# Univariate testen tonen geen afwijking van normaliteit
#2. Bivariate marginalen
M = colMeans(X); M
C = cov(X); C
plot(X)
## Puntenwolk toont op het eerste zicht geen sterke afwijking van de elliptische vorm
lines(ellipse::ellipse(C,centre=M,level=0.95),col='red')#ellipse:: is om aan te geven uit welk package het commando ellipse komt
points(M[1],M[2],col='red',pch='x')#pch is puntstijl
#3. Radiale marginalen
MD = mahalanobis(X,M,C) # mahalanobis() geeft de kwadratische afstanden van elk punt tot colmeans!
qqplot(qchisq(ppoints(n),df=p),MD)#qqplot maken met kwantielen van chi-kwadraatverdeling. ppoints(n) kiest n punten tussen 0 en 1 (grenzen niet inbegrepen) op gelijke afstand
# points(qchisq((seq(1:n)-1/3)/(n+1/3),2),sort(MD),col='red') ## De termen 1/3 in de cursus (p.51) zijn nogal arbitrair en worden door R anders gekozen, het verschil is niet relevant.
abline(0,1,col='blue')#0=veric verschuiving, 1 is rico
abline(h=qchisq(.975,p),col='red')
## Het belangrijkste aspect van deze qqplot is dat de punten de eerste bissectrice volgen
## Twee van 200 punten liggen hoger dan het 97.5%-kwantiel, er is een lichte afwijking in de staart: de verdeling is wat te "compact".
## De meeste technieken zijn robuust genoeg en blijven betrouwbaar bij dergelijke lichte afwijkingen van normaliteit.
## De qq-plot is zelf niet zo robuust: M en C zijn sterk onderhevig aan uitschieters (zie later)!

####VOOR ANDERE 5 VARIABELEN
X = crabs[,4:8]
n = dim(X)[1]
p = dim(X)[2]
qqnorm(X[,1]); qqline(X[,1])
qqnorm(X[,2]); qqline(X[,2])
qqnorm(X[,3]); qqline(X[,3])
qqnorm(X[,4]); qqline(X[,4])
qqnorm(X[,5]); qqline(X[,5])
shapiro.test(X[,1])#W = 0.99037, p-value = 0.2023
shapiro.test(X[,2])#W = 0.9951, p-value = 0.7646
shapiro.test(X[,3])#W = 0.9921, p-value = 0.3527
shapiro.test(X[,4])#W = 0.99106, p-value = 0.2542
shapiro.test(X[,5])#W = 0.99027, p-value = 0.1957
## Univariate testen tonen geen afwijking van normaliteit
M = colMeans(X); M
C = cov(X); C
plot(X)
## Puntenwolken met CL, CW en BD tonen op het eerste zicht geen sterke afwijking van de elliptische vorm, FL en RW wel: 2 groepen?
plot(X[,2:3])
lines(ellipse::ellipse(C[2:3,2:3],centre=M[2:3],level=0.95),col='red')
points(M[2],M[3],col='red',pch='x')
## Een afzonderlijke puntenplot bevestigt bovenstaande bevinding (geen mooie verdeling van datapunten binnen ellips)
MD = mahalanobis(X,M,C) # mahalanobis() geeft de kwadratische afstanden!
qqplot(qchisq(ppoints(n),df=p),MD)
#points(qchisq((seq(1:n)-1/3)/(n+1/3),2),sort(MD),col='red') ## De termen 1/3 in de cursus p.51 zijn nogal arbitrair en worden door R anders gekozen, het verschil is niet relevant.
abline(0,1,col='blue')
abline(h=qchisq(.975,p),col='red')
# De bivariate marginalen tonen hier waarschijnlijk het sterkst de afwijking van multivariate normaliteit,
# in de qqplot is hetzelfde minder duidelijk te zien: de punten zouden rond de rechte moeten kronkelen,
# maar blijven hier lange tijd aan dezelfde kant
# De groepen die zich lijken af te tekenen konden we nochtans eerder bij clustering niet onderscheiden (clusters stemden overeen met groot vs klein)
# Hier doorbreken die groepen (stijlere en vlakkere wolk) de multivariate normaliteit.

### HOTELLING TEST VOOR 1 GROEP
#a)

X = cbind(crabs$CL,crabs$CW)
X
n = dim(X)[1]
p = dim(X)[2]
C = cov(X); C
M = colMeans(X); M
M0 = c(32.5,36)

t.test(crabs$CL, mu=M0[1])
# t = -0.78369, df = 199, p-value = 0.4342
# alternative hypothesis: true mean is not equal to 32.5
# 95 percent confidence interval:
#   31.11284 33.09816
# sample estimates:
#   mean of x 
# 32.1055 
t.test(crabs$CW, mu=M0[2])
# t = 0.74466, df = 199, p-value = 0.4574
# alternative hypothesis: true mean is not equal to 36
# 95 percent confidence interval:
#   35.31685 37.51215
# sample estimates:
#   mean of x 
# 36.4145 
CL.BI = t.test(crabs$CL)$conf.int; CL.BI #31.11284 33.09816
CW.BI = t.test(crabs$CW)$conf.int; CW.BI #35.31685 37.51215
## Alle waarden (mu_CL,mu_CW) binnen [31,33]x[35,38] worden aanvaard
plot(c(30,34),c(34,38),type='n',xlab="Carapace Length",ylab="Carapace Width")#kader openen met goede asbereiken
points(crabs$CL,crabs$CW)#punten plotten
#lines(ellipse::ellipse(C,centre=M,level=0.95))#tolerantieellips: bij een normale verdeling met cov C en gem m ligt hier 95% van de data in
abline(v=CL.BI,col='blue')#betrouwbaarheidsintervallen per variabele
abline(h=CW.BI,col='blue')
points(M0[1],M0[2],col='blue',pch='x')#theoretisch gemiddelde
points(M[1],M[2],col='red',pch='x')#empirisch gemiddelde

#b)
#install.packages('rrcov')#eenmalig uitvoeren
library(rrcov)
T2.test(X,mu=M0)
# T2 = 234.64, F = 116.73, df1 = 2, df2 = 198, p-value < 2.2e-16
# alternative hypothesis: true mean vector is not equal to (32.5, 36)' 
# 
# sample estimates:
# [,1]    [,2]
# mean x-vector 32.1055 36.4145
lines(ellipse::ellipse(C/n,centre=M,level=0.95),col='red')#betrouwbaarheidsellips voor het geschatte gemiddelde

F = n*(n-p)/p/(n-1)*t(M-M0)%*%solve(cov(X))%*%(M-M0); F #116.732 = (n-p)*T^2/((n-1)*p) (p 60 cursus)
1-pf(F,p,n-p)#berekenen van de p-waarde
cor(X)
## Verschil tussen de F- en de 2 t-testen is zo groot wegens extreem hoge correlatie

### Hotelling test voor 2 groepen
#a)
#univariaat voor gewicht
x=crabs$CW[crabs$sex=="M"]
y=crabs$CW[crabs$sex=="F"]
shapiro.test(x)#W = 0.98327, p-value = 0.2368
shapiro.test(y)#W = 0.98823, p-value = 0.5256
#maw beiden afzonderlijk normaal verdeeld
var.test(x,y)#mag enkel als normaal verdeeld is
# F test to compare two variances
# data:  x and y
# F = 1.2738, num df = 99, denom df = 99, p-value = 0.2304
t.test(x,y=y,var.equal=TRUE)
# Two Sample t-test
# data:  x and y
# t = 1.0503, df = 198, p-value = 0.2948
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.025804  3.363804
# sample estimates:
#   mean of x mean of y 
# 36.999    35.830
#we besluiten dat de nulhypothese niet wordt verworpen: gemiddelden verschillen niet significant

#univariaat voor lengte
x=crabs$CL[crabs$sex=="M"]
y=crabs$CL[crabs$sex=="F"]
shapiro.test(x)
#W = 0.98595, p-value = 0.3707
shapiro.test(y)
#W = 0.98818, p-value = 0.5217
var.test(x,y)
# F test to compare two variances
# data:  x and y
# F = 1.2423, num df = 99, denom df = 99, p-value = 0.282
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.8359045 1.8464218
# sample estimates:
#   ratio of variances 
# 1.242349
t.test(x,y=y,var.equal=TRUE)
# data:  x and y
# t = 1.4854, df = 198, p-value = 0.139
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.4883888  3.4703888
# sample estimates:
#   mean of x mean of y 
# 32.851    31.360

#b)
X = cbind(crabs$CL,crabs$CW)[crabs$sex=="M",]
Y = cbind(crabs$CL,crabs$CW)[crabs$sex=="F",]
plot(crabs$CL,crabs$CW,col=crabs$sex)
lines(ellipse(cov(X),centre=colMeans(X),level=0.95),col='red')#tolerantie-ellips
lines(ellipse(cov(Y),centre=colMeans(Y),level=0.95),col='black')#tolerantie-ellips
## modelveronderstellingen lijken voldaan

#c)
T2.test(X,Y)
# T2 = 22.439, F = 11.163, df1 = 2, df2 = 197, p-value = 2.556e-05
# alternative hypothesis: true difference in mean vectors is not equal to (0,0)
# sample estimates:
#   [,1]   [,2]
# mean x-vector 32.851 36.999
# mean y-vector 31.360 35.830
plot(c(30,34),c(34,38),type='n',xlab="Carapace Length",ylab="Carapace Width")
points(crabs$CL,crabs$CW,col=crabs$sex)
MX = colMeans(X) #32.851 36.999
MY = colMeans(Y) #31.36 35.83
lines(ellipse(cov(X),centre=MX,level=0.95),col='red')#tolerantie-ellips mannen
lines(ellipse(cov(X)/length(x),centre=MX,level=0.95),col='red')#betrouwbaarheidsellips voor schatter voor gemiddelde mannen
lines(ellipse(cov(Y)/length(y),centre=MY,level=0.95),col='black')#betrouwbaarheidsellips voor schatter voor gemiddelde vrouwen
lines(ellipse(cov(Y),centre=MY,level=0.95),col='black')#tolerantie-ellips vrouwen
points(MX[1],MX[2],col='red',pch='x')
points(MY[1],MY[2],col='black',pch='x')
## M/V krabben verschillen niet echt van lengte of van breedte, maar op basis van de combinatie is er wel een duidelijk onderscheid
## Merk op dat de populaties M en V grotendeels overlappen en dat tot nu toe enkel is aangetoond dat de geslachten gemiddeld morfologisch verschillen, niet dat individuen op basis van morfologische kenmerken volgens geslacht kunnen worden ingedeeld

