### Voorbeeld
rnorm(10, 5, 1)
rbinom(10, 10, 0.2)

### Voorbeeld
x = seq(-7, 7, length=100)
plot(c(-7,7), c(0,0.4), type='n',
     xlab="x", ylab="y", main = "De normale verdeling",
     sub="Dichtheidsfunctie voor verschillende parameterwaarden")
for (k in -2:2) {lines(x,dnorm(x,k,1),col=k+3)}
for (k in 1:5) {lines(x,dnorm(x,0,k),col=k)}

### Voorbeeld
tt = t.test(x,mu=1); tt
attributes(tt)
tt$statistic
tt$p.value

### Oefening
n = 0
for (m in 1:1000) {
  x = rnorm(100)
  if (t.test(x)$p.value<0.05) {n = n+1}
}
n
## Van 1000 steekproeven zullen er telkens ongeveer 50 abusievelijk worden verworpen: dit komt natuurlijk overeen met het significantieniveau.

## Volgend stukje code giet voorgaande in een functie die voor het vervolg kan worden gebruikt
functie = function (mu=0) {
  n = 0
  for (m in 1:1000) {
    x = rnorm(100,mu)
    if (t.test(x)$p.value<0.05) {n = n+1}
  }
  n  
}
functie() # functie-aanroep met standaard-invoer mu=0
functie(1) # functie-aanroep met mu=1: de nulhypothese wordt (altijd) verworpen

### Oefening
plot(c(0,.5), c(0,1), type='n',xlab="Afwijking van de nulhypothese",
     ylab="Empirische kans op Type II-fout",
     main="Type II-fout in functie van het populatiegemiddelde")
abline(h=c(0,.95),col='blue')
for (k in seq(0,0.5,length=100)) {points(k,1-functie(k)/1000)}
# De type II-fout daalt van 1-alpha voor zeer kleine verschillen (de betrouwbaarheid) tot 0 voor zeer grote verschillen |mu1-mu0|
D = (qnorm(.975)+qnorm(.95))/sqrt(100); D
points(D,.05,col='red',pch='*'); abline(h=.05,col='red'); abline(v=D,col='red')
# Het onderscheidend vermogen, het verschil dat in 95% van de gevallen zal worden ontdekt is hier 0.36 (reken bovenstaande formule na!)

### Oefening
X = NULL
P = NULL
n = 10
p = 0.1
for (i in 1:1000)  {
  x = rbinom(n,1,p)
  X = c(X,mean(x))
  P = c(P,t.test(x,mu=p)$p.value)
}
table(P<0.05)
# In ongeveer een derde van de gevallen wordt de ware nulhypothese verworpen ondanks het gebruikte 5%-significantieniveau. Dit komt uiteraard omdat de veronderstelde verdeling van de teststatistiek ongeldig is.
x=rbinom(n,1,p)
hist(x, main="Histogram van één steekproef"); abline(v=0.1, col='red')
hist(X, main="Histogram van duizend steekproefgemiddelden"); abline(v=0.1, col='red')
## Voor een zeer scheve verdeling (hist(x)) is ook het gemiddelde van een niet al te grote steekproef nog scheef (hist(X)). De p-waarde op basis van de symmetrische t-statistiek is derhalve niet betrouwbaar. Het significantieniveau van 5% resulteert hier kennelijk in een veel grotere type I-fout.
y = 0:10
d = dbinom(y,10,.1)
plot(c(-2,10), c(0,.45), type='n', xlab="x", ylab="Dichtheid",
     main="Binomale verdeling en benadering voor np<5")
lines(y,d,type='h')
Y = seq(-2,10,length=100)
lines(Y, dnorm(Y,1,sqrt(.9)), col='red')
round(rbind(y = 0:10,
            binom = d,
            norm = pnorm(y+0.5,1,sqrt(.9))-pnorm(y-0.5,1,sqrt(.9)) ),
      digits=2)
## De exacte verdeling van het aantal successen (y) is binomiaal (d). De normale benadering die de teststatistiek veronderstelt, geldt dan ook geenszins want np=1<5.

### Oefening
X = NULL
P = NULL
Q = NULL
for (i in 1:1000)  {
  x = rnorm(50,0,1)
  y = rnorm(50,0,10)
  P = c(P,t.test(x,y, var.equal=FALSE)$p.value)
  Q = c(Q,t.test(x,y, var.equal=TRUE)$p.value)
}
rbind(table(P<0.05),table(Q<0.05))
# In het geval van gelijke steekproefgrootten zijn de verschillen niet noodzakelijk erg groot (hoewel ze in een concrete testsituatie natuurlijk wel het verschil kunnen maken tussen significant en niet-significant)
X = NULL
P = NULL
Q = NULL
for (i in 1:1000)  {
  x = rnorm(50,0,1)
  y = rnorm(500,0,10)
  P = c(P,t.test(x,y, var.equal=FALSE)$p.value)
  Q = c(Q,t.test(x,y, var.equal=TRUE)$p.value)
}
rbind(table(P<0.05),table(Q<0.05))
# Als de steekproeven een andere omvang hebben blijft de type I-fout bij de test voor verschillende varianties betrouwbaar. De test op basis van de gepoolde variantieschatting is echter helemaal niet meer betrouwbaar.