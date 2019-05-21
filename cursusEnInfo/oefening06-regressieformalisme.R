##### Oefenzitting 7 - Matrixformalisme
#install.packages("car")
library(car)
attach(Prestige); head(Prestige)
?Prestige

#### Oefeningen
### 2) X en H construeren
## Gegevensmatrices:
X = cbind(1,income,education,women); head(X); dim(X)
y = prestige; head(y)
n = dim(X)[1]; n
p = dim(X)[2]; p
# voorlopig enkel continue regressoren: niet type (een categorische veranderlijke) en niet census (een ID, waarvan het label de rijnaam is)

## Schattingsmatrix:
H = X %*%solve(t(X)%*%X)%*% t(X); dim(H)
max(abs(t(H)-H))
max(abs(H%*%H-H))
table(round(eigen(H)$values)); max(abs(eigen(H)$values-round(eigen(H)$values)))
# wars van numerieke fouten is H inderdaad symmetrisch en idempotent met eigenwaarden 1 (p keer) en 0 (n-p keer)

### 3) Schattingen:
beta = solve(t(X)%*%X) %*% t(X) %*% y; beta
# beta bevat de "estimates" uit summary(lm(prestige~income+education+women))
yhat = H %*% y; head(yhat)
I = diag(rep(1,length=n))
e = (I-H) %*% y; head(e)
S = sqrt(as.numeric((t(e)%*%e)/(n-p))); S # let op het aantal vrijheidsgraden
# as.numeric() zorgt ervoor dat S als een getal en niet als een 1x1-matrix wordt opgeslagen

### 4) Variantie-covariantiematrices:
Sigma_hatbeta = S^2 * solve(t(X)%*%X)
# sqrt(diag(Sigma_hatbeta)) zijn de "std. Errors" uit summary(lm(prestige~income+education+women))
# van covarianties is enkel het teken makkelijk te interpreteren:
# de covariantie tussen bijvoorbeeld de coëfficiënt van education en de intercept is negatief:
# deze schatter stelt dat bij een steekproef uit dezelfde populatie waarbij de coëficiënt bij education hoger is, de intercept gemiddeld lager zal zijn
# een meer kwantitatieve uitspraak is mogelijk door uit variantie en covariantie de correlatie te berekenen
Sigma_haty = S^2*H
Sigma_e = S^2*(I-H)
# Overtuig jezelf er van te begrijpen wat de betekenis is van de getallen in deze matrices!


### 5) Regressie-equivariantie:
solve(t(X)%*%X) %*% t(X) %*% e
# Immers beta(x,e) = beta(x,y-x.beta) = beta(x,y) - beta = beta - beta = 0


### 6a) Schaal-equivariantie 
beta3 = solve(t(X)%*%X) %*% t(X) %*% y/100; cbind(beta,beta3)
# De coefficienten veranderen volgens de gegeven formule

### 6b) Affiene equivariantie voor herschaling van 1 van de veranderlijken
X2 = cbind(1,income,education,women2=women/100); head(X2); dim(X2)
beta2 = solve(t(X2)%*%X2) %*% t(X2) %*% y; cbind(beta,beta2)
# De coefficient bij women2 is effectief 100 keer groter

### 7) Gestandaardiseerd regressiemodel:
Xs = scale(cbind(income,education,women)); head(Xs); dim(Xs)
ys = scale(y)
betas = solve(t(Xs)%*%Xs) %*% t(Xs) %*% ys; betas
si = apply(X,2,sd)
sy = sd(prestige)
(si*beta)/sy
es = ys - Xs %*% betas
max(abs(e/sy-es))
# Merk op dat in de cursus een extra factor sqrt(n-1) wordt gebruikt, die hier geen extra rol speelt!

### 8) Affiene equivariantie:
PCA = prcomp(Xs)
P = PCA$rotation
Z = predict(PCA); head(Z)
betaPC = solve(t(Z)%*%Z) %*% t(Z) %*% ys; betaPC
max(betaPC - solve(P) %*% betas)
# Let op! De transformatiematrix uit de formule voor affiene invariantie is gelijk aan het getransponeerde van de rotatiematrix bij PCA wegens
# Z = XP dus t(z_i) = t(x_i)P of dus z_i = t(P)x_i
