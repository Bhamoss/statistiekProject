#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#Oefenzitting 13: logistische regressie
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

###*******************************************************
### Space Shuttle
###*******************************************************
library(Sleuth3)
challenger = ex2011; attach(challenger)
head(challenger); summary(challenger)

# 1
model.glm = glm(y~x, family=binomial)
summary(model.glm)
anova(model.glm,test="LRT")
attributes(model.glm)
# 2
predict(model.glm, data.frame(x=...),se.fit = TRUE)

###--------------------------------------------------------------------
### 1. 
###--------------------------------------------------------------------
challenger.glm = glm(Failure~Temperature, family=binomial)
summary(challenger.glm) # Wald-test is vergelijkbaar met t-test: nul-zijn van de coëfficiënt
# volgens de Wald-test is het effect van de temperatuur significant (p=4%)
# Null-model (Failure~1) heeft deviantie 29
# Gesatureerd model heeft deviantie 0
# Beschouwde model ligt daar tussen:
# - vergelijking met null-model test of het model hoegenaamd wel iets verklaart,
# - vergelijking met gesatureerd model test of model significant minder verklaart dan "perfect model" (vergelijk met goodness-of-fit bij ANOVA)
# ?anova.glm
anova(challenger.glm,test="LRT") # LRT is vergelijkbaar met F-test, maar is ongelijk in OLS context niet equivalent met test op nul zijn van coëfficiënt
# manueel via chisq-verdeling:
challenger.glm$null.deviance # 29 totale deviantie in y~1 model
challenger.glm$df.null       # 24 meetwaarden - "gemiddelde"schatting = 23 df
challenger.glm$deviance      # 23 overblijvende deviantie in y~x model
challenger.glm$df.residual   # 24 meetwaarden - 2 coeff (intercept en slope) = 22 df
1-pchisq(
  challenger.glm$null.deviance-challenger.glm$deviance,
  challenger.glm$df.null-challenger.glm$df.residual) # 6 deviantie verklaard door 1 df is licht significant p=1%
# goodness-of-fit-test: hypothetisch "saturated" model heeft 0 onverklaarde deviantie en 0 df
1-pchisq(challenger.glm$deviance,challenger.glm$df.residual)
# logistisch model wijkt niet significant af van het gesatureerde model, verklaart "niet significant minder", de resterende 22 vrijheidsgraden verklaren een blijkbaar lage extra  23 deviantie

# Ook de deviantie-test is significant, zij het met p=1%: Wald- en deviantie-test zijn niet equivalent, in tegenstelling tot t- en F-test die dat wel zijn in een eenvoudig lineair model
###--------------------------------------------------------------------
### 2. Coefficienten verklaren
###--------------------------------------------------------------------
A = exp(challenger.glm$coefficients)[1]; A
A/(1+A)
# intercept is natuurlijk niet relevant wegens enorm verre extrapolatie: bij nul graden (Fahrenheit) wordt de odds-ratio 52857 keer groter, wordt de kans op faling 52857 keer groter dan die op geen faling, of is de kans op faling 99.99%
B = exp(challenger.glm$coefficients)[2]; B; 1/B # B = odds(t+1)/odds(t)
# per extra graad Fahrenheit verkleint de odds ratio met factor 0.84

# voorbeeld: voorspelling bij 70 graden Fahrenheit
P = exp(predict(challenger.glm,data.frame(Temperature=70))); 1/P
P/(1+P)
# kans op success is drie keer zo groot als kans op failure, kans op failure bij 70 graden Fahrenheit is 25%, 75% kans op succes

# Grafische voorstelling
plot(c(20,85),c(0,1),type='n',xlab='Temperature',ylab='Failure')
x = 20:100
y = predict(challenger.glm, data.frame(Temperature=x),se.fit = TRUE)
# ?predict.glm # werkt een beetje anders dan predict.lm
points(as.numeric(Failure)-1~Temperature) #Failure is 1 or 2. We want 0 or 1.
lines(x,1/(1+exp(-y$fit)),col='red',lwd=2) #formule p 367
L = y$fit - qnorm(.975)*y$se.fit
R = y$fit + qnorm(.975)*y$se.fit
lines(x,1/(1+exp(-L)),col='red',lty=2)#p379 cursus 1819
lines(x,1/(1+exp(-R)),col='red',lty=2)
abline(h=.5,col='green')

#kans op falen = 50%
xm = -challenger.glm$coefficients[1]/challenger.glm$coefficients[2]
abline(v=xm,col='green')
xm

###--------------------------------------------------------------------
### 3. Voorspelling
###--------------------------------------------------------------------
y0 = 1/(1+exp(-c(y$fit[12],L[12],R[12]))); y0 # temp 31 is 12e element in onze dataset
abline(v=31,col='blue'); points(c(31,31,31),y0,col='blue')
# Bijna zekere faling, maar zware extrapolatie: enorm betrouwbaarheidsinterval, niks is zeker: veel te kleine dataset!
detach(challenger)

###*******************************************************
### Donner Party
###*******************************************************
donner = case2001
attach(donner)
donner

###--------------------------------------------------------------------
### 4. Additief model
###--------------------------------------------------------------------
donner.null = glm(Status~1, family=binomial)
donner.glm = glm(Status~Age+Sex, family=binomial)
summary(donner.glm)
anova(donner.glm,test="LRT")
anova(donner.glm,donner.null,test="LRT")
1-pchisq(
  donner.glm$null.deviance-donner.glm$deviance,
  donner.glm$df.null-donner.glm$df.residual) # vgl. met null-model
1-pchisq(donner.glm$deviance,donner.glm$df.residual) # vgl. met verzadigd model
# zeer significant model dat niet significant minder verklaart dan verzadigd model

plot(as.numeric(Status)-1~Age,col=Sex,ylab="Status")
x=sort(Age)
y=donner.glm$fitted.values[order(Age)]
s=Sex[order(Age)]
lines(x[s=="Male"],y[s=="Male"],col=2)
lines(x[s=="Female"],y[s=="Female"],col=1)

###--------------------------------------------------------------------
### 5. Coefficienten
###--------------------------------------------------------------------
beta = exp(donner.glm$coefficients); beta
# - voor een vrouwelijke nul-jarige is de kans op overleven 25 keer groter dan de kans op omkomen (sterk geextrapoleerd!)
# - odds voor overleven bij vrouwen 5 keer groter dan bij mannen
# - per extra levensjaar verkleint de odd ratio op overleven met 7.5%
exp(donner.glm$coefficients[3]+c(-1,1)*qnorm(.975)*summary(donner.glm)$coefficients[3,2])
# M/V odds-ratio voor overleven = 1/5, BI = [.05,.9]: sowieso kleinere overlevingskans voor mannen

###--------------------------------------------------------------------
### 6. Overlevingskans 50%
###--------------------------------------------------------------------
abline(h=.5,col='green')
xF = -donner.glm$coefficients[1]/donner.glm$coefficients[2]; xF
xM = (-donner.glm$coefficients[1]-donner.glm$coefficients[3])/donner.glm$coefficients[2]; xM
points(xF,.5,col='green'); abline(v=xF,col='green')
points(xM,.5,col='green'); abline(v=xM,col='green')

###--------------------------------------------------------------------
### 7. Multiplicatief model
###--------------------------------------------------------------------
donner.glm2 = glm(Status~Sex*Age, family=binomial)
summary(donner.glm2)
anova(donner.glm2,test="LRT")
# randsignificant. steekproef mogelijk te klein om uitsluitsel te geven, effect is wel enorm zoals blijkt uit grafiek:
C = exp(summary(donner.glm2)$coefficients[1])
A = exp(summary(donner.glm2)$coefficients[3])
M = exp(summary(donner.glm2)$coefficients[2])
E = exp(summary(donner.glm2)$coefficients[4])
yF2 = C*A^x/(1+C*A^x)
yM2 = C*M*(A*E)^x/(1+C*M*(A*E)^x)
lines(x,yF2,col=1,lty=2)
lines(x,yM2,col=2,lty=2)
# vanaf ongeveer 42 jaar keert de odds om! vrouwen lijken een veel uitgesprokener leeftijdseffect te hebben, terwijl de odds bij mannen minder onderhevig lijkt aan ouderdom
detach(donner)



#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
### UITBREIDING: valt buiten bestek van de cursus
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

###*******************************************************
### Binomiale respons: Island Size and Bird Extinctions
###*******************************************************
Krunnit = case2101
#install.packages('gtools')
library(gtools)
attach(Krunnit); head(Krunnit)
boxplot(Area)
plot(logit(Extinct/AtRisk)~Area)
boxplot(log(Area))
plot(logit(Extinct/AtRisk)~log(Area))
# scheefheid van Area suggereert duidelijk logaritmische transformatie
Krunnit.glm = glm(I(Extinct/AtRisk)~log(Area),weights=AtRisk,family=binomial)
summary(Krunnit.glm)
# in plaats van binaire respons is respons nu een schatting voor pi_i
# maar een schatting op basis van meer pogingen is nauwkeuriger
# vandaar de gewichten
glm(cbind(Extinct,AtRisk-Extinct)~log(Area),family=binomial)
# alternatieve syntax:
# multivariate (succes,faling) als respons, geen gewichten
Krunnit.lm = lm(logit(Extinct/AtRisk)~log(Area))
abline(Krunnit.glm,col='red')
abline(Krunnit.lm,col='red',lty="dashed")
plot(Extinct/AtRisk~log(Area))
lines(log(Area),Krunnit.glm$fitted.values,col='red')
lines(log(Area),inv.logit(Krunnit.lm$fitted.values),col='red',lty="dashed")
# glm geeft niet zelfde schatting als lm op logit-waarden!
# door andere "family" worden residuen in MLE anders gewogen
summary(Krunnit.glm)
attributes(Krunnit.glm)
cbind(
  Observed = Extinct/AtRisk,
  Estimate = Krunnit.glm$fitted.values,
  Raw = residuals(Krunnit.glm,"response"),
  Pearson = residuals(Krunnit.glm,"pearson"),
  Deviance = residuals(Krunnit.glm,"deviance"))
# Display 21.5
# de gewone ("response" of "Raw") residuen - i.e. afstand tussen de regressielijn en getransformeerde gegevens in de residuplot hierboven - hebben weinig betekenis.

## Goodness-of-Fit Test
Krunnit.sat = glm(Extinct/AtRisk~as.factor(1:18),weights=AtRisk,family=binomial)
Krunnit.red = glm(Extinct/AtRisk~1,weights=AtRisk,family=binomial)
anova(Krunnit.red,Krunnit.glm)
1-pchisq(33.277,1)
# model zeer significant ten opzichte van het gereduceerde pi_i = constant
anova(Krunnit.glm,Krunnit.sat)
summary(Krunnit.red)
1-pchisq(12.062,16)
# model niet significant verschillend van het verzadigde model pi_i = alpha_i
# het model Krunnit.red is eigenlijk overbodig: zelfde berekening op basis van
# "Residual deviance: 12.062  on 16  degrees of freedom"
# in de output van summary(Krunnit.glm)
detach(Krunnit)

###*******************************************************
### Poissonrespons
###*******************************************************
### Age and mating Success of Male Elephants
elephant = case2201
attach(elephant); head(elephant)
plot(Matings~Age)
elephant.glm = glm(Matings~Age,family=poisson)
summary(elephant.glm)
lines(Age,elephant.glm$fitted.values,col='red')
plot(residuals(elephant.glm,"deviance")~Age); abline(h=0,col='red')
detach(elephant)

## Litter
attach(case0902)
animals = data.frame(Litter, log10(Body), log10(Gestation), log10(Brain))
litter.glm = glm(Litter ~ ., data=animals,family=poisson)
summary(litter.glm)
1-pchisq(litter.glm$deviance,litter.glm$df.residual)
1-pchisq(
  litter.glm$null.deviance-litter.glm$deviance,
  litter.glm$df.null-litter.glm$df.residual)
# zeer significant en dicht bij verzadigd model

