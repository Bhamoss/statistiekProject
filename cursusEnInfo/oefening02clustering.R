#### Oefenzitting 5 - Cluster analyse
#install.packages("cluster")
library(cluster)
attach(iris)

##---------------
##DATASET IRIS
##---------------

summary(iris)
X = iris[,1:4]

##********
##1.
##********
boxplot(X)
# zelfde eenheden en grosso modo zelfde grootte-orde, herschalen niet nodig
boxplot(Petal.Length~Species)
boxplot(Petal.Width~Species)
boxplot(Sepal.Length~Species)
boxplot(Sepal.Width~Species)
# Setosa zou moeten te onderscheiden zijn, de andere soorten overlappen

##********
##2.
##********
#a) zonder schalen
start = apply(X,2,quantile)[2:4,]
X.km = kmeans(X,3,centers=start)
X.km
attributes(X.km)
table(X.km$cluster,iris$Species)
plot(Petal.Length,Petal.Width,col=as.numeric(Species),pch=X.km$cluster)
#De groep setosa wordt volledig correct geidentificeerd, 16 van 100 andere bloemen 
#worden verkeerd ingedeeld (wat behoorlijk is, zonder voorkennis over de soorten!)

#b) met schalen
start.scale = apply(scale(X),2,quantile)[2:4,]
X.km.scale = kmeans(scale(X),3,centers=start.scale)
table(X.km.scale$cluster,iris$Species)
#Schalen doet de classificatie geen goed, integendeel, 25 bloemen worden nu verkeerd 
#ingedeeld

#vergelijk geschaalde en niet-geschaalde dataclustering
table(X.km.scale$cluster,X.km$cluster)

##********
##3.
##********
table(kmeans(scale(X),3)$cluster,iris$Species)
table(kmeans(scale(X),3)$cluster,X.km.scale$cluster)
table(kmeans(X,3)$cluster,X.km$cluster)
# Random startwaarden: de nummering van de groepen is niet steeds gelijk, maar de 
#classificiatie is meestal wel equivalent. Heel af en toe wijkt ze helemaal af. Dat 
#de meeste begincondities naar dezelfde oplossing convergeren, versterkt de idee dat
#dit de optimale oplossing is. 
#Dat deze oplossing in het algemeen overeenkomt met re?le groepen (zoals hier met 
#Species) is op geen enkele manier gegarandeerd. In het algemeen is het ook helemaal
# niet a priori duidelijk hoeveel groepen er zijn.

##********
##4.
##********
X.pam = pam(X,3)
attributes(X.pam)
table(X.pam$clustering,X.km$cluster)
# identieke indeling (is niet algemeen zo)
table(X.pam$clustering,iris$Species)

X.silhouette = silhouette(X.pam); X.silhouette[1:5,]
summary(X.silhouette)
order.silhouette = as.numeric(row.names(X.silhouette)) # beetje technisch, zo kan je werkelijke groepen tonen op silhouette-plot
plot(X.silhouette,col=iris$Species[order.silhouette])
# De zestien verkeerd geclassificeerde bloemen hebben inderdaad relatief kleine 
#silhouette-breedte.

table(X.silhouette[,1],iris$Species[order.silhouette])
X.silhouette[X.silhouette[,1]==2&iris$Species[order.silhouette]=="virginica",2]
X.silhouette[X.silhouette[,1]==3&iris$Species[order.silhouette]=="versicolor",2]
# bovenste rij indices (in de oorspronkelijke volgorde!) en alternatieve groep van fout ingedeelde 
#bloemen
# De verkeerd ingedeelde bloemen liggen inderdaad dicht bij de correcte groep
which(X.silhouette[,1]==2&iris$Species[order.silhouette]=="virginica")
#indices in de oorspronkelijke volgorde (als kolomnaam - uit oorspronkelijke data) en indices volgens 
#aflopende silhouetten (indices uit X.silhouette)

##********
##5.
##********
summary(X.silhouette)
attributes(summary(X.silhouette))
summary(X.silhouette)$avg.width

# De gemiddelde silhouette-breedte is 0.55 wat wijst op een redelijke indeling.
K = 2:20; S = NULL
for (k in K) {
  S = c(S,summary(silhouette(pam(X,k)))$avg.width)
}
rbind(K,S)
# De gemiddelde silhouette-breedte is maximaal voor 2 groepen, de gebruikte 
#clustering-methode suggereert dus een opdeling in maar twee groepen: 
#setosa vs de rest. Zonder voorkennis ziet clustering dus geen verschil
#tussen de twee meest gelijkende variëteiten

##********
##6.
##********
clusplot(X.pam)
# Cluster plot start van principaalcomponenten en toont inderdaad 1 duidelijk 
# afgelijnde groep, terwijl de twee andere overlappen waardoor een aantal bloemen moeilijk 
# kunnen ingedeeld worden

##********
##7.
##********
X.fa = fanny(X,3)
attributes(X.fa)
head(X.fa$membership)
table(X.fa$clustering,X.km$cluster)
# vergelijkbare maar niet identieke indeling. Let op de andere volgorde van codering!
table(X.fa$clustering,iris$Species)
# Slechts 13 verkeerd ingedeelde bloemen.
fout = X.fa$clustering==2&Species=="versicolor"|X.fa$clustering==3&Species=="virginica"
which(fout)
X.fa$membership[fout,]
tapply(abs(X.fa$membership[,3]-X.fa$membership[,2]),fout,summary)
# Fout ingedeelde bloemen hebben kleine verschillen in gewicht tussen cluster 2 en 3 
#(maximaal 0.26). 
#Er zijn echter ook goed ingedeelde bloemen met zo een laag verschil.
X.silhouette = silhouette(X.fa)
order.silhouette = as.numeric(row.names(X.silhouette))
plot(X.silhouette,col=iris$Species[order.silhouette])
# Er is een zekere structuur, maar absoluut niet "crisp"
sum(X.fa$membership**2)/nrow(X.fa$membership)

##---------------
##HIERARCHISCHE INDELING
##---------------

##********
##1.
##********

X.an = agnes(X)
X.an
attributes(X.an)
bannerplot(X.an)
pltree(X.an) # duidelijke suggestie voor 2 groepen
cutree(X.an,k=2)
table(Species,cutree(X.an,k=2))
cutree(X.an,k=c(2,3)) # meerdere indelingen vergelijken
table(Species,cutree(X.an,k=3))

X.da = diana(X)
attributes(X.da)
bannerplot(X.da)
pltree(X.da) # suggereert eerder 3 groepen
cutree(X.da,3)
table(Species,cutree(X.da,k=3))

detach(iris)

##********
##2.
##********
D = cbind( a=c(0,2,6,8,9), b=c(2,0,3,7,6), c=c(6,3,0,5,5), d=c(8,7,5,0,4), e=c(9,6,5,4,0) )
D.av = agnes(D, diss= TRUE, method='average')
bannerplot(D.av)
pltree(D.av)

D.sl = agnes(D, diss= TRUE, method='single')
bannerplot(D.sl)
pltree(D.sl)

D.cl = agnes(D, diss= TRUE, method='complete')
bannerplot(D.cl)
pltree(D.cl)


##********
##3.
##********

library(MASS)
attach(crabs)
pairs(crabs)
# zeer sterk gecorreleerde afmetingen, geen overduidelijke samenhang met soort en geslacht

X = crabs[,4:8]

type = as.factor(paste(sex,sp,sep="+"));type

### Hierarchische modellen om structuur van data te verkennen
## Agglomerate nesting
X.agnes = agnes(X)
bannerplot(X.agnes)
pltree(X.agnes)
# bannerplot suggereert indeling in 2 of 3 clusters

# Indeling in twee clusters:
table(cutree(X.agnes,2),type)
plot(RW,FL,col=type,pch=cutree(X.agnes,2))
# Beide cluster bevatten zowel F als M, zowel B als O,
# Indeling slaat op globale grootte van het beest

# Indeling in drie clusters:
table(cutree(X.agnes,3),type)
plot(RW,FL,col=type,pch=cutree(X.agnes,3))
# Indeling opnieuw louter op afmetingen van het beest (S, M, L)
# geen direcht verband met de groepen

## Divisive analysis
X.da=diana(X)
bannerplot(X.da)
pltree(X.da)
# bannerplot suggereert indeling in 2 clusters (ev. 4 clusters)

table(cutree(X.da,4),type)
plot(RW,FL,col=type,pch=cutree(X.da,4))
# more of the same: S, M, L, XL

# Hierarchische methodes leveren geen clustering op die overeenkomt met de 4 groepen

### Partitionerende modellen
K = 2:10; S = NULL
for (k in K) {
  S = c(S,summary(silhouette(pam(X,k)))$avg.width)
}
rbind(K,S)
table(kmeans(X,2)$cluster,type)
table(pam(X,2)$cluster,type)
table(fanny(X,2)$cluster,type)
plot(RW,FL,pch=as.numeric(type),col=fanny(X,2)$cluster)
# Geen enkele indeling correspondeert met sex X sp,
# louter opdeling van "grootte" 

# Schalen is niet nodig: veranderlijken hebben zelfde dimensie en grootte-orde
# Geschaalde crabs-data zal hier gelijkaardige resultaten opleveren (dit is niet algemeen zo!).
# Met principaalcomponenten zijn wel dingen te zien, maar daarvoor nog even geduld oefenen!

detach(crabs)

##********
##4.
##********

library(car)
attach(Prestige)
head(Prestige)

# Data verkennen:
pairs(Prestige[1:4])
# hoge lonen, lange opleiding hoge prestige horen samen
# %vrouwen redelijk gespreid over drie andere
boxplot(Prestige$income~Prestige$type)
boxplot(Prestige$education~Prestige$type)
boxplot(Prestige$prestige~Prestige$type)
boxplot(Prestige$women~Prestige$type)
# prof > wc > bc voor loon/opleiding/prestige
# enkel bij bedienden beroepen met uitgesproken vrouwelijke vertegenwoordiging

# Verschillende eenheden (jaar, dollar, procent), schalen absoluut noodzakelijk
X = scale(Prestige[,1:4])

# Clustering
X.agnes = agnes(X)
bannerplot(X.agnes)
pltree(agnes(scale(Prestige[,1:4])))
K = 2:10; S = NULL
for (k in K) {
  S = c(S,summary(silhouette(pam(X,k)))$avg.width)
}
rbind(K,S)
# Zwakke structuur: het dendrogram toont geen echte gap in height, de SC is kleiner dan 
# 0.4 - bereikt bij drie groepen.
X.pam = pam(X,3)

table(Prestige$type,X.pam$clustering)
# groepen volgen min of meer de types job, maar niet erg strikt,
# 1 Prof
# 2 wc
# 3 bc
boxplot(Prestige$income~X.pam$clustering)
boxplot(Prestige$education~X.pam$clustering)
boxplot(Prestige$prestige~X.pam$clustering)
boxplot(Prestige$women~X.pam$clustering)
# mogelijk weegt % vrouwen ook een rol
# alle fout geclusterde prof-beroepen zijn eerder vrouwelijk
# 1 prof + %women laag
# 2 %women hoog
# 3 non-prof + %women laag

detach(Prestige)