## PCA

library(MASS)
attach(crabs); help(crabs)
summary(crabs); head(crabs)
# Afmetingen: http://www.cma4ch.org/chemo/image/crab-carapace.png

## Manuele berekeningen
X = scale(crabs[,4:8])
crabs.eig = eigen(cor(X))
crabs.eig$values/sum(crabs.eig$values)
cumsum(crabs.eig$values/sum(crabs.eig$values))
plot(1:5,crabs.eig$values,main="Scree plot",xlab="Index",ylab="Variantie")
P = crabs.eig$vectors; P # Y = XP
Y1 = X%*%P; head(Y1)
## Eerste principaalcomponent correleert in dezelfde mate met alle veranderlijken:  dit is de "grootte" van de krab: kleinere PC1 (wegens mintekens) betekent grotere krab
## Tweede component kijkt naar het verschil tussen het achterlijf RW en de andere veranderlijken: hogere PC2 betekent breder achterlijf *in vergelijking met de rest van het lichaam*
## Derde component vergelijkt de grootte van het karkas met de dikte en de voorkant van het lijf (RW speelt geen rol van betekenis): hoe groter PC3, hoe dikker het lijf/breder de voorkant van de krab

## Ingebouwde commando's
crabs.pca = prcomp(X,scale=TRUE)
plot(crabs.pca)
summary(crabs.pca) 
attributes(crabs.pca)
P = crabs.pca$rotation; P
Y = predict(crabs.pca); head(Y)
max(abs(Y)-abs(Y1))
colSums(P^2)
# de eigenvectoren zijn genormeerd (en dus op het teken na bepaald, vergelijk manuele berekeningen met output van ingebouwde commando's)
## Rotatie-matrix is identiek op teken na, maar dat is arbitrair bij keuze van (genormaliseerde) eigenvector
## In de praktijk natuurlijk altijd ingebouwde commando's gebruiken

# De eerste principaalcomponent verklaart ruim 95% van alle variantie in de data.
# Een grotere krab heeft een groter achterlijf en grotere poten grotere alles.
# Deze veranderlijke kan dus in grote mate de vijf afmetingen vervangen in één maat "grootte"

tapply(Y[,2],sex,mean)
tapply(Y[,3],sp,mean)
SEX = as.factor(c("M","M","F","F")) # voor legende
SP  = as.factor(c("O","B","O","B")) # voor legende
plot(Y[,1],Y[,2],col=as.numeric(sp),pch=as.numeric(sex))
legend(-5,-.4, paste(SEX,SP), col=as.numeric(SP), pch=as.numeric(SEX))
plot(Y[,1],Y[,3],col=as.numeric(sp),pch=as.numeric(sex))
legend(-5,-0.3, paste(SP,SEX), col=as.numeric(SP), pch=as.numeric(SEX))
plot(Y[,2],Y[,3],col=as.numeric(sp),pch=as.numeric(sex))
legend(-.8,-0.4,ncol=2, paste(SP,SEX), col=as.numeric(SP), pch=as.numeric(SEX))
## Grootte van een krab (PC1) verraadt geslacht noch soort.
## Vrouwelijke krabben hebben een relatief breder achterlijf (PC2 negatief) ten opzichte van de rest van het lichaam.
## Oranje krabben hebben een dikker lijf in vergelijking met de grootte van het schild (positieve PC3)

# Ondanks het feit dat heel wat variantie in één maat te vatten is, zit er duidelijk wel nog veel informatie in de bijkomende componenten
# Herinner je dat er bij de clusteringtechnieken niet echt structurele groepen werden waargenomen (tenzij een eerder arbitraire indeling in kleinere en grotere krabben)
# In vorige sessie bleek dat de afmetingen niet verschillen naargelang één afmeting, maar wel bij simultaan vergelijken van afmetingen
# Door naar de onafhankelijke richtingen in de data te kijken, is het hier blijkbaar wel mogelijk om een quasi perfect onderscheid te maken

biplot(crabs.pca,choices=c(1,2))
biplot(crabs.pca,choices=c(1,3))
biplot(crabs.pca,choices=c(2,3))
## biplot voegt aan getransformeerde data ook "oude basisvectoren" toe zodat effect van de originele veranderlijken meteen af te lezen is
detach(crabs)

library(MASS)
data(UScereal); help(UScereal)
summary(UScereal); head(UScereal)
attach(UScereal)
names(UScereal)
## Als vereenvoudiging gaan we er van uit dat
## de voedingscategorieen elkaar onderling niet overlappen en
## de volledige inhoud van de ontbijtgranen beschrijven
## Sugars: snelle suikers, aanwezig in fruit, honing (doorgaans lekker maar calorierijk en niet beschouwd als gezond)
## Carbo: (meestal verwerkte) complexe suikers als in rijst, bloem (enkel gezond indien niet verwerkt vb. granen vs bloem)
## Fiber: onverteerbare suikers uit planten (beschouwd als erg gezond)

X = cbind(protein,fat,sodium=sodium/1000,fibre,carbo,sugars,potassium=potassium/1000)
row.names(X) = 1:dim(X)[1]
cereal.pca = prcomp(X)
# aangezien alle veranderlijken dezelfde eenheid hebben, is ongeschaalde PCA te overwegen
screeplot(cereal.pca)
summary(cereal.pca)
# drie PC's beschrijven 98% van de variabiliteit
Y = predict(cereal.pca)
cereal.pca$rotation
## PC1: hoe hoger PC1, hoe minder massa per volume (elke observatie is 1 cup, dus volume is constant). hoe hoger coefficient hoe meer variatie in die component en dus (in het algemeen) meer van die component aanwezig. granen met hoge PC1 zijn dus luchtig (gepofte rijst), lage PC1 betekent dens (noten). 
## PC2: vergelijkt carbo enerzijds met fibre en sugars anderzijds, dit zijn niet toevallig de stoffen die in grootste mate voorkomen. hoe hoger PC2 hoe minder complexe suikers en hoe meer van de andere suikers/vezels (carbo zullen automatisch aanwezig zijn in ontbijtgranen, suiker en vezels moeten doorgaans expliciet worden toegevoegd om de meerwaarde smaak/gezond).
## PC3: vergelijkt fibres (gezond) met suikers (ongezond). hoe hoger PC3, hoe minder fibers en hoe meer suiker.

cereal.pca = prcomp(X,scale=TRUE)
screeplot(cereal.pca)
summary(cereal.pca)
## drie PC's beschrijven slechts 83% van de variabiliteit, er is niet meteen een duidelijk elleboogpunt, minder duidelijk dimensiereductie mogelijk
Y = predict(cereal.pca)
cereal.pca$rotation
## PC1: maat voor de densiteit van de ontbijtgranen, vergelijkbaar met boven al geeft de grootte van de coefficient nu geen indicatie meer van hoeveel er van die stof aanwezig is.
## PC2: vet en suiker vs de rest: lagere PC2 is zoeter en vetter, kan worden beschouwd als extra lekker (maar ongezond)
## PC3: vezels en potassium vs de rest: lagere PC3 heeft meer kalium en vezels, worden beschouwd als extra gezond, moeten doorgaans expliciet worden toegevoegd
## weliswaar zelfde eenheid maar toch andere schalen (tot factor duizend), andere voedingsstoffen hebben andere "drempels" (1g zout is veel impactvoller dan 1g proteinen) dus beter schalen!

biplot(cereal.pca,choices=c(1,2))
biplot(cereal.pca,choices=c(1,3))
biplot(cereal.pca,choices=c(2,3))
row.names(UScereal)[c(1,2,3,31,32)] # Lage PC 1
row.names(UScereal)[c(47)] # Hoge PC 1
## All-bran en grape-nuts relatief zwaar/dens in vergelijking met andere ontbijtgranen, gepofte rijst aan andere uiterste: zeer licht bij eenzelfde volume
row.names(UScereal)[c(32,18,56)] # Lage PC 2
row.names(UScereal)[c(3,31,54,55)] # Hoge PC 2
## Smacks (en ook beide andere) zijn extra gezoete ontbijtgranen (honing, bruine suiker), de andere zijn niet extra gezoet
row.names(UScereal)[c(1,2,3)] # Lage PC 3
row.names(UScereal)[c(31,32)] # Hoge PC 3
## All-bran bevat inderdaad expliciet extra vezels terwijl noten eerder vetten bevatten

plot(Y[,1],Y[,2],col=shelf)
legend(-6,2, 1:3, col=1:3,pch=1)
plot(Y[,1],Y[,3],col=shelf)
legend(-6,2, 1:3, col=1:3,pch=1)
plot(Y[,2],Y[,3],col=shelf)
legend(0,3,ncol=3, 1:3, col=1:3,pch=1)
## PC1: De meest dense ontbijtgranen steeds op de bovenste plank (vaak noten = duurder), lichtere ontbijtgranen komen voor op alle shelves.
## PC2: Niet gezoete granen (Y2>0) doorgaans op eerste schap en gezoete op het tweede. Op het derde schap staan deze soorten door elkaar.
## PC3: Geen duidelijk verband met het schap
## Algemeen: Extreemste waarden (expliciete branding = duurder) doorgaans op eerste schap

detach(UScereal)

