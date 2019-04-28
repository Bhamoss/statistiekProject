###########################################################
#################   Project src   #########################
###########################################################






###########################################################
###############   Load and format data   ##################
###########################################################

# TODO: remove comments starting with #* to #* because they are for team communication and clarification
 
# set wd
wd = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)

# loading data, which is in csv2 format (I checked manually and the data is loaded correctly like this)
deaths = read.csv2(file = "deaths.csv",header = TRUE, row.names = "State")

# Rescale deaths to percentage of deaths of the country
#* TODO: chech to make sure we shouldnt use a scale method, the default scale() method doesnt work. I checked it.
c = deaths[,5:36]
c = prop.table(as.matrix(c),1)
deaths[,5:36] = c[,1:32]

# changing the column names to something more verbose
# getting rid of the .. first by chaning it to one dot.
colnames(deaths) = gsub("\\.\\.","\\.",names(deaths))

# changing the . in names to spaces
colnames(deaths) = gsub("\\."," ",names(deaths))

library(MASS)
library(ellipse)
library(car)
library(class)
library(klaR)

#*
# making sure the sum of the rows for the desease is 1
#isOne = TRUE
#for (i in 1:nrow(deaths)) {
#  print(sum(deaths[i,6:ncol(deaths)]))
#  if (abs( sum(deaths[i,6:ncol(deaths)]) - 1) > 0.0000001)
#  {
#    isOne = FALSE
#  }
#}
#isOne
#sum(deaths[,6:37])
#
# prop.table rescales the row to proportions, which is exactly as we want. I checked if it did what we want/need with the following code
#prop.table(as.matrix(c[1,]),1)
#c[1,] / sum(c[1,])
#prop.table(as.matrix(c[1,]),1) * sum(c[1,])
#sum(prop.table(as.matrix(c[1,]),1))
#prop.table(c(50,25,25))
#sum(c[1,])
#typeof(prop.table(as.matrix(c),1))
#*




###########################################################
#################   Clustering   ##########################
###########################################################

library(cluster)

#* pca before clustering?



# functions for analysis, to reduce code duplication with scaling and not scaling

clusteringAnalysis <- function(clustFeatures, name)
{

#*
#clustFeatures
#sum(clustFeatures[1,])
#*

# estimating how many clusters are needed

# vary the method parameters, we saw 3 different ones in the course book
# average, single and complete are the 3 found in the handbook
# agnes
cl.agnesAv = agnes(clustFeatures, method = "average")
cl.agnesSi = agnes(clustFeatures, method = "single")
cl.agnesCo = agnes(clustFeatures, method = "complete")



par(mfrow=c(1,2))
bannerplot(cl.agnesAv)
pltree(cl.agnesAv)
# diana
cl.diana = diana(clustFeatures)


# plotting dendogram and  and saving the plot
#* drawing very wide to be able to read the country codes on the figure
svg(filename=paste("hierachicalClustering", name ,".svg", sep = ""),
    width=30, 
    height=5*4, 
    pointsize=12)


# prepare to combine the plots
par(mfrow=c(4,2))

# plot with country codes as labels
bannerplot(cl.agnesAv, labels = deaths[,2])
pltree(cl.agnesAv, labels = deaths[,2])
bannerplot(cl.agnesSi, labels = deaths[,2])
pltree(cl.agnesSi, labels = deaths[,2])
bannerplot(cl.agnesCo, labels = deaths[,2])
pltree(cl.agnesCo, labels = deaths[,2])
bannerplot(cl.diana, labels = deaths[,2])
pltree(cl.diana, labels = deaths[,2])



dev.off()

par(mfrow=c(1,1))

#* Looking at the returned figure, 4 or 5 classes seem like a reasonable choice

# clustering algorithms
#kmeans
#pam
#fanny
cl.maxIters = 25
cl.nstarts = 50
k1 = 2
k2 = 4
k3 = 6

cl.k1 = kmeans(x = clustFeatures, centers = k1, iter.max = cl.maxIters, nstart = cl.nstarts)
cl.k2 = kmeans(x = clustFeatures, centers = k2, iter.max = cl.maxIters, nstart = cl.nstarts)
cl.k3 = kmeans(x = clustFeatures, centers = k3, iter.max = cl.maxIters, nstart = cl.nstarts)


cl.p1 = pam(x = clustFeatures, k = k1)
cl.p2 = pam(x = clustFeatures, k = k2)
cl.p3 = pam(x = clustFeatures, k = k3)

cl.f1 = fanny(x = clustFeatures, k1)
cl.f2 = fanny(x = clustFeatures, k2)
cl.f3 = fanny(x = clustFeatures, k3)

# see if clusters are clusterry enough

#plot(silhouette())
#clusplot()



# plotting silhouette and clusplots and  and saving the plot
#* drawing very wide to be able to read the country codes on the figure
svg(filename=paste("clusteringEvaluation", name ,".svg", sep = ""), 
    width=30, 
    height=5*9, 
    pointsize=12)


# prepare to combine the plots
par(mfrow=c(9,2))

#* apparently, you cannot make silhouette or clusplots from kmeans

plot(silhouette(cl.k1$cluster, daisy(clustFeatures)))
clusplot( clustFeatures , cl.k1$cluster , main = paste("Clusplot kmeans with k =",toString( length(unique(cl.k1$cluster))),sep = " "))
plot(silhouette(cl.k2$cluster, daisy(clustFeatures)))
clusplot( clustFeatures , cl.k2$cluster , main = paste("Clusplot kmeans with k =",toString( length(unique(cl.k2$cluster))),sep = " "))
plot(silhouette(cl.k3$cluster, daisy(clustFeatures)))
clusplot( clustFeatures , cl.k3$cluster , main = paste("Clusplot kmeans with k =",toString( length(unique(cl.k3$cluster))),sep = " "))

plot(silhouette(cl.p1))
clusplot(cl.p1)
plot(silhouette(cl.p2))
clusplot(cl.p2)
plot(silhouette(cl.p3))
clusplot(cl.p3)

plot(silhouette(cl.f1))
clusplot(cl.f1)
plot(silhouette(cl.f2))
clusplot(cl.f2)
plot(silhouette(cl.f3))
clusplot(cl.f3)

dev.off()
}


# not scaled

#clustFeatures = deaths[,6:ncol(deaths)]

clusteringAnalysis(deaths[,6:ncol(deaths)], "NoScaling")


# Scaled

#clustFeatures = scale(deaths[,6:ncol(deaths)])

clusteringAnalysis(scale(deaths[,6:ncol(deaths)]), "Scaled")


# Not scaled pam with 2 clusters is best, with an average cluster width of 0.5 (and 0.69 instead of 0.68 for best cluster against kmeans with k=2)
# this shows somewhat of a structere, but is getting borderline to low structure
# scaling is clearly worse

# best clustering
cl = pam(x = deaths[,6:ncol(deaths)], k = 2)

summary(cl)
# class 1 has 48 members, class 2 has 135
# The members of class 1 are closer together then those of class 2 (smaller distance and diameter)

svg(filename="difMediods.svg", 
    width=10, 
    height=10, 
    pointsize=12)

# overeenkosmsten met bestaande indelingen
# see how the mediods differ
plot(1:32, 1:32,main = "Difference in mediods", xlim = c(1,32), ylim = c(-0.25,0.25), col = "white", xlab = "Diseases", ylab = "Mediod class 1 - mediod class 2")
points(1:5, cl$medoids[1,1:5]-cl$medoids[2,1:5], col = 2)
points(6:20,cl$medoids[1,6:20]-cl$medoids[2,6:20], col=3)
points(21:32,cl$medoids[1,21:32]-cl$medoids[2,21:32], col=4)
abline(v=5.5)
abline(v=20.5)
#segments(20.5,0.16,20.5,0.26899, col = "white")
abline(h=0)
text(28,0.25, labels = c("Communicable, maternal,\n perinatal and nutritional conditions"), col = 2)
text(28,0.21, labels = c("Noncommunicable diseases"), col = 3)
text(28,0.18, labels = c("Injuries"), col = 4)

#* swerelds lelijkste figuur

dev.off()
# this figure shows class 1 has higher Communicable, maternal, perinatal and nutritional conditions then class 2
# it also overall has lower Noncommunicable diseases
# espicially Infectious and parasitic diseases are much higher and Respiratory diseases are much lower
table(cl$clustering, deaths[,4])
# printing it to latex format
library(xtable)
print(xtable(table(cl$clustering, deaths[,4]), type = "latex"), file = "clusteringToDevelopment.tex")
table(cl$clustering, deaths[,3])
print(xtable(table(cl$clustering, deaths[,3]), type = "latex"), file = "clusteringToContinent.tex")
# looking at this table, almost all countries of class 1 are developing african countries
# which makes a lot of sense considering the data and diseases
# probably less respiratory deaths because of cleaner air, and less malignent neoplasm (= cancers and tumors) because of less chemicals

# about 1/4 of the countries have significantly higher com... and lower noncom... then the rest and are tightly packed compared to the rest





###########################################################
######################   PCA   ############################
###########################################################


diseases = deaths[,5:36]
diseases.pca = prcomp(diseases, scale=FALSE) #geen scaling
plot(diseases.pca)
summary(diseases.pca)
attributes(diseases.pca)
P = diseases.pca$rotation; P[,1:4] # enkel PC1-PC4 hier bekijken, want die verklaren 90% van de variantie van de data
Y = predict(diseases.pca); Y[,1:4] # idem als hierboven
colSums(P^2)


#analyse op de PC1 - 4
P[P[,1]>0.1 | P[,1] < -0.2,1:4] # Malignant neoplasms en cardiovascular diseases zijn grootste positieve PC1, resp 0.33, 0.66
P[P[,1] < -0.1,1:4] # Infectious and parasitic diseases grootste negatieve met -0.62

P[P[,2]>0.1 | P[,2] < -0.1,2:4] # Cardiovascular diseases grootste positieve voor PC2, 0.67 (Infectious and parasitic diseases heeft 0.31)
P[P[,2] < -0.1,1:4] # Malignant neoplasms grootste negatieve met -0.58

P[P[,3]>0.1 | P[,3] < -0.1,1:4] # Diabetes mellitus grootste positieve voor PC3, 0.53 (Collective violence and legal intervention 0.28)
P[P[,3] < -0.1,1:4] # Malignant neoplasms en Infectious and parasitic diseases grootste negatieve met -0.47 en -0.51

P[P[,4]>0.1 | P[,4] < -0.1,1:4] # Diabetes mellitus grootste positieve voor PC4, 0.76P[P[,4] < -0.1,1:4] # Collective violence and legal intervention en Neurological conditions grootste negatieve met -0.34 en -0.23

PCHigh = diseases.pca$x > 0 # PC's zijn groot
PCLow = diseases.pca$x < 0 # PC's zijn klein, PCLow[,1] = alle lage PC1
table(deaths$Region[PCHigh[,1]])
table(deaths$Region[PCLow[,1]])

table(deaths$Region[PCHigh[,2]])
table(deaths$Region[PCLow[,2]])

table(deaths$Region[PCHigh[,3]])
table(deaths$Region[PCLow[,3]])

table(deaths$Region[PCHigh[,4]])
table(deaths$Region[PCLow[,4]])

table(deaths$Developement[PCHigh[,1]], deaths$Region[PCHigh[,1]])
table(deaths$Developement[PCLow[,1]], deaths$Region[PCLow[,1]])

table(deaths$Developement[PCHigh[,2]], deaths$Region[PCHigh[,2]])
table(deaths$Developement[PCLow[,2]], deaths$Region[PCLow[,2]])

table(deaths$Developement[PCHigh[,3]], deaths$Region[PCHigh[,3]])
table(deaths$Developement[PCLow[,3]], deaths$Region[PCLow[,3]])

table(deaths$Developement[PCHigh[,4]])
table(deaths$Developement[PCLow[,4]])

PCExtrHigh = diseases.pca$x > 0.09 # PC's zijn groot
PCExtrLow = diseases.pca$x < -0.05 # PC's zijn klein, PCLow[,1] = alle lage PC1
row.names.data.frame(deaths)[PCExtrLow[,3]] # return de landen
table(deaths$Region[PCExtrLow[,3]])
row.names.data.frame(deaths)[PCExtrHigh[,4]] # return de landen
table(deaths$Region[PCExtrHigh[,4]])
table(deaths$Developement[PCExtrLow[,3]],deaths$Region[PCExtrLow[,3]])
table(deaths$Developement[PCExtrHigh[,3]],deaths$Region[PCExtrHigh[,3]])

tapply(Y[,1],deaths$Region,mean)
tapply(Y[,2],deaths$Region,mean)
tapply(Y[,3],deaths$Region,mean)
tapply(Y[,4],deaths$Region,mean)


tapply(Y[,1],deaths$Developement,mean)
tapply(Y[,2],deaths$Developement,mean)
tapply(Y[,3],deaths$Developement,mean)
tapply(Y[,4],deaths$Developement,mean)


par(mfrow=c(1,1))
REGION = as.factor(levels(deaths$Region)); REGION
DEV =  as.factor(levels(deaths$Developement))
plot(Y[,1],Y[,2],col=as.numeric(deaths$Region),main="PC1 t.o.v. PC2 ",xlab = "PC1",ylab="PC2",pch=as.numeric(deaths$Developement))
plot(Y[,1],Y[,3],col=as.numeric(deaths$Region),main="PC1 t.o.v. PC3 ",xlab = "PC1",ylab="PC3",pch=as.numeric(deaths$Developement))
plot(Y[,1],Y[,4],col=as.numeric(deaths$Region),main="PC1 t.o.v. PC4 ",xlab = "PC1",ylab="PC4",pch=as.numeric(deaths$Developement))

legend("topleft", legend=c(levels(deaths$Region)), col=as.numeric(REGION), pch=1)
legend("bottomleft", legend=c(levels(deaths$Developement)), pch=as.numeric(DEV), col=1)

plot(Y[,4],Y[,2],col=as.numeric(deaths$Region),pch=as.numeric(deaths$Developement))
legend("topleft", legend=c(levels(deaths$Region)), col=as.numeric(REGION), pch=1)
legend("bottomleft", legend=c(levels(deaths$Developement)), pch=as.numeric(DEV), col=1)

pairs(cbind(X),col=as.numeric(deaths$Region))



diseases = deaths[,6:37]
pca.pc = prcomp(scale(deaths[,6:37]))
pca.su = summary(pca.pc)
pca.var = pca.pc$sdev^2/sum(pca.pc$sdev^2)
pca.cum = 1:32
for (i in 1:32) {
  pca.cum[i] = sum(pca.var[1:i])
}
pca.slope = pca.var[1:31] - pca.var[2:32]
pca.pc
pca.su
pca.var
pca.cum
pca.slope


svg(filename="pcaVar.svg", 
    width=14, 
    height=14, 
    pointsize=12)

par(mfrow=c(2,2))

plot(1:32, pca.var, type = "l", main = "Variance proportions", ylab = "Var. proportion", xlab = "Principal components")
points(1:32, pca.var)

plot(1:32, pca.cum, type = "l", main = "Cumulative variance proportions", ylab = "Cum. var. proportion", xlab = "Principal components")
points(1:32, pca.cum)

plot(1:31, pca.slope, main = "Variance proportion descent/slope", ylab = "Variance proportion descent/slope", xlab = "Principal components")

dev.off()

par(mfrow=c(1,1))

plot(1:31, pca.slope)
pca.var[1:5]
pca.cum[2]
pca.cum[4]
# The most important pc's are pc 1 & 2, and after that, 3 & 4 are a bit more important then the rest (zie de knik in de grafiek)

svg(filename="pcaTop4.svg", 
    width=14, 
    height=14, 
    pointsize=12)

par(mfrow=c(4,2))

i = sort(abs(pca.pc$rotation[1,]), index.return=TRUE, decreasing = TRUE)$ix[1:10]
print(names(deaths[,6:37])[i])
barplot(pca.pc$rotation[1,], main = "Weights of PC1", xlab = "Diseases", ylab = "Coefficients")
text(i[1:5]*1.15, pca.pc$rotation[1,i[1:5]]*0.95, labels = substr( names(deaths[,6:37])[i[1:5]],1,3), col = "red")
text(i[6:10]*1.15, pca.pc$rotation[1,i[6:10]]*0.95, labels = substr( names(deaths[,6:37])[i[6:10]],1,3), col = "blue")
barplot( abs(pca.pc$rotation[1,]),  main = "Absolut weights of PC1", xlab = "Diseases", ylab = "Absolute value of coefficients")
text(i[1:5]*1.15, abs(pca.pc$rotation[1,i[1:5]])*0.95, labels = substr( names(deaths[,6:37])[i[1:5]],1,3), col = "red")
text(i[6:10]*1.15, abs(pca.pc$rotation[1,i[6:10]])*0.95, labels = substr( names(deaths[,6:37])[i[6:10]],1,3), col = "blue")

i = sort(abs(pca.pc$rotation[2,]), index.return=TRUE, decreasing = TRUE)$ix[1:10]
print(names(deaths[,6:37])[i])
barplot(pca.pc$rotation[2,], main = "Weights of PC2", xlab = "Diseases", ylab = "Coefficients")
text(i[1:5]*1.15, pca.pc$rotation[2,i[1:5]]*0.95, labels = substr( names(deaths[,6:37])[i[1:5]],1,3), col = "red")
text(i[6:10]*1.15, pca.pc$rotation[2,i[6:10]]*0.95, labels = substr( names(deaths[,6:37])[i[6:10]],1,3), col = "blue")
barplot(abs(pca.pc$rotation[2,]),  main = "Absolut weights of PC2", xlab = "Diseases", ylab = "Absolute value of coefficients")
text(i[1:5]*1.15, abs(pca.pc$rotation[2,i[1:5]])*0.95, labels = substr( names(deaths[,6:37])[i[1:5]],1,3), col = "red")
text(i[6:10]*1.15, abs(pca.pc$rotation[2,i[6:10]])*0.95, labels = substr( names(deaths[,6:37])[i[6:10]],1,3), col = "blue")

i = sort(abs(pca.pc$rotation[3,]), index.return=TRUE, decreasing = TRUE)$ix[1:10]
print(names(deaths[,6:37])[i])
barplot(pca.pc$rotation[3,], main = "Weights of PC3", xlab = "Diseases", ylab = "Coefficients")
text(i[1:5]*1.15, pca.pc$rotation[3,i[1:5]]*0.95, labels = substr( names(deaths[,6:37])[i[1:5]],1,3), col = "red")
text(i[6:10]*1.15, pca.pc$rotation[3,i[6:10]]*0.95, labels = substr( names(deaths[,6:37])[i[6:10]],1,3), col = "blue")
barplot(abs(pca.pc$rotation[3,]),  main = "Absolut weights of PC3", xlab = "Diseases", ylab = "Absolute value of coefficients")
text(i[1:5]*1.15, abs(pca.pc$rotation[3,i[1:5]])*0.95, labels = substr( names(deaths[,6:37])[i[1:5]],1,3), col = "red")
text(i[6:10]*1.15, abs(pca.pc$rotation[3,i[6:10]])*0.95, labels = substr( names(deaths[,6:37])[i[6:10]],1,3), col = "blue")

i = sort(abs(pca.pc$rotation[4,]), index.return=TRUE, decreasing = TRUE)$ix[1:10]
print(names(deaths[,6:37])[i])
barplot(pca.pc$rotation[4,], main = "Weights of PC4", xlab = "Diseases", ylab = "Coefficients")
text(i[1:5]*1.15, pca.pc$rotation[4,i[1:5]]*0.95, labels = substr( names(deaths[,6:37])[i[1:5]],1,3), col = "red")
text(i[6:10]*1.15, pca.pc$rotation[4,i[6:10]]*0.95, labels = substr( names(deaths[,6:37])[i[6:10]],1,3), col = "blue")
barplot(abs(pca.pc$rotation[4,]),  main = "Absolut weights of PC4", xlab = "Diseases", ylab = "Absolute value of coefficients")
text(i[1:5]*1.15, abs(pca.pc$rotation[4,i[1:5]])*0.95, labels = substr( names(deaths[,6:37])[i[1:5]],1,3), col = "red")
text(i[6:10]*1.15, abs(pca.pc$rotation[4,i[6:10]])*0.95, labels = substr( names(deaths[,6:37])[i[6:10]],1,3), col = "blue")

dev.off()

#TODO: next step, try to interpret the pca in words with the help of the picture

# PCX is large (negatively/positively) when
# PC1: very negative if
# 
# PC2

# analyse what the country does to it

par(mfrow=c(1,1))

# PC values for the countries
pca.pr = predict(pca.pc)[,1:4]
barplot(pca.pr[,1])
barplot(pca.pr[,2])
barplot(pca.pr[,3])
barplot(pca.pr[,4])
pca.pr1 = pca.pr[,1]
pca.pr2 = pca.pr[,2]
pca.pr3 = pca.pr[,3]
pca.pr4 = pca.pr[,4]
pca.pr
pca.pr1

# constructing info for

pca.pr1p = c(mean(pca.pr1[which(pca.pr1 >= 0)]), sd(pca.pr1[which(pca.pr1 >= 0)]))
pca.pr1n = c(mean(pca.pr1[which(pca.pr1 < 0)]), sd(pca.pr1[which(pca.pr1 < 0)]))
pca.pr2p = c(mean(pca.pr2[which(pca.pr2 >= 0)]), sd(pca.pr2[which(pca.pr2 >= 0)]))
pca.pr2n = c(mean(pca.pr2[which(pca.pr2 < 0)]), sd(pca.pr2[which(pca.pr2 < 0)]))
pca.pr3p = c(mean(pca.pr3[which(pca.pr3 >= 0)]), sd(pca.pr3[which(pca.pr3 >= 0)]))
pca.pr3n = c(mean(pca.pr3[which(pca.pr3 < 0)]), sd(pca.pr3[which(pca.pr3 < 0)]))
pca.pr4p = c(mean(pca.pr4[which(pca.pr4 >= 0)]), sd(pca.pr4[which(pca.pr4 >= 0)]))
pca.pr4n = c(mean(pca.pr4[which(pca.pr4 < 0)]), sd(pca.pr4[which(pca.pr4 < 0)]))



pca.regionColors = c()   
u = unique(deaths$Region)
for (i in 1:length(deaths$Region)) {
  if (deaths$Region[i] == "Asia")
  {
    pca.regionColors = c(pca.regionColors, "yellow")
  }
  else if (deaths$Region[i] == "Europe")
  {
    pca.regionColors = c(pca.regionColors, "blue")
  }
  else if (deaths$Region[i] == "Africa")
  {
    pca.regionColors = c(pca.regionColors, "black")
  }
  else if (deaths$Region[i] == "America")
  {
    pca.regionColors = c(pca.regionColors, "red")
  }
  else if (deaths$Region[i] == "Oceania")
  {
    pca.regionColors = c(pca.regionColors, "green")
  }
}


pca.devColors = c()   
u = unique(deaths$Developement)
for (i in 1:length(deaths$Developement)) {
  if (deaths$Developement[i] == "Developing")
  {
    pca.devColors = c(pca.devColors, "red")
  }
  else if (deaths$Developement[i] == "Transition")
  {
    pca.devColors = c(pca.devColors, "yellow")
  }
  else if (deaths$Developement[i] == "Developed")
  {
    pca.devColors = c(pca.devColors, "green")
  }
  else if (deaths$Developement[i] == "#N/B")
  {
    pca.devColors = c(pca.devColors, "black")
  }
}

#colfunc <- colorRampPalette(c("green", "red"))
#u = order(deaths$Population)
#pca.popColors = colfunc(length(u))[u]

q = quantile(deaths$Population)
pca.popColors = c()
for (i in 1:length(deaths$Population)) {
  if (deaths$Population[i] >= q[4])
  {
    pca.popColors = c(pca.popColors, "red")
  }
  else if (deaths$Population[i] >= q[3])
  {
    pca.popColors = c(pca.popColors, "yellow")
  }
  else if (deaths$Population[i] >= q[2])
  {
    pca.popColors = c(pca.popColors, "green")
  }
  else if (deaths$Population[i] >= q[1])
  {
    pca.popColors = c(pca.popColors, "blue")
  }
}

pcs = cbind(pca.pr1, pca.pr2, pca.pr3, pca.pr4)
cols = cbind(pca.regionColors, pca.devColors, pca.popColors)
pcsp = cbind(pca.pr1p, pca.pr2p, pca.pr3p, pca.pr4p)
pcsn = cbind(pca.pr1n, pca.pr2n, pca.pr3n, pca.pr4n)
ma = c("Region","Development","Population")

svg(filename="pcaCountryAnalysis.svg", 
    width=5*ncol(cols), 
    height=5*ncol(pcs), 
    pointsize=12)

par(mfrow=c(ncol(pcs), ncol(cols)))
for (i in 1:ncol(pcs)) {
  for (j in 1:ncol(cols)) {
    
    
    barplot(pcs[,i], col = cols[,j], border = cols[,j], xlab = "Countries", ylab = paste("PC",i ," value of country" , sep = ""))
    
    if (j == 1) {
      mtext(text = paste("PC", i ,": ",ma[j] ,sep = ""), side = 3, line = 3, col = "black")
      mtext(text = "Asia", side = 3, line = 2, col = "yellow")
      mtext(text = "Europe", side = 3, line = 1, col = "blue")
      mtext(text = "Africa", side = 1, line = -1, col = "black")
      mtext(text = "America", side = 1, line = 0, col = "red")
      mtext(text = "Oceania", side = 1, line = 1, col = "green")
    } 
    else if(j ==2)
    {
      mtext(text = paste("PC", i ,": ",ma[j] ,sep = ""), side = 3, line = 3, col = "black")
      mtext(text = "Developing", side = 3, line = 2, col = "red")
      mtext(text = "Transition", side = 3, line = 1, col = "yellow")
      mtext(text = "Developed", side = 1, line = 0, col = "green")
      mtext(text = "#N/B", side = 1, line = 1, col = "black")
    }
    else if(j ==3)
    {
      mtext(text = paste("PC", i ,": ",ma[j] ,sep = ""), side = 3, line = 3, col = "black")
      mtext(text = paste(">= ", q[4]), side = 3, line = 2, col = "red")
      mtext(text = paste(">= ", q[3]), side = 3, line = 1, col = "yellow")
      mtext(text = paste(">= ", q[2]), side = 1, line = 0, col = "green")
      mtext(text = paste(">= ", q[1]), side = 1, line = 1, col = "blue")
    }
    
    #legend(20,0,legend = l)
    # drawing mean and sd for positive values
    abline(h = pcsp[1,i], col = "red")
    abline(h = (pcsp[1,i] + pcsp[2,i]), col = "blue")
    abline(h = (pcsp[1,i] - pcsp[2,i]), col = "blue")
    # drawing mean and sd for negative values
    abline(h = pcsn[1,i], col = "red")
    abline(h = (pcsn[1,i] + pcsn[2,i]), col = "blue")
    abline(h = (pcsn[1,i] - pcsn[2,i]), col = "blue")
  }
}

dev.off()

par(mfrow=c(1, 1))
barplot(pcs[,1], col = cols[,1], border = cols[,1])
legend(120 ,123 , legend = legr)
barplot(1:123)
legend(180,4.5, legend = c("lol", "haha"))
mtext("test", side = 4, line = 1)


nb = 10
sink(paste("pca",nb,"HighestLowestPCs.txt",sep = ""))
for (i in 1:ncol(pcs)) {
  cat("\n\n")
  cat(paste("Top", nb," greatest positive values for pc",i))
  cat("\n\n")
  u = order(pcs[,i], decreasing = TRUE)[1:nb]
  k = data.frame(deaths$State[u], deaths$Region[u], deaths$Developement[u],deaths$Population[u] , pcs[,i][u] )
  colnames(k) = c("Country", "Region", "Development", "Population", "PCvalue")
  cat(paste(capture.output(k), "\n", sep=""))
  cat("\n\n\n")
  cat(paste("Top", nb," greatest negative values for pc",i))
  cat("\n\n")
  u = order(pcs[,i])[1:nb]
  k = data.frame(deaths$State[u], deaths$Region[u], deaths$Developement[u],deaths$Population[u] , pcs[,i][u])
  colnames(k) = c("Country", "Region", "Development", "Population", "PCvalue")
  cat(paste(capture.output(k), "\n", sep=""))
  cat("\n")
}
sink()


###########################################################
###########   Multivariate normaliteit   ##################
###########################################################

# Verderop wordt de classificatie van landen op basis van doodsoorzaken bestudeerd, waarvoor het nodig
# is om na te gaan of de verdeling van de verklarende variabelen multivariaat normaal is voor elke groep.
# Onderzoek de hypothese van multivariaat normale verdeling van de doodsoorzaken voor de afzonderlijke
# groepen. Hou rekening met de conclusies in het vervolg van het onderzoek.


#TODO: this is probably wrong, look at this again.


names(deaths)
# So I would think the verklarende variables are the ones who have nothing to do with the deaths
# So Region and Development I think.
# Everything after population is not verklaredn but an effect of the verklarende.
# Population is just here so we could calculate the proportions.
# Population and State = Code is not usefull because they function as individual labels for 1 sample.
# So we have to verify normality for each region and each degree of development.

# verklarende variabelen
table(deaths$Region)
table(deaths$Developement)
X = deaths[,5:36]
n = dim(X)[1]
p= dim(X)[2]
for (i in 1:p) {
  #if (shapiro.test(X[,i])$p.value > 0.05) {
    qqnorm(X[,i], main = (shapiro.test(X[,i])$p.value > 0.05)); qqline(X[,i])
  #}
}

for (i in 1:p) {
  #if (shapiro.test(logit(X[,i]))$p.value > 0.05) {
    qqnorm(sqrt(X[,i]), main = (shapiro.test(sqrt(X[,i]))$p.value > 0.05)); qqline(sqrt(X[,i]))
  #}
}





africa =  deaths[deaths$Region == "Africa", 6:37]
america =  deaths[deaths$Region == "America", 6:37]
asia =  deaths[deaths$Region == "Asia", 6:37]
europe =  deaths[deaths$Region == "Europe", 6:37]
oceania =  deaths[deaths$Region == "Oceania", 6:37]

# #N/B not usefull to analyse, because only one sample
NB =  deaths[deaths$Developement == "#N/B", 6:37]
developed =  deaths[deaths$Developement == "Developed", 6:37]
developing =  deaths[deaths$Developement == "Developing", 6:37]
transition =  deaths[deaths$Developement == "Transition", 6:37]

multiNormality <- function(dat, dataName)
{
  # create a directory if it does not exist already.
  datDir = paste(dataName, "MultiNorm", sep = "")
  uniDir = "uniMarg"
  biDir = "biMarg"
  dir.create(file.path(wd, datDir), showWarnings = FALSE)
  dir.create(file.path(wd, datDir, uniDir), showWarnings = FALSE)
  dir.create(file.path(wd, datDir, biDir), showWarnings = FALSE)
  
  # 32 colummen, dus 4 * 8 en dan kan de scatterplotmatrix 8*8
  
  w = 5
  h = 5
  
  
  par(mfrow=c(1,1))
  
  #### Univariate marginale
  
  allTheSame = c()
  
  for (colIndex in 1:ncol(dat) ) {
    column = dat[,colIndex]
    
    svg(filename=file.path(datDir, uniDir , paste("UniMarg", gsub(" ","_",names(dat)[colIndex]), ".svg",sep = "")), 
        width=w, 
        height=h, 
        pointsize=12)
    
    # some commands do not work when a column has all the same values, decide if this automaticly refutes normality 
    if (length(unique(column)) == 1)
    {
      scatterplot(c(0,1), c(0,1), main = paste(names(dat)[colIndex], "\n!!!ALL VALUES EQUAL!!!!" , sep = ""))
      abline(0,1,col='red', lwd = 20)
      abline(1,-1,col='red', lwd = 20)
      allTheSame = c(colIndex, allTheSame)
    }
    else
    {
      # Shapiro, ook al zei stijn dat dat bucht is, kan het nog altijd eens handig zijn.
      shap = shapiro.test(column)
      
      # QQplot
      qqnorm(column, main = paste(names(dat)[colIndex], " QQ plot", "\nShapiro p-value: ", toString(shap$p.value) , sep = ""))
      qqline(column)
    }
    
    dev.off()
  }
  
  # deleting the columns with all the same values
  for (i in allTheSame) {
    dat[i] = NULL
  }
  
  # 
  # ### Bivariate marginalen / Paarsgeweijze plots
  # 
  # for (i in 1:(ncol(dat) - 1)) {
  #   for (j in (i+1):ncol(dat)) {
  # 
  #     svg(filename=file.path(datDir, biDir , paste("BiMarg_", gsub(" ","_",names(dat)[i]),"_X_", gsub(" ","_",names(dat)[j]), ".svg",sep = "")), 
  #         width=5, 
  #         height=5, 
  #         pointsize=12)
  #     
  #     plot(dat[,i],dat[,j], xlab = names(deaths)[i], ylab = names(deaths)[j], main = paste(names(deaths)[i], " versus ",names(deaths)[j] , sep = ""))
  #     lines(ellipse::ellipse(cov(dat[,i],dat[,j]), centre=c(mean(dat[,i]),mean(dat[,j])), level=0.95),col='red')
  #     
  #     dev.off()
  #   }
  # }
  # 
  # 
  # 
  # #scatterplotMatrix(data, diagonal = "boxplot", main = "Pair-wise plots", id.n =1, id.col=4, smoother = F, regLine = F, pch = 19, cex = 1.25)
  # 
  # ### Verdeling van de Mahalanobis afstanden
  # 
  # svg(filename=file.path(datDir , paste("Mahalanobis", dataName,".svg",sep = "")), 
  #     width=8, 
  #     height=8, 
  #     pointsize=12)
  # 
  # tryCatch(
  #   {
  #     MD = mahalanobis(dat, colMeans(dat), cov(dat))
  # 
  #     qqplot(qchisq(ppoints(nrow(dat)),df=ncol(dat)), MD, main = "Mahalanobis chisq QQ plot")
  #     abline(0,1,col='blue')
  #     abline(h=qchisq(.975,ncol(dat)),col='red')
  #     
  #     # first one of last row
  #   }, error=function(e){
  #     scatterplot(c(0,1), c(0,1), main = "Mahalanobis got error")
  #     abline(0,1,col='red', lwd = 20)
  #     abline(1,-1,col='red', lwd = 20)
  #   }
  # )
  dev.off()
}

verVars = c(africa, america, asia, europe, oceania, developed, developing, transition)
verVars.names = c("Africa", "America", "Asia", "Europe", "Oceania", "Developed", "Developing", "Transition")

multiNormality(africa, "Africa")
multiNormality(america, "America")
multiNormality(asia, "Asia")
multiNormality(europe, "Europe")
multiNormality(oceania, "Oceania")
multiNormality(developed, "Developed")
multiNormality(developing, "Developing")
multiNormality(transition, "Transition")


###########################################################
#################   Classificatie   ########################
###########################################################

# Ga na in hoeverre het mogelijk is om de regio van een land te identificeren aan de hand van de doods-
# oorzaken. Doe hetzelfde voor de globale ontwikkeling. Welke methode is het meest geschikt? Beschrijf
# de werking van het model. Welke landen worden niet correct ingedeeld en waarom?

# niet multivariaat normaal, dus niet naar de aposteriori kans kijken

#LDA
region = deaths$Region; region
i = which(is.na(region)); i # geen NA waarden
X = cbind(deaths[,6:37])[,-(c(12,20))] # constanten eruit smijten, 12 en 20
y = region; y
table(y)
orig.lda.CV = lda(X,y,CV=TRUE)$class; orig.lda.CV
orig.lda.AER = table(orig.lda.CV,y); orig.lda.AER
sum(orig.lda.AER-diag(diag(orig.lda.AER)))/sum(orig.lda.AER) 

#QDA kan niet worden uitgevoerd, want meer kolommen dan de smallest van de klassen
#   kan opgelost worden door kolommen te verwijderen 

#idee: ipv alle variablen te bekijken, hier enkel de groepen (communicable, non-comm en injuries)
#   dus alles optellen 
# werkt ook niet 
comm = rowSums(deaths[, 6:10]); comm
noncomm = rowSums(deaths[, 11:26]); noncomm
inj = rowSums(deaths[, 27:37]); inj
rowSums(cbind(comm, noncomm,inj)) # moeten allemaal 1 zijn
deathsTogether = cbind(deaths[,1:5], comm, noncomm, inj)
X = cbind(deathsTogether[,6:8]); X
y = deathsTogether$Region; y
orig.qda = predict(qda(X,y))$posterior
# weer geen qda, omwille van te kleine groepen voor qda
library(class)
orig.knn5.cv = knn.cv(train=X,cl=y,k=5); orig.knn5.cv
orig.knn5.AER = table(orig.knn5.cv,y); orig.knn5.AER
sum(orig.knn5.AER-diag(diag(orig.knn5.AER)))/sum(orig.knn5.AER)

orig.knn4.cv = knn.cv(train=X,cl=y,k=4); orig.knn4.cv
orig.knn4.AER = table(orig.knn4.cv,y); orig.knn4.AER
sum(orig.knn4.AER-diag(diag(orig.knn4.AER)))/sum(orig.knn4.AER)

orig.knn3.cv = knn.cv(train=X,cl=y,k=3); orig.knn3.cv
orig.knn3.AER = table(orig.knn3.cv,y); orig.knn3.AER
sum(orig.knn3.AER-diag(diag(orig.knn3.AER)))/sum(orig.knn3.AER)

orig.knn2.cv = knn.cv(train=X,cl=y,k=2); orig.knn2.cv
orig.knn2.AER = table(orig.knn2.cv,y); orig.knn2.AER
sum(orig.knn2.AER-diag(diag(orig.knn2.AER)))/sum(orig.knn2.AER)

orig.knn1.cv = knn.cv(train=X,cl=y,k=1); orig.knn1.cv
orig.knn1.AER = table(orig.knn1.cv,y); orig.knn1.AER
sum(orig.knn1.AER-diag(diag(orig.knn1.AER)))/sum(orig.knn1.AER)


#LDA
dev = deaths$Developement; dev
# i = which(dev == "#N/B"); i # de #N/B eruit halen
X = cbind(deaths[,6:37])[,-(c(12,20))] # constanten eruit smijten, 12 en 20
y = dev; y
table(y)
orig.lda.CV = lda(X,y,CV=TRUE)$class; #orig.lda.CV
orig.lda.AER = table(orig.lda.CV,y); orig.lda.AER
sum(orig.lda.AER-diag(diag(orig.lda.AER)))/sum(orig.lda.AER) 

library(class)
orig.knn5.cv = knn.cv(train=X,cl=y,k=5); #orig.knn5.cv
orig.knn5.AER = table(orig.knn5.cv,y); orig.knn5.AER
sum(orig.knn5.AER-diag(diag(orig.knn5.AER)))/sum(orig.knn5.AER)

orig.knn4.cv = knn.cv(train=X,cl=y,k=4); orig.knn4.cv
orig.knn4.AER = table(orig.knn4.cv,y); orig.knn4.AER
sum(orig.knn4.AER-diag(diag(orig.knn4.AER)))/sum(orig.knn4.AER)

orig.knn3.cv = knn.cv(train=X,cl=y,k=3); #orig.knn3.cv
orig.knn3.AER = table(orig.knn3.cv,y); orig.knn3.AER
sum(orig.knn3.AER-diag(diag(orig.knn3.AER)))/sum(orig.knn3.AER)

orig.knn2.cv = knn.cv(train=X,cl=y,k=2); #orig.knn2.cv
orig.knn2.AER = table(orig.knn2.cv,y); orig.knn2.AER
sum(orig.knn2.AER-diag(diag(orig.knn2.AER)))/sum(orig.knn2.AER)



cla.dat = deaths[,6:37]
cla.region = deaths[,3]
cla.development = deaths[,4]
cla.scaledDat = scale(cla.dat)

# CV= TRUE?
# doen met principaalcomponenten?
#TODO: lda problem with 12 and 20 constant per class
cla.region.lda = lda(cla.dat, cla.region)
cla.region.scaledLda = lda(cla.scaledDat, cla.region)
#TODO: problem some classes do not have enough samples
cla.region.qda = qda(cla.dat, cla.region)
cla.region.scaledQda = qda(cla.scaledDat, cla.region)
cla.region.knn5 = knn.cv(train=cla.dat ,cl=cla.region ,k=5)
cla.region.scaledKnn5 = knn.cv(train=cla.scaledDat ,cl=cla.region ,k=5)
cla.region.knn3 = knn.cv(train=cla.dat ,cl=cla.region ,k=3)
cla.region.scaledKnn3 = knn.cv(train=cla.scaledDat ,cl=cla.region ,k=3)
cla.region.knn1 = knn.cv(train=cla.dat ,cl=cla.region ,k=1)
cla.region.scaledKnn1 = knn.cv(train=cla.scaledDat ,cl=cla.region ,k=1)

cla.development.lda = lda(cla.dat, cla.development)
cla.development.scaledLda = lda(cla.scaledDat, cla.development)
cla.development.qda = qda(cla.dat, cla.development)
cla.development.scaledQda = qda(cla.scaledDat, cla.development)
cla.development.knn5 = knn.cv(train=cla.dat ,cl=cla.development ,k=5)
cla.development.scaledKnn5 = knn.cv(train=cla.scaledDat ,cl=cla.development ,k=5)
cla.development.knn3 = knn.cv(train=cla.dat ,cl=cla.development ,k=3)
cla.development.scaledKnn3 = knn.cv(train=cla.scaledDat ,cl=cla.development ,k=3)
cla.development.knn1 = knn.cv(train=cla.dat ,cl=cla.development ,k=1)
cla.development.scaledKnn1 = knn.cv(train=cla.scaledDat ,cl=cla.development ,k=1)

partimat(cla.dat, cla.region, method='lda', imageplot=FALSE)
partimat(cla.dat, cla.region, method='qda', imageplot=FALSE)
# sknn is for knn
partimat(cla.dat, cla.region, method='sknn', imageplot=FALSE)

partimat(cla.scaledDat, cla.region, method='lda', imageplot=FALSE)
partimat(cla.scaledDat, cla.region, method='qda', imageplot=FALSE)
partimat(cla.scaledDat, cla.region, method='sknn', imageplot=FALSE)

partimat(cla.dat, cla.development, method='lda', imageplot=FALSE)
partimat(cla.dat, cla.development, method='qda', imageplot=FALSE)
partimat(cla.dat, cla.development, method='sknn', imageplot=FALSE)

partimat(cla.scaledDat, cla.development, method='lda', imageplot=FALSE)
partimat(cla.scaledDat, cla.development, method='qda', imageplot=FALSE)
partimat(cla.scaledDat, cla.development, method='sknn', imageplot=FALSE)

errorRate <- function(actual, predictions)
{
  tb = table(actual, predictions)
  sum(tb - diag(diag(tb)))/sum(tb)
}

# normaliteit en classificatie lijken totaal niet te werken 
# er zijn veel te veel kolommen om iets deftigs te kunnen zeggen
# Dus misschien is het best om dat opnieuw te doen maar met enkel de 3
# hoofdcategorieen. Zie de pdf.

communible = rowSums(deaths[,6:10])
noncommunible = rowSums(deaths[,11:26])
injuries = rowSums(deaths[,27:37])

relevantData = data.frame(deaths[,3],deaths[,4],communible,noncommunible,injuries, row.names = deaths[,1])
colnames(relevantData) = c("Region","Development","Communible Diseases", "Non-communible Diseases", "Injuries")
# remove congo with its #N/B
# maybe wait to remove until development
relevantData[-38,]
cla.dat = relevantData[3:5]
# maybe enkel van injuries opsplitsen in de 3 subcategorien
