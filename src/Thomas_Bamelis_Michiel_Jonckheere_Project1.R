###########################################################
#################   Project src   #########################
###########################################################






###########################################################
###############   Load and format data   ##################
###########################################################

# TODO: remove comments starting with #* to #* because they are for team communication and clarification
 
# set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# loading data, which is in csv2 format (I checked manually and the data is loaded correctly like this)
deaths = read.csv2(file = "deaths.csv",header = TRUE)

# Rescale deaths to percentage of deaths of the country
#* TODO: chech to make sure we shouldnt use a scale method, the default scale() method doesnt work. I checked it.
c = deaths[,6:37]
c = prop.table(as.matrix(c),1)
deaths[,6:37] = c[,1:32]

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
#TODO: do this for the continents/development/...

###########################################################
###########   Multivariate normaliteit   ##################
###########################################################




###########################################################
#################   Classificatie   ########################
###########################################################



