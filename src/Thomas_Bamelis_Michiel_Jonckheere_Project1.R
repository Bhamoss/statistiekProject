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

# changing the column names to something more verbose
# getting rid of the .. first by chaning it to one dot.
colnames(deaths) = gsub("\\.\\.","\\.",names(deaths))

# changing the . in names to spaces
colnames(deaths) = gsub("\\."," ",names(deaths))

library(MASS)
library(ellipse)
library(car)

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

multiNormality <- function(data, dataName)
{
  # zet het al op 1 figuur
  
  # 32 colummen, dus 4 * 8 en dan kan de scatterplotmatrix 8*8
  
  #### Univariate marginale
  
  print(data)
  
  for (colIndex in 1:ncol(data) ) {
    column = data[,colIndex]
    
    
    # Shapiro, ook al zei stijn dat dat bucht is, kan het nog altijd eens handig zijn.
    shap = shapiro.test(column)
    
    # QQplot
    qqnorm(column, main = paste(names(data)[colIndex], " QQ plot", "\nShapiro p-value: ", toString(shap$p.value) , sep = ""))
    qqline(column)
  }
  
  ### Bivariate marginalen / Paarsgeweijze plots
  scatterplotMatrix(data, diagonal = "boxplot", main = "Pair-wise plots")
  
  ### Verdeling van de Mahalanobis afstanden
  MD = mahalanobis(data, colMeans(data), cov(data))
  qqplot(qchisq(ppoints(nrow(data)),df=ncol(data)), MD, main = "Mahalanobis chisq QQ plot")
  abline(0,1,col='blue')
  abline(h=qchisq(.975,ncol(data)),col='red')
}

verVars = c(africa, america, asia, europe, oceania, developed, developing, transition)
verVars.names = c("Africa", "America", "Asia", "Europe", "Oceania", "Developed", "Developing", "Transition")

multiNormality(africa, "africa")

for (i in 1:length(verVars)) {
  multiNormality(verVars[i], verVars.names[i])
}

###########################################################
#################   Classificatie   ########################
###########################################################



