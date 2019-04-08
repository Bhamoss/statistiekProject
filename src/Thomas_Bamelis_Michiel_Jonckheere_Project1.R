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
segments(20.5,0.16,20.5,0.26899, col = "white")
abline(h=0)
text(24,0.25, labels = c("Communicable, maternal,\n perinatal and nutritional conditions"), col = 2)
text(24,0.21, labels = c("Noncommunicable diseases"), col = 3)
text(24,0.18, labels = c("Injuries"), col = 4)

#* swerelds lelijkste figuur

dev.off()
# this figure shows class 1 has higher Communicable, maternal, perinatal and nutritional conditions then class 2
# it also overall has lower Noncommunicable diseases

# about 1/4 of the countries have significantly higher com... and lower noncom... then the rest and are tightly packed compared to the rest

###########################################################
######################   PCA   ############################
###########################################################




###########################################################
###########   Multivariate normaliteit   ##################
###########################################################




###########################################################
#################   Classificatie   ########################
###########################################################



