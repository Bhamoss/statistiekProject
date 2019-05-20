###########################################################
#################   Project src   #########################
###########################################################






###########################################################
###############   Load and format data   ##################
###########################################################

# set wd
wd = dirname(rstudioapi::getActiveDocumentContext()$path); wd
setwd(wd)

# eerst alle data apart inlezen van Gent, Antwerpen en Brussel
gent = read.csv("listingsGent.csv", header = TRUE, sep = ",", dec = ".")
gent$city = "Gent"
antwerpen = read.csv("listingsAntwerpen.csv", header = TRUE, sep = ",", dec = ".")
antwerpen$city = "Antwerpen"
brussel = read.csv("listingsBrussel.csv", header = TRUE, sep = ",", dec = ".")
brussel$city = "Brussel"

# merge de 3 data frames tesamen 
airbnb = merge(gent, antwerpen, all = TRUE)
airbnb = merge(airbnb, brussel, all = TRUE)
# test of dat alle rows van de drie data frames in de nieuwe dataframe zitten
#   naive test, enkel op aantal rijen gebaseerd
nrow(airbnb) == (nrow(gent) + nrow(antwerpen) + nrow(brussel))

