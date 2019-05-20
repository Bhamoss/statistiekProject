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

attributes(airbnb)$names
# price == 0 -> NA
airbnb$price[airbnb$price == 0] = NA
# minimum_nights > 365 -> NA
airbnb$minimum_nights[airbnb$minimum_nights > 365] = NA

# TODO: is dit correct???
# availability_365 < 365 -> full = FALSE
# availability_365 == 365 -> full = TRUE
airbnb$full[airbnb$availability_365 < 365] = FALSE
airbnb$full[airbnb$availability_365 == 365] = TRUE

airbnb$last_review[airbnb$last_review == ""] = NA

# TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# last_review aanpassen brussel
datum = as.Date("2019-04-13");
airbnb$date_compiled[airbnb$city == "Brussel"] = as.Date(datum)

# last_review aanpassen Gent
datum = as.Date("2019-04-16")
airbnb$date_compiled[airbnb$city == "Gent"] = as.Date(datum)

# last_review aanpassen Antwerpen
datum = as.Date("2019-04-20");
airbnb$date_compiled[airbnb$city == "Antwerpen"] = as.Date(datum)


attach(airbnb)




detach(airbnb)
