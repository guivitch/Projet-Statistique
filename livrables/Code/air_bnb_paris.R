### PROJET STATISTIQUES, PROBAS & ANALYSE SPATIALE ###
### Autor : Guillaume MORAT
### 05/11/2022
### TOPIC : SPATIAL DISTRIBUTION OF AIRBNB RENTING PRICES IN PARIS, (MONTR2AL & SANTIAGO) 
### Disciplines : Statistiques, Probabilités, Géostatistiques & Analyse Spatiale
### Bonus : Algo d'Apprentissage Automatique (ML)

## INSTALL LIBRARY 
install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)
install.packages("sf")
library(sf)
install.packages("ggplot2")
library(ggplot2)

## OPEN DATA
rbnb = st_read("D:\\DeSIgeo/Projet Statistique/Data/listings.csv")

### Split the original airbnb data set into multiple according to :
## Type of room :
entire_home_apt <-  rbnb[rbnb$room_type %in% c("Entire home/apt"), ]
entire_home_prices <-  as.numeric(entire_home_apt$price)
hist(entire_home_prices) ## histogramme des prix des appartements
med_entire_home <- median(entire_home_prices) ## 103 euros
plot(as.numeric(quatiers),rbnb$price)

private_rooms <- rbnb[rbnb$room_type %in% c("Private room"), ]
private_rooms_prices <-  as.numeric(private_rooms$price)
hist(private_rooms_prices) ## histogramme des prix des hotels
med_private_rooms <- median(private_rooms_prices) ## 65euros

shared_rooms <- rbnb[rbnb$room_type %in% c("Shared room"), ]
shared_rooms_prices <-  as.numeric(shared_rooms$price)
hist(shared_rooms_prices)
med_shared_rooms <- median(shared_rooms_prices)##40 euros

### Hotel rooms
hotel_rooms <- rbnb[rbnb$room_type %in% c("Hotel room"), ]
hotel_prices <-  as.numeric(hotel_rooms$price)
hist(hotel_prices) ## histogramme des prix des hotels
med_hotel <- median(hotel_prices) ## 350euros

new_df <- data.frame(category=c("entire home/apt","private rooms","hotel rooms","shared rooms"),
                     proportion=c(nrow(entire_home_apt),nrow(private_rooms),
                                  nrow(hotel_rooms), nrow(shared_rooms)),
                     med_price = c(med_entire_home,med_private_rooms,med_hotel,med_shared_rooms))


rbnb$prices <- as.numeric(rbnb$price)
hist(rbnb$prices)
rbnb_abord <- rbnb[rbnb$prices < 500,]
rbnb_abord[rbnb_abord$room_type %in% c("entire home/apt","private rooms"),]
r <-  data.frame()
rbind(r,rbnb_abord['room_type']  == "Entire home/apt")

# Box plot of prices according to the room type
type_diff_prices <- ggplot(rbnb_abord, aes( prices)) + 
  geom_boxplot(aes(fill = room_type)) +
  ggtitle("Boxplot of renting prices in euros according to the room type")+
  theme(plot.title = element_text(hjust = 0.3)) +
  theme_minimal() +
  theme(legend.position = "top")
type_diff_prices

# Box plot of prices for each arrondissements
arr_diff_prices <- ggplot(geo_bnb1, aes(neighbourhood, prices)) + 
  geom_boxplot(aes(fill = neighbourhood)) +
  theme_minimal() +
  theme(legend.position = "top")
arr_diff_prices

# Box plot of reviews per month for each arrondissements
rbnb_abo$reviews_per_month <- as.numeric(rbnb_abo$reviews_per_month)
rbnb_abord$availability_365 <- as.numeric(rbnb_abo$availability_365)
arr_availibility <- ggplot(rbnb_abo, aes(neighbourhood, availability_365)) + 
  geom_boxplot(aes(fill = neighbourhood)) +
  theme_minimal() +
  theme(legend.position = "top")
arr_availibility
new_df[4,8 ] <- 0.2
new_df[3,8 ] <- 0.10
new_df[2,8 ] <- 0.8
new_df[1,8 ] <- 0.5
# Compute percentages
new_df$fraction <- new_df$proportion / sum(new_df$proportion )
new_df$fractions <-new_df$fraction*100
# Compute the cumulative percentages (top of each rectangle)
new_df$ymax <- cumsum(new_df$fraction)
population[order(population$age),]
ggplot(new_df, aes(med_price,fraction,col=category,)) +
  geom_point(aes(size =5,))  + 
  theme(
  legend.title = element_text(color, size, face),)+
  theme(legend.text = element_text(colour="black", size=14)) +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("Répartition des locations AirBNB à Paris selon le type de chambre en pourcentage")
# Compute the bottom of each rectangle
new_df$ymin <- c(0, head(new_df$ymax, n=-1))

# Compute label position
new_df$labelPosition <- (new_df$ymax + new_df$ymin) / 2
new_df$labelPosition["shared rooms"] <- (new_df$ymax["shared rooms",] + new_df$ymin["shared rooms",]) / 3
options( "digits"=3)
# Compute a good label
a =as.numeric(new_df[4,5])
b= 0.994
new_df[7,4] <- (a+b)/4
if(new_df$category %in% c('shared rooms'))
  new_df$labelPosition <- new_df$labelPosition 

new_df$labels <- paste0(new_df$category, " : \n  ", format(new_df$fraction*100, digits = 2)," % " )

# Make the plot
ggplot(new_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_bar(colour="black")
  ggtitle("Répartition des locations AirBNB à Paris selon le type de chambre en pourcentage")+
  geom_label( x=3.5, aes(y=labelPosition, label=labels), size=5,col='black') +
  scale_fill_brewer(palette = "Oranges")+
  coord_polar(theta="y") +
  xlim(c(2.6, 4)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.3)) +
  theme(plot.title=element_text(size=16,face="bold")) +
  theme(plot.border = element_rect(color = "black",fill = NA,linewidth =  2))

#read geojson
install.packages("geojsonsf")
library(geojsonsf)
library(ggplot2)
library(maps)
maps(entire_home_apt)
### zones d'études : quartiers < arrondissements??
quarter_paris <- geojson_sf("D:\\DeSIgeo/Projet Statistique/Data/quartier_paris.geojson")
arr <- geojson_sf("D:\\DeSIgeo/Projet Statistique/Data/arrondissements.geojson")

arr<- st_as_sf(arr, crs=st_crs(geo))
geo <- quarter_paris
ggplot(geo) + geom_sf(aes(fill = geo$c_arr)) 

geo_bnb <-  st_read("D:\\DeSIgeo/Projet Statistique/Data/listings.csv")
x <-  geo_bnb$longitude
y <-  geo_bnb$latitude
pts<- st_as_sf(geo_bnb, coords = c("longitude", "latitude"),crs=st_crs(geo))
ggplot() + geom_sf(aes(fill =geo$density))
inter <- st_intersects(geo, pts) # Points par quartiers
niveau1 <- sapply(X = inter, FUN = length) # Comptage des points
inter2 <- st_intersects(arr, pts) # Points par arrondissement
niveau2 <- sapply(X = inter2, FUN = length) # Comptage des points
niveau3 <-  sapply(X = inter2, FUN = sum)
niveau3
## densité
geo$density <- niveau1
arr$density <- niveau2
geo$density_surface <- geo$density/as.numeric(geo$surface)
arr$density_sufacique <- (arr$density/as.numeric(arr$surface)) %<% Filter(density > 3000)


# Small multiple
ggplot(arr, aes(fill=l_aroff, y=density, x=c_ar)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Densité de AirBNB par arrondissement") +
  theme_ipsum() +
  xlab("")
## Library Graphics

library(ggplot2)
install.packages("viridis")
install.packages("hrbrthemes")
library(viridis)
library(hrbrthemes)

#plot map de densité airbnb
ggplot(data = arr_name_1, aes(geometry = geometry)) + geom_sf(aes(fill = sd_prices)) + scale_fill_brewer(palette = "Oranges")+
  geom_sf_text(
    data = arr,
    aes(label = numero),
    check_overlap = TRUE,
    size = 4,
    color = "white"
  )

## carte de densité de airbnb par arrondissement
ggplot(data = arr, aes(geometry = geometry)) + geom_sf(aes(fill = density )) 

ggplot(data = pts,aes(geometry = geometry)) + geom_sf(aes(fill=neighbourhood))
plot(x=as.numeric(geo$l_qu[1:10]),y=geo$density[1:10])

n <- nrow(pts)

#arr_list <- distinct(as.numeric(arr$neighbourhood)
#for (i in pts$neighbourhood)

## momuments & stations RER + métro
## get data
monu <- geojson_sf("D:\\DeSIgeo/Projet Statistique/Data/main_sites_touristiques_idf.geojson")
metro <- geojson_sf("D :\\DeSIgeo/Projet Statistique/Data/gare_metro.geojson")
ggplot(data=metro,aes(geometry=geometry)) +geom_sf(aes(fill=arr$neighbourhood))
rer <- geojson_sf("D:\\DeSIgeo/Projet Statistique/Data/gare_RER.geojson")
ggplot(data=rer,aes(geometry=geometry)) +geom_sf(aes(fill=arr$neighbourhood))

intersect_metro <- st_intersects(arr, metro) # station de métro par arrondissement
arr$density_metro <- sapply(X = intersect_metro, FUN = length) # Comptage des points

cor(arr$density,arr$density_metro)## = 0.2243
cov(arr$density,arr$density_metro)## = 1764.605

intersect_rer <- st_intersects(arr, rer) # station de rer par arrondissement
arr$density_rer <- sapply(X = intersect_rer, FUN = length) # Comptage des points

cor(arr$density,arr$density_rer)## = -0.1447
cov(arr$density,arr$density_rer)## = -266.5

intersect_monu <- st_intersects(arr, monu) # sites touristiques par arrondissement
arr$density_monu <- sapply(X = intersect_monu, FUN = length) # Comptage des points

cor(arr$density,arr$density_monu)## = -0.5118
cov(arr$density,arr$density_monu)## = -5654.75

ggplot(arr, aes(fill=l_aroff, y=density, x=c_ar)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T,option= 'magma') +
  ggtitle("Densité de AirBNB par arrondissement") +
  theme_ipsum() +
  xlab("")

ggplot(arr, aes(fill=l_aroff, y=density_metro, x=c_ar)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T,option= 'magma') +
  ggtitle("Densité de métro par arrondissement") +
  theme_ipsum() +
  xlab("")


d = data.frame()
for(i in arr$l_aroff){
  d <-  rbnb[rbnb$neighbourhood %in% c(i), ]
  print(i)
}

Luxembourg_apt <-  g_bnb[g_bnb$neighbourhood %in% c("Luxembourg"), ]
Luxembourg_apt$prices <- as.numeric(Luxembourg_apt$price)
hist(Luxembourg_apt$prices)
Lux_prices <- Luxembourg_apt$prices
boxplot(Lux_prices)
IQR(Lux_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Louvre"), ]
Louvre_apt$prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)
boxplot(Louvre_prices)

Hdv_apt <-  g_bnb[g_bnb$neighbourhood %in% c("Hôtel-de-Ville"), ]
Hdv_apt$prices <- as.numeric(Hdv_apt$price)
hist(Hdv_apt$prices)
Hdv_prices <- Hdv_apt$prices
boxplot(Hdv_prices)
IQR(Hdv_prices)

Gobelins_apt <-  rbnb[rbnb$neighbourhood %in% c("Gobelins"), ]
Gobelins_apt$prices <- as.numeric(Gobelins_apt$price)
hist(Gobelins_apt$prices)
Gobelins_apt <-  g_bnb[g_bnb$neighbourhood %in% c("Gobelins"), ]
Gobelins_apt$prices <- as.numeric(Gobelins_apt$price)
hist(Gobelins_apt$prices)
Gobelins_apt_prices <- Gobelins_apt$prices
boxplot(Gobelins_apt_prices)
IQR(Gobelins_apt_prices)

Entrepot_apt <-  rbnb[rbnb$neighbourhood %in% c("Entrepôt"), ]
Entrepot_apt$prices <- as.numeric(Entrepot_apt$price)
hist(Entrepot_apt$prices)

Elysee_apt <-  rbnb[rbnb$neighbourhood %in% c("Élysée"), ]
Elysee_apt$prices <- as.numeric(Elysee_apt$price)
hist(Elysee_apt$prices)

Buttes_Montmartre_apt <-  rbnb[rbnb$neighbourhood %in% c("Buttes-Montmartre"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Buttes_Chaumont_apt <-  rbnb[rbnb$neighbourhood %in% c("Buttes-Chaumont"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Bourse_apt <-  rbnb[rbnb$neighbourhood %in% c("Bourse"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Monceau_apt <-  rbnb[rbnb$neighbourhood %in% c("Batignolles-Monceau"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Ménilmontant"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Observatoire"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Opéra"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Palais-Bourbon"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Panthéon"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Passy"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Popincourt"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Reuilly"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Temple"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Vaugirard_apt <-  rbnb[rbnb$neighbourhood %in% c("Vaugirard"), ]
Vaugirard_apt$prices <- as.numeric(Vaugirard_apt$price)
hist(Vaugirard_apt$prices)

# Histogramme basique
ggplot(rbnb_abord, aes(x=prices)) + geom_histogram(color="black", fill="white",binwidth=20) + geom_vline(aes(xintercept=median(prices)),color="blue", linetype="dashed", size=1)+
 geom_vline(aes(xintercept=mean(prices)),color="red", linetype="dashed", linewidth=1) + ggtitle("Histogramme des prix eu euros des locations AirBNB dans Paris","prix médian en bleu et prix moyen en rouge")  +
  theme(plot.title = element_text(hjust = 0.3)) +
  theme(plot.title=element_text(size=16,face="bold")) 
# Changer la largeur des barres
ggplot(rbnb_abord, aes(x=prices)) + 
  geom_histogram(binwidth=8, color="black", fill="white")
# Changer la couleur
p<-ggplot(rbnb_abord, aes(x=prices)) + 
  geom_histogram(color="black", fill="white",binwidth=10)
p
## affiche la moyenne
p+ geom_vline(aes(xintercept=median(prices)),
              color="blue", linetype="dashed", size=1)
p + geom_vline(aes(xintercept=mean(prices)),
               color="red", linetype="dashed", size=1)
p+ geom_vline(aes(xintercept=var(prices)),
              color="green", linetype="dashed", size=1)
Vaugi_new <- Filter(Vaugirard_apt, prices < 500)


## summary



## histo 2
arr_diff_prices <- ggplot(g_bnb, aes(numero, prices)) + 
  geom_boxplot(aes(fill = neighbourhood)) +
  ggtitle("Distribution des prix Arrondissement Parisiens") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y= "Prix / Nuit (en Euros)", x = "Numéro d'Arrondissement") +
  theme(plot.title=element_text(size=12,face="bold")) +
  theme_minimal() 
  theme(legend.position = "top")
arr_diff_prices
hist_plot <- ggplot(g_bnb)+
  geom_histogram(aes(y=prices, x=numero, fill=neighbourhood), color="gray")+
  #geom_boxplot(aes(y=prices, x=numero, group=neighbourhood), color="black", fill="#eeeeee" ,width=0.1)+
  labs(title = "Distribution des prix par numéro d'arrondissement")+
  theme(plot.title = element_text(hjust = 0.3)) +
  theme(legend.position = "top")  +
  labs(y= "Prix / Nuit (en Euros)", x = "Numéro d'Arrondissement") +
  # theme(plot.title = element_text(hjust = 0.3)) +
  theme(plot.title=element_text(size=12,face="bold")) 
#theme_light()
hist_plot
##VIOLIN PLOT

violin_plot <- ggplot(g_bnb)+
  #geom_violin(aes(y=prices, x=numero, fill=neighbourhood), color="gray", trim=F)+
  geom_boxplot(aes(y=prices, x=numero, group=neighbourhood))+
  labs(title = "Distribution des prix par numéro d'arrondissement")+
  theme(plot.title = element_text(hjust = 0.3)) +
  theme(legend.position = "top")  +
  labs(y= "Prix / Nuit (en Euros)", x = "Numéro d'Arrondissement") +
 # theme(plot.title = element_text(hjust = 0.3)) +
  theme(plot.title=element_text(size=12,face="bold")) 
  #theme_light()
violin_plot +scale_color_gradientn(colours = rainbow(7))

median(rbnb_abord$prices)
g_bnb <- merge(x =rbnb_abord, y = arr_name, by = "neighbourhood")
g_bnb <-  g_bnb[order(g_bnb, g_bnb$numero),]
ggplot(rbnb_abord, aes(x=prices)) + 
  ggtitle("Densité des prix en euros pour les locations airBNB dans Paris","prix médian en bleu et prix moyen en rouge")+
   geom_vline(aes(xintercept=median(prices)),color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=mean(prices)),color="red", linetype="dashed", linewidth=1)+
  geom_density(alpha=0.5, fill="orange")

rbnb$prices <- as.numeric(rbnb$price)
ggplot(rbnb_abo2, aes(x=as.numeric(prices),)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",binwidth=80) +
  geom_density(alpha=0.5, fill="#FF6666")

ggplot(rbnb_abo2, aes(x=as.numeric(prices),)) +
  geom_histogram(fill="white",colour="black", position="dodge",,binwidth=70)+
  theme(legend.position="top")

## rbnb abordable 
rbnb_abo <- rbnb[rbnb$prices < 500, ]
rbnb_abo2 <- rbnb_abo[as.numeric(rbnb$reviews_per_month)> 1,]
mu <- dplyr(rbnb_abo, "neighbourhood", summarise, grp.mean=mean(prices))

# Ajouter le trait de la moyenne
p<-ggplot(rbnb_abo2, aes(x=prices, color=room_type)) +
  geom_histogram(fill="white", position="dodge",binwidth=30)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
             linetype="dashed")+
  theme(legend.position="top")
p
entire_home_prices <-  as.numeric(entire_home_apt$price)

