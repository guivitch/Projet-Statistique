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

# Box plot of prices according to the room type
type_diff_prices <- ggplot(rbnb_abo, aes(room_type, prices)) + 
  geom_boxplot(aes(fill = room_type)) +
  theme_minimal() +
  theme(legend.position = "top")
type_diff_prices

# Box plot of prices for each arrondissements
arr_diff_prices <- ggplot(rbnb_abo, aes(neighbourhood, prices)) + 
  geom_boxplot(aes(fill = neighbourhood)) +
  theme_minimal() +
  theme(legend.position = "top")
arr_diff_prices

# Box plot of reviews per month for each arrondissements
rbnb_abo$reviews_per_month <- as.numeric(rbnb_abo$reviews_per_month)
rbnb_abo$availability_365 <- as.numeric(rbnb_abo$availability_365)
arr_availibility <- ggplot(rbnb_abo, aes(neighbourhood, availability_365)) + 
  geom_boxplot(aes(fill = neighbourhood)) +
  theme_minimal() +
  theme(legend.position = "top")
arr_availibility

# Compute percentages
new_df$fraction <- new_df$proportion / sum(new_df$proportion )

# Compute the cumulative percentages (top of each rectangle)
new_df$ymax <- cumsum(new_df$fraction)

# Compute the bottom of each rectangle
new_df$ymin <- c(0, head(new_df$ymax, n=-1))

# Compute label position
new_df$labelPosition <- (new_df$ymax + new_df$ymin) / 2

# Compute a good label
new_df$label <- paste0(new_df$category, "\n value: ", new_df$proportion )

# Make the plot
ggplot(new_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3,col='black') +
  scale_fill_brewer(palette = "Dark2")+
  #scale_fill_viridis(discrete = T,option="magma") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

#read geojson
install.packages("geojsonsf")
library(geojsonsf)
library(ggplot2)

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
ggplot(data = arr, aes(geometry = geometry)) + geom_sf(aes(fill = density)) 
+ scale_fill_viridis(discrete = T) +
  geom_sf_text(
    data = arr,
    aes(label = l_qu),
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
metro <- geojson_sf("D:\\DeSIgeo/Projet Statistique/Data/gare_metro.geojson")
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

Luxembourg_apt <-  rbnb[rbnb$neighbourhood %in% c("Luxembourg"), ]
Luxembourg_apt$prices <- as.numeric(Luxembourg_apt$price)
hist(Luxembourg_apt$prices)
boxplot(Lux_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Louvre"), ]
Louvre_apt$prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)
boxplot(Louvre_prices)

Hdv_apt <-  rbnb[rbnb$neighbourhood %in% c("Hôtel-de-Ville"), ]
Hdv_apt$prices <- as.numeric(Hdv_apt$price)
hist(Hdv_apt$prices)

Gobelins_apt <-  rbnb[rbnb$neighbourhood %in% c("Gobelins"), ]
Gobelins_apt$prices <- as.numeric(Gobelins_apt$price)
hist(Gobelins_apt$prices)

Entrepot_apt <-  rbnb[rbnb$neighbourhood %in% c("Entrepôt"), ]
Entrepot_apt$prices <- as.numeric(Entrepot_apt$price)
hist(Entrepot_apt$prices)

Elysee_apt <-  rbnb[rbnb$neighbourhood %in% c("Élysée"), ]
Elysee_apt$prices <- as.numeric(Elysee_apt$price)
hist(Elysee_apt$prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Buttes-Montmartre"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Buttes-Chaumont"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Bourse"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Batignolles-Monceau"), ]
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
ggplot(Vaugirard_apt, aes(x=prices)) + geom_histogram()
# Changer la largeur des barres
ggplot(Vaugirard_apt, aes(x=prices)) + 
  geom_histogram(binwidth=80)
# Changer la couleur
p<-ggplot(Vaugirard_apt, aes(x=prices)) + 
  geom_histogram(color="black", fill="white",binwidth=80)
p
## affiche la moyenne
p+ geom_vline(aes(xintercept=median(prices)),
              color="blue", linetype="dashed", size=1)
p + geom_vline(aes(xintercept=mean(prices)),
               color="red", linetype="dashed", size=1)
p+ geom_vline(aes(xintercept=var(prices)),
              color="green", linetype="dashed", size=1)
Vaugi_new <- Filter(Vaugirard_apt, prices < 500)
ggplot(Gobelins_apt, aes(x=prices)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white",binwidth=80) +
  geom_density(alpha=0.5, fill="#FF6666")

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




#read shapefile :

"""
st_read(dsn = 'D:\\DeSIgeo/Projet Statistique/Data',layer = 'air_bnb_quartiers.shp')
install.packages("readr")
library(rgdal)
my_spdf <- readOGR( 
  dsn= paste0(getwd(),"/DATA/world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

regions <- st_read(dsn='data/regions/',layer='regions_2015_metropole_region')
"""

#  - voir les drivers disponibles (formats de fichiers)



# - plot point moyen uniquement



#  - "carte" avec point moyen et regions


# -  meme carte "zoomee"



#  - distance type ponderee




# 1.4) faire de même avec le point médian et point médian pondéré
#  package pour calculer le point median en 2d:

library(ICSNP)




#######
#  2 ) Analyse et visualisation des données: 
## Librairie
library(sf)
## Paris
#read geopackage/geojson
rbnb = st_read("D:\\DeSIgeo/Projet Statistique/Data/listings.csv")
prices <- as.numeric(rbnb$price)
View(prices)
## Vizualization
# Histogram
l =c(0 ,12000)

h <- hist(prices,col='orange',border='blue', xlim =range(l), ylim = NULL,breaks=10, main="With breaks=20")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
med <-median(prices)
mean <- mean.default(prices)
variance <- var(prices)
covariance <- cov(prices,quartiers)
for (i in rbnb$neighbourhood){
  print(i)
}



#entire_home_apt <- rbnb %>% Filter(room_type == "Entire home/apt")

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

# Compute percentages
new_df$fraction <- new_df$proportion / sum(new_df$proportion )

# Compute the cumulative percentages (top of each rectangle)
new_df$ymax <- cumsum(new_df$fraction)

# Compute the bottom of each rectangle
new_df$ymin <- c(0, head(new_df$ymax, n=-1))

# Compute label position
new_df$labelPosition <- (new_df$ymax + new_df$ymin) / 2

# Compute a good label
new_df$label <- paste0(new_df$category, "\n value: ", new_df$proportion )

# Make the plot
ggplot(new_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6,col='grey') +
  scale_fill_viridis(discrete = T) +
  #coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


typ_of_apt <-  c(entire_home_apt,private_rooms,shared_rooms,hotel_rooms)
function(price_info){
  price <- type$price
  print(price)
}
prices <- as.numeric(rbnb$price)


liste_quartier <- distinct(quartiers)
bplt <- boxplot(agg$x,agg$Category)
agg <- aggregate(as.numeric(rbnb$price), by=list(Category=rbnb$neighbourhood), FUN=median)
n
compte <-  aggregate(rbnb$neighbourhood_group, by=list(Category=rbnb$neighbourhood), count)
hist(agg)
li <- as.numeric(rbnb$id)
c <- c()
for(i in agg$Category){
  a <-  filter(rbnb, neighbourhood == i)
  "print(a)"
  n <- nrow(a)
  c <-c(c,n)
  
}

#read shapefile :
airB <- st_read(dsn = 'D:\\DeSIgeo/Projet Statistique/Data',layer = 'air_bnb_quartiers.shp')


#read geojson
install.packages("geojsonsf")
library(geojsonsf)
library(ggplot2)

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
ggplot(data = arr, aes(geometry = geometry)) + geom_sf(aes(fill = density)) 
+ scale_fill_viridis(discrete = T) +
  geom_sf_text(
  data = arr,
  aes(label = l_qu),
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
metro <- geojson_sf("D:\\DeSIgeo/Projet Statistique/Data/gare_metro.geojson")
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

ggplot(arr, aes(fill=l_aroff, y=density_metro, x=c_ar)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T,option= 'magma') +
  ggtitle("Densité de AirBNB par arrondissement") +
  theme_ipsum() +
  xlab("")


d = data.frame()
 for(i in arr$l_aroff){
   d <-  rbnb[rbnb$neighbourhood %in% c(i), ]
   print(i)
 }

Luxembourg_apt <-  rbnb[rbnb$neighbourhood %in% c("Luxembourg"), ]
Luxembourg_apt$prices <- as.numeric(Luxembourg_apt$price)
hist(Luxembourg_apt$prices)
boxplot(Lux_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Louvre"), ]
Louvre_apt$prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)
boxplot(Louvre_prices)

Hdv_apt <-  rbnb[rbnb$neighbourhood %in% c("Hôtel-de-Ville"), ]
Hdv_apt$prices <- as.numeric(Hdv_apt$price)
hist(Hdv_apt$prices)

Gobelins_apt <-  rbnb[rbnb$neighbourhood %in% c("Gobelins"), ]
Gobelins_apt$prices <- as.numeric(Gobelins_apt$price)
hist(Gobelins_apt$prices)

Entrepot_apt <-  rbnb[rbnb$neighbourhood %in% c("Entrepôt"), ]
Entrepot_apt$prices <- as.numeric(Entrepot_apt$price)
hist(Entrepot_apt$prices)

Elysee_apt <-  rbnb[rbnb$neighbourhood %in% c("Élysée"), ]
Elysee_apt$prices <- as.numeric(Elysee_apt$price)
hist(Elysee_apt$prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Buttes-Montmartre"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Buttes-Chaumont"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Bourse"), ]
Louvre_prices <- as.numeric(Louvre_apt$price)
hist(Louvre_prices)

Louvre_apt <-  rbnb[rbnb$neighbourhood %in% c("Batignolles-Monceau"), ]
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
ggplot(Vaugirard_apt, aes(x=prices)) + geom_histogram()
# Changer la largeur des barres
ggplot(Vaugirard_apt, aes(x=prices)) + 
  geom_histogram(binwidth=80)
# Changer la couleur
p<-ggplot(Vaugirard_apt, aes(x=prices)) + 
  geom_histogram(color="black", fill="white",binwidth=80)
p
## affiche la moyenne
p+ geom_vline(aes(xintercept=median(prices)),
              color="blue", linetype="dashed", size=1)
p + geom_vline(aes(xintercept=mean(prices)),
             color="red", linetype="dashed", size=1)
p+ geom_vline(aes(xintercept=var(prices)),
              color="green", linetype="dashed", size=1)
Vaugi_new <- Filter(Vaugirard_apt, prices < 500)
ggplot(Gobelins_apt, aes(x=prices)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white",binwidth=80) +
  geom_density(alpha=0.5, fill="#FF6666")
  
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

