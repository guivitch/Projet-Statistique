### PROJET STATISTIQUES, PROBAS & ANALYSE SPATIALE ###
### Autor : Guillaume MORAT
### 05/11/2022
### TOPIC : SPATIAL DISTRIBUTION OF AIRBNB RENTING PRICES IN PARIS 
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
install.packages("maps")


#read geojson
install.packages("geojsonsf")
library(geojsonsf)
library(ggplot2)
library(maps)
map(pts_rer$X,pts_rer$Y)
bnb <-  st_read("D:\\DeSIgeo/Projet Statistique/Data/listings.csv")
x <-  bnb$longitude
y <-  bnb$latitude
pts_a<- st_as_sf(bnb, coords = c("longitude", "latitude"),crs=st_crs(arr))
pts_n <- st_transform(pts_a,crs="EPSG:2154")
rbnb_abo <- bnb[as.numeric(bnb$price) < 500, ]
rbnb_abo2 <- rbnb_abo[as.numeric(rbnb_abo$reviews_per_month)> 1,]
private_room <- rbnb_abo2[rbnb_abo2$room_type == "Private room",]
### zones d'études : quartiers < arrondissements??
quarter_paris <- geojson_sf("D:\\DeSIgeo/Projet Statistique/Data/quartier_paris.geojson")
arr <- geojson_sf("D:\\DeSIgeo/Projet Statistique/Data/arrondissements.geojson")
pts_bnb<- st_as_sf(rbnb_abo, coords = c("longitude", "latitude"),crs=st_crs(arr))
## densité par arrondissement 
quarter_paris <-st_transform(quarter_paris, st_crs(pts_2154))
inter_quart <- st_intersects(quarter_paris, pts_2154)
quarter_paris$nb <-  sapply(X = inter_quart, FUN = length)
ggplot(quarter_paris, aes(geometry =geometry)) 
inter2 <- st_intersects(arr, pts_2154) # Points par arrondissement
arr_name_n$nombre_rbnb <- sapply(X = inter2, FUN = length) # Comptage des points
arr$density <- niveau2
geom_
x <- st_coordinates(pts_2154)
pts_2154$X <- x[,1]
pts_2154$Y <- x[,2]
pts_2154 %>% ggplot(aes(X, Y))+
  stat_density_2d(geom = "polygon", contour = TRUE,
                  aes(fill = after_stat(level)), colour = "black",
                  bins = 5) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_classic()
## momuments & stations RER + métro
## get data
monu <- geojson_sf("D:\\DeSIgeo/Projet Statistique/Data/main_sites_touristiques_idf.geojson")
pts_monu <- st_as_sf(monu, coords = c("longitude", "latitude"),crs="EPSG:2154")

metro <- geojson_sf("D:\\DeSIgeo/Projet Statistique/Data/gare_metro.geojson")
pts_metro <- st_as_sf(metro, coords = c("longitude", "latitude"),crs="EPSG:2154")
#ggplot(data=metro,aes(geometry=geometry)) +geom_sf(aes(fill=arr$neighbourhood))
rer <- geojson_sf("D:\\DeSIgeo/Projet Statistique/Data/gare_RER.geojson")
pts_rer <- st_as_sf(rer, coords = c("longitude", "latitude"),crs=st_crs(arr))

intersect_metro <- st_intersects(arr, metro) # station de métro par arrondissement
arr$density_metro <- sapply(X = intersect_metro, FUN = length) # Comptage des points

cor(arr$density,arr$density_metro)## = 0.2243
cov(arr$density,arr$density_metro)## = 1764.605

intersect_rer <- st_intersects(arr, rer) # station de rer par arrondissement
arr$density_rer <- sapply(X = intersect_rer, FUN = length) # Comptage des points

cor(arr$density,arr$density_rer)## = -0.1447
cov(arr$density,arr$density_rer)## = -266.5
st_crs(arr)
pts_monu <- st_transform(pts_monu,st_crs(arr))
intersect_monu <- st_intersects(arr,pts_monu) # sites touristiques par arrondissement
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

### VIZUALIZATION 
## Histogram
ggplot(rbnb_abo2, aes(x=as.numeric(price),)) +
  geom_histogram(fill="white",colour="black", position="dodge",binwidth=70)+
  geom_density(alpha=0.5, fill="#FF6666")+
  theme(legend.position="top")

geo_bnb$prices <-  as.numeric(geo_bnb$price)

geo_bnb <-geo_bnb[geo_bnb$prices < 421,]
arr_diff_prices <- ggplot(geo_bnb, aes(n, prices)) + 
  geom_boxplot(aes(fill = neighbourhood)) +
  scale_color_gradient(low = "white", high = "red") 
  theme_minimal() +
  theme(legend.position = "top")
arr_diff_prices

arr_name <-  data.frame(arr$l_aroff,arr$l_ar)
arr_name$neighbourhood <- arr$l_aroff
arr_name$numero <-arr$c_ar
##JOIN
## AGGREGATE
med_prices <- aggregate(arrBNB_merge$prices, by=list(Category=arrBNB_merge$neighbourhood), FUN=median)
med_prices$neighbourhood <- med_prices$Category

quarter_paris
mean_prices <- aggregate(arrBNB_merge$prices, by=list(Category=arrBNB_merge$neighbourhood), FUN=mean)
mean_prices$neighbourhood <- mean_prices$Category

sd_prices <- aggregate(arrBNB_merge$prices, by=list(Category=arrBNB_merge$neighbourhood), FUN=sd)
sd_prices$neighbourhood <- med_prices$Category

var_prices <- aggregate(arrBNB_merge$prices, by=list(Category=arrBNB_merge$neighbourhood), FUN=var)
var_prices$neighbourhood <- var_prices$Category

min_prices <- aggregate(arrBNB_merge$prices, by=list(Category=arrBNB_merge$neighbourhood), FUN=min)
min_prices$neighbourhood <- min_prices$Category

delta_q <- aggregate(arrBNB_merge$prices, by=list(Category=arrBNB_merge$neighbourhood), FUN=quantile)
delta_q$neighbourhood <- delta_q$Category

max_prices <- aggregate(arrBNB_merge$prices, by=list(Category=arrBNB_merge$neighbourhood), FUN=max)
max_prices$neighbourhood <- max_prices$Category


quartbnb <- quartbnb[quartbnb$prices < 500,]
quartbnb$prices <- as.numeric(quartbnb$price)

interquartile_price_quart <- aggregate(quartbnb$prices,by=list(Category=quartbnb$c_qu), FUN=quantile)
interquartile_price_quart$c_qu <- mean_price_quart$Category
quarter_paris$median_price <- quarter_paris$x.y

mean_price_quart <- aggregate(quartbnb$prices,by=list(Category=quartbnb$c_qu), FUN=mean)
mean_price_quart$c_qu <- mean_price_quart$Category
quarter_paris$median_price <- quarter_paris$x.y

nb_quart <- aggregate(quartbnb$prices,by=list(Category=quartbnb$c_qu), FUN=mean)
mean_price_quart$c_qu <- mean_price_quart$Category
quarter_paris$median_price <- quarter_paris$x.y

median_price_quart <- aggregate(quartbnb$prices,by=list(Category=quartbnb$c_qu), FUN=median)
median_price_quart$c_qu <- median_price_quart$Category
quarter_paris$mean_price <- quarter_paris$x.x
quarter_paris <- merge(quarter_paris,interquartile_price_quart, by='c_qu')

ggplot(data = quarter_paris, aes(geometry = geometry)) + geom_sf(aes(fill = DeltaQ )) +scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_classic() + ggtitle("Prix moyen en euros des locations AirBNB dans Paris par quartiers") +labs(y = "Latitude (°)", x = "Longitude (°)", colour = "Prix moyen en euros")
ggplot(data = quarter_paris, aes(geometry = geometry)) + geom_sf(aes(fill = median_price )) +scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_classic() + ggtitle("Prix médian en euros des locations AirBNB dans Paris par quartiers") +labs(y = "Latitude (°N)", x = "Longitude (°E)", colour = "Prix moyen en euros")

quarter_paris <- merge(x=quarter_paris,y = interquartile_price_quart,by='c_qu')
arr_name_n <- merge(x = arr_name_n, y= delta_q, by = "neighbourhood")
arr_name_n <- arr_name_n[ -c(7) ]
quarter_paris$DeltaQ <- quarter_paris$x[,"75%"] -  quarter_paris$x[,"25%"]
arr_name_n$DeltaQ <- arr_name_n$x[,"75%"] - arr_name_n$x[,"25%"]
arr_name_n$DeltaQ1 <- arr_name_n$x[,"100%"] - arr_name_n$x[,"0%"]
arr_name_n$DeltaQ2 <- arr_name_n$x[,"100%"] - arr_name_n$x[,"25%"]
arr_name_n$DeltaQ3 <- arr_name_n$x[,"100%"] - arr_name_n$x[,"50%"]
arr_name_n$DeltaQ4 <- arr_name_n$x[,"100%"] - arr_name_n$x[,"75%"]
mat <- data.frame(arr_name_n$DeltaQ,arr_name_n$sd_prices,arr_name_n$median_prices,arr_name_n$mean_prices)
mat <- data.frame(arr_name_n$DeltaQ,arr_name_n$DeltaQ1,arr_name_n$DeltaQ2,arr_name_n$DeltaQ3,arr_name_n$DeltaQ4)
res <- prcomp(mat, center = TRUE, scale = TRUE)
fviz_screeplot(res, addlabels = TRUE)

res$rotation
res$sdev
res$center
res$scale
xresult <- res$x
ggplot(arr_name_n, aes(DeltaQ2,sd_prices,col=numero)) +
geom_point(aes(size =5))
df1 <- data.frame(arr_name_n$DeltaQ,arr_name_n$mean_prices)
df <-  data.frame(arr_name_n$DeltaQ,arr_name_n$median_prices)
plot(df)
db <- fpc::dbscan(df, eps = 15, MinPts = 3)
a <- fviz_cluster(db, df, geom = "point")
a
ab <-  data.frame()
ab <- data.frame(ab,arr_BNB[arr_BNB$prices > 500,])
g_bnb$latitude <- as.numeric(g_bnb$latitude)
g_bnb$longitude <- as.numeric(g_bnb$longitude)
pts_gbnb <- st_as_sf(g_bnb, coords = c("longitude", "latitude"),st_crs("EPSG:2154"))
xg <- st_coordinates(g_bnb)
ggplot(data=g_bnb, aes(longitude,latitude,col=prices))+geom_point() + scale_fill_distiller(palette = "Reds", direction = 1) + ggtitle("Jeu de données initial sur les AirBNB")
## linear regression
## K-MEANS
## Arrondissements
km.res <- kmeans(df1, 3, nstart = 10)
plot(df1)
fviz_cluster(km.res, df1, ellipse = FALSE, geom = "point") +labs(x ='écart-interquartile (en euros)', y ='prix médian (en euros)') +ggtitle("K-Means Classification des arrondissements selon la médiane des prix et l'écart inter-quartile ")
## Quartiers
km.res2 <- kmeans(df2,3,nstart=1)
df2  <- data.frame(quarter_paris$mean_price,quarter_paris$DeltaQ)
plot(df2)
fviz_cluster(km.res2, df2, ellipse = FALSE, geom = "point") +labs(x ='écart-interquartile (en euros)', y ='prix médian (en euros)') +
ggtitle("K-Means Classification des quartiers selon la médiane des prix et l'écart inter-quartile ")
ggplot(arr_name_n, aes(nombre_rbnb,median_prices,col=mean_prices,size=numero)) +
geom_point(aes(size =numero)) +scale_fill_distiller(palette = "Reds", direction = 1) +
theme_classic()
ggplot(arr_name_n$xresultx,arr_name_n$xresulty,arr_name_n$numero)
arr_name_n$xresultx <- xresult[,1]
arr_name_n$xresulty <- xresult[,2]
arr_name_n$median_prices <- arr_name_n$x
arr_name_n$mean_prices <- arr_name_n$x.y
arr_name_n$sd_prices <- arr_name_n$x.y
arr$nb_of_renting <-  aggregate(arrBNB_merge$prices, by=list(Category=arrBNB_merge$neighbourhood), FUN=count)
arrBNB_merge = merge(x = geo_bnb, y = arr_name, by = "neighbourhood")
arr$neighbourhood <- arr$l_aroff
arr_name_1 <- merge(arr_name_n,arr, by='neighbourhood')
periph <- arrBNB_merge[arrBNB_merge$prices < 150,]
plot(periph$X,periph$Y)

## SPATIAL JOIN
arr_BNB <- arrBNB_merge[order(arrBNB_merge$numero),]
arr_BNB$numero_arrondissement
arr_dif_prices <- ggplot(arr_BNB, aes(numero, prices)) + 
  geom_boxplot(aes(fill = neighbourhood)) +
  theme(legend.position = "top")
arr_dif_prices +scale_color_gradientn(colours = rainbow(7))

quartbnb$pri
quartbnb$numero <- as.numeric(quartbnb$c_qu)7
quartbnb$numero_quart <- as.factor(quartbnb$numero)
quart_diff_prices_2 <- ggplot(quartbnb[quartbnb$numero_quart %in% c(40:80),], aes(numero_quart, prices, group_by(numero_quart %in% c(1:20)))) + 
  geom_boxplot(aes(fill = l_qu)) +
  ggtitle(" Distribution des prix (euros/nuit) des AirBNB par numéro de quartier (40 à 80)")+ labs( x= "Numéro de quartier", y ="Prix / Nuit (en euros)") +
  theme_minimal() +
  theme(legend.position = "top")
quart_diff_prices_2

quart_diff_prices <- ggplot(quartbnb[quartbnb$numero_quart %in% c(1:40),], aes(numero_quart, prices, group_by(numero_quart %in% c(1:20)))) + 
  geom_boxplot(aes(fill = l_qu)) +
  ggtitle(" Distribution des prix (euros/nuit) des AirBNB par numéro de quartier (1 à 40)")+ labs( x= "Numéro de quartier", y ="Prix / Nuit (en euros)") +
  theme_minimal() +
  theme(legend.position = "top")
quart_diff_prices 



arr_diff_prices <- ggplot(g_bnb, aes(numero, prices)) + 
  geom_boxplot(aes(fill = neighbourhood)) +
  ggtitle(" Distribution des prix (euros/nuit) des AirBNB par numéro d'arrondissement")+ labs( x= "Numéro d'Arrondissement", y ="Prix / Nuit (en euros)") +
  theme_minimal() +
  theme(legend.position = "top")
arr_diff_prices 
+ ggtitle(label) + # for the main title
xlab("Numéro d'arrondissement")+ # for the x axis label
ylab(label)+ # for the y axis label
#labs(...) # for the main title, axis labels and legend titles

install.packages("viridis")
install.packages("hrbrthemes")
library(viridis)
library(hrbrthemes)
quartbnb <- st_join(pts_2154,quarter_paris)

ggplot(data = quartbnb, aes(geometry = geometry))
quarter_paris$classe0 <- c(" 1 : quartiers attractifs")
hist(quarter_paris$median_price)
#quarter_paris[quarter_paris$median_price>160,"classe0"] = " 6 : quartiers très attractifs"
quarter_paris[quarter_paris$median_price>150,"classe0"] = " 1 : quartiers attractifs"
quarter_paris[quarter_paris$median_price<151,"classe0"] = " 2 : quartiers assez attractifs"
quarter_paris[quarter_paris$median_price<120,"classe0"] = " 3 : quartiers intermédiaires"
quarter_paris[quarter_paris$median_price<100,"classe0"] = " 4 : quartiers peu attractifs"
quarter_paris[quarter_paris$median_price<80,"classe0"] = " 5 : quartiers pas attractifs"
quarter_paris$classes <- as.character(quarter_paris$classe0)
ggplot(data = quarter_paris, aes(geometry = geometry)) + geom_sf(aes(fill = classes )) + 
  geom_sf_text(
    data = quarter_paris,
    aes(label = c_ar),
    check_overlap = TRUE,
    size = 4,
    color = "black") +
scale_fill_discrete("Classe") +  
ggtitle("Carte de classification des quartiers parisiens en fonction du prix médian en euros") +
labs(y = "N (mètres)", x="E (mètres)")


quarter_paris$classe1 <- c(1)

quarter_paris[quarter_paris$DeltaQ<200,"classe1"] = 1
quarter_paris[quarter_paris$DeltaQ<145,"classe1"] = 2
quarter_paris[quarter_paris$DeltaQ<105,"classe1"] = 3
quarter_paris[quarter_paris$DeltaQ<85,"classe1"] = 4
quarter_paris[quarter_paris$DeltaQ<55,"classe1"] = 5
ggplot(data =quarter_paris,aes(DeltaQ,median_price))  + geom_point(size = 4)
quarter_paris$classes1 <- as.character(quarter_paris$classe1)
ggplot(data = quarter_paris, aes(geometry = geometry)) + geom_sf(aes(fill = classes1 )) + 
  geom_sf_text(
    data = quarter_paris,
    aes(label = c_ar),
    check_overlap = TRUE,
    size = 4,
    color = "white") +
ggtitle("Carte de classification des quartiers parisiens en fonction de l'écart inter-quartile du prix de location Airbnb en euros") +
labs(y = "N (mètres)", x="E (mètres)") +scale_fill_manual(values = c("blue","green","yellow", "orange", "red"), name = "Classe", 
                                                          guide = guide_legend(reverse = FALSE))

arr_name_n$classe <- c("Arrondissements Intermédiaires")
arr_name_n[arr_name_n$median_prices>130,"classe"] = "Arrondissements centraux et attractifs"
arr_name_n[arr_name_n$median_prices<109,"classe"] = "Arrondssements périphériques et moins attractifs"
arr_name_n$classe01 <- c(2)
arr_name_n[arr_name_n$median_prices>130,"classe01"] = 3
arr_name_n[arr_name_n$median_prices<109,"classe01"] = 1

arr_gold1 <- arr_name_1[arr_name_1$median_prices<10,]
arr_gold <- arr_gold1[arr_gold1$mean_prices>120,]
arr_periph <- arr_name_1[arr_name_1$median_prices < 120,]
ggplot(data = arr_gold1, aes(geometry = geometry)) + geom_sf(aes(fill = median_prices )) + 
  geom_sf_text(
    data = arr_gold1,
    aes(label = numero.x),
    check_overlap = TRUE,
    size = 4,
    color = "white") +ggtitle("Arrondissement dont les prix médian sont supérieurs à 130 euros") +labs(y = "N (mètres)", x="E (mètres)")
arr_geom21 <- merge(arr_name_n,arr,by="neighbourhood")
arr_geom21$classes01 <- as.character(arr_geom21$classe01)
ggplot(data = arr_geom21, aes(geometry = geometry)) + geom_sf(aes(fill = classes01 )) + geom_sf_text(
    data = arr_geom21,
    aes(label = numero.x),
    check_overlap = TRUE,
    size = 4,
    color = "black")+
 scale_fill_discrete("Classe")+ scale_fill_manual(values = c("yellow", "orange", "red"), name = "Classe", 
                                                  guide = guide_legend(reverse = TRUE)) +ggtitle("Carte de classification des Arrondissements en fonction des profil de prix") +labs(y = "N (mètres)", x="E (mètres)")

g1 <- ggplot(data = quarter_paris, aes(geometry = geometry)) + geom_sf(aes(fill = nb )) 
g1 + 
ggplot(data = arr_periph, aes(geometry = geometry)) + geom_sf(aes(fill = sd_prices )) + 
  geom_sf_text(
    data = arr_gold,
    title ='New',
    aes(label = numero.x, title='new'),
    check_overlap = TRUE,
    size = 4,
    color = "white"
  )
median(g_bnb$prices)
arr_centroide <- st_centroid(arr)
arr_geom3 <- merge(arr_centroide, arr_geom)
ggplot(data = arr_centroide, aes(geometry = geometry)) + geom_sf(aes(size = med_prices$x, col= med_prices$x )) 
ggplot(data = arr_name_1, aes(geometry = geometry)) + geom_sf(aes(fill = median_prices ))
ggplot(data = arr_geom, aes(geometry = geometry)) + geom_sf(aes( fill=DeltaQ)) + labs(title ="Carte des écarts-interquartiles des prix (en euros) par arrondissement dans Paris",x='Est(m)',y='Nord(m)') +
geom_sf_text(
  data = arr_geom,
  aes(label = numero.y),
  check_overlap = TRUE,
  size = 4,
  color = "white") 
arr$mean_prices

lm1 <- lm(median_prices ~ DeltaQ2, data = arr_geom)
lm2 <- lm(median_prices ~ sd_prices, data = arr_geom)
plot(arr_geom$sd_prices,arr_geom$median_prices, pch = 16, col = "blue") #Plot the results
abline(lm1) #Add a regression line
abline(lm2,col='red')


### ANALYSIS TREATMENT 

## Analyse Spatiale :

#Analyse des semis de points :
#2) calcule du point moyen, point moyen pondéré, distance-type pour chaque quartiers
st_crs(pts)
st_crs(arr)
geo_bnb <-  st_read("D:\\DeSIgeo/Projet Statistique/Data/listings.csv")
geo_bnb$x <-  as.numeric(geo_bnb$longitude)
geo_bnb$y <-   as.numeric(geo_bnb$latitude)
coords <- c(geo_bnb$x,geo_bnb$y)
geo_bnb$prices <- as.numeric(geo_bnb$price)
geo_bnb0 <- geo_bnb[as.numeric(geo_bnb$reviews_per_month)> 0.9,]
geo_bnb1 <- geo_bnb0[geo_bnb0$prices< 420,]

bnb$dates_last_reviews <-as.Date(bnb$last_review, "%y/%m/%d")


pts_utm1<- st_as_sf(geo_bnb, coords = c("x", "y"),crs="EPSG:4326")
pts_2154<- st_transform(pts_utm1,"+init=EPSG:2154")
pts_monu <- st_transform(pts_monu,"EPSG:2154")
intersection_monu <- st_intersection(pts_monu,arr)
pts_metro <- st_transform(pts_metro,"EPSG:2154")
pts_rbnb <- st_transform(pts_utm1,crs=st_crs(arr))

bnb$dates_last_reviews <-as.Date(bnb$last_review)
hist(bnb_recently_booked$dates_last_reviews,"years")
a="2015-01-01"
a <- as.Date(a)
bnb_recently_booked <- bnb[bnb$dates_last_reviews > a,]

arr <- st_transform(arr,"EPSG:2154")
pts_rer <- st_transform(pts_rer, "EPSG:2154")
intersection_rbnb <- st_intersection(pts_2154,arr)
intersection_metro <- st_intersection(pts_metro,arr)
intersection_rer <- st_intersection(pts_rer,arr)

pts_utm <- st_transform(pts_utm1,"+proj=lambert +zone=31 ellps=WGS84")
pts_2154$geometry
pts_rbnb$geometry
pts_rbnb <- st_transform(pts_rbnb,st_crs(pts_metro))

pts_rbnb$geometry
pts_utm$geometry
pts_utm1$geometry
pts_metro$geometry
pts_rer$geometry
arr$geometry
#  - reprojection vers "EPSG:2154"
rbnb_2154 = st_transform(rbnb_abo2,"EPSG:2154")
arr_2154 = st_transform(arr,"EPSG:2154")
pts_2154 <- st_intersection(pts_2154,arr)
pts_2154 <- 
pts_2154$x = pts_2154$longitude
pts_2154$y =pts_2154$lattitude
installed.packages("rgdal")
library(rgdal)
#  - point moyen
x <- st_coordinates(pts_2154)
pts_2154$X <- x[,1]
pts_2154$Y <- x[,2]

pts_2154$prices <- as.numeric(pts_2154$price)
mean_x = mean(X)
mean_y = mean(Y)

mean_point = apply(geo_bnb[, c("x","y")], MARGIN = 2, mean)

plot(X,Y)
points(mean_x,mean_y,col='red')

#  - point moyen pondere par les prix
wmean <- data.frame()
wmean$x = sum(pts_2154$prices*pts_2154$X)/sum(pts_2154$prices)
wmean$y = sum(pts_2154$prices*pts_2154$Y)/sum(pts_2154$prices)
wmeanx = sum(geo_bnb1$prices*geo_bnb1$x)/sum(geo_bnb1$prices)
wmeany = sum(geo_bnb1$prices*geo_bnb1$y)/sum(geo_bnb1$prices)

geo_bnb1 = bnb_recently_booked[,]
pts_utm1<- st_as_sf(geo_bnb1, coords = c("lattitude", "longitude"),crs="EPSG:4326")
pts_2154<- st_transform(pts_utm1,"+init=EPSG:2154")
pts_2154 <- st_join(pts_2154,arr)
pts_215 <-  pts_2154[prices < 90,]
pts_21 <- pts_215[prices<80,]
pts_21
centroide <-  st_centroid(arr)

x21 <-  st_coordinates()
 b <- ggplot(pts_21,aes(x = )) + geom_point(aes(x = wmeanx,y =wmeany, col ='red')) 
b <- b +scale_color_gradient(low = "blue", high = "red") 
wmeanx = sum(geo_bnb1$prices*geo_bnb1$x)/sum(geo_bnb1$prices)
wmeany = sum(geo_bnb1$prices*geo_bnb1$y)/sum(geo_bnb1$prices)
plot(pts_2154$X,pts_2154$Y);points(wmeanx,wmeany,col='red');points(mean_x,mean_y,col='blue')
g=ggplot()
g+geom_point(aes(x = wmeanx,y =wmeany, col ='red'))
g+geom_point(aes(x = mean_x,y =mean_y, col= 'blue'))
g + geom_polygon(data=arr, aes(x=a[1], y= a[2],fill=l_ar))
a <-  st_coordinates(arr)

## distance type pondéré

sigma = (sum((pts_2154$prices)*((pts_2154$X-wmeanx)^2+(pts_2154$Y-wmeany)^2))/sum(pts_2154$prices))^(1/2)
## 3442 mètres

## Distance PPV
dist_pts <- dist(x, method = "euclidean")
matDist_pts <- as.matrix(dist_pts)




### Kernel Density estimation (KDE) ###
install.packages("MASS")
library(MASS)

## Densité de AirBNB
pts_2154$geometry
x <- st_coordinates(pts_2154)
pts_2154$X <- x[,1]
pts_2154$Y <- x[,2]
Density <- kde2d(X,Y,h=1050, n = 500, lims = c(range(X), range(Y)))
print(Density)
map_rbnb_density <- image(Density$x, Density$y, Density$z)

## Densité de AirBNB
intersection_rbnb$geometry
x <- st_coordinates(intersection_rbnb)
X <- x[,1]
Y <- x[,2]
Density <- kde2d(X,Y, n = 500, lims = c(range(X), range(Y)))
print(Density)
map_rbnb_density <- image(Density$x, Density$y, Density$z)


## Densité de Monuments
pts_monu$geometry
x_monu <- st_coordinates(pts_monu)
X_monu <- x_monu[,1]
Y_monu <- x_monu[,2]
Density_monu <- kde2d(X_monu,Y_monu, h=950, n = 500, lims = c(range(X), range(Y)))
print(Density_monu)
map_monu_density <- image(Density_monu$x, Density_monu$y, Density_monu$z)

## Densité de Métro
pts_metro$geometry
x_metro <- st_coordinates(pts_metro)
X_metro <- x_metro[,1]
Y_metro <- x_metro[,2]
Density_metro <- kde2d(X_metro,Y_metro,h=1300,n = 500,lims = c(range(X), range(Y)))
print(Density_metro)
map_metro_density <- image(Density_metro$x, Density_metro$y, Density_metro$z)

## Densité de Métro
pts_metro$geometry
x_imetro <- st_coordinates(intersection_metro)
X_imetro <- x_imetro[,1]
Y_imetro <- x_imetro[,2]
Density_imetro <- kde2d(X_imetro,Y_imetro,h=1300,n = 500,lims = c(range(X), range(Y)))
print(Density_imetro)
map_metro_density <- image(Density_imetro$x, Density_imetro$y, Density_imetro$z)

## Densité de RER dans Paris

x_irer <- st_coordinates(intersection_rer)
X_irer <- x_irer[,1]
Y_irer <- x_irer[,2]
Density_irer <- kde2d(X_irer,Y_irer,h=2100,n = 500,lims = c(range(X), range(Y)))
print(Density_irer)
map_rer_density <- image(Density_irer$x, Density_irer$y, Density_irer$z)

## Fonction de calcul de corrélation entre Densité d'entité

correl_matrice <- function(mat1,mat2){
  z1  <- mat1$z - mean(mat1$z)
  z2 <- mat2$z - mean(mat2$z)
  Z <-  z1*z2

  correl <- mean(Z)/(sd(z1)*sd(z2))

  print(correl)
}

c <-correl_matrice(Density,Density_metro)
c <-correl_matrice(Density,Density_imetro)## Cor = 0.7550
c <-correl_matrice(Density,Density_irer)## Cor = 0.3699
c <-correl_matrice(Density,Density_monu)## Cor = 0.4174
c <-correl_matrice(Density_monu,Density_metro)## Cor = 0.666

## Validation de la corrélation
## Vérification de la robustesse de cette corrélation
#BOOTSTRAP
## AirBNB - Métro :
B = rep(0,100)
for(i in 1:100){
  index_1 = sample(1:length(X_imetro),length(X_imetro),replace = TRUE)
  index_2 = sample(1:length(X),length(X),replace = TRUE)
  D1 <- kde2d(X_imetro[index_1],Y_imetro[index_1], h = 1300,n = 500,lims = c(range(X), range(Y)))
  D2 <- kde2d(X[index_2],Y[index_2], h=1050, n = 500,lims = c(range(X), range(Y)))
  B[i] <- correl_matrice(D1,D2)
}
incertitude <- sd(B) ## Corrélation de (37 +/- 3)%
median(B)
B0 <- data.frame(B)
ggplot(B0,aes(x=B)) + 
  geom_histogram( colour="black", fill="white",binwidth=0.01) 
  #geom_density(alpha=0.5, fill="#FF6666")
incertitude

## AirBNB - RER :
B_rer = rep(0,50)
for(i in 1:50){
  index_1 = sample(1:length(X_irer),length(X_irer),replace = TRUE)
  index_2 = sample(1:length(X),length(X),replace = TRUE)
  D1 <- kde2d(X_irer[index_1],Y_irer[index_1], h = 2300,n = 500,lims = c(range(X), range(Y)))
  D2 <- kde2d(X[index_2],Y[index_2], h=1050, n = 500,lims = c(range(X), range(Y)))
  B_rer[i] <- correl_matrice(D1,D2)
}
incertitude_rer <- sd(B_rer) ## Corrélation de (75.51 +/- 4)%
median(B_rer)
B0_rer <- data.frame(B_rer)
ggplot(B0_rer,aes(x=B_rer)) + 
  geom_histogram( colour="black", fill="white",binwidth=0.01) 
#geom_density(alpha=0.5, fill="#FF6666")
incertitude
## AirBNB - Monuments :
B_monu = rep(0,100)
for(i in 1:100){
  index_1 = sample(1:length(X_monu),length(X_monu),replace = TRUE)
  index_2 = sample(1:length(X),length(X),replace = TRUE)
  D1 <- kde2d(X_monu[index_1],Y_monu[index_1], h = 1350,n = 500,lims = c(range(X), range(Y)))
  D2 <- kde2d(X[index_2],Y[index_2], h=1050, n = 500,lims = c(range(X), range(Y)))
  B_monu[i] <- correl_matrice(D1,D2)
}
incertitude_monu <- sd(B_monu) ## corrélation : (41.74 +/- 3)%
incertitude_monu

## Comparaison avec une répartition aléatoire 

B1 = rep(0,100)
for(i in 1:100){
  x_al <- sample(min(X):max(X),length(X_metro))
  y_al <- sample(min(Y):max(Y),length(Y_metro))
  #index_1 = sample(1:length(X_imetro),length(X_imetro),replace = TRUE)
  index_2 = sample(1:length(X),length(X),replace = TRUE)
  D1 <- kde2d(x_al,y_al, h = 1300,n = 500,lims = c(range(X), range(Y)))
  #D2 <- kde2d(X,Y, h=1050, n = 500,lims = c(range(X), range(Y)))
  B1[i] <- correl_matrice(D1,Density)
}
hist(B1)
B1 <- data.frame(B1)
ggplot(B1,aes(x=B1)) + 
  #geom_histogram(aes(y=..density..), colour="black", fill="white",binwidth=0.01) +
  geom_density(alpha=0.5, fill="#FF6666") + ggtitle(("Distribution des corrélations de densité avec les AirBNB pour des répartition de points aléatoire sur Paris"))


### ALGO CLUSTERING ###
### PACKAGES
### K-Means

install.packages("fpc")
library(fpc)
install.packages("dbscan")
library(dbscan)

### DBSCAN
set.seed(123)

df <-  data.frame(pts_2154$X,pts_2154$Y)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
print(db)
# Plot DBSCAN results
plot(db, df, main = "DBSCAN", frame = FALSE)





db$cluster[sample(1:1089, 50)]

data("iris")
iris <- as.matrix(iris[, 1:4])
dbscan::kNNdistplot(iris, k =  4)
abline(h = 0.4, lty = 2)

set.seed(123)
# fpc package
res.fpc <- fpc::dbscan(df, eps = 0.4, MinPts = 4)
res.fpc <- fpc::dbscan(iris, eps = 0.4, MinPts = 4)
# dbscan package
res.db <- dbscan::dbscan(iris, 0.4, 4)

all(res.fpc$cluster == res.db)

install.packages("factoextra")
fviz_cluster(res.fpc, df, geom = "point")

library("factoextra")
fviz_cluster(db, df, stand = FALSE, geom = "point")








### Stats par arrondissement 


Luxembourg_apt <-  pts_2154[pts_2154$neighbourhood %in% c("Luxembourg"), ]
Luxembourg_apt$prices <- as.numeric(Luxembourg_apt$price)
x <- st_coordinates(Luxembourg_apt)
Luxembourg_apt$X <- x[,1]
Luxembourg_apt$Y <- x[,2]
d <- get_dist(x, method = "euclidean", stand = FALSE)
fviz_dist(d,order = TRUE,
          show_labels = TRUE,
          lab_size = NULL,
          gradient = list(low = "red", mid = "white", high = "blue")
)

#wmean$x = sum(Luxembourg_apt$prices*Luxembourg_apt$X)/sum(Luxembourg_apt$prices)
#wmean$y = sum(Luxembourg_apt$prices*Luxembourg_apt$Y)/sum(Luxembourg_apt$prices)
mean_x <- mean(Luxembourg_apt$X)
mean_y <- mean(Luxembourg_apt$Y)
wmeanx = sum(Luxembourg_apt$prices*Luxembourg_apt$X)/sum(Luxembourg_apt$prices)
wmeany = sum(Luxembourg_apt$prices*Luxembourg_apt$Y)/sum(Luxembourg_apt$prices)

plot(Luxembourg_apt$X,Luxembourg_apt$Y);points(wmeanx,wmeany,col='red');points(mean_x,mean_y,col='blue')

#plot map de densité airbnb
ggplot(data = Luxembourg_apt, aes(x = Luxembourg_apt$X,y=Luxembourg_apt$Y)) + geom_sf(aes(fill = prices))

## Kernel Density estimation
x_Lux <-  st_coordinates(Luxembourg_apt)
X_Lux = x_Lux[,1]
Y_Lux = x_Lux[,2]
library("MASS")
D_Lux <- kde2d(X_Lux,Y_Lux, h =200,n = 500,lims = c(range(X_Lux), range(Y_Lux)))
D_metro_Lux <-  kde2d(st_coordinates(Lux_metro)[,1],st_coordinates(Lux_metro)[,2], h =400,n = 500,lims = c(range(X_Lux), range(Y_Lux)))
D_monu_Lux <-  kde2d(st_coordinates(Lux_monu)[,1],st_coordinates(Lux_monu)[,2], h =400,n = 500,lims = c(range(X_Lux), range(Y_Lux)))

image(D_monu_Lux$x,D_monu_Lux$y,D_monu_Lux$z)
image(D_metro_Lux$x,D_metro_Lux$y,D_metro_Lux$z)
image(D_Lux$x,D_Lux$y,D_Lux$z)

c <-correl_matrice(D_Lux,D_metro_Lux)
c <-correl_matrice(D_Lux,D_monu_Lux)

arr <-st_transform(arr,st_crs(pts_metro))
Lux_metro <- st_intersection(arr[arr$l_aroff == "Luxembourg",],pts_metro)
Lux_monu <- st_intersection(arr[arr$l_aroff == "Luxembourg",],pts_monu)
plot(st_coordinates(Lux_monu))

g=ggplot()
g+geom_point(aes(x = wmeanx,y =wmeany, col ='red'))
g+geom_point(aes(x = mean_x,y =mean_y, col= 'blue'))
g + geom_polygon(data=Luxembourg_apt, aes(x = Y, y=X, fill=room_type))
a <-  st_coordinates(arr)

## distance type pondéré
## mesure de dispersion autour du barycentre
sigma_Luxembourg = (sum((Luxembourg_apt$prices)*((Luxembourg_apt$X-wmeanx)^2+(Luxembourg_apt$Y-wmeany)^2))/sum(Luxembourg_apt$prices))^(1/2)

## DBSCAN
df <-  data.frame(Luxembourg_apt$X,Luxembourg_apt$Y,Luxembourg_apt$prices)
plot(df)
db <- fpc::dbscan(df, eps = 70, MinPts = 10)
fviz_cluster(db, df, geom = "point") + labs(x="Est",y ='Nord') + ggtitle("DBSCAN classification (eps = 70, Minpts = 10) de quartier à l'intérieur de l'arrondissement Luxembourg")
## K-MEANS
km.res <- kmeans(df, 4, nstart = 555)
fviz_cluster(km.res, df, ellipse = FALSE, geom = "point") + labs(x="Est",y ='Nord') + ggtitle("K-Means classification (n = 4, Minpts = 10) de quartier à l'intérieur de l'arrondissement Luxembourg")
## EM ??

em.res <- em()

hist(Luxembourg_apt$prices)
boxplot(Luxembourg_apt$prices)

Louvre_apt <-  pts_2154[pts_2154$neighbourhood %in% c("Louvre"), ]
Louvre_apt$prices <- as.numeric(Louvre_apt$price)

df <-  data.frame(Louvre_apt$X,Louvre_apt$Y)
db <- fpc::dbscan(df, eps = 75, MinPts = 10)
fviz_cluster(db, df,ellipse = FALSE, geom = "point")

## ACP
mat <- data.frame(Luxembourg_apt$X,Luxembourg_apt$Y,Luxembourg_apt$prices,as.numeric(Luxembourg_apt$number_of_reviews),as.numeric(Luxembourg_apt$minimum_nights))
mat <- data.frame(Luxembourg_apt$X,Luxembourg_apt$Y,Luxembourg_apt$prices)
mat <- data.frame(quarter_paris$DeltaQ,quarter_paris$median_price,quarter_paris$surface, quarter_paris$classe1,quarter_paris$classe0)
res <- prcomp(mat, center = TRUE, scale = TRUE)
fviz_screeplot(res, addlabels = TRUE)

hist(as.numeric(Louvre_apt$minimum_nights))
boxplot(Louvre_prices)

Hdv_apt <-  pts_2154[pts_2154$neighbourhood %in% c("Hôtel-de-Ville"), ]
Hdv_apt$prices <- as.numeric(Hdv_apt$price)
Hdv_apt_abordable <- Hdv_apt[Hdv_apt$prices < 1000,]
df <-  data.frame(Hdv_apt$X,Hdv_apt$Y,Hdv_apt$prices)
db <- fpc::dbscan(df, eps = 75, MinPts = 10)
fviz_cluster(db, df,ellipse = FALSE, geom = "point")
hist(Hdv_apt_abordable$prices)

Gobelins_apt <-  rbnb[rbnb$neighbourhood %in% c("Gobelins"), ]
Gobelins_apt$prices <- as.numeric(Gobelins_apt$price)
hist(Gobelins_apt$prices)

Entrepot_apt <-  rbnb[rbnb$neighbourhood %in% c("Entrepôt"), ]
Entrepot_apt$prices <- as.numeric(Entrepot_apt$price)
hist(Entrepot_apt$prices)

Elysee_apt <-  rbnb[rbnb$neighbourhood %in% c("Élysée"), ]
Elysee_apt$prices <- as.numeric(Elysee_apt$price)
hist(Elysee_apt$prices)

for(i in arr$l_aroff){
  intersection_i = st_intersection(arr$i,pts_2154) 
}


## Krigeage private rooms 