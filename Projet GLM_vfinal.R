# Projet Modèle Lineaire Generalise _ Nathalie Ung


################################################################################
#-1.Librairies------------------------------------------------------------------
################################################################################
library(MASS)
library(car)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(lares)
library(gridExtra)
library(tidyverse)
library(knitr)
library("FactoMineR")
library("factoextra")

################################################################################
#-2.Chargement des donnees------------------------------------------------------
################################################################################

meteo_train = read.table("meteo.train.csv", sep=",",header = TRUE,
                         na.strings = "NA")
meteo_test = read.table("meteo.test.csv", sep=",",header = TRUE,
                        na.strings = "NA")


################################################################################
#-3.Traitement des donnees------------------------------------------------------
################################################################################


#dimension de l'ensemble d'entrainement
dim(meteo_train)

colnames(meteo_train)
summary(meteo_train)

#Vérifier s'il y a des valeurs manquantes dans les ensembles de données
sum(is.na(meteo_train))
sum(is.na(meteo_test))

#Verification du type de chaque variable
str(meteo_train)

head(meteo_train)

#Renommer lees variables
colnames(meteo_train)=c("X","Year","Month","Day","Hour","Minute","Temperature",
                        "Humidity","Sea.Level","Precipitation","Snowfall",
                        "Cloud.Cover.Total","Cloud.Cover.High",
                        "Cloud.Cover.Medium","Cloud.Cover.Low","Sunshine.Duration",
                        "Shortwave.Radiation","Wind.Speed.10m","Wind.Direction.10m",
                        "Wind.Speed.80m","Wind.Direction.80m","Wind.Speed.900mb",
                        "Wind.Direction.900mb","Wind.Gust.","Temperature.max",
                        "Temperature.min","Humidity.max","Humidity.min",
                        "Sea.Level.max","Sea.Level.min","Cloud.Cover.Total.max",
                        "Cloud.Cover.Total.min","Cloud.Cover.High.max",
                        "Cloud.Cover.High.min","Cloud.Cover.Medium.max",
                        "Cloud.Cover.Medium.min","Cloud.Cover.Low.max",
                        "Cloud.Cover.Low.min","Wind.Speed.max.10m",
                        "Wind.Speed.min.10m","Wind.Speed.max.80m",
                        "Wind.Speed.min.80m","Wind.Speed.max.900mb",
                        "Wind.Speed.min.900mb","Wind.Gust.max","Wind.Gust.min",
                        "pluie.demain")

meteo_train=meteo_train[,-1]
attach(meteo_train)



#Transformation de la variable: FALSE par 0 et TRUE par 1
meteo_train$pluie.demain=as.integer(as.logical(meteo_train$pluie.demain))

#meteo_train=meteo_train[,-1]
#meteo_test=meteo_test[,-1]


str(meteo_train)


################################################################################
#-4.Analyse descriptive---------------------------------------------------------
################################################################################

#Nombre d'occurence de pluie ou de non pluie
freq.pluie=data.frame(cbind(table(pluie.demain),round(prop.table(table(select(meteo_train, pluie.demain), 
                             exclude = NULL)), 2)))
colnames(freq.pluie)=c("Fréquence","Pourcentage")
kable(freq.pluie)
#Proportion de pluie.demain dans les ensembles d'entrainement 
summary(meteo_train)

#Type de chacune des variables explicatives
str(meteo_train)



##--------Boxplot---------------------------------------------------------------

#ggplot(meteo_train, aes(x=factor(pluie.demain), y=Cloud.Cover.Total)) +geom_boxplot()


par(mfrow=c(3,4))

boxplot(Cloud.Cover.Total~pluie.demain,cex.lab=0.7,horizontal = TRUE,col="lightblue")
boxplot(Cloud.Cover.High~pluie.demain,cex.lab=0.7,horizontal = TRUE,col="pink")
boxplot(Cloud.Cover.Medium~pluie.demain,cex.lab=0.7,horizontal = TRUE,col="darkgreen")
boxplot(Cloud.Cover.Low~pluie.demain,cex.lab=0.7,horizontal = TRUE,col="orange")

boxplot(Cloud.Cover.Total.max~pluie.demain,cex.lab=0.7,horizontal = TRUE,col="lightblue")
boxplot(Cloud.Cover.High.max~pluie.demain,cex.lab=0.7,horizontal = TRUE,col="pink")
boxplot(Cloud.Cover.Medium.max~pluie.demain,cex.lab=0.7,horizontal = TRUE,col="darkgreen")
boxplot(Cloud.Cover.Low.max~pluie.demain,cex.lab=0.7,horizontal = TRUE,col="orange")

boxplot(Cloud.Cover.Total.min~pluie.demain,cex.lab=0.7,horizontal = TRUE,col="lightblue")
boxplot(Cloud.Cover.High.min~pluie.demain,cex.lab=0.7,horizontal = TRUE,col="pink")
boxplot(Cloud.Cover.Medium.min~pluie.demain,cex.lab=0.7,horizontal = TRUE,col="darkgreen")
boxplot(Cloud.Cover.Low.min~pluie.demain,cex.lab=0.7,horizontal = TRUE,col="orange")

#Boxplot temperature
par(mfrow=c(1,3))
boxplot(Temperature ~pluie.demain, cex.lab=0.7,horizontal = TRUE)
boxplot(Temperature.max ~pluie.demain,cex.lab=0.7,horizontal = TRUE)
boxplot(Temperature.min ~pluie.demain,cex.lab=0.7,horizontal = TRUE)

#Boxplot humidite
boxplot(Humidity~pluie.demain,cex.lab=0.7,horizontal = TRUE)
boxplot(Humidity.max~pluie.demain,cex.lab=0.7,horizontal = TRUE)
boxplot(Humidity.min~pluie.demain,cex.lab=0.7,horizontal = TRUE)

#Boxplot de la mer
boxplot(Sea.Level ~pluie.demain,cex.lab=0.7,horizontal = TRUE,color="darkblue")
boxplot(Sea.Level.max ~pluie.demain,cex.lab=0.7,horizontal = TRUE,color="darkblue")
boxplot(Sea.Level.min ~pluie.demain,cex.lab=0.7,horizontal = TRUE,color="darkblue")

#Par nature: vent
par(mfrow=c(3,3))
boxplot(Wind.Speed.10m ~pluie.demain, cex.lab=0.7,horizontal = TRUE,color="coral")
boxplot(Wind.Speed.80m ~pluie.demain, cex.lab=0.7,horizontal = TRUE,color="aquamarine")
boxplot(Wind.Speed.900mb ~pluie.demain, cex.lab=0.7,horizontal = TRUE,color="azure")

boxplot(Wind.Speed.min.10m ~pluie.demain, cex.lab=0.7,horizontal = TRUE,color="coral")
boxplot(Wind.Speed.min.80m ~pluie.demain, cex.lab=0.7,horizontal = TRUE,color="aquamarine")
boxplot(Wind.Speed.min.900mb ~pluie.demain, cex.lab=0.7,horizontal = TRUE,color="azure")

boxplot(Wind.Speed.max.10m ~pluie.demain, cex.lab=0.7,horizontal = TRUE,color="coral")
boxplot(Wind.Speed.max.80m ~pluie.demain, cex.lab=0.7,horizontal = TRUE,color="aquamarine")
boxplot(Wind.Speed.max.900mb ~pluie.demain, cex.lab=0.7,horizontal = TRUE,color="azure")

#nature vent: rafale
par(mfrow=c(1,3))

boxplot(Wind.Direction.10m ~pluie.demain, cex.lab=0.7,horizontal = TRUE, color="cornflowerblue")
boxplot(Wind.Direction.80m ~pluie.demain, cex.lab=0.7,horizontal = TRUE, color="cornflowerblue")
boxplot(Wind.Direction.900mb ~pluie.demain, cex.lab=0.7,horizontal = TRUE, color="cornflowerblue")

#nature vent: direction 
boxplot(Wind.Gust.~pluie.demain, cex.lab=0.7,horizontal = TRUE,color="deepskyblue")
boxplot(Wind.Gust.max~pluie.demain, cex.lab=0.7,horizontal = TRUE,color="deepskyblue")
boxplot(Wind.Gust.min~pluie.demain, cex.lab=0.7,horizontal = TRUE,color="deepskyblue")



par(mfrow=c(2,3))
boxplot(Precipitation~pluie.demain,cex.lab=0.7,horizontal = TRUE)
boxplot(Snowfall~pluie.demain,cex.lab=0.7,horizontal = TRUE)
boxplot(Sunshine.Duration~pluie.demain,cex.lab=0.7,horizontal = TRUE)
boxplot(Shortwave.Radiation~pluie.demain,cex.lab=0.7,horizontal = TRUE)
boxplot(Month~pluie.demain,cex.lab=0.7,horizontal = TRUE)
boxplot(Year~pluie.demain,cex.lab=0.7,horizontal = TRUE)
par(mfrow=c(1,1))



##-----Correlation entre les variables -----------------------------------------

##Tableau de corrélation
cor_var=cor(meteo_train[, 6:46])
round(cor_var,2)


##Représentation matrice de correlation
corrplot(cor_var,type="upper",tl.cex=0.5)



# seuil de correlation 
cor_seuil = 0.9
# Indices des paires de variables ayant une correlation sup ou égale à 90%
corr_sup=which(abs(cor_var)>=cor_seuil & row(cor_var)<col(cor_var),
               arr.ind=TRUE)
# Extraction des paires de variables et de la valeur de leurs correlations
corr_paires = cbind(rownames(cor_var)[corr_sup[, 1]], 
                    colnames(cor_var)[corr_sup[, 2]], 
                    round(cor_var[corr_sup],2))
colnames(corr_paires)=c("Variable","Variable","Corrélation")
#tableau des paires de variables avec une correlation sup ou égale à90%
kable(corr_paires)



##------2.3. ACP --------------------------------------------------------------------

#ACP normée
meteo_pca = PCA(meteo_train[,7:46],scale.unit = TRUE, graph = FALSE)


#Valeurs propres: 
##64,1% de variance expliquée en 4 dimensions  
##73,5% expliqués en 5 dimensions
meteo_pca$eig
fviz_eig(meteo_pca, addlabels = TRUE, 
         main = "Représentation des valeurs propres",
         ylab="Variances expliquées (en %)")

#Le top 20 des variables les plus corrélées avec les dimensions 
var=get_pca_var(meteo_pca) 
round(head(var$coord, 20),2)

# correlation des var Dimensions 1 et 2 
meteo_corr1.2=dimdesc(meteo_pca,axes = c(1,2))
meteo_corr1.2


# correlation des var Dimensions 3 et 4
meteo_corr3.4=dimdesc(meteo_pca,axes = c(3,4))
meteo_corr3.4




# Top 20 des variables contributrices sur les dimensions 1 et 2
contrib_var1.2=fviz_pca_var(meteo_pca, col.var = "contrib",
                         gradient.cols = c("blue", "yellow", "red"), 
                         repel = TRUE, 
                         title="Contribution des variables dimensions 1 et 2",
                         legend.title = "Contribution",
                         select.var = list(contrib = 20)
                         )
contrib_var1.2
#Autre vision de la contribution des variables sur dim1 puis sur dim2
fviz_contrib(meteo_pca, choice = "var", axes = 1, top = 20)
fviz_contrib(meteo_pca, choice = "var", axes = 2, top = 20)
fviz_contrib(meteo_pca, choice = "var", axes = 1:2, top = 20)


# Top 20 des variables contributrices sur les dimensions 3 et 4 
contrib_var3.4=fviz_pca_var(meteo_pca, col.var = "contrib",
                            gradient.cols = c("blue", "yellow", "red"), 
                            repel = TRUE, 
                            title="Contribution des variables dimensions 1 et 2",
                            legend.title = "Contribution",
                            select.var = list(contrib = 20),axes = c(3,4))
contrib_var3.4

# Contributions des variables selon axes

fviz_contrib(meteo_pca, choice = "var", axes = 3, top = 20)
fviz_contrib(meteo_pca, choice = "var", axes = 4, top = 20)
fviz_contrib(meteo_pca, choice = "var", axes = 3:4, top = 20)




################################################################################
#-5.Modélisation---------------------------------------------------------
################################################################################


# holdout aléatoire
trainset = sample(c(T, F), nrow(meteo_train), replace = TRUE, prob = c(.8, .2))

taille=cbind(sum(trainset), length(pluie.demain)-sum(trainset))
colnames(taille)=c("trainset","valset")
kable(taille)

##--------3.1. Identification des colinéarités ---------------------------------

#Analyse de la colinéarité des variables liées à la Température
modele3=glm(pluie.demain~ Temperature+Temperature.max+Temperature.min,
            data=meteo_train[trainset, ],
            family = "binomial")

summary(modele3)

vif_modele3=data.frame(vif(modele3))
corr_pluie_temp=cor(meteo_train[,c("pluie.demain","Temperature","Temperature.max",
                                   "Temperature.min")])
vif_cor_mod3=cbind(round(vif_modele3,2),round(corr_pluie_temp[-1,1],2))

colnames(vif_cor_mod3)=c("VIF","Correlation avec pluie.demain")
kable(vif_cor_mod3)

#Analyse de la colinéarité des variables liées à la pression au niveau de la mer
modele4=glm(pluie.demain~ Sea.Level+Sea.Level.max+Sea.Level.min,
            data=meteo_train,family = "binomial")

summary(modele4)


vif_modele4=data.frame(vif(modele4))
corr_pluie_sea=cor(meteo_train[,c("pluie.demain","Sea.Level","Sea.Level.max",
                                  "Sea.Level.min")])
vif_cor_mod4=cbind(vif_modele4,round(corr_pluie_sea[-1,1],2))
colnames(vif_cor_mod4)=c("VIF","Correlation avec pluie.demain")
kable(vif_cor_mod4)

#Analyse de la colinéarité des variables liées à la couverture nuageuse

modele5=glm(pluie.demain~ Cloud.Cover.Total+Cloud.Cover.Low+
              Cloud.Cover.Medium+Cloud.Cover.Medium.max+
              Cloud.Cover.High+
              Cloud.Cover.High.max,
            data=meteo_train[trainset,],family = "binomial")

summary(modele5)

vif_modele5=data.frame(vif(modele5))
corr_pluie_cloud=cor(meteo_train[,c("pluie.demain","Cloud.Cover.Total",
                                    "Cloud.Cover.Low","Cloud.Cover.Medium",
                                    "Cloud.Cover.Medium.max",
                                    "Cloud.Cover.High",
                                    "Cloud.Cover.High.max"
)])

vif_cor_mod5=cbind(vif_modele5,round(corr_pluie_cloud[-1,1],2))
colnames(vif_cor_mod5)=c("VIF","Correlation avec pluie.demain")
kable(vif_cor_mod5)


##oMdele 6: avec les variables liées à la couverture nuageuse 1
modele6=glm(pluie.demain~ Cloud.Cover.Medium.max+Cloud.Cover.High.max,
            data=meteo_train,family = "binomial")

summary(modele6)

vif_modele6=data.frame(vif(modele6))
vif_modele6


modele7=glm(pluie.demain~ Cloud.Cover.Low.max+Cloud.Cover.Total.max,
            data=meteo_train[trainset,],family = "binomial")

summary(modele7)

vif_modele7=data.frame(vif(modele7))
vif_modele7

#Analyse de la colinéarité des variables liées à la direction du vent
modele8=glm(pluie.demain~ Wind.Direction.10m+Wind.Direction.80m+
              Wind.Direction.900mb,
            data=meteo_train[trainset, ],family = "binomial")

summary(modele8)

vif_modele8=data.frame(vif(modele8))
corr_pluie_wind.dir=cor(meteo_train[,c("pluie.demain","Wind.Direction.10m",
                                       "Wind.Direction.80m","Wind.Direction.900mb"
                                       
)])

vif_cor_mod8=cbind(vif_modele8,round(corr_pluie_wind.dir[-1,1],2))
colnames(vif_cor_mod8)=c("VIF","Correlation avec pluie.demain")
kable(vif_cor_mod8)


#Analyse de la colinéarité entre les variables liées à la direction du vent et les rafales de vent
modele9.1= glm(pluie.demain~Wind.Direction.80m+Wind.Gust.,
               data=meteo_train[trainset, ],family = "binomial")

summary(modele9.1)
vif(modele9.1)


modele9.2= glm(pluie.demain~Wind.Direction.10m+Wind.Gust.,
               data=meteo_train[trainset, ],family = "binomial")
vif(modele9.2)


#Analyse de la colinéarité entre les variables liées à la vitesse du vent
modele9.3= glm(pluie.demain~Wind.Speed.10m+Wind.Speed.max.10m+Wind.Speed.max.80m,
               data=meteo_train[trainset, ],family = "binomial")
vif_modele9.3=vif(modele9.3)
corr_pluie_windsd1=cor(meteo_train[,c("pluie.demain","Wind.Speed.10m",
                                      "Wind.Speed.max.10m","Wind.Speed.max.80m" )])

vif_cor_mod9.3=cbind(vif_modele9.3,round(corr_pluie_windsd1[-1,1],2))
colnames(vif_cor_mod9.3)=c("VIF","Correlation avec pluie.demain")
kable(vif_cor_mod9.3)


modele9.4= glm(pluie.demain~Wind.Speed.min.10m+Wind.Speed.min.80m,
               data=meteo_train[trainset, ],family = "binomial")

vif_modele9.4=vif(modele9.4)
corr_pluie_windsd2=cor(meteo_train[,c("pluie.demain",
                                      "Wind.Speed.min.10m","Wind.Speed.min.80m" )])
vif_cor_mod9.4=cbind(vif_modele9.4,round(corr_pluie_windsd2[-1,1],2))
colnames(vif_cor_mod9.4)=c("VIF","Correlation avec pluie.demain")
kable(vif_cor_mod9.4)



modele9.6=glm(pluie.demain~ Wind.Speed.min.900mb+Wind.Speed.900mb,
              data=meteo_train[trainset,],family = "binomial")

vif_modele9.6=vif(modele9.6)
corr_pluie_windsd3=cor(meteo_train[,c("pluie.demain",
                                      "Wind.Speed.min.900mb","Wind.Speed.900mb")])
vif_cor_mod9.6=cbind(vif_modele9.6,round(corr_pluie_windsd3[-1,1],2))

colnames(vif_cor_mod9.6)=c("VIF","Correlation avec pluie.demain")
kable(vif_cor_mod9.6)


modele9.5=glm(pluie.demain~ Wind.Speed.max.10m+Wind.Gust.max,
              data=meteo_train,family = "binomial")

vif_modele9.5=vif(modele9.5)
vif_modele9.5


#Analyse de la colinéarité des variables liées à l'humidité

modele11=glm(pluie.demain~ Humidity+Humidity.min+Humidity.max,
             data=meteo_train[trainset, ],family = "binomial")

vif_modele11=data.frame(vif(modele11))
corr_pluie_humid=cor(meteo_train[,c("pluie.demain","Humidity",
                                    "Humidity.min","Humidity.max")])

vif_cor_mod11=cbind(vif_modele11,round(corr_pluie_humid[-1,1],2))
colnames(vif_cor_mod11)=c("VIF","Correlation avec pluie.demain")
kable(vif_cor_mod11)


#------3.2. Ajustement des modèles----------------------------------------------



##--------Modele 1--------------------------------------------------------------
modele1=glm(pluie.demain~.-Year-Day-Hour-Minute,
            data=meteo_train[trainset, ],family = "binomial")

summary(modele1)




##-----Modele automatique: step forward-------------------------------------------------

modele.auto = step(glm(pluie.demain~.-Year-Day-Hour-Minute,
                       data=meteo_train[trainset, ],
                       family = "binomial"),direction = "backward")


summary(modele.auto)




##--------Modèle Stepwise both--------------------------------------------------

modele.both=step(glm(pluie.demain~1,data=meteo_train[trainset,]), 
                 pluie.demain ~ Month+Temperature+Sea.Level
                 +Snowfall+Cloud.Cover.Total+Cloud.Cover.High+Cloud.Cover.Medium
                 +Cloud.Cover.Low+Sunshine.Duration
                 +Wind.Speed.10m+Wind.Direction.10m+Wind.Speed.80m
                 +Wind.Direction.80m+Wind.Speed.900mb+Wind.Direction.900mb
                 +Wind.Gust.
                 +Sea.Level.max+Sea.Level.min+Cloud.Cover.Total.max
                 +Cloud.Cover.Total.min+Cloud.Cover.High.max+Cloud.Cover.High.min
                 +Cloud.Cover.Medium.max+Cloud.Cover.Medium.min+Cloud.Cover.Low.max
                 +Cloud.Cover.Low.min+Wind.Speed.max.10m+Wind.Speed.min.10m
                 +Wind.Speed.max.80m+Wind.Speed.min.80m+Wind.Speed.max.900mb
                 +Wind.Speed.min.900mb+Wind.Gust.max+Wind.Gust.min+Temperature.max
                 +Shortwave.Radiation
                 +Precipitation+Humidity+Humidity.max+Humidity.min+Temperature.min, 
                 data=meteo_train[trainset,],
                 direction="both")
summary(modele.both)


##--------Modele 2--------------------------------------------------------------
modele2=glm(pluie.demain ~ Temperature.min + Sea.Level.min 
            + Cloud.Cover.Total + Wind.Speed.min.10m
            +Wind.Speed.max.900mb,
            family = "binomial", data = meteo_train[trainset, ])
summary(modele2)



##--------Modele 12: step avec variables non colineaire-------------------------------------------------------------
modele12=glm(pluie.demain~ Temperature.min+Cloud.Cover.Total
             +Wind.Speed.max.900mb+Wind.Direction.900mb
             +Sea.Level.min,
             data=meteo_train[trainset, ],family = "binomial")


summary(modele12)
##--------Modele 13-------------------------------------------------------------
modele13=glm(pluie.demain~ Temperature.min
             +Cloud.Cover.Total+Cloud.Cover.Medium.max
             +Wind.Speed.max.900mb
             +Wind.Direction.900mb
             +Sea.Level.min,
             data=meteo_train[trainset, ],family = "binomial")
summary(modele13)

##--------Modele 14-------------------------------------------------------------
modele14=glm(pluie.demain~ Temperature.min
             +Cloud.Cover.Total+Cloud.Cover.Medium.max
             +Wind.Speed.max.80m
             +Wind.Direction.80m+Wind.Direction.900mb
             +Sea.Level.max,
             data=meteo_train[trainset, ],family = "binomial")
summary(modele14)



##--------modele15--------------------------------------------------------------
modele15=glm(pluie.demain~ Temperature.min
             +Cloud.Cover.Total+Cloud.Cover.Medium.max
             +Wind.Speed.max.80m
             +Wind.Direction.900mb
             +Sea.Level.min
             +Month:Temperature.min,
             data=meteo_train[trainset, ],family = "binomial")
summary(modele15)


##--------modele16--------------------------------------------------------------
modele16=glm(pluie.demain~ Temperature.max
             +Cloud.Cover.Total+Cloud.Cover.Medium.max
             +Wind.Speed.max.80m
             +Wind.Direction.900mb
             +Wind.Speed.max.80m:Wind.Direction.900mb
             +Sea.Level.min
             +Month:Temperature.min,
             data=meteo_train[trainset, ],family = "binomial")
summary(modele16)


##--------modele17--------------------------------------------------------------

modele17=glm(pluie.demain~ Temperature.max
             +Cloud.Cover.Total+Cloud.Cover.Medium.max
             +Wind.Speed.max.80m:Wind.Direction.900mb
             +Sea.Level.min
             +Month:Temperature.min,
             data=meteo_train[trainset, ],family = "binomial")
summary(modele17)


##--------modele18--------------------------------------------------------------
modele18=glm(pluie.demain~ Temperature.max
             +Cloud.Cover.Total+Cloud.Cover.Medium.max:Humidity.max
             +Wind.Speed.max.80m:Wind.Direction.900mb
             +Sea.Level.min
             +Month:Temperature.min,
             data=meteo_train[trainset, ],family = "binomial")
summary(modele18)


##--------modele19--------------------------------------------------------------
modele19=glm(pluie.demain~ Temperature.max
             +Cloud.Cover.Total*Cloud.Cover.Medium.max:Humidity.max
             +Wind.Speed.max.80m:Wind.Direction.900mb
             +Sea.Level.min
             +Month:Temperature.min,
             data=meteo_train[trainset, ],family = "binomial")
summary(modele19)






##------3.3. Comparaison des modèles-------------------------------------------------

nom.modele=c("Modèle auto backward","Modèle 17","Modèle 18","Modèle 19")
valeur.AIC=rbind(modele.auto$aic,modele17$aic,modele18$aic,modele19$aic)
mod.aic=cbind(nom.modele,valeur.AIC)
colnames(mod.aic)=c("Modèle","Valeur AIC")
kable(mod.aic)


################################################################################
#-6.Prediction------------------------------------------------------------------
################################################################################

#Fonction calculant les erreurs de prédictions
f.err=function(pred2){
  err.pred=mean(abs(pred2 - meteo_train[!trainset, "pluie.demain"]), 
                na.rm = T) 
  return(err.pred)
}


#Fonction permettant de calculer la matrice de confusion
f.confusion=function(pred2,seuil){
  confusion=table(meteo_train[!trainset, "pluie.demain"], pred2>seuil)
  return(confusion)
}


#Fonction utilisée pour évaluer la qualité du modèle
f.qualite=function(pred2){
  qualite=mean(pred2 == (meteo_train[!trainset, "pluie.demain"]=="1")) 
  return (qualite)
}







##--------Prediction du modele auto------------------------------------------------
pred.auto = predict(modele.auto, newdata=meteo_train[!trainset, ], 
                    type = "response")

seuil = seq(0, 1, by=.01)

res = rep(NA, length(seuil))
for(i in 1:length(seuil)){
  pred.auto.seuil = (pred.auto >= seuil[i])
  res[i] = sum(pred.auto.seuil & pluie.demain=="FALSE") + 
    sum(!pred.auto.seuil & pluie.demain=="TRUE")
}


plot(seuil, res, type="l", col="blue", 
     main="Détermination du seuil optimal", cex.main=0.7,cex.lab=0.7,cex.axis=0.7)

seuil[which.min(res)]

pred.auto2 = (pred.auto >= 0.31)

f.err(pred.auto2)

kable(f.confusion(pred.auto2,0.31))

#Classe en vrai si sup aux different seuils
pred.auto2.2 = (pred.auto >= 0.41)
pred.auto2.3 = (pred.auto >= 0.42)
pred.auto2.4 = (pred.auto >= 0.43)
pred.auto2.5 = (pred.auto >= 0.44)


#Qualite de prediction

seuil.test=rbind("0.31","0.41","0.42","0.43","0.44")

err.test=rbind(f.err(pred.auto2),f.err(pred.auto2.2),f.err(pred.auto2.3),
               f.err(pred.auto2.4),f.err(pred.auto2.5))
qualite.test=rbind(f.qualite(pred.auto2),
                   f.qualite(pred.auto2.2),
                   f.qualite(pred.auto2.3),
                   f.qualite(pred.auto2.4),
                   f.qualite(pred.auto2.5))


qualite.auto=cbind(seuil.test,round(err.test*100,2),round(qualite.test*100,2))
colnames(qualite.auto)=c("Seuil","Erreur de prédiction (%)", "Qualité de prédiction (%)")
kable(qualite.auto)


#kable(f.confusion(pred.auto2.3,0.45))

##--------Prediction du modele17------------------------------------------------

#Prédiction du modèle
pred17 = predict(modele17, newdata=meteo_train[!trainset, ], type = "response")

# Détection du seuil optimal
seuil17 = seq(0, 1, by=.01)

res17 = rep(NA, length(seuil17))
for(i in 1:length(seuil17)){
  pred.17.seuil = (pred17 >= seuil17[i])
  res17[i] = 1 * sum(pred.17.seuil & pluie.demain=="FALSE") + 
    sum(!pred.17.seuil & pluie.demain=="TRUE")
}

plot(seuil17, res17, type="l", col="blue", 
     main="Détermination du seuil optimal",
     cex.main=0.7,cex.lab=0.7,cex.axis=0.7)


seuil17[which.min(res17)]

#Classe en vrai si sup au seuil
pred17.2 = (pred17 >= 0.29)

#Erreur de prédiction du modèle
f.err(pred17.2)

#Matrice de confusion
f.confusion(pred17.2,0.29)

#Classe en vrai si sup au seuil
pred17.2.2 = (pred17 >= 0.32)
pred17.2.3= (pred17 >= 0.38)
pred17.2.4 = (pred17 >= 0.41)
pred17.2.5 = (pred17 >= 0.44)

#Qualite de prediction

seuil.17=rbind("0,29","0.32","0.38","0.41","0.44")

err.test17=rbind(f.err(pred17.2),
                 f.err(pred17.2.2),
                 f.err(pred17.2.3),
                 f.err(pred17.2.4),
                 f.err(pred17.2.5))


qualite.test17=rbind(f.qualite(pred17.2),
                     f.qualite(pred17.2.2),
                     f.qualite(pred17.2.3),
                     f.qualite(pred17.2.4),
                     f.qualite(pred17.2.5))

qualite.17=cbind(seuil.17,round(err.test17*100,2),round(qualite.test17*100,2))
colnames(qualite.17)=c("Seuil","Erreur de prédiction (%)","Qualité de prédiction (%)")
kable(qualite.17)


# Matrice de confusion
#f.confusion(pred17.2.2,0.38)

##--------Prediction du modele 18------------------------------------------------
pred18 = predict(modele18, newdata=meteo_train[!trainset, ], type = "response")

# Détection du seuil optimal
seuil18 = seq(0, 1, by=.01)

res18 = rep(NA, length(seuil18))
for(i in 1:length(seuil18)){
  pred.18.seuil = (pred18 >= seuil18[i])
  res18[i] = 1 * sum(pred.18.seuil & pluie.demain=="FALSE") + 
    sum(!pred.18.seuil & pluie.demain=="TRUE")
}

plot(seuil18, res18, type="l", col="blue", 
     main="Détermination du seuil optimal",
     cex.main=0.7,cex.lab=0.7,cex.axis=0.7)



seuil18[which.min(res18)]


pred18.2 = (pred18 >= 0.29)

#Erreur de prédiction du modèle
f.err(pred18.2)

f.confusion(pred18.2,0.29)

#Classe en vrai si sup au seuil
pred18.2.2 = (pred18 >= 0.29)
pred18.2.3 = (pred18>= 0.32)
pred18.2.4 = (pred18 >= 0.38)
pred18.2.5 = (pred18 >= 0.45)

#Qualite de prediction

seuil.18=rbind("0,29","0.3","0.32","0.38","0.45")

err.test18=rbind(f.err(pred18.2),
                 f.err(pred18.2.2),
                 f.err(pred18.2.3),
                 f.err(pred18.2.4),
                 f.err(pred18.2.5))

qualite.test18=rbind(f.qualite(pred18.2),
                     f.qualite(pred18.2.2),
                     f.qualite(pred18.2.3),
                     f.qualite(pred18.2.4),
                     f.qualite(pred18.2.5))

qualite.18=cbind(seuil.18,round(err.test18*100,2),round(qualite.test18*100,2))
colnames(qualite.18)=c("Seuil","Erreur de prédiction","Qualité de prédiction")
kable(qualite.18)




##--------Prediction du modele 19------------------------------------------------
pred19 = predict(modele19, newdata=meteo_train[!trainset, ], type = "response")


# Détection du seuil optimal
# Détection du seuil optimal
seuil19 = seq(0, 1, by=.01)

res19 = rep(NA, length(seuil19))
for(i in 1:length(seuil19)){
  pred.19.seuil = (pred19 >= seuil19[i])
  res19[i] = 1 * sum(pred.19.seuil & pluie.demain=="FALSE") + 
    sum(!pred.19.seuil & pluie.demain=="TRUE")
}

plot(seuil, res19, type="l", col="blue", 
     main="Détermination du seuil optimal",
     cex.main=0.7,cex.lab=0.7,cex.axis=0.7)

seuil19[which.min(res19)]

#Classe en vrai si sup au seuil
pred19.2 = (pred19 >= 0.21)

#Erreur de prédiction du modèle
f.err(pred19.2)

# Erreur 0-1
f.confusion(pred19.2,0.21)


#Classe en vrai si sup au seuil
pred19.2.2 = (pred19 >= 0.39)
pred19.2.3 = (pred19>= 0.4)
pred19.2.4 = (pred19 >= 0.41)
pred19.2.5 = (pred19 >= 0.42)

#Qualite de prediction

seuil.19=rbind("0.21","0.39","0.4","0.41","0.42")

err.test19=rbind(f.err(pred19.2),
                 f.err(pred19.2.2),
                 f.err(pred19.2.3),
                 f.err(pred19.2.4),
                 f.err(pred19.2.5))


qualite.test19=rbind(f.qualite(pred19.2),
                     f.qualite(pred19.2.2),
                     f.qualite(pred19.2.3),
                     f.qualite(pred19.2.4),
                     f.qualite(pred19.2.5))

qualite.19=cbind(seuil.19, round(err.test19*100,2),round(qualite.test19*100,2))
colnames(qualite.19)=c("Seuil","Erreur de présdiction (%)","Qualité de prédiction (%)")
kable(qualite.19)


##--------Comparaison des modèles-----------------------------------------------

modele.compar=c("Modèle auto","Modèle 17","Modèle 18","Modèle 19")
aic.compar=rbind(modele.auto$aic,modele17$aic,modele18$aic,modele19$aic)
seuil.compar=c("0.42","0.38","0.38","0.4")
err.compar=rbind(f.err(pred.auto2),f.err(pred17.2.3),f.err(pred18.2.4),f.err(pred19.2.2))
qualite.compar=rbind(f.qualite(pred.auto2),
                     f.qualite(pred17.2.3),
                     f.qualite(pred18.2.4),
                     f.qualite(pred19.2.2))
compar=cbind(modele.compar,round(aic.compar*100,2),round(err.compar*100,2),
             seuil.compar,round(qualite.compar*100,2))
colnames(compar)=c("Modèle","AIC","Erreur de Prédiction (%)", "Seuil de prédiction","Qualité de prédiction (%)")
kable(compar)

################################################################################
#-------------5.PREDICTION------------------------------------------------------
################################################################################


#renommer l'ensemble de test
colnames(meteo_test)=c("X","Year","Month","Day","Hour","Minute","Temperature",
                       "Humidity","Sea.Level","Precipitation","Snowfall",
                       "Cloud.Cover.Total","Cloud.Cover.High",
                       "Cloud.Cover.Medium","Cloud.Cover.Low","Sunshine.Duration",
                       "Shortwave.Radiation","Wind.Speed.10m","Wind.Direction.10m",
                       "Wind.Speed.80m","Wind.Direction.80m","Wind.Speed.900mb",
                       "Wind.Direction.900mb","Wind.Gust.","Temperature.max",
                       "Temperature.min","Humidity.max","Humidity.min",
                       "Sea.Level.max","Sea.Level.min","Cloud.Cover.Total.max",
                       "Cloud.Cover.Total.min","Cloud.Cover.High.max",
                       "Cloud.Cover.High.min","Cloud.Cover.Medium.max",
                       "Cloud.Cover.Medium.min","Cloud.Cover.Low.max",
                       "Cloud.Cover.Low.min","Wind.Speed.max.10m",
                       "Wind.Speed.min.10m","Wind.Speed.max.80m",
                       "Wind.Speed.min.80m","Wind.Speed.max.900mb",
                       "Wind.Speed.min.900mb","Wind.Gust.max","Wind.Gust.min")

meteo_test=meteo_test[,-1]
attach(meteo_test)

#Prediction sur l'ensemble de test
pred.finale = predict(modele17, newdata=meteo_test, type = "response")
#classification
pred.finale2 = (pred.finale >= 0.38)
head(pred.finale2)

#mis sous dataframe
prediction.meteo=as.data.frame(pred.finale2)
head(prediction.meteo)

#Generer le fichier csv avec les prédictions
write.csv(prediction.meteo, file = "C:\\Users\\do\\Documents\\Cours\\EMSB\\MODULE 2\\1.Modele lineaire generalisé\\2. Projet\\Prediction pluie NU.csv", row.names = TRUE)

