#Task 1 : Import Data
seed<-read.table(file=file.choose(),header=TRUE,sep=",",dec=".", na.strings="")
View(seed)  
str(seed)
dim(seed) #data dimension
summary(seed) #summarize attribute distributions
sapply(seed, class)# type of each attribute
#attach(seed)
sum(is.na(seed))
#Task 2 : data preprocessing :

#1) outliers : impute outliers with NA

boxplot(seed[,c('A','P','C','Lk','Wk','Ac','Lkg')])
outliers <- boxplot(seed$Ac, plot = FALSE)$out
outliers

seed[seed$Ac %in% outliers, "Ac"] = NA#imputer les valeurs abberantes par NA
outliers <- boxplot(seed$Ac, plot = FALSE)$out
View(seed)
sum(is.na(seed))

#2)missing value :
na.fail(seed) 
is.na(seed) #missing values
sum(is.na(seed)) 
colSums(is.na(seed))
taux = sum(is.na(seed))/prod(dim(seed)) #taux des valeurs manquants 0.0054

#imput missing values in C using mean function:
summary(seed$C)
seed$C[is.na(seed$C)] <- mean(seed$C, na.rm = TRUE)  

#imput missing values in Lk using mean function:
seed$Lk[is.na(seed$Lk)] <- mean(seed$Lk, na.rm = TRUE)
summary(seed$Lk)

#imput missing values in Ac using mean function:
seed$Ac[is.na(seed$Ac)] <- mean(seed$Ac, na.rm = TRUE) #na.rm = true missing values should not be used for the mean calculation 
summary(seed$Ac)

#imput missing values in varietie using random function:

sum(is.na(seed$varietie))
seed$varietie<-with(seed,impute(varietie,"random"))
View(seed)

#task 3 : Univariate analysis

#1)normality of quantitative variables
# utiliser le test shapiro : test statistique pour vérifier si une distribution
#suit un loi normal ou non
#hypothése : H0: l'echantillon suit une loi normal
            #H1: l'echantillon ne suit pas une loi normal

shapiro.test(seed$A) # on rejette H0 
shapiro.test(seed$P) # on rejette H0
shapiro.test(seed$C) # on rejette H0
shapiro.test(seed$Lk) # on rejette H0
shapiro.test(seed$Wk) # on rejette H0
shapiro.test(seed$Ac) # on accepte H0
shapiro.test(seed$Lkg) # on rejette H0

#On n a pas la normalité (p_value<0.05 donc on accepte H1=> On n a pas la normalité)

#on utilise lhistogramme pas de symetrie par rapport à la moyenne la normalité n est pas assuré(voir tp6 part2)

hist(seed$A, col="red")
hist(seed$P, col="green")
hist(seed$C, col="blue")
hist(seed$Lk, col="pink")
hist(seed$Wk, col="orange")
hist(seed$Ac, col="purple")#histogramme presque symétrique
hist(seed$Lkg, col="grey")
#hist(seed$varietie, col="grey")
#seed$varietie<-as.factor(numeric(seed$varietie))
View(seed)
#On remarque que l’histogramme est non sym ́etrique ce qui implique que la normalit ́e
#des donn ́ees n’est pas assur ́ee.
#2) modality 
unique(seed$varietie)
table(seed$varietie)
hist(seed$varietie, col="grey")
barchart(seed$varietie)
#on a trois modalités kama :53, rosa: 74, canadian: 58


#Task 4 : Bivariate analysis:

#mapcor=cor(seed, method = c("pearson", "kendall", "spearman"))
#install.packages("corrplot")
#library(corrplot)
#corrplot(mapcor, type = "upper", order = "hclust", 
#         tl.col = "black", tl.srt = 45)


# correlation of quantitative variables

var = c("A","P","C","Lk","Wk","Ac","Lkg")
plot(seed[, var])

#1) corr entre A et P :
plot(seed$A,seed$P) 
#la liaison lineaire entre la variable A et la variable P
#est claire, on a une forte liaison lineaire positive(varie dans le meme sens)

#validation : calcul de la coeff de corr
#vérifier la normalité : on a déjà verifier l'abscence de la normalité
#on utilise spearman
Rho1=cor(seed$A,seed$P,method = "spearman") # 0.9892 : on peut conclure un forte liaison entre les 2 variables
cor.test(seed$A,seed$P,method = "spearman")#=>p-value < 2.2e-16 on accepte alors H1 ce qui implique les deux variables sont bien corr ́el ́ee

#2) corr entre A et C
plot(seed$A,seed$C) #On remarque une liaison lineaire positive entre la variable A et la variable C
#vérifier la normalité : on a déjà verifier l'abscence de la normalité
#on utilise spearman
Rho2=cor(seed$A,seed$C,method = "spearman") #0.66 on peut conclure une forte liaison entre les deux variables.
cor.test(seed$A,seed$C,method = "spearman")#=>p-value < 2.2e-16 on accepte alors H1 ce qui implique les deux variables sont bien corr ́el ́e

#3) corr entre A et Lk
plot(seed$A,seed$Lk)
Rho3=cor(seed$A,seed$Lk,method = "spearman")
cor.test(seed$A,seed$Lk,method = "spearman")

#4) corr entre A et Wk
plot(seed$A,seed$Wk)
Rho4=cor(seed$A,seed$Wk,method = "spearman")

#5) corr entre A et Ac #pas de corr
plot(seed$A,seed$Ac)
Rho5=cor(seed$A,seed$Ac,method = "spearman")


#6) corr entre A et Lkg
plot(seed$A,seed$Lkg)
Rho6=cor(seed$A,seed$Lkg,method = "spearman")

#7) corr entre A et varietie : relation entre une var quant et une var qual 
#On est dans le cas de deux variables une quantitative A et une qualita-
#tive varietie, alors la représentation graphique pour d ́ecrire le lien entre les deux est
#par les boites `a moustaches

boxplot(seed$A~seed$varietie)
#On remarque la liaison entre les deux variables, en changant la modalit ́e de la variable
#varietie on remarque un changement au niveau des valeurs de r ́ef ́erence de la variable
#A. Alors il existe un effet des varietie sur A.

#utiliser un test 
#On cherche `a  ́etudier l’effet d’une variable qualitative sur une variable quantitative,
#le premier reflexe est d’appliquer un test d’analyse de la variance `a un facteur. Pour
#cela on doit tout d’abord v ́erifier la normalit ́e de chaque  ́echantillon !

tapply(seed$A,seed$varietie,shapiro.test)
#On a un  ́echantillont qui est  normalement distribu ́es et deux autres ne le sont pas.
#Alors la normalit ́e n’est pas assur ́ee par tous les  ́echantillons, on ne peut pas appliquer
#un test d’ANOVA. On applique un test d’hypoth`ese non param ́etrique ppour un cas
#de 3  ́echantillons( vent présenté 3  modalit ́es), alors c’est un test de Kruskal Wallis.
kruskal.test(seed$A~seed$varietie)

#p−value < 2.2e−16 < 0.05, on accepte H1 : les distributions des  ́echantillons ne sont
#pas les mˆemes, il existe alors une diff ́erence entre les diff ́erentes modalit ́es, donc l’effet
#est pr ́esent et par la suite on ne peut pas ignorer la relation entre les deux variables.

#1) corr entre P et C
plot(seed$P,seed$C)
PC=cor(seed$P,seed$C,method = "spearman")
cor.test(seed$P,seed$C,method = "spearman")

#2) corr entre P et Lk
plot(seed$P,seed$Lk)
PLk=cor(seed$P,seed$Lk,method = "spearman")
cor.test(seed$P,seed$Lk,method = "spearman")

#3) corr entre P et Wk
plot(seed$P,seed$Wk)
PWk=cor(seed$P,seed$Wk,method = "spearman")
cor.test(seed$P,seed$Wk,method = "spearman")

#4) corr entre P et Ac
plot(seed$P,seed$Ac)
PAC=cor(seed$P,seed$Ac,method = "spearman")
cor.test(seed$P,seed$Ac,method = "spearman") # pas de cor

#5) corr entre P et Lkg
plot(seed$P,seed$Lkg)
PLkg=cor(seed$P,seed$Lkg,method = "spearman")
cor.test(seed$P,seed$Lkg,method = "spearman")

#) corr entre P et varietie

boxplot(seed$P~seed$varietie)

#On remarque la liaison entre les deux variables, en changant la modalit ́e de la variable
#varietie on remarque un changement au niveau des valeurs de r ́ef ́erence de la variable
#A. Alors il existe un effet des varietie sur A.

tapply(seed$P,seed$varietie,shapiro.test) 
#On a un  ́echantillont qui est  normalement distribu ́es et deux autres ne le sont pas.
#Alors la normalit ́e n’est pas assur ́ee par tous les  ́echantillons, on ne peut pas appliquer
#un test d’ANOVA. On applique un test d’hypoth`ese non param ́etrique ppour un cas
#de 3  ́echantillons( vent présenté 3  modalit ́es), alors c’est un test de Kruskal Wallis.
kruskal.test(seed$P~seed$varietie)
#p−value < 2.2e−16 < 0.05, on accepte H1 : les distributions des  ́echantillons ne sont
#pas les mˆemes, il existe alors une diff ́erence entre les diff ́erentes modalit ́es, donc l’effet
#est pr ́esent et par la suite on ne peut pas ignorer la relation entre les deux variables.


#1) corr entre C et Lk
plot(seed$C,seed$Lk)
CLk=cor(seed$C,seed$Lk,method = "spearman")
cor.test(seed$C,seed$Lk,method = "spearman") # pas de correlation

#2) corr entre C et Wk
plot(seed$C,seed$Wk)
Cwk=cor(seed$C,seed$Wk,method = "spearman")
cor.test(seed$C,seed$Wk,method = "spearman")

#3) corr entre C et Ac
plot(seed$C,seed$Ac)
CAc=cor(seed$C,seed$Ac,method = "spearman")
cor.test(seed$C,seed$Ac,method = "spearman") #pas de correlation

#4) corr entre C et Lkg
plot(seed$C,seed$Lkg)
CLkg=cor(seed$C,seed$Lkg,method = "spearman")
cor.test(seed$C,seed$Lkg,method = "spearman")#pas de correlation

#5) corr entre C et varietie

boxplot(seed$C~seed$varietie)

tapply(seed$C,seed$varietie,shapiro.test)
#on a la normalité pour les 3 groupe 
#on verifie l'homogénéité : les variances des groupes sont égales ou non

bartlett.test(seed$C~seed$varietie) #0.007<0.05
#on accepte alors H1 : pas d’ ́egalit ́e entre les variances
#des  ́echantillons. On ne peut pas appliquer l’ANOVA. Le test non param ́etrique le
#plus ad ́equat pour notre cas est un test qui compare entre 3  ́echantillons puisque la
#variable Species est `a 3 modalit ́es. On applique le test de Kruskal Wallis.
kruskal.test(seed$C~seed$varietie)


#1) corr entre LK et WK
plot(seed$Lk,seed$Wk)
Lkwk=cor(seed$Lk,seed$Wk,method = "spearman")
cor.test(seed$Lk,seed$Wk,method = "spearman")

#2) corr entre LK et Ac
plot(seed$Lk,seed$Ac)
LkAc=cor(seed$Lk,seed$Ac,method = "spearman")
cor.test(seed$Lk,seed$Ac,method = "spearman") #pas de correlation

#3) corr entre LK et Lkg
plot(seed$Lk,seed$Lkg)
LkLkg=cor(seed$Lk,seed$Lkg,method = "spearman")
cor.test(seed$Lk,seed$Lkg,method = "spearman")

#4) corr entre C et varietie

boxplot(seed$Lk~seed$varietie)

tapply(seed$Lk,seed$varietie,shapiro.test) 

#On a deux  ́echantillont qui sont  normalement distribu et un autres n'est pas.
#Alors la normalit ́e n’est pas assur ́ee par tous les  ́echantillons, on ne peut pas appliquer
#un test d’ANOVA. On applique un test d’hypoth`ese non param ́etrique ppour un cas
#de 3  ́echantillons( vent présenté 3  modalit ́es), alors c’est un test de Kruskal Wallis.
kruskal.test(seed$Lk~seed$varietie)
#p−value < 2.2e−16 < 0.05, on accepte H1 : les distributions des  ́echantillons ne sont
#pas les mˆemes, il existe alors une diff ́erence entre les diff ́erentes modalit ́es, donc l’effet
#est pr ́esent et par la suite on ne peut pas ignorer la relation entre les deux variables.


#1) corr entre Wk et Ac
plot(seed$Wk,seed$Ac)
WkAc=cor(seed$Wk,seed$Ac,method = "spearman")
cor.test(seed$Wk,seed$Ac,method = "spearman") #pas de correlation

#2) corr entre Wk et Lkg
plot(seed$Wk,seed$Lkg)
WkLkg=cor(seed$Wk,seed$Lkg,method = "spearman")
cor.test(seed$Wk,seed$Lkg,method = "spearman")


#3) corr entre wk et varietie

boxplot(seed$Wk~seed$varietie)
tapply(seed$Wk,seed$varietie,shapiro.test) 
#On a deux  ́echantillont qui sont  normalement distribu et un autres n'est pas.
#Alors la normalit ́e n’est pas assur ́ee par tous les  ́echantillons, on ne peut pas appliquer
#un test d’ANOVA. On applique un test d’hypoth`ese non param ́etrique ppour un cas
#de 3  ́echantillons( vent présenté 3  modalit ́es), alors c’est un test de Kruskal Wallis.
kruskal.test(seed$Wk~seed$varietie)
#p−value < 2.2e−16 < 0.05, on accepte H1 : les distributions des  ́echantillons ne sont
#pas les mˆemes, il existe alors une diff ́erence entre les diff ́erentes modalit ́es, donc l’effet
#est pr ́esent et par la suite on ne peut pas ignorer la relation entre les deux variables.



#1) corr entre Ac et Lkg
plot(seed$Ac,seed$Lkg)
AcLkg=cor(seed$Ac,seed$Lkg,method = "spearman")
cor.test(seed$Ac,seed$Lkg,method = "spearman") #pas de corr

#2) corr entre Ac et varietie
boxplot(seed$Ac~seed$varietie)
tapply(seed$Ac,seed$varietie,shapiro.test) 


#On a deux  ́echantillont qui sont  normalement distribu et un autres n'est pas.
#Alors la normalit ́e n’est pas assur ́ee par tous les  ́echantillons, on ne peut pas appliquer
#un test d’ANOVA. On applique un test d’hypoth`ese non param ́etrique ppour un cas
#de 3  ́echantillons( vent présenté 3  modalit ́es), alors c’est un test de Kruskal Wallis.
kruskal.test(seed$Ac~seed$varietie) #<0.05
#p−value = 7.37e-14< 0.05, on accepte H1 : les distributions des  ́echantillons ne sont
#pas les mˆemes, il existe alors une diff ́erence entre les diff ́erentes modalit ́es, donc l’effet
#est pr ́esent et par la suite on ne peut pas ignorer la relation entre les deux variables.



#2) corr entre Lkg et varietie
boxplot(seed$Lkg~seed$varietie)
tapply(seed$Lkg,seed$varietie,shapiro.test) 

#On a deux  ́echantillont qui sont  normalement distribu et un autres n'est pas.
#Alors la normalit ́e n’est pas assur ́ee par tous les  ́echantillons, on ne peut pas appliquer
#un test d’ANOVA. On applique un test d’hypoth`ese non param ́etrique ppour un cas
#de 3  ́echantillons( vent présenté 3  modalit ́es), alors c’est un test de Kruskal Wallis.
kruskal.test(seed$Lkg~seed$varietie)

#p−value < 2.2e−16 < 0.05, on accepte H1 : les distributions des  ́echantillons ne sont
#pas les mˆemes, il existe alors une diff ́erence entre les diff ́erentes modalit ́es, donc l’effet
#est pr ́esent et par la suite on ne peut pas ignorer la relation entre les deux variables.

#matrice de corr
library(corrplot)
df1=seed[1:7]

mydata.cor=cor(df1, method = "spearman", use = "complete.obs")
corrplot(mydata.cor)

corr_matrix <- cor(df1)

mydata.cor %>% 
  as_tibble(rownames = "var1") %>% 
  gather(var2, value, -var1) %>% 
  
  ggplot(aes(x = var1, y = var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, digits = 2))) +
  labs(x = "", y = "", fill = "Corr", title = "Matrice de Corrélation") +
  coord_fixed() +
  theme_minimal() +
  scale_fill_gradientn(
    limits = c(-1,1),
    
    # here choose the colours you want
    colours = c("blue", "green", "red"), 
    
    # here choose the intervals you want (must be inside rescale!)
    values = scales::rescale(c(-1, 0.5, 0.7, 1)))
##On remarque une liaison forte ente les deux variables.






#Task 5 :

#1) MLR :
#on a 8 variables dont une est qualitative (varitie)
#alors on va regresser Lkg qui la variable target quantitative en fonction 
#des var A, P, C, Lk, Wk, Ac

RM <- lm(Lkg~A+P+C+Lk+Wk+Ac, data=seed)

#2) stratégie pour améliorer la performance du modéle de regression :

summary(RM)


#R2-adjusted = 0.9106 on peut conclure que 91% de la variabilité de Lkg est expliqué par les
#autes variables : ce modéle est un bon modéle
# R2=0.91 => 
# fournit une estimation de la force de la relation 
#entre votre modèle et la variable de réponse et comme

#la p_valeur du test de significativité globale 
#est $ p-value: < 2.2e-16$ donc le resultat obtenu pour 
#R2ajusté=0.9106 est statistiquement significatif.

resid=residuals(RM)
plot(resid) #Un comportement unimodal autour de zéros, les résidus sont non corrélées et de moyenne nulle


#stratégie :
#On va  ́eliminer les variables les moins significatives qui sont les variables avec la valeur
#du pvalue la plus  ́elev ́ee, c’est le param`et
#re qui est le plus susceptible d’ˆetre nul.

#Wk : 0.005190

M1 = lm(Lkg ~ A+P+C+Lk+Ac, data = seed)
summary(M1) # 0.9097 ~ 0.91
#Le mod`ele ne perd pas de qualit ́e, on a toujours R2=0.91 alors le mˆeme taux
#d’information expliqu ́ee de la variabilit ́e de Lkg


resid1=residuals(M1)
plot(resid1)
summary(M1)
#comparaison de deux modéles 

AIC_RM=AIC(RM)
AIC_M1=AIC(M1)

#M1 est meilleur

#test de la normalité des residus

#représentation de la droite de Henry qui représente la conformité des quantiles pour la
#distribution cumm ́el ́ee des r ́esidus
#et et la droite des quantiles th ́eoriques d’une normale.

qqnorm(resid)
qqline(resid)

qqnorm(resid1)
qqline(resid1)
#(voir tp6 pour l'interpretation)


#3) PCA:( methode 1)
data<- Factoshiny(seed)
seed_PCA <- seed[,1:7]
names(seed_PCA)
View(seed_PCA)
res <- prcomp(seed_PCA, scale = TRUE,center = TRUE, retx = T)
names(res)
attributes(res)
summary(res)
print(res)
dim(res$x)
biplot(res, main = "Biplot", scale = 0)
res$sdev
res.var <- res$sdev ^ 2
res.var
propve <- res.var / sum(res.var)
propve
plot(propve, xlab = "principal component",ylab = "Proportion of Variance Explained",ylim = c(0, 1), type = "b",main = "Scree Plot")
plot(cumsum(propve),
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")
fviz_eig(res, addlabels = TRUE, ylim = c(0, 75))
which(cumsum(propve) >= 0.9)
train.data <- data.frame(lkg = seed_PCA$Lkg, res$x[, 1:2])
summary(train.data)
#install.packages("rpart")
#install.packages("rpart.plot")
#library(rpart)
#library(rpart.plot)
#rpart.model <- rpart(Lkg ~ .,data = train.data, method = "anova")
#rpart.plot(rpart.model)
M3 <- lm(lkg ~ PC1+PC2, data = train.data)
AIC_M3 =AIC(M3)


#Task6



#2)
plot(seed$Lkg)
#verifying the inverse guassian distribution
ig_test(seed$Lkg, method = "transf")
y <- seed$Lkg
ig_test(y)[[2]]$p.value 
#Plotting hist
hist(y,xlab = "Weight",col = "yellow",border = "blue")
# Application du gaussian
#AIC = -166.3
glm(y~A+P+C+Lk+Wk,family = 'gaussian' , data=seed)
#Application de inverse gaussian
#AIC: -160.5
glm(y~A+P+C+Lk+Wk,family= inverse.gaussian(link = "log") , data=seed)
#on ne peut pas appliquer poisson regression model valeurs continues
glm(y~A+P+C+Lk+Wk,family= poisson(link = "log") , data=seed)
#on ne peut pas appliquer Binomial regression model valeurs continues
glm(y~A+P+C+Lk+Wk,family=binomial(link = "logit"), data=seed)

