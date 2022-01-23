
class(dataset)

dataset<-dataset[,-c(1,2)]
colnames(dataset)<-c("genere", "età", "sitSentim", "status", "etàPt", "statusPt", "anniRel", "km", "figli", "y", "durataMedia", "visti", "sms", "fasciaOraria", "soddisfazione")
dataset1<-dataset1[,-c(1,2)]

DATA PREPROCESSING
colnames(dataset1)<-c("genere", "età", "sitSentim", "status", "etàPt", "statusPt", "anniRel", "km", "figli", "y", "durataMedia", "visti", "sms", "fasciaOraria", "soddisfazione")
dataset1$figli[167]<-0
which(dataset1$genere=="Altro")
dataset1[c(154,491),]
dataset1<-dataset1[-c(154,491),]
library()
boxplot(dataset1$y)

dataset<-as.data.frame(dataset)
dataset$figli[167]<-0
which(dataset$genere=="Altro")
dataset[c(154,491),]
dataset<-dataset[-c(154,491),]
dataset<-dataset[-300,]
dataset$genere<-as.factor(dataset$genere)
dataset$sitSentim<-as.factor(dataset$sitSentim)
dataset$status<-as.factor(dataset$status)
dataset$statusPt<-as.factor(dataset$statusPt)
dataset$km<-as.numeric(dataset$km)
dataset$figli<-as.numeric(dataset$figli)
dataset$durataMedia<-as.factor(dataset$durataMedia)
levels(dataset$durataMedia)
dataset$durataMedia<-factor(dataset$durataMedia, levels= c("Non ci sono state telefonate","Meno di 10 minuti", "Tra 10 e 30 minuti", "Tra 30 e 60 minuti", "Più di 60 minuti" ))
dataset$visti<-factor(dataset$visti, levels=c("No","Sì, una volta",  "Sì, più di una volta","Sì, conviviamo"))
levels(dataset$visti)       
dataset$fasciaOraria<-factor(dataset$fasciaOraria, levels = c("Nessuna","Mattina", "Pomeriggio", "Sera"))
levels(as.factor(dataset$fasciaOraria))
dataset$sms<-as.factor(dataset$sms)


entrambiStud<-(dataset$status=="Studente")+(dataset$statusPt=="Studente")
entrambiStud<-ifelse(entrambiStud==2,1,0)
entrambiLav<-(dataset$status=="Lavoratore")+(dataset$statusPt=="Lavoratore")
entrambiLav<-ifelse(entrambiLav==2,1,0)
entrambiBoth<-(dataset$status=="Entrambi")+(dataset$statusPt=="Entrambi")
entrambiBoth<-ifelse(entrambiBoth==2,1,0)
entrambiAltro<-(dataset$status=="Altro")+(dataset$statusPt=="Altro")
entrambiAltro<-ifelse(entrambiAltro==2,1,0)

diffeta<-dataset$età - dataset$etàPt
diffetabs<-abs(dataset$età - dataset$etàPt)

nonvisti <-ifelse(dataset$visti=="No", 1,0)
vistialmeno<-(dataset$visti=="Sì, più di una volta")+(dataset$visti=="Sì, una volta"); vistialmeno
conviventi<-ifelse(dataset$visti=="Sì, conviviamo", 1,0)

smspoco<-(dataset$sms==1)+(dataset$sms==2)
smsmetà<-ifelse(dataset$sms==3, 1, 0)
smsmolto<-(dataset$sms==4)+(dataset$sms==5)

dataset$sms<-relevel(dataset$sms, ref = "5")

#MODELLO GLM POISSON ALL SIGNIFICANT VARIABLES 
#modfull_significativo<-glm(y~entrambiAltro+entrambiStud +sitSentim+nonvisti+figli+ vistialmeno+ sms+ soddisfazione, family = poisson, data = dataset)
#summary(modfull_significativo)
# dispersiontest(modfull_significativo,trafo=1)
# modfullqp<-glm(y~entrambiAltro+entrambiStud +sitSentim+nonvisti+figli+ vistialmeno+ as.numeric(sms)+ soddisfazione, family = quasipoisson, data = dataset)
# summary(modfullqp)
# modfullnb<-glm.nb(y~entrambiAltro+entrambiStud +sitSentim+nonvisti+figli+ vistialmeno+ sms+ soddisfazione, data = dataset)
# summary(modfullnb)
# modfullnb1<-glm.nb(y~entrambiAltro+entrambiStud +sitSentim+nonvisti+figli+ vistialmeno+ sms+ soddisfazione, data = dataset)
# summary(modfullnb1)

#MODELLO 2 IS THE BEST
# modfullnb2<-glm.nb(y~entrambiStud+sitSentim+nonvisti+ vistialmeno+ as.numeric(sms)+ soddisfazione, data = dataset)
# summary(modfullnb2) #UNDERDISPERSION 0.97
# 
# modfullnb3<-glm.nb(y~entrambiStud+sitSentim+conviventi+as.numeric(sms)+ soddisfazione + figli, data = dataset)
# summary(modfullnb3)
# which(dataset$km>1000)
# hist(dataset$km)

vicinivicini<-ifelse(dataset$km<=2,1,0) #quartiere, 0-2 km 
vicini<-ifelse(dataset$km>2 & dataset$km<=10,1,0)
vicini50<-ifelse(dataset$km>10 & dataset$km<=50,1,0)
vicini200<-ifelse(dataset$km>50 & dataset$km<=200,1,0); vicini200
lontani<-ifelse(dataset$km>200,1,0); lontani
sum(vicini+vicini200+vicini50+vicinivicini+lontani) #ok

#
require(MASS)
modfullnb5<-glm.nb(y~lontani+vicini+vicini50+entrambiStud+sitSentim+conviventi+as.numeric(sms)+ soddisfazione + figli, data = dataset)
summary(modfullnb5)
#poisson proprio carino
modpoisson<-glm(y~lontani+vicini+vicini50+entrambiStud+sitSentim+conviventi+as.numeric(sms)+ soddisfazione + figli,family=poisson, data = dataset)
summary(modpoisson)
library(AER)
dispersiontest(modpoisson,trafo=1)
2982/515
#QUASI-POISSON MODEL
modquasipoisson<-glm(y~lontani+vicini+vicini50+entrambiStud+sitSentim+conviventi+as.numeric(sms)+ soddisfazione + figli,family=quasipoisson, data = dataset)
summary(modquasipoisson)

modfullnb5
confint(modfullnb5)

require(ggplot2)
ggplot(dataset, aes(y)) + geom_histogram(fill='red', colour='black')
plot(table(dataset$y))
hist(dataset$y, breaks = 60, col = "dodgerblue", main="Istogramma numero di telefonate", xlab = "")
boxplot(dataset$y, col = "dodgerblue", main="Boxplot del numero di telefonate")
length(which(dataset$y==0))
length(which(dataset$y==1))
length(which(dataset$y==2))
length(which(dataset$y==3))
table(dataset$y)
mean(dataset$y)
var(dataset$y)
mean(dataset$età); var(dataset$età)
mean(dataset$etàPt); var(dataset$etàPt)
summary(dataset$km)
mean(dataset$soddisfazione)
var(dataset$km)
mean(dataset$soddisfazione); var(dataset$soddisfazione)
summary(dataset$soddisfazione)
summary(dataset$età)
summary(dataset$etàPt)
summary(dataset$anniRel)
var(dataset$anniRel)
summary(dataset$figli); var(dataset$figli)
summary(as.numeric(dataset$sms)); var(as.numeric(dataset$sms))

#ZERO INFLATED POISSON MODEL
require(pscl)
modzeroinfpoisson<-zeroinfl(y~lontani+vicini+vicini50+entrambiStud+sitSentim+conviventi+as.numeric(sms)+ soddisfazione + figli|sitSentim+conviventi+vistialmeno, data = dataset)
summary(modzeroinfpoisson)
deviance(modzeroinfpoisson)
# ZERO INFLATED NEGATIVE BINOMIAL MODEL
modzeroinfnegbin <- zeroinfl(y~lontani+vicini+vicini50+entrambiStud+sitSentim+conviventi+as.numeric(sms)+ soddisfazione + figli|sitSentim+conviventi+vistialmeno, dist= "negbin", data = dataset)
summary(modzeroinfnegbin)
AIC(modzeroinfnegbin)
BIC(modzeroinfnegbin)
# COMPARING GLM POISSON MODEL AND ZERO INFLATED POISSON
vuong(modpoisson,modzeroinfpoisson)  #ZERO INFLATED MODEL SLECTED
AIC(modpoisson); BIC(modpoisson); AIC(modzeroinfpoisson); BIC(modzeroinfpoisson)


library(pscl)
AIC(modfullnb5); BIC(modfullnb5) 
vuong(modfullnb5,modzeroinfpoisson) #yeeeeeeeahahhah
AIC(modquasipoisson); BIC(modquasipoisson)
vuong(modquasipoisson, modfullnb5)
vuong(modfullnb5, modpoisson)
vuong(modfullnb5, modzeroinfnegbin) 

confronto<-cbind(dataset$y,fitted(modfullnb5))
View(confronto) #fa un po' cagare con i valori estremi .-.


install.packages("hnp")
library("hnp")
hnp(modfullnb5,col="dodgerblue",pch=19)




summary(modfullnb5)
coef(modfullnb5)

#THE FOLLOWING FUNCTION MODIFIES AS.VECTOR BECAUSE THERE IS NOT POSSIBLE TO HAVE A ZERO INFLATED MODEL
f <- function(data, i) {
  m <- glm.nb(y ~ lontani + vicini + vicini50 + entrambiStud + 
                sitSentim + conviventi + as.numeric(sms) + soddisfazione + 
                figli, data = data[i, ],
              start = list(-0.525, 0.379, 0.307, 0.513, -0.276, 0.994, 2.052, -0.435, 0.052, 0.103,
                           -0.322))
  as.vector(t(coef(summary(m))[, 1:2]))
}



round(exp(coef(modfullnb5)), 3)
set.seed(10)
library(MASS)
library(boot)
res <- boot(dataset, f, R = 1200, parallel = "snow", ncpus = 4)

## PRINT RESULTS 
res

# rbind(coef(summary(modfullnb5)))

modfullnb5




modcaso <- glm.nb(y~lontani + as.numeric(sms), data = dataset)
summary(modcaso)
fitted(modcaso)[1]
exp(coef(modcaso)[1] + coef(modcaso)[2]*0 + coef(modcaso)[3]*5)/(1+exp(coef(modcaso)[1] + coef(modcaso)[2]*0 + coef(modcaso)[3]*5))

coef(modfullnb5)
new <- c()
predict.glm(modfullnb5, type = "response")
mean(as.numeric(dataset$sms))



modfullnb5
summary(modfullnb5)

newdata1 <- expand.grid(0:1, 0:1, 0:1, 0:1, factor(dataset$sitSentim), 0:1, 1:5, 1:10, 0:5)
colnames(newdata1) <- c("lontani", "vicini", "vicini50", "entrambiStud", "sitSentim", 
                        "conviventi", "sms", "soddisfazione", "figli")
newdata1
newdata1 <- subset(newdata1, subset=(vicini==0 & vicini50==0 & entrambiStud==1
                                     & conviventi==0 & sms==4 & figli==0))

newdata1$phat <- predict(modfullnb5, newdata1, type = "response")

ggplot(newdata1, aes(x = soddisfazione, y = phat, colour = factor(sitSentim))) +
  geom_point(pch=19) +
  geom_line(lwd=1.1) +
  facet_wrap(~lontani) +
  labs(x = "Soddisfazione", y = "Numero di telefonate previste")


clog <- function(x) log(x + 0.5)
cfac <- function(x, breaks = NULL) {
  if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
  x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
  levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
  c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
  sep = "")
  return(x)  }
plot(clog(dataset$y))


require(pscl)
length(which(dataset$y==0))  
98/526 
#similarly
100*sum(dataset$y == 0)/nrow(dataset)
#While our data seems to be zero-inflated, this doesn't necessarily mean we need to use a zero-inflated model. 
#In many cases, the covariates may predict the zeros under a Poisson or Negative Binomial model. 
#So let's start with the simplest model, a Poisson GLM
               


modzeroinf4<-zeroinfl(y~entrambiAltro+entrambiStud + entrambiBoth+figli+ sitSentim + vistialmeno+ soddisfazione|nonvisti + entrambiLav + as.numeric(sms) + sitSentim, data = dataset)
summary(modzeroinf4)
vuong(modzeroinf4, modfull_significativo) #BETTER MODELLO1 ZERO INFLATED

modell<-glm(y ~ entrambiAltro + entrambiStud + entrambiBoth + figli + 
                 sitSentim + vistialmeno + soddisfazione, family = poisson, data = dataset)
summary(modellino)
vuong(modellino, modzeroinf4) #better modzeroinfl4
require(lmtest)
coeftest(modellino, vcov = sandwich)



OVERDISPERSION ON MODELL?
#su modell
#residual deviance/ dof residual deviance > 1.5
3031.8/517   #5.8642
#THERE IS EXTRA VARIANCE NOT EXPLAINED BY THE MODEL OR THE ERROR STRUCTURE
#THUS, THERE IS THE NEED OF CHANGING THE DISTRIBUTION RELATED TO THE ERRORS (NO MORE POISSON)
mean(dataset$y)
var(dataset$y)
E2 <- resid(modellino, type = "pearson")
N  <- nrow(dataset)
p  <- length(coef(modellino))   
sum(E2^2) / (N - p)  #6.99
dispersiontest(modellino,trafo=1)
# zero inflated model 4
# Dispersion statistic for zeroinflated4
E5 <- resid(modzeroinf4, type = "pearson")
N5  <- nrow(dataset)
p5  <- length(coef(modzeroinf4))  
sum(E5^2) / (N5 - p5)  #3.440  l'overdispersion reduced


#Over-dispersion is a problem if the conditional variance (residual variance) is larger than the conditional mean.  
#One way to check for and deal with over-dispersion is to run a quasi-poisson model, 
#which fits an extra dispersion parameter to account for that extra variance.
#Looks like the Negative Binomial GLM resulted in some minor underdispersion. 
#In some cases, this might be OK. But in reality, we want to avoid both under- and overdispersion.
#Overdispersion can bias parameter estimates and produce false significant relationships. 
#On the otherhand, underdisperion can mask truly significant relationships. So let's try to avoid all of this



#estimating GLM negative binomial 
require(MASS)
modGLMngb<-glm.nb(y ~ entrambiAltro + entrambiStud + entrambiBoth + figli + 
  sitSentim + vistialmeno + soddisfazione, data = dataset)
summary(modGLMngb)
E3 <- resid(modGLMngb, type = "pearson")
N3  <- nrow(dataset)
p3  <- length(coef(modGLMngb))   
sum(E3^2) / (N3 - p3) #1.04 l'overdispersion è stata ridotta di molto

modGLMngb2<-glm.nb(y ~ entrambiStud+fasciaOraria+soddisfazione, data = dataset)
summary(modGLMngb2)
E4 <- resid(modGLMngb2, type = "pearson")
N4  <- nrow(dataset)
p4  <- length(coef(modGLMngb2))   
sum(E4^2) / (N4 - p4) #1.41 MOST SIGNIFICANT MODEL BUT IT HAS A HIGHER OVERDISPERSION



#ESTIMATING A GLM QUASI-POISSON


modquasipoisson<-glm(y ~ entrambiAltro + entrambiStud + entrambiBoth + figli + 
  sitSentim + vistialmeno + soddisfazione, family = quasipoisson, data = dataset)
summary(modquasipoisson)
EQP <- resid(modquasipoisson, type = "pearson")
NQP  <- nrow(dataset)
pQP  <- length(coef(modquasipoisson))   
sum(EQP^2) / (NQP - pQP)   #già scritto nel summary
#This parameter (dispersion parameter) tells us how many times larger the variance is than the mean.


library(pscl)
library(MASS)
install.packages("countreg", repos="http://R-Forge.R-project.org")
countreg::rootogram((modfullnb5),style = "standing",fill = "steelblue3",col="red",lwd=2,main = "GLM Negative Binomial",xlab = "", ylab = "")
countreg::rootogram((modfullnb5),style = "hanging",fill = "steelblue3",col="red",lwd=2,main = "GLM Negative Binomial",xlab = "", ylab = "")
