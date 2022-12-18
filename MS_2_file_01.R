

# Generatore lineare di numeri pseudo -casuali (tipo congruenziale misto)

?runif

# Metodo congruenziale lineare
a <- 65537
m <-34359738368
xini <- 47838
c <- 5
(a*xini + c)%%m
# primo numero
x1 <- (a*xini+c)%%m
x1/m
# secondo numero
(a*x1 + c)%%m
x2 <- (a*x1+ c)%%m
x2/m

# sequenza di 1857 num
n <- 1857
random.n <- numeric(n)
for(j in 1:n){
  x1 <- (a*x1+c)%%m
  random.n[j] <- x1/m
}
head(random.n)
tail(random.n)

# Valutazione dello pseudo-casualita della serie
# statistiche descritibe
require(skimr)
skim_without_charts(random.n)

var(random.n)

# Test grafici
# diagramma a dispersione
plot(random.n[1:1856],
     random.n[2:1857],
     main="Grafico (1,n-1)*(2,n)",
     ylab = "valori generati con metodo congruenziale")

# Istogramma
hist(random.n, 
     col = 'purple',
     breaks = 20,
     freq=FALSE,
     ylim =c(0,1.5),
     main='Istogramma',
     xlab='numeri pseudo-casuali',
     ylab='Densità di frequenza')
abline(v=mean(random.n),
       col='purple',
       lwd=2,lty=2)
text(0.7,1.3,
     c("valore medio"),
     col="blue")

# Funzione di ripartizione empirica
plot(ecdf(random.n),
     do.points=FALSE,
     main ='Funzione di ripartizione empirica vs teorica')
curve(punif(x,0,1),
      lty='dashed',
      col='red',
      lwd='3',
      add=TRUE)
legend(0.8,0.4,
       col=c("black","red"),
       c("f.r. empirica","f.r. teorica"),
       lty=c(1,2),
       cex=0.7)

# Test di Kolmogorov Smirnov
# il test non parametrico che permette di confrontare la funzione di ripartizione empirica con quella teorica della distribuzionee
ks.test(random.n, "punif")


# Test Chi Quadrato
n <- length(random.n)
int<-seq(0,1,by=0.1); int

foss<-table(cut(random.n, int))/n; foss

p<- rep(0.1,10); p

chisq.test(foss,p=p)

qchisq(0.95,df=9)


n <- length(random.n)
int<-seq(0,1,by=0.1); int

fossN<-table(cut(random.n, int)); fossN

chisq.test(fossN)

# Funzione di Autocorrelazione empirica == correlogramma = 10.log(n)
acf(random.n,
    main = " funzione di autocorrelazione")

n<-length(random.n)
acf(random.n, main = " funzione di autocorrelazione", lag.max =n)

# Funzione runif
set.seed(3882)
n <- 2000
rand <- runif(n, min=0, max=1)
head(rand)
# diagramma a dispersione
plot(rand[1:(n-1)],
     rand[2:n],
     main="Grafico (1,n-1)*(2:n)",
     ylab = "valori generati con funzione runif")

# Funzione di ripartizione empirica
plot(ecdf(rand),
     do.points=FALSE,
     main ='funzione di ripartizione empirica vs teorica')
curve(punif(x,0,1),
      lty='dashed',
      col='red',
      lwd='3',
      add=TRUE)
legend(0.6,0.3,
       col=c("black","red"),
       c("f.r. empirica","f.r. teorica"),
       lty=c(1,2),
       cex=0.6)

# Istogramma
hist(rand, col = 'yellow',
     breaks = 20,
     freq=FALSE,
     ylim =c(0,1.5),
     main='Istogramma',
     xlab='numeri pseudo-casuali (runif)',
     ylab='Densità di frequenza')
abline(v=mean(random.n),
       col='purple',
       lwd=2,lty=2)
text(0.7,1.3,
     c("valore medio"),
     col="blue")

# Test statistici
# Kolmogorov Smirnov
ks.test(rand, "punif")
# Chi Quadrato
foss<-table(cut(rand, int))/n; foss

chisq.test(foss,p=p)

# Autocorrelazione
acf(rand,
    main = " funzione di autocorrelazione")

acf(rand, lag.max = n,
    main = " funzione di autocorrelazione")





# Generazione di determinazioni da variabili casuali
# d- per calcolare la densita in un punto
# p- per calcolare la funzione di ripartizione in un punto
# q per cacolare un quantile
# r - per generare pseudo -determinazioni dalla distribuzione

# Exp: exp
# gamma: gamma
# student t : t
# normale : norm

# per la normale standard
# dnorm
# pnorm
# qnorm
# rnorm

# Generazione di pseudo -determinazioni dalla v.c. di Gauss
rnorm(10, 0.1)

n <- 1000
set.seed(27732)
Z <- rnorm(1000)
mean(Z); sd(Z)

hist(Z,
     breaks = 30,
     freq=FALSE,
     main="Z = N(0,1)",
     col ="blue",
     ylab="Densità",
     ylim =c(0,0.6))


rnorm(10,mean = 4, sd = 4 )


# Generazione dalla variabile casuale Esponenziale
x<-seq(0,5,length=101)
head(x)

h<-dexp(x,rate = 1)
head(h)

plot(x,h,
     type="l", 
     col = "blue",
     lwd = 3, 
     ylim = c(0,2),
     xlab = "Tempo di sopravvivenza",
     ylab = "Densità")


n<-3
m<-rep(0,1000)
for(i in 1:1000){
  m[i]<-mean(rexp(n, rate=1))
}
head(m)

plot(x,h,
     type="l", col = "blue",
     lwd = 3, ylim = c(0,2),
     xlab = "Tempo di sopravvivenza", ylab = "Densità")
hist(m, prob= T, add=T, col = rgb(0,0,1,1/4), breaks = 25)
legend(3,1.5, c("teorica", "realizzazioni n = 3"),
       col = c("blue", "lightblue"),
       lty = c(1,1),
       lwd = c(2,1),
       cex = 0.6)


# Generazione di pseudo-derminazioni dalla v.c. Beta
n<-1000
set.seed(27732)
alpha <- 1
beta<- 1

B <- rbeta(n,alpha,beta)
summary(B)

hist(B,
     breaks = 50,
     freq=FALSE,
     main="Beta (1,1)",
     col ="grey",
     ylab="Densità"
)
curve(dbeta(x,1,1),
      col = "red",
      add = TRUE )


alpha <- 0.5
beta <- 0.7
B1 <- rbeta(n,alpha,beta)
#
hist(B1,
     breaks = 50,
     freq=FALSE,
     main="Beta (0.5,0.7)",
     col ="grey",
     ylab="Densità",
     ylim=c(0,6)
)
#
curve(dbeta(x,alpha,beta),
      col = "red",
      add = TRUE, lwd=2 )
legend(0.6,4, c("realizzazioni n = 1000",
                "distr. teorica"),
       col = c("grey", "red"),
       lty = c(1,1),
       lwd = c(1,2),
       cex = 0.4)


# Generazione di pseudo-determinazioni dalla v.c. Binomiale
y= seq(0, 12, 1); y

dy <- dbinom(y,12,0.2); dy

plot(y, dy, type="h")
set.seed(123)
rbinom(1, 8, 0.20)
rbinom(8, 1, 0.20)

# Generazione di pseudo-derminazioni dalla v.c. di Poisson
y = seq(30, 120, 3)
plot(y, dpois(y, 80), type='h')
set.seed(163)
y <- rpois(100, 22); y
mean(y)
var(y)

# Modello lineare generalizzato per i conteggi basato sulla distribuzione Binomiale Negativa


Crabs <- read.table("http://stat4ds.rwth-aachen.de/data/Crabs.dat", header=TRUE)
head(Crabs)

require(skimr)
skimr::skim_without_charts(Crabs)

table(Crabs$sat)

hist(Crabs$sat, breaks=c(0:16)-0.5,
     ylim = c(0,70),
     col = "pink", ylab = "Frequenze",
     xlab = "Numero satelliti",
     main = " ")


library(MASS)
stima <- glm.nb(sat ~ weight + factor(color), link=log, data=Crabs)
summary(stima)

1/0.9596


# Analisi della serie storica dei conteggi riferiti a COVID-19
# Modello autoregressivo di Poisson non omogeneo
repository <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/"
overall.dataset <- "dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
overall.filename<-paste(repository,overall.dataset,sep="")
Italy<-read.csv(overall.filename)
names(Italy)


library(dplyr)
dataItaly<- Italy %>%
  select(data,
         dimessi_guariti,
         isolamento_domiciliare,
         ricoverati_con_sintomi,
         terapia_intensiva,deceduti)
mydate <- as.Date(as.POSIXct(dataItaly$data, format="%Y-%m-%dT %H:%M:%S"))
df<-data.frame(mydate,dataItaly$isolamento_domiciliare)
colnames(df)<-c("date", "Isolati")
df1 <- subset(df, date >= as.Date('2022-08-01') & date <= as.Date('2022-10-20'))

min(df$date)
max(df$date)

require(skimr)
skimr::skim_without_charts(df1$Isolati)

n<-length(df1$Isolati)
ma<-max(df1$Isolati)
mi<-min(df1$Isolati)
ytick<-c(mi,409247,ma)
xtick<-c(1,11, 21, 31, 41, 51, 61, 71, 81)
plot(df1$Isolati,
     ylab=expression("Isolamento ("*italic(y[t])*")"),
     xlab=expression("Giorni ("*italic(t)*")"),
     yaxt="n",
     xaxt="n",
     xlim =c(1,n+5),
     ylim = c(0,ma+10),
     lwd = 0.5,
     lty = 1,col ="black" )
axis(side=2, at=ytick, labels = FALSE,
     cex.lab = 0.5, padj = 2,tck=-0.009)
text(par("usr")[1],
     ytick,
     labels = ytick, srt = 45,
     pos = 2, xpd = TRUE, cex.lab = 0.1)
axis(side=1, at=xtick,
     labels = FALSE, cex=0.1,tck=-0.009)
text(x=xtick,
     par("usr")[3],
     labels = xtick,
     cex.lab = 0.5,
     pos = 1, xpd = TRUE)


regressors1Italy <- cbind(linearTrend=seq(along=df1$Isolati),
                          quadTrend = seq(along=df1$Isolati)^2/100,
                          linlogTrend = log(seq(along=df1$Isolati)))
head(regressors1Italy)


options(scipen = 100)
require(tscount)
M3Italy <- tsglm(ts=df1$Isolati,
                 link = "log",
                 model=list(past_obs=1),
                 xreg=regressors1Italy,
                 distr = "poisson")
summary(M3Italy)


go <- 5
TT <- length(df1$date)
P3Italy <-predict(M3Italy,
                  newxreg = data.frame(linearTrend = ((TT+1):(TT+go)),
                                       quadTrend = ((TT+1):(TT+go))^2/100,
                                       linlogTrend = log((TT+1):(TT+go))),
                  n.ahead=go,
                  method="bootstrap" )

pred<-data.frame(cbind(P3Italy$pred,P3Italy$interval))
colnames(pred)<-c("previsti", "PIinf", "PIsup")
pred


ytick<-c(mi,409247,ma)
xtick<-c(1,11, 21, 31, 41, 51, 61, 71, 81)
plot(df1$Isolati,
     ylab=expression("Isolamento ("*italic(y[t])*")"),
     xlab=expression("Giorni ("*italic(t)*")"),
     yaxt="n",
     xaxt="n",
     xlim =c(1,n+5),
     ylim = c(0,ma),
     lwd = 0.5,
     lty = 1,col ="black" )
abline(v=240, col = "gray")
axis(side=2, at=ytick, labels = FALSE,
     cex.lab = 0.5, padj = 2,tck=-0.009)
text(par("usr")[1],
     ytick,
     labels = ytick, srt = 45,
     pos = 2, xpd = TRUE, cex.lab = 0.1)
axis(side=1, at=xtick,
     labels = FALSE, cex=0.1,tck=-0.009)
text(x=xtick,
     par("usr")[3],
     labels = xtick,
     cex.lab = 0.5,
     pos = 1, xpd = TRUE)
lines(c(M3Italy$fitted.values,
        pred$previsti),
      lwd=3, col = 3, lty = 4)
legend("bottomleft",
       pch = c(20,NA,NA,NA,NA),
       lty = c(NA,2),
       legend = c("osservati", "interpolati e previsti"),
       col = c("black", "green"),
       bty = "n",
       x.intersp = 0.1,
       cex= 0.6, pt.cex = .5,
       xpd = TRUE,
       text.width = 0.0001)







# Modello Autoregressivo con distribuzione Binomiale Negativa
options(scipen = 100)
require(tscount)
M4Italy <- tsglm(ts=df1$Isolati,
                 link = "log",
                 model=list(past_obs=1),
                 xreg=regressors1Italy,
                 distr = "nbinom")
M4Italy
summary(M4Italy)

go <- 5
TT <- length(df1$date)
P4Italy <-predict(M4Italy,
                  newxreg = data.frame(linearTrend = ((TT+1):(TT+go)),
                                       quadTrend = ((TT+1):(TT+go))^2/100,
                                       linlogTrend = log((TT+1):(TT+go))),
                  n.ahead=go,
                  method="bootstrap" )
pred<-cbind(P4Italy$pred,P4Italy$interval)
colnames(pred)<-c("previsti", "PIinf", "PIsup")
pred

plot(df1$Isolati, lwd = 0.5,
     lty = 1,col ="black",
     xlab = "Giorni",
     ylab= "Conteggio Isolati",
     ylim = c(mi,ma+2), xlim = c(0,n+5))
abline(v=81, col = "gray")
lines(c(M4Italy$fitted.values,P4Italy$pred),
      lwd=2, col = 2, lty = 2)
legend("bottomleft",
       lty = c(1,2),
       legend = c("osservati", "interpolati e previsti"),
       col = c("black", "red"),
       bty = "n",
       x.intersp = 0.1,
       cex= 0.6, pt.cex = .5,
       xpd = TRUE,
       text.width = 0.0001)


# Valutazione della prevalenza nel tempo
prev<-c(M4Italy$fitted.values,P4Italy$pred)/60317000
prev<-prev*1000
plot(prev, type = "l",
     xlab = "Giorni",
     ylim = c(0,20),
     lwd=2,
     col = "blue", ylab = "(prevalenza isolati)*1000")



# Metodi di ricampionamento: il bootstrap
# Dati nervo
setwd("C:/Users/UTENTE/OneDrive/Desktop/MS_2/dati_ms2")
nervo<-read.table("nervo.dat", header = TRUE)
head(nervo$A)
require(skimr)
skim_without_charts(nervo)
boxplot(nervo$A, xlab="Pulsazioni", horizontal=TRUE, col = "brown")
hist(nervo$A,
     breaks = 50,
     ylim=c(0,5),
     main = "Pulsazioni fibra del nervo",
     ylab="Densità",
     freq =FALSE,
     col = "grey")
rateA <- 1/mean(nervo$A); rateA
x<-seq(0,1.4,length=799)
h<-dexp(x,rate = rateA)
plot(x,h,
     ylim=c(0,5),
     type="l", col = "blue",
     lwd = 3,
     xlab = "pulsazioni",
     ylab = "densità")
hist(nervo$A,
     breaks = 50,
     col = rgb(0,0,1,1/4),
     freq =FALSE, add=T)
legend(0.8,3, c("teorica", "campione"),
       col = c("blue", "lightblue"),
       lty = c(1,1),
       lwd = c(2,1),
       cex = 0.6)
plot(ecdf(nervo$A),
     col="lightblue",
     main= "funz. di ripartizione emp. pulsazioni")
plot(ecdf(nervo$A),
     col="lightblue",
     main= "Funz. di ripartizione")
#
curve(pexp(x,rate=rateA),
      lty='dashed',
      col='red',
      lwd='3',
      add=TRUE)
#
legend(0.8,0.4, col=c("lightblue","red"),
       c("f.r. empirica","f.r. teorica"), lty=c(1,2),
       cex=0.7)



# Indice di asimetria
require(e1071)
skewness(nervo$A)


# BOOTSTRAP:
# Passo 1: si ottiene campione
n <- length(nervo$A)
B1 <- sample(nervo$A, n, replace = TRUE)
summary(B1)
B2 <- sample(nervo$A, n, replace = TRUE)
summary(B2)
B3 <- sample(nervo$A, n, replace = TRUE)
summary(B3)
# Passo 2: calcola indice di assimetria in ogni campione 
s1<-skewness(B1)
s2<-skewness(B2)
s3<-skewness(B3)
s <- c(s1,s2,s3); s

# oppure
BB<-cbind(B1,B2,B3)
apply(BB,2,skewness)


# Passo 3: dev std
sd(s)

# for ciclo
B <- 1000
n <- length(nervo$A)
Tboot <- rep(0, B)
set.seed(16253)
for (i in 1:B) {
  Xstar <- sample(nervo$A,
                  n,
                  replace = TRUE)
  Tboot[i] <- e1071::skewness(Xstar)
}
head(Tboot)
summary(Tboot)
seTboot <- sd(Tboot); seTboot


# Intervalli di confidenza bootstrap
# metodo del percentile
nervo<-read.table("nervo.dat", header = TRUE)
B <- 1000
n <- length(nervo$A)
Tboot <- rep(0, B)
for (i in 1:B) {
  Xstar <- sample(nervo$A,
                  n,
                  replace = TRUE)
  Tboot[i] <- e1071::skewness(Xstar)
}
sk <- e1071::skewness(nervo$A)
hist(Tboot,
     breaks=50,
     freq=FALSE,
     main = "Distribuzione con 1000 realizzazioni bootstrap",
     xlab = "Indice di asimmetria",
     ylim = c(0,4),
     col= "gray",
     ylab = "Densità",
     xlim = c(1,2.5))
abline(v=sk, col="red")
legend("topleft", 2,
       c("valore sul campione"),
       col = "red",
       lty= 1,
       cex = 0.8)





sB<- mean(Tboot)
Q <- quantile(Tboot, c(0.025, 0.975))
Q[1]; Q[2]

hist(Tboot,
     breaks = 60,
     freq=FALSE,
     main = "Distribuzione con 1000 realizzazioni bootstrap",
     xlab = "Indice di asimmetria",
     ylim = c(0,4),
     col = "gray",
     ylab = " Densità",
     xlim = c(1, 3))
#
abline( v = c(sk,sB,Q[1], Q[2]),
        col = c("red", "blue", "green", "green"))
#
legend(2.5,2.5,
       c("valore or", "media boot",
         "conf. int1", "conf. int2"),
       col = c("red", "blue", "green", "green"),
       lty = c(1,1,1,1),
       lwd = c(3,3,3,3),
       cex = 0.7)


# Metodo Bias Corrected Accelerated Bootstrap
n
B<-1000
theta<-e1071::skewness
require(bootstrap)
set.seed(1013)
CIbca<-bcanon(nervo$A, B,
              theta,
              alpha = c(0.025, 0.975))
CIbca$confpoints
CIbca$acc
CIbca$z0

hist(Tboot,
     breaks = 60,
     freq=FALSE,
     main = "Distribuzione bootstrap con 1000 realizzazioni bootstrap",
     xlab = "Indice di asimmetria",
     ylim = c(0,4),
     col = "gray",
     ylab = " Densità",
     xlim = c(1, 3))
#
abline( v = c(sk,
              sB,
              Q[1],
              Q[2],
              CIbca[["confpoints"]][3],
              CIbca[["confpoints"]][4]),
        col = c("red",
                "blue",
                "green",
                "green",
                "pink",
                "pink"))
#
legend(2.6,2.5,
       c("valore or",
         "media boot",
         "conf. int1",
         "conf. int2",
         "conf. abc1",
         "conf. abc2"),
       col = c("red",
               "blue",
               "green",
               "green",
               "pink",
               "pink"),
       lty = c(1,1,1,1,1,1),
       lwd = c(3,3,3,3,3,3),
       cex = 0.7)



# Stimare del rischio relativo: intervalli di confidenza bootstrap
# Esempio 1:
X <- rep(c("aspirina","placebo"), c(11037,11034))
head(X)
table(X)
Y = rep(c("NO", "SI", "SI", "NO"),
        c(11037-104, 104, 189,11034-189))
head(Y)
#> [1] "NO" "NO" "NO" "NO" "NO" "NO"
table(Y)

dataR <- data.frame(X,Y)
head(dataR)
table(dataR)
CC <- prop.table(table(dataR),1); CC
RR <- CC[4]/CC[3]; RR
B <- 2000
RRB <- rep(0,B)
n <- dim(dataR)[1]
set.seed(1023)
for(i in 1:B){
  ind <-sample(1:n,
               size = n,
               replace = TRUE)
  datB <- dataR[ind,]
  CC <- prop.table(table(datB),1)
  RR <- CC[4]/CC[3]
  RRB[i]<-RR
}
head(RRB)

summary(RRB)

sd(RRB)

sB<- mean(RRB)
hist(RRB,
     breaks = 60,
     freq=FALSE,
     main = "Dist. boot. Rischio Relativo",
     xlab = "2000 realizzazioni bootstrap",
     ylim = c(0,2.5),
     col = "gray",
     ylab = " Densità",
     xlim = c(1, 3))

# Metodo del percentile
Q <- quantile(RRB, c(0.025,0.975)); Q
Tm <- mean(RRB); Tm
#> [1] 1.835642
hist(RRB,
     main = "Distr. Boot. per il rischio Relativo",
     breaks=60,
     freq=FALSE,
     ylab="Densità",
     xlab="2000 Realizzazioni bootstrap")
abline(v=c(RR, Tm, Q[1], Q[2]),col=c("red","blue", "violet","violet"))
legend(2.5, 1.5,
       c("mediaOR","mediaB", "conf.int1", "conf.int2"),
       col=c("red","blue", "violet", "violet"),
       lty=c(1,1,1,1),
       cex=0.7)


# Metodo Bca
thetaR <- function(ind){
  datB <- dataR[ind,]
  CC <- prop.table(table(datB),1)
  CC[4]/CC[3]
}
require(bootstrap)
set.seed(1023)
CIBca <- bcanon(1:n,
                B,
                thetaR,
                alpha=c(0.025,0.975))
CIBca$confpoints

CIBca$acc

CIBca$z0

Tm <- mean(RRB); Tm

hist(RRB,
     main = "Distr. Boot. per il Rischio Relativo",
     breaks=60,
     freq=FALSE,
     ylab="Densità",
     xlab="2000 Realizzazioni bootstrap")
abline(v=c(RR, Tm, Q[1], Q[2],
           CIBca[["confpoints"]][3],
           CIBca[["confpoints"]][4]),
       col=c("red","blue", "violet","violet", "green", "green"))
legend(2.5, 1.8,
       c("mediaOR",
         "mediaB",
         "c.i1 p.",
         "c.i2 p.",
         "c.i1 bca",
         "c.i2 bca"),
       col=c("red","blue", "violet", "violet", "green", "green"),
       lty=c(1,1,1,1),
       cex=0.5)