
setwd("C:/Users/UTENTE/OneDrive/Desktop/MS_2")
getwd()

# Modello Beta_Binomiale: 
## esempio Ipertensione
# caompione = 20, 13-ipertesi
# stimare la proporzione nella popolazione
# si desegna la posteriori in base a priori

# scenari:
# distribuzione a priori uniforme alpha = 1, beta = 1
# dist.a priori a Jeffrey alpha = beta = 0.5
# distribuzione a priori alpha, beta > 1

alpha <- 1
beta <- 1

pval <- seq(0, 1, by= 0.01)
# distribuzione a priori

plot(pval, dbeta(pval, alpha, beta),
     type = 'l', col="blue", ylim = c(0,6),
     ylab = "Densita",
     xlab = "theta",
     main = "Scenario 1: alpha = beta = 1")

# verosimiglianza
n <- 20
k <- 13
lines(pval, dbeta(pval, k+1, n-k+1),
      lwd = 2,
      col = "darkblue")

# distribuzione a posteriori
lines(pval, dbeta(pval, k + alpha, n-k+beta),
      lwd = 1,
      col = "red")

legend("topleft",
       c("a Priori", "Verosomiglianza", "a Posteriori"),
       lty = c(1,1,1),
       lwd = c(1,2,1),
       col = c("blue", "darkblue", "red"),
       cex = 0.6)

# parametro == proporzione di ipertesi nella popolazione
(alpha + k) / (n + alpha + beta)
# la moda
(alpha +  k - 1) / (n + alpha + beta - 2)


# Scenario 2:
# alpha beta = 0.5

alpha <- 0.5
beta <- 0.5
# distribuzione a priori
pval<-seq(0,1,by=0.01)
plot(pval, dbeta(pval,alpha,beta),
     type = "l", col = "blue", ylim = c(0,6),
     ylab = "Densità", xlab = "theta",
     main = "Scenario 2: alpha = beta = 0.5")
# verosimiglianza
lines(pval, dbeta(pval, k+1, n-k+1),
      lwd = 2,
      col = "darkblue")
# distribuzione a posteriori
lines(pval, dbeta(pval, k+alpha, n-k+beta),
      lwd = 1,
      col = "red")
legend("topleft",
       c("a Priori","Verosimiglianza","a Posteriori"),
       lty=c(1,1,1),
       lwd=c(1,2,1),
       col=c( "blue","darkblue","red" ), cex=0.6)

# valore atesso
(alpha + k)/(n+alpha + beta)
# la moda
(alpha + k -1)/(n + alpha + beta -2)


# Scenario 3:
# a priori = molto informativa
alpha <- 3.26
beta<- 7.19
# distribuzione a priori
pval<-seq(0,1,by=0.01)
plot(pval, dbeta(pval,alpha,beta),
     type = "l", col = "blue", ylim = c(0,6),
     ylab = "Densità", xlab = "theta",
     main = "Scenario 3: alpha = 3.26, beta = 7.19")
# verosimiglianza
lines(pval, dbeta(pval, k+1, n-k+1),
      lwd = 2,
      col = "darkblue")
# distribuzione a posteriori
lines(pval, dbeta(pval, k+alpha, n-k+beta),
      lwd = 1,
      col = "red")
legend("topleft",
       c("a Priori","Verosimiglianza","a Posteriori"),
       lty=c(1,1,1),
       lwd=c(1,2,1),
       col=c( "blue","darkblue","red" ), cex=0.6)

(alpha + k)/(n+alpha + beta)

(alpha + k -1)/(n + alpha + beta -2)



##############

# Specificazione della distribuzione a priori
library(LearnBayes)
quantile1 <- list(p = 0.5, x=0.3); 
quantile1

quantile2 <- list(p = 0.9, x=0.5); 
quantile2

require(LearnBayes)
beta.select(quantile1, quantile2)

# p(theta) ¬ Beta(3.26, 7.19)

M <- 3.26/(3.26+7.19); 
M


# 1. Determinazione della distribuzione a posteriori


alpha1 <- 3.26
beta1 <- 7.19

n1 <- 12
k1 <- 6

# a priori
pval <- seq(0,1, by=0.01)
plot(pval, dbeta(pval,alpha1, beta1),
     xlab="p",
     ylab="Densità",
     ylim=c(0,7),
     lty=3,
     lwd=4,
     col="green")
# verosimiglianza
lines(pval,dbeta(pval, k1+1,n1-k1+1),
      lty = 2,
      lwd = 1, col = "darkblue")
# a posteriori
lines(pval, dbeta(pval,
                  k1 + alpha1,
                  n1-k1+beta1),
      lty=1,lwd=4,
      col="orange")
legend("topright",
       c("a Priori","Verosimiglianza","a Posteriori"),
       lty=c(1,3,1),
       lwd=c(3,3,3),
       col=c("green", "blue","orange" ),
       cex = 0.7)



# 2. Aggiornamento della distribuzione a posteriori
k <- 19 + k1
n<- 39
# a posteriori aggiornata
plot(pval, dbeta(pval,
                 alpha1 + k,
                 beta1+n-k),
     xlab="p",
     type = "l",
     ylab="Densità",
     ylim=c(0,7),
     lty=1,
     lwd=3,
     col="blue")

# a posteriori campione 1
lines(pval, dbeta(pval,
                  k1 + alpha1,
                  n1-k1+beta1),
      lty=1,lwd=3,
      col="orange",
      )
legend("topleft",
       c("a Posteriori aggiornata", "a Posteriori camp. 1"),
       lty=c(1,1),
       lwd=c(3,3),
       col=c("blue", "orange" ),
       cex = 0.6)

(alpha1+k)/(n+alpha1 + beta1)



# Esempio Bayes Billiard Balls
# pagina 20 di teoria

# beta( alpha = 1, beta = 1) = uniforme in[0,1]
nsim <- 10^5
set.seed(1234)
p <- rbeta(nsim, shape1 = 1, shape2 = 1)
length(p)
head(p)


n <- 10
set.seed(1234)
pxk <- rbinom(1,n,p)
head(p)
head(pxk)


n <- 10
set.seed(1234)
pxk <- rbinom(nsim,n,p)
head(pxk)


h <- hist(pxk,
          breaks = seq(-0.5,n+0.5,1),
          freq=FALSE,
          ylim = c(0,0.20),
          xlab = "Numero palline blu a sinistra della gialla ",
          ylab= "Densita'",
          main = expression("Distribuzione Marginale")
)

h$density



# Modello Gausiano: misurazione del farmaco FFV 1
# 1. specificazione dei parametri della distribuzione a priori - iper-parametri
# 5 percentile = 100
# = 180

# a priori p(theta) ¬ N(mu, tao^2)

quantile1 <- list(p = 0.05, x=100); quantile1
quantile2 <- list(p = 0.7, x=180); quantile2

require(LearnBayes)
normal.select(quantile1, quantile2)

# N(161,37^2)

# 1. Informazion campionarie
# 31 rilevazioni 
# valore medio 135
# dev.std = 12
# f(x, theta) ¬ N( thao, sigma^2/n)

# 2. Distribuzione a posteriori
# p(theta / x)¬ N(mu_1, thao_1 ^2)

# stima del valore attesso
n <- 31
mu1<- (161*12^2 + (n*135*37^2))/(n*37^2 + 12^2)
mu1

# varianza a posteriori
tau12<- ((37^2)*12^2)/(n*37^2+12^2)
tau12
# oppure
1/(31/12^2 + 1/37^2)


tau1<-sqrt(tau12)
tau1

# Rapresantazione grafica: scenario 1
# distribuzione a priori
mu <- 161
tau <- 36.879
curve(dnorm(x,mu,tau),
      xlab = expression(theta),
      ylab="Densità",
      xlim=c(100,200),
      ylim = c(0,0.2),
      lwd=4,
      col="green")
n<-31
# verosimiglianza della media campionaria
xbar <- 135
sigma <- 12
ss<-sigma/sqrt(n)
curve(dnorm(x,xbar,ss),
      lwd=4,
      col= "blue",
      add=TRUE)
# distribuzione a posteriori
curve(dnorm(x,mu1,tau1),
      lwd=1,
      col="orange",
      add=TRUE)
legend("topleft",
       c("a Priori","Verosimiglianza","a Posteriori"),
       lwd=c(4,2,4),
       cex = 0.7,
       col=c("green", "blue","orange" ))





# Prior maggiormente informativa
mu1 <- (161*12^2 + (n*135*1^2))/(n*1^2 + 12^2); mu1
tau12 <- ((1^2)*12^2)/(n*1^2+12^2);
tau12

tau1 <-sqrt(tau12)


# Scenario : 2
# distribuzione a priori
mu <- 161
tau <- 1
curve(dnorm(x,mu,tau),
      xlab= expression(theta),
      ylab="Densita'",
      xlim=c(100,170),
      ylim = c(0,0.6),
      lwd=4,
      col="green")
# verosimiglianza
sigma <- 12
n<-31
ss<-sigma/sqrt(n)
curve(dnorm(x,xbar,ss),
      lwd=2,
      col= "blue",
      add=TRUE)
# distribuzione a posteriori
curve(dnorm(x,mu1,tau1),
      lwd=4,
      col="orange",
      add=TRUE)
legend("topleft",
       c("a Priori","Verosimiglianza","a Posteriori"),
       lwd=c(4,2,4),
       col=c("green", "blue","orange" ), cex = 0.7)



# Numerosita campionaria ridotta
n1 <- 6
mu11<- (161*12^2 + (n1*135*37^2))/(n1*37^2 + 12^2); mu11

tau121<- ((37^2)*12^2)/(n1*37^2+12^2);
tau121

tau11<-sqrt(tau121)

# distribuzione a priori
mu <- 161
tau <- 37
curve(dnorm(x,mu,tau),
      xlab = expression(theta),
      ylab="Densita'",
      xlim=c(100,165),
      ylim = c(0,0.15),
      lwd=4,
      col="green")
# verosimiglianza
sigma <- 12
ss<-sigma/sqrt(n1)
curve(dnorm(x,xbar,ss),
      lwd=2,
      col= "blue",
      add=TRUE)
# distribuzione a posteriori
curve(dnorm(x,mu11,tau121),
      lwd=4,
      col="orange",
      add=TRUE)
legend("topleft",
       c("a Priori","Verosimiglianza","a Posteriori"),
       lwd=c(4,2,4),
       col=c("green", "blue","orange" ), cex = 0.7)


# Distribuzione predittiva
curve(dnorm(x,mu11,sqrt((tau12 + sigma^2))),
      xlab = expression(theta),
      ylab="Distribuzione predittiva",
      xlim=c(90,180),
      ylim = c(0,0.04),
      lwd=4,
      col="pink")


# Confronto tra distribuzione a priori: Gauss e t di Student
require(LearnBayes)
quantile1 <- list(p=.5,x=100);
quantile2 <- list(p=.95,x=120)
ris <- normal.select(quantile1, quantile2);
mu <- ris$mu; mu

tau <- ris$sigma; tau

sigma <- 15
xnn <- c(110, 125, 140)
n<-4
# deviazione standard
tau12 <- 1/(n/sigma^2 + 1/tau^2); tau12

# media
mu1 <- (mu*sigma^2 + (n*xnn*tau^2))/(n*tau^2 + sigma^2); mu1

summ1 <- cbind(xnn, mu1, tau12)
summ1


# Distribuzione a priori T di Student
qt(0.95,2)

taut <- 20/qt(0.95,2); taut

# Confronto tra due distribuzioni
curve(1/taut*dt((x-mu)/taut,2),
      from=60,
      to=140,
      xlab = expression(theta),
      ylab ="Densità",
      main ="Confronto tra distribuzioni a priori",
      col ='purple',
      lwd=3)
curve(dnorm(x,mean=mu,sd=tau),
      add=TRUE,
      lwd=1)
legend("topright",
       legend=c("t di Student","Gaussiana"),
       lwd=c(3,1),
       col=c("purple", "black"))


# Determinazione della distribuzione a posteriori
theta <- seq(60, 180, length = 500)
summary(theta)

n <- 4
like <- dnorm(theta,mean=xnn,sd=sigma/sqrt(n))
summary(like)

prior <- dt((theta - mu)/taut, 2)
post <- prior * like
post <- post/sum(post)
posizione <- sum(theta * post)
scala <- sqrt(sum(theta^2 * post) - posizione^2)


norm.t.compute <- function(xnn){
  theta <- seq(60, 180, length = 500)
  like <- dnorm(theta,mean=xnn,sd=sigma/sqrt(n))
  prior <- dt((theta - mu)/taut, 2)
  post <- prior * like
  post <- post/sum(post)
  posizione <- sum(theta * post)
  scala <- sqrt(sum(theta^2 * post) - posizione^2)
  c(xnn, posizione, scala)
}

summ2<- t(sapply(c(110, 125, 140),norm.t.compute))
dimnames(summ2)[[2]] =
  c("xnn","mu1 t","tau1 t")
summ2


cbind(summ1,summ2)


# Modello coniugato Poisson - Gamma
# apriori: p(alpha) ¬ Gamma(alpha, beta)

alpha <- 16
beta <- 15174
curve(dgamma(x,alpha,beta),
      xlab="lambda",
      ylab="Densità",
      type = "l",
      xlim=c(0,0.0025),
      lwd=4,
      col="green",
      main ="Distribuzione a priori")

# distribuzione di Poisson
success <- 0:3
la <- 1/66
plot(success, dpois(success, lambda=la), type = "h",
     lwd=4,
     col = "red")

# a Postriori
n<-66
alpha1<- alpha + 1; alpha1

beta1 <- beta + n; beta1
curve(dgamma(x,alpha1,beta1),
      lty=1,
      lwd=4,
      ylab="Densità",
      xlim=c(0,0.002),
      col="orange",
      main = " ")
# prior
curve(dgamma(x, alpha, beta),
      lty=1,lwd=1,
      col="green",
      add=TRUE)
legend("topright",
       c("a Posteriori", "a priori"),
       lwd=c(3,3),
       cex = 0.6,
       col=c("orange","green" ))

Elambda <- (alpha+1)/(beta+n); Elambda

sqrt(alpha1/((beta1)^2))


# Distribuzione preditiva
ex <- 1767
ys <- 0:10
(alpha/beta)*ex

pyn1 <- dpois(ys, (alpha/beta)*ex)*
  dgamma(alpha/beta, shape = alpha, rate = beta)/
  dgamma(alpha1/beta1, shape = alpha1 + ys,
         rate = beta1 + ex)
cbind(ys, round(pyn1, 3))