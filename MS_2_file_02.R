
# Distribuzione di Gauss Bivariata
# Generazione di determinazioni dalla Normale Bivariata

require(mvtnorm)
sigma1 <- matrix(c(3,2,2,3), ncol=2); sigma1

set.seed(1234)
n <- 10
x <- rmvnorm(n,
             mean=c(0,0),
             sigma=sigma1)
x

cov(x)
cor(x)


set.seed(1234)
n <- 2000
x <- rmvnorm(n, mean=c(0,0), sigma=sigma1)
cov(x)
cor(x)

skim_without_charts(x)
cov(x)
cor(x)
sd(x[,1])
sd(x[,2])

# Diagrmma a dispersione dei valori ottenuti
plot(x[,1],
     x[,2], main = "Realizzazioni da Normale Bivariata N(0,0,3,3,2)")


# Curve di levello
x1 <- x2 <- seq(-10, 10, length = 51)
dens <- matrix(dmvnorm(expand.grid(x1, x2),
                       sigma = sigma1),
               ncol = length(x1))
contour(x1,
        x2,
        dens,
        main = "Livelli della dist. N(0,0,3,3,2)",
        col="blue",
        xlab = "x1",
        ylab = "x2")



# Algoritmo Expectation-Maximization - EM
y <- matrix(c(10,15, 17, 22, 23, NA),2,3,byrow=TRUE); y

em1 <- function(y23, y){
  ystar <- y
  ystar[2,3] <- y23
  mu.hat <- mean(ystar)
  alpha.hat <- apply(ystar, MAR = 1,
                     mean) - mean(ystar)
  beta.hat <- apply(ystar, MAR = 2,
                    mean) - mean(ystar)
  y23 <- mu.hat + alpha.hat[2] + beta.hat[3]
  return(c(mu = mu.hat,
           alpha = alpha.hat,
           beta = beta.hat,
           y23 = y23))
}

em1(21,y)


set.seed(1832)
em.step <- function(y, epsilon= 1e-8){
  trace <- NULL
  convergenza <- FALSE
  trace <- t(em1(y23 = mean(y, na.rm = TRUE), y = y))
  y23id <- grep("y23", colnames(trace))
  h <- 0
  while(!convergenza){
    h <- h + 1
    trace <- rbind(trace,
                   em1(y23 = trace[h, "y23"],
                       y = y))
    convergenza <- (dist(trace[h:(h+1), -y23id]) < epsilon)
  }
  return(trace)
}

em.step(y)



# trace plot
ris<- em.step(y)
matplot(ris[,-7], type = "l")

names1 <- expression(mu,
                     alpha[1],
                     alpha[2],
                     beta[1],
                     beta[2],
                     beta[3])
pal1<- c("red", "yellow", "green", "violet", "blue", "orange")
matplot(ris[,-7],
        type = "l",
        col = pal1,
        lwd = 2,
        lty = 1,
        xlab = "Iterazioni dell'algoritmo EM",
        ylab = "Stime dei parametri del modello")
legend(x = 0,
       y = 15,
       legend = names1,
       lwd = 2 ,
       col = pal1,
       lty = 1,
       horiz=TRUE,
       cex=0.8)


# Densita miscuglio di componenti Gaussiane
funcmxn <- function(x, p, mu, sd){
  f1 <- dnorm(x, mu[1], sd[1])
  f2 <- dnorm(x, mu[2], sd[2])
  f <- p*f1 + (1-p)*f2
  f
}

# Scenario 1:
mu1 <- c(1,4)
sd1 <- c(1,1)
p1 <- 0.4
funcmxn(0.5,
        p1,
        mu1,
        sd1)


y1 <- seq(-5,10,0.01)
length(y1)
#> [1] 1501
pr1 <- funcmxn(x = y1,
               p = p1,
               mu = mu1,
               sd = sd1)
require(skimr)
skim_without_charts(pr1)

plot(y1,
     pr1,
     xlab = "y",
     ylab="Densita'",
     lwd=3,
     col="lightsteelblue1",
     type = "l",
     main="Miscuglio di N(1,1) e N(4,1) con peso 0.4")


# Scenario 2:
mu2 <- c(4,4)
sd2 <- c(1,8)
p2 <-0.1
set.seed(1235)
funcmxn(0.5,
        p2,
        mu2,
        sd2)

y2 <- seq(-30,40,0.01)
pr2 <- funcmxn(x = y2,
               p = p2,
               mu = mu2,
               sd = sd2)
skim_without_charts(pr2)

plot(y2,
     pr2,
     xlab = "y",
     ylab="Densità miscuglio",
     lwd=3,
     col="orange",
     type = "l",
     main="Miscuglio di N(4,1) e N(4,64) con peso 0.1 ")

# Scenario 3:
mu3 <- c(0,0)
sd3 <- c(1,3)
p3 <-0.5
funcmxn(0.5,
        p3,
        mu3,
        sd3)

y3 <- seq(-10,20,0.01)
pr3 <- funcmxn(x = y3,
               p = p3,
               mu = mu3,
               sd = sd3)
skim_without_charts(pr3)

plot(y3,
     pr3,
     xlab = "y",
     ylab="Densità",
     lwd=3,
     col="Pink",
     type = "l",
     main="Miscuglio di N(0,1) e N(0,9) con peso 0.5"
)


# Stima dei parmetri del modello miscuglio
require('mclust')

# Modello miscuglio univariato con due componenti Gaussiane
load('datacol.Rdata')
dim(datacol)
head(datacol)
require(skimr)
skim_without_charts(datacol)

table(datacol$sex)

require(dplyr)
datacol%>%
  dplyr::group_by(sex) %>%
  skim_without_charts()

# oppure
# tapply(datacol$cholst, datacol$sex, summary)
# tapply(datacol$cholst, datacol$sex, sd)

n <-dim(datacol)[1]
with(datacol,
     symbols(x=1:n,
             y=cholst,
             circles=sex,
             inches=1/30 ,
             xlab = "id",
             ylab = "Colesterolo",
             bg="red",
             fg=NULL))








# Stima dei parametri del modello miscuglio

require('mclust')

mod1 <- Mclust(datacol$cholst,
               G = 2,
               modelNames = "E")
control = emControl(tol = 1.e-6)

summary(mod1)

summary(mod1,parameters = TRUE)

# Classificazione delle unita
head(mod1$z)

head(apply(mod1$z,1,which.max))

plot(mod1,
     what='classification', xlab = "colesterolo")


class<-mod1$classification; head(class)

table(class,datacol$sex)

# Rappresentazione della densità miscuglio
plot(mod1,
     what='density', xlab = "Colesterolo")

# Scelta del numero delle componneti
bayesinf <- mclustBIC(datacol$cholst)
bayesinf


plot(bayesinf)

# Modello miscuglio multivariato con componenti Gaussiane

load("data.Rdata")
head(data)
require(skimr)
skim_without_charts(data)
cov(data)
cor(data)

plot(data$Y1.1, data$Y2.1, xlab = "Globuli Bianchi",
     ylab = "Emoglobina", col = "orange")

plot(data$Y2.1, data$Y1.1,
     xlab = "Emoglobina", ylab = "Globuli Bianchi", col = "blue")


# Selezione del numero delle componenti
require(mclust)
mcc <-Mclust(data, modelNames = c("EII", "VII"))
mcc$BIC


# Stima dei parametri
mc <-Mclust(data,
            G = 3,
            modelNames = c("EII"))
summary(mc,parameters = TRUE )
#> ----------------------------------------------------

plot(mc,"classification", xlab = "globuli bianchi",
     ylab = "emoglobina")


plot(mc,"density", xlab = "globuli bianchi", ylab = "emoglobina")


# Modello miscuglio con componenti sferiche e varianze specifiche per ogni componente
mc1 <-Mclust(data, G = 3, modelNames = c( "VII"))
summary(mc1, parameters = TRUE)

plot(mc1,"classification")


# Modello miscuglio non sferico
mc2 <-Mclust(data, G = 3, modelNames = c( "VEE"))
summary(mc2, parameters = TRUE)

plot(mc2,"classification")


# Bootstrap per errori standard e intervalli di confidenza
bootClust <- MclustBootstrap(mc)

summary(bootClust, what = "se")

summary(bootClust, what = "ci")


# Modello a classi latenti
load("psico.Rdata")
dim(Y)
head(Y)

n<-dim(Y)[1]
apply(Y,2,table)/n


require(MultiLCIRT)
Yout <- aggr_data(Y)
S <- Yout$data_dis;S
yv <- Yout$freq; yv
cbind(S,yv)
mod2 <- est_multi_poly(S,yv,k=2)
summary(mod2)
mod2$np
mod2 <- est_multi_poly(S,yv,k=2,
                       output = TRUE)

