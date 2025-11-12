
library('plot.matrix')
# numeric matrix

x <- matrix(runif(35), ncol=5) # create a numeric matrix object
x<-matrix(runif(500), nrow = 25, ncol = 25, byrow = FALSE)

N<-10
M<-10
x<-matrix(rnorm(N*M,mean=10,sd=1), N, M) 





n <- 20
p<- 0.5
x <- 1:n
z<-dbinom(x,n,p)
hist(z)
x<-matrix(dbinom(x,n,p), n,n) 


x<-(log(x)*-1)
hist(x)

x<-matrix(runif(400), nrow = 20, ncol = 20, byrow = FALSE)


N<-2
M<-2
x<-matrix(rnorm(N*M,mean=10,sd=1), N, M) 
hist(x)
x<-ceiling(x)
class(x)
#> [1] "matrix" "array"
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(x,col=topo.colors)





---
  # https://www.davidzeleny.net/anadat-r/doku.php/en:data:bci
  # https://www.davidzeleny.net/anadat-r/doku.php/en:data:bci:script-soil
library(vegan)
data(BCI)
data(BCI.env)
BCI.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/bci.env.txt', row.names = 1)
BCI.soil <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/bci.soil.txt')


# soil20x20 <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/bci-soil-20x20.txt')
# BCI.soil <- NULL
# for (x in seq (10, 990, by = 100))
#   for (y in seq (10, 490, by = 100))
#     BCI.soil <- rbind (BCI.soil, colMeans (soil20x20[soil20x20$x >= x & soil20x20$x < (x+100) & soil20x20$y >= y & soil20x20$y < (y+100),]))
# BCI.soil <- as.data.frame (BCI.soil)
# write.table (BCI.soil, file = 'BCI.soil.txt', sep = '\t')


soil20x20 <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/bci-soil-20x20.txt')
filled.contour (matrix (soil20x20$pH, ncol = 25, byrow = T), color.palette = terrain.colors, main = 'pH')
filled.contour (matrix (soil20x20$N, ncol = 25, byrow = T), color.palette = terrain.colors, main = 'Nitrogen')

filled.contour (matrix (soil20x20$N, ncol = 25, byrow = T), color.palette = terrain.colors, main = 'Nitrogen')

