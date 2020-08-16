library(ggplot2)
z<-read.csv("C:/Users/Jayesh/Desktop/Covid data/10 countries/Full/CHILE.csv")

r<-lm(tpm~cpm, data=z)
print(r)
summary(r)
qplot(z$tpm,z$cpm,xlab="Test per Million", ylab="case per Million")
par(mfrow=c(2,2))
plot(r)
scatter.smooth(x=z$tpm, y=z$cpm, main="cpm ~ tpm",xlab="Test per Million", ylab="case per Million") 
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(z$cpm, main="cpm", sub=paste("Outlier rows: ", boxplot.stats(z$cpm)$out))  # box plot for 'speed'
boxplot(z$tpm, main="tpm", sub=paste("Outlier rows: ", boxplot.stats(z$tpm)$out))  # box plot for 'distance'


cor(z$tpm,z$cpm)

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(z$tpm), main="Density Plot: tpm", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(z$tpm), 2)))  # density plot for 'speed'
polygon(density(z$tpm), col="red")
plot(density(z$cpm), main="Density Plot: cpm", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(z$cpm), 2)))  # density plot for 'dist'
polygon(density(z$cpm), col="red")