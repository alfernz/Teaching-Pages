# A Humble Vector
x <- c(5,0,4,0,6,5,8)
x

# Matrix
x1 <- c(14,-34)
x2 <- c(-4, 24)
X <- cbind(x1,x2)
# Invers
solve(X)
invX <- solve(X)
X %*% invX
round(X %*% invX, 3)


# regression with matrices
weight <- c(73,	72,	81,	73,	70,	71,	75,	80,	73,	68,	62,	70,	63,	77,	76,	78,	71,	76,	82,	80)
height <- c(175,175,189,180,175,185,178,186,180,178,174,180,172,180,180,180,182,178,179,182)

# matrix
X <- cbind(rep(1, 20), height)

# get beta with (X'X)^{-1}X'y
XprimeX <- crossprod(X)
XprimeXinv <- solve(XprimeX)
XprimeXinvXprime <- XprimeXinv %*% t(X)
XprimeXinvXprimey <- XprimeXinvXprime %*% weight
XprimeXinvXprimey

beta <- solve(crossprod(X)) %*% crossprod(X, weight)
beta

# predicted values
y_hat <- X %*% beta
# residuals
e <- weight - y_hat

head(y_hat, 5)
head(e, 5)

# get residual VCov
RSS <- t(e) %*% e
sigma <- RSS/(20-2)
Vcov <- as.numeric(sigma) * (solve(crossprod(X)))

# get standard errors
se <- sqrt(diag(Vcov))
se

# confidence intervals
beta[2] - se[2]*qt(0.975, 18)
beta[2] + se[2]*qt(0.975, 18)


# simpler regression
rovers <- data.frame(height, weight)
model1 <-lm(weight ~ height, data = rovers)
summary(model1)

# Bonus Scatterplot of Data
library(ggplot2)
library(ggrepel)

# add labels
rovers$name <- c("J. Byrne","G. Burke","L. Grace","G. Bolger","A. McEneff","R. Finn",
                 "G. Cummins","R. Lopes","A. Greene","D. Watts","S. Kavanagh","J. O'Brien",
                 "B. Kavanagh","D. Carr","G. O'Neill","E. Boyle","O. Vojic","N. Farrugia",
                 "T. Oluwa","S. Callan")

ggplot(data = rovers, aes(x = height, y = weight, label = name)) +  
  theme_minimal(base_size = 20, base_family = "serif") +
  geom_point(color="darkblue", alpha = 0.25, size = 2) + 
  geom_smooth(se= F, size = 1, col = "red", method="lm")+
  geom_text_repel() + 
  xlab("Height (cm)") + 
  ylab("Weight (kg)") 









