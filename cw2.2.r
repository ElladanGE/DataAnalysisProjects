library(ggplot2)

#read our csv file
seavington = read.csv("seavington2020.csv")

#Non-zero rainfall amounts
non_zero = subset(seavington, prcp > 0)
non_zero_vec = non_zero[2]

#Function to evaluate CDF
cdf <- function(y, alpha, beta){
  1-exp(-(beta*y)^alpha)
}

#Function to evaluate inverse of CDF
inv_cdf <- function(p, alpha, beta){
  (-log(1-p))^(1/alpha)/beta
}

cdf_seavington = cdf(non_zero_vec, 0.775, 0.272)
inv_cdf_seavingtion = inv_cdf(cdf_seavington, 0.775, 0.272)

#Plotting QQ-plot
cdf_params = list(alpha = 0.775, beta = 0.272)
ggplot(non_zero, aes(sample = prcp)) + 
  stat_qq(distribution = inv_cdf, dparams = cdf_params) + geom_abline()


#Pearsons Chi-Squared test
bins = c(0, 5, 10, 15, 20, 25, 30 , 35 ,40, Inf)
O <- table(cut(non_zero$prcp, bins, right = FALSE))
Phi <- cdf(bins, 0.775 , 0.272)
E <- length(non_zero$prcp) * diff(Phi)
J <- length(bins-1)
contingency <- rbind(Observed = O, Expected = E)
print(contingency, digits = 4)
test_stat <- sum((O - E)^2 / E)
1 - pchisq(test_stat, J - 3)


#Kernel Density square-root function
kde_sqrt <- function(y, y_i, w, h){
  d <- outer(sqrt(y), sqrt(y_i), FUN="-")
  rowMeans(w(d/h))/(h*2*sqrt(y))
}


#Estimating Seavington rainfall
w_normal <- function(u) dnorm(u)
h <- 0.3
y_plot <- pretty(range(non_zero_vec) + h*c(-1,1), 200)
y_plot <- subset(y_plot, y_plot > 0)
f_hat <- kde_sqrt(y_plot, non_zero$prcp, w_normal, h)
ggplot() + geom_line(aes(x = y_plot, y = f_hat )) +
  labs(x = "y", y = expression(hat(f[h])(y))) + xlim(0, 40) +
  ggtitle("Estimate of pdf of non-zero rainfall amounts based on kernel density 
          on square-root transformed rainfall amounts")

























