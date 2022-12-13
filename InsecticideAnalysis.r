library(ggplot2)
library(dplyr)
#We read in our csv file
mosquito = read.csv("mosquito2021.csv")
#We make sure dose is considered as a factor from low to high
mosquito$dose = factor(mosquito$dose, levels = c("low", "medium", "high"))

#We arrange by dose
arrange(mosquito, dose)

#Making a plot to compare effectivness of insecticide
ggplot(data = mosquito, aes(x=insecticide, y=hours10, color = block, shape = dose))+ 
  geom_jitter(size=2) + ylab("Survival time (in 10s of hours)") + xlab("Insecticide") + 
  ggtitle("Effectivness of different types and doses of insecticide on mosquitoes")

#Sample size
length(mosquito$X)

#AOV analysis exploration
summary(aov(hours10~dose, data = mosquito))
summary(aov(hours10~insecticide, data = mosquito))
summary(aov(hours10~block, data = mosquito))
summary(aov(hours10~dose+block, data = mosquito))
summary(aov(hours10~insecticide+block, data = mosquito))
summary(aov(hours10~dose+insecticide, data = mosquito))
summary(aov(hours10~dose+insecticide+block, data = mosquito))
summary(aov(hours10~insecticide*dose*block, data = mosquito))
summary(aov(hours10~dose+insecticide*block, data = mosquito))
summary(aov(hours10~dose*block+insecticide, data = mosquito))
summary(aov(hours10~dose*insecticide, data = mosquito))


#3Way test
summary(aov(hours10~dose*insecticide+block, data = mosquito))

#Final ANOVA choice
final_anova <- summary(aov(hours10~block+insecticide*dose, data = mosquito))
capture.output(final_anova, file = "anova results.doc")

#Linear model
lm_model <- summary(model <- lm(hours10~insecticide*dose, data = mosquito))
capture.output(lm_model, file = "lm_results.doc")
lm_model

#Fitted vs Residuals
library(ggfortify)
p <- autoplot(lm_model, which = c(1:3, 6)) # diagnostic plots
p[1] <- p[1] + ggtitle("Residuals vs Fitted")
print(p)



