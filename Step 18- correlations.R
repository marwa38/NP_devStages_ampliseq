# Title: Correlations
# Author: Marwa Tawfik
# Date: 03-05-2024


# Summary: as stated that the fish of growth performance and microbiota work are differnet fish
# so the average per tank (triplicate tanks/treatment/phase) was measured before the correlation analysis
# between specific growth rate and Shannon index of alpha diversity as an example. 
# the spearman correlation results showed no correlation between SGR and Shannon index at any of the phases or across phases
# however pearson correlation showed correlation between SGR and Shannon at stimulus phase. 
# We can't rely on these results as sample size of 3 is not enough to detect meeting assumptions of 
# either of the tests.

library(readxl)

sgr <- read_excel("sgr_averaged_byTank.xlsx")
shannon <- read_excel("shannon.xlsx")


# check if assumption of linear model is met or not
model <- lm(stimulus ~ treatment, data = sgr)
plot(model)
# nearly equal variance for each tested group/treatmnet (plot1)
# linearity of the model is not achieved (plot 2)

model <- lm(stimulus ~ treatment, data = shannon)
plot(model)

## stimulus
# Extract numeric vectors for treatment M and V
shannon_M <- shannon$stimulus[shannon$treatment == "M"]
shannon_V <- shannon$stimulus[shannon$treatment == "V"]

sgr_M <- sgr$stimulus[sgr$treatment == "M"]
sgr_V <- sgr$stimulus[sgr$treatment == "V"]
class(sgr_M)
# Perform correlation tests for treatment M and V
correlation_M <- cor.test(shannon_M, sgr_M, method = "spearman")
correlation_V <- cor.test(shannon_V, sgr_V, method = "spearman")

# Print the results
print("Correlation for Treatment M:")
print(correlation_M)
# Pearson's product-moment correlation
# 
# data:  shannon_M and sgr_M
# t = -16.406, df = 1, p-value = 0.03876
# alternative hypothesis: true correlation is not equal to 0
# sample estimates:
#        cor 
# -0.9981475

# Spearman's rank correlation rho
# 
# data:  shannon_M and sgr_M
# S = 8, p-value = 0.3333
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
#  -1 

print("Correlation for Treatment V:")
print(correlation_V)

# Pearson's product-moment correlation
# 
# data:  shannon_V and sgr_V
# t = -0.53243, df = 1, p-value = 0.6885
# alternative hypothesis: true correlation is not equal to 0
# sample estimates:
#        co

# Spearman's rank correlation rho
# 
# data:  shannon_V and sgr_V
# S = 6, p-value = 1
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# -0.5 

## intermediate
shannon_M <- shannon$intermediate[shannon$treatment == "M"]
shannon_V <- shannon$intermediate[shannon$treatment == "V"]

sgr_M <- sgr$intermediate[sgr$treatment == "M"]
sgr_V <- sgr$intermediate[sgr$treatment == "V"]

# Perform correlation tests for treatment M and V
correlation_M <- cor.test(shannon_M, sgr_M, method = "spearman")
correlation_V <- cor.test(shannon_V, sgr_V, method = "spearman")

# Print the results
print("Correlation for Treatment M:")
print(correlation_M)
# Pearson's product-moment correlation
# 
# data:  shannon_M and sgr_M
# t = -1.6653, df = 1, p-value = 0.3443
# alternative hypothesis: true correlation is not equal to 0
# sample estimates:
#       cor 
# -0.857309

# Spearman's rank correlation rho
# 
# data:  shannon_M and sgr_M
# S = 6, p-value = 1
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# -0.5 

print("Correlation for Treatment V:")
print(correlation_V)
# Pearson's product-moment correlation
# 
# data:  shannon_V and sgr_V
# t = -0.056261, df = 1, p-value = 0.9642
# alternative hypothesis: true correlation is not equal to 0
# sample estimates:
#         cor 
# -0.05617205 

# Spearman's rank correlation rho
# 
# data:  shannon_M and sgr_M
# S = 6, p-value = 1
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 

## challenge 
shannon_M <- shannon$challenge[shannon$treatment == "M"]
shannon_V <- shannon$challenge[shannon$treatment == "V"]

sgr_M <- sgr$challenge[sgr$treatment == "M"]
sgr_V <- sgr$challenge[sgr$treatment == "V"]

# Perform correlation tests for treatment M and V
correlation_M <- cor.test(shannon_M, sgr_M, method = "spearman")
correlation_V <- cor.test(shannon_V, sgr_V, method = "spearman")

# Print the results
print("Correlation for Treatment M:")
print(correlation_M)
# Pearson's product-moment correlation
# 
# data:  shannon_M and sgr_M
# t = -6.0534, df = 1, p-value = 0.1042
# alternative hypothesis: true correlation is not equal to 0
# sample estimates:
#        cor 
# -0.9866282 

# Spearman's rank correlation rho
# 
# data:  shannon_M and sgr_M
# S = 8, p-value = 0.3333
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
#  -1

print("Correlation for Treatment V:")
print(correlation_V)
# Pearson's product-moment correlation
# 
# data:  shannon_V and sgr_V
# t = -0.61869, df = 1, p-value = 0.6473
# alternative hypothesis: true correlation is not equal to 0
# sample estimates:
#        cor 
# -0.5261332 

# Spearman's rank correlation rho
# 
# data:  shannon_V and sgr_V
# S = 6, p-value = 1
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# -0.5 

## M and V fish across phases 
sgr <- read_excel("sgr_averaged_byTank_devStages.xlsx")
shannon <- read_excel("shannon_devStages.xlsx")

# check if assumption of linear model is met or not
model <- lm(sgr ~ treatment, data = sgr)
plot(model)
# nearly equal variance for each tested group/treatmnet (plot1)
# linearity of the model is not achieved (plot 2)

model <- lm(shannon ~ treatment, data = shannon)
plot(model)

## M an V fish (not treatment here) so it is across devStages
sgr_M <- sgr$sgr[sgr$treatment == "M"]
sgr_V <- sgr$sgr[sgr$treatment == "V"]

shannon_M <- shannon$shannon[shannon$treatment == "M"]
shannon_V <- shannon$shannon[shannon$treatment == "V"]

class(shannon_V)

# Perform correlation tests for treatment M and V
correlation_M <- cor.test(shannon_M, sgr_M, method = "spearman")
correlation_V <- cor.test(shannon_V, sgr_V, method = "spearman")

# Print the results
print("Correlation for Treatment M:")
print(correlation_M)
# Pearson's product-moment correlation
# 
# data:  shannon_M and sgr_M
# t = -16.677, df = 1, p-value = 0.03813
# alternative hypothesis: true correlation is not equal to 0
# sample estimates:
#        cor 
# -0.9982071

# Spearman's rank correlation rho
# 
# data:  shannon_M and sgr_M
# S = 8, p-value = 0.3333
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
#  -1 

print("Correlation for Treatment V:")
print(correlation_V)
# Pearson's product-moment correlation
# 
# data:  shannon_V and sgr_V
# t = 0.30523, df = 1, p-value = 0.8114
# alternative hypothesis: true correlation is not equal to 0
# sample estimates:
#       cor 
# 0.2919343 

# Spearman's rank correlation rho
# 
# data:  shannon_V and sgr_V
# S = 2, p-value = 1
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.5 


