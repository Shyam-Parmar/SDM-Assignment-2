###########################################
# Assignment 2 ############################

rm(list = ls())

# install packages
pacman::p_load(dplyr, ggplot2, readxl, tidyverse, mlbench, caret, magrittr, mltools, PerformanceAnalytics, Hmisc, stargazer, fpp2)

# Load the dataset
df <- read_excel("C:/Users/Shyam/Downloads/HuntersGreenHomeSales.xlsx")

##########################################
# Analysis ###############################

df_adom <- df
df_pricesold <- df

# ADOM
plot(df$adom_agentdaysonmarket ~ df$sqft)

plot(df$adom_agentdaysonmarket ~ df$pricesold)

hist(df$adom_agentdaysonmarket)

den <- density(df$adom_agentdaysonmarket)
plot(den, main = "ADOM Density", col = 'Red')
hist(df$adom_agentdaysonmarket, pch = 20, breaks = 15, prob = T, col = "light blue",
     ylab = "Density", xlab = "ADOM", main = "Histogram for ADOM")
lines(den, col = "red")

# Price Sold
plot(df$pricesold ~ df$sqft)

plot(df$pricesold ~ df$Pool)

hist(df$pricesold)

den <- density(df$adom_agentdaysonmarket)
plot(den, main = "ADOM Density", col = 'Red')
hist(df$adom_agentdaysonmarket, pch = 20, breaks = 15, prob = T, col = "light blue",
     ylab = "Density", xlab = "ADOM", main = "Histogram for ADOM")
lines(den, col = "red")

##########################################
# Clean the data #########################

# Remove unnecessary columns
df <- select(df, -slnoskm, -Address, -subdivn, -Status)

# Replace values of columns
df$Roof[df$Roof == "Shingle"] <- 0          # Shingle = 0
df$Roof[df$Roof == "Tile"] <- 1             # Tile = 0
df$Roof[df$Roof %in% c("Built-Up", "Concrete, Tile", "Other", "Shake, Shingle", "Shingle, Tile", "Slate", "Slate, Tile")] <- 2 # Everything else as 2
#df$Roof <- as.factor(df$Roof)

# Convert Spa to 0 and 1
df$spa  <- as.numeric(df$spa) # 0 = False and 1 = True
df$spa[is.na(df$spa)] <- 0
#df$spa <- as.factor(df$spa)

# Convert splsale to numeric
df$splsale[df$splsale == "None"] <- 0
df$splsale[df$splsale == "Short Sale"] <- 1
df$splsale[df$splsale == "Bank Owned/REO"] <- 2
df$splsale[df$splsale == "Auction, Bank Owned/REO"] <- 3
#df$splsale <- as.factor(df$splsale)

# Convert Pool to numeric
df$Pool[df$Pool == "Private"] <- 0
df$Pool[df$Pool == "Private, Community"] <- 1
df$Pool[df$Pool == "None"] <- 2
df$Pool[df$Pool == "Community"] <- 3
#df$Pool <- as.factor(df$Pool)

df_adom <- filter(df_adom, adom_agentdaysonmarket < 350)

str(df_adom)


##########################################
# Modelling ##############################

# ADOM ###################################
# Baseline m1
m1 <- lm(adom_agentdaysonmarket ~ Beds + bathsfull + bathshalf+ sqft+ garages+ 
           Roof+ lotsqft+ yrblt+ Pool+ spa+ cdom_cumuldaysmls+ listprice+ lppersqft+ 
           PendingDate + pricesold + sppersqft+ datesold + splsale, data = df_adom)
summary(m1)
plot(m1)

# m2
m2 <- lm(adom_agentdaysonmarket ~ Beds + bathsfull + bathshalf+ sqft+ garages+ 
           Roof+ lotsqft+ yrblt+ Pool+ spa+ cdom_cumuldaysmls+ listprice+ lppersqft+ 
            pricesold+ sppersqft+ splsale + I(datesold - PendingDate), data = df_adom)
summary(m2)
plot(m2)

# m3
m3 <- lm(adom_agentdaysonmarket ~ Beds + bathsfull + bathshalf+ sqft+ garages+ 
           Roof+ lotsqft+ yrblt+ Pool+ spa+ cdom_cumuldaysmls+ I(pricesold - listprice) + lppersqft+
           sppersqft+ splsale + I(datesold - PendingDate), data = df_adom)
summary(m3)
plot(m3)

# Stargazer
stargazer(m1, m2, m3, type="text")

# Price SOld #############################
# Baseline m1
ps1 <- lm(pricesold ~ Beds + bathsfull + bathshalf+ sqft+ garages+ 
           Roof+ lotsqft+ yrblt+ Pool+ spa+ adom_agentdaysonmarket+ cdom_cumuldaysmls+ listprice+ lppersqft+ 
           PendingDate + sppersqft+ datesold + splsale, data = df_adom)
summary(ps1)
plot(ps1)

# m2
ps2 <- lm(pricesold ~ Beds + bathsfull + bathshalf+ sqft+ garages+ 
           Roof+ lotsqft+ yrblt+ Pool+ spa+ cdom_cumuldaysmls+ listprice+ lppersqft+ 
            adom_agentdaysonmarket+ sppersqft+ splsale + I(PendingDate - datesold), data = df_adom)
summary(ps2)
plot(ps2)

# m3
ps3 <- lm(pricesold ~ Beds + bathsfull + bathshalf+ sqft+ garages+ 
           Roof+ lotsqft+ yrblt+ Pool+ spa+ I(cdom_cumuldaysmls - adom_agentdaysonmarket) + 
            listprice + lppersqft+ sppersqft+ splsale + I(PendingDate - datesold), data = df_adom)
summary(ps3)
plot(ps3)

# Stargazer
stargazer(ps1, ps2, ps3, type="text")

############################################
# Test for assumptions

m3$res
hist(m3$res)
m3$fit

plot(m3$res ~ m3$fit)                     # Residual plot
hist(m3$res)

qqnorm(m3$res)                              # Q-Q plot
qqline(m3$res, col="red")

shapiro.test(m3$res)                        # Shapiro-Wilk's test of multivariate normality
norm <- rnorm(200)
ks.test(norm, m3$res)                       # Kolmogorov-Smirnov test

# install.packages("car")
library("car")
bartlett.test(list(m3$res, m3$fit))       # Bartlett's test of homoskedasticity
leveneTest(m3$res, m3$fit, center=mean)   # Levene's test of homoskedasticity
# install.packages("lmtest")
library(lmtest)
bptest(ps3)                                      # Breush-Pagan test

library("car")                                # Test of multicollinearity
vif(ps3)                                     # Variance inflation factor

dwtest(ps3)                                  # Durbin-Watson test of autocorrelation
