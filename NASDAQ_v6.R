library(tidyverse)
library(imputeTS)

path <- "D:\\RepoStack\\NASDAQ\\NASDAQ-Analysis-2022\\2022_03_17_02_06_nasdaq.xlx"

#inputfile <- readxl::read_excel(path = path, "2022_03_17_02_06_nasdaq.xlx")
inputfile <- read_csv("2022_03_17_02_06_nasdaq.csv") 


inputfile_concentrated <- inputfile %>% 
  select(symbol, name, price, pricing_changes, pricing_percentage_changes, market_cap, earnings_per_share, annualized_dividend, symbol_yield, beta) %>% 
  mutate(pricing_percentage_changes = str_extract(pricing_percentage_changes, "\\d+\\.?\\d*")) %>% 
  mutate(pricing_percentage_changes = as.numeric(pricing_percentage_changes)) %>%
  mutate(market_cap = as.numeric(gsub(",", "",market_cap))) %>% 
  mutate(pricing_changes = as.numeric(pricing_changes)) %>%
  mutate(earnings_per_share = as.numeric(gsub("[^0-9.]", "", earnings_per_share))) %>%
  mutate(annualized_dividend = as.numeric(gsub("[^0-9.]", "",annualized_dividend))) %>% 
  mutate(symbol_yield = as.numeric(gsub("[^0-9.]", "",symbol_yield))) %>%
  mutate(beta = as.numeric(beta))

# symbol yield are percentages
# price, earnings and dividend are in dollars

view(inputfile_concentrated)
#get rid of NA's using na_interpolation() 
inputfile_concentrated$earnings_per_share <- na_interpolation(inputfile_concentrated$earnings_per_share)
inputfile_concentrated$annualized_dividend <- na_interpolation(inputfile_concentrated$annualized_dividend)
inputfile_concentrated$symbol_yield <- na_interpolation(inputfile_concentrated$symbol_yield)
inputfile_concentrated$market_cap <- na_interpolation(inputfile_concentrated$market_cap)


# linear model, using price and market_cap

Price_Market_Cap_Reg <- lm(price ~ market_cap, data = inputfile_concentrated)
summary(Price_Market_Cap_Reg)

# mult reg model using price, market_cap, price change, beta (metric of volatility)

MultReg_Price_Market_Cap <- lm(price ~ market_cap + pricing_changes + beta, data = inputfile_concentrated)
summary(MultReg_Price_Market_Cap)
# better adjusted R squared, beta is not significant

# mult reg model using price, market_cap, price change, earnings_per_share (metric of volatility)
MultReg_Price_Market_Cap2 <- lm(price ~ market_cap + pricing_changes + earnings_per_share, data = inputfile_concentrated)
summary(MultReg_Price_Market_Cap2)
# earnings per share not significant according to the regression

# mult reg model using price, market_cap, price change, earnings_per_share (metric of volatility)
MultReg_Price_Market_Cap3 <- lm(price ~ market_cap + pricing_changes + pricing_percentage_changes, data = inputfile_concentrated)
summary(MultReg_Price_Market_Cap3)

# predcitDf values
mean_market_cap <- mean(inputfile_concentrated$market_cap, na.rm = TRUE)
mean_pricing_changes <- mean(inputfile_concentrated$pricing_changes, na.rm = TRUE)
mean_pricing_percentage_changes <- mean(inputfile_concentrated$pricing_percentage_changes, na.rm = TRUE)

predictDF <- data.frame(market_cap = mean_market_cap, pricing_changes = mean_pricing_changes, pricing_percentage_changes = mean_pricing_percentage_changes)

# predict function
predict(MultReg_Price_Market_Cap3, predictDF)

#Multi Reg using price_change as outcome, and market_cap, earnings_per_share and pricing_percentage_changes as predictors 
MultiReg_PriceChange_1 <- lm (pricing_changes ~ market_cap + earnings_per_share + pricing_percentage_changes, data = inputfile_concentrated)
summary(MultiReg_PriceChange_1)

#Mutli Reg using price_change as outcome, and market_cap, earnings_per_share and beta as predictors
MultiReg_PriceChange_2 <- lm (pricing_changes ~ market_cap + earnings_per_share + beta, data = inputfile_concentrated)
summary(MultiReg_PriceChange_2)

#Multi Reg using price_change as outcome, and market_cap, earnings_per_share, and price as predictors
MultiReg_PriceChange_3 <- lm (pricing_changes ~ market_cap + earnings_per_share + price, data = inputfile_concentrated)
summary(MultiReg_PriceChange_3)

#Ordering in Descending Order to Comparatively Observe Best Performing Stocks According to Significant Factors
Final5 = inputfile_concentrated[order(-inputfile_concentrated$pricing_changes, -inputfile_concentrated$pricing_percentage_changes, -inputfile_concentrated$earnings_per_share, -inputfile_concentrated$market_cap, -inputfile_concentrated$beta), ]
view(Final5)

#Isolating the best performing stocks from entire file
Best_To_Purchase = head(Final5, 6)
view(Best_To_Purchase)


Best_To_Purchase_Final = na.omit(Best_To_Purchase)
view(Best_To_Purchase_Final)


Best_To_Purchase_Final2 <- Best_To_Purchase[-1,]
write_csv(Best_To_Purchase_Final2, file = "Best to Purchase")

# Omit NA 
inputfile_concentrated_na_omit <- na.omit(inputfile_concentrated)





# Statistic Models With Omitted NA

# linear model, using price and market_cap

Price_Market_Cap_Reg_Na_Omit <- lm(price ~ market_cap, data = inputfile_concentrated_na_omit)
summary(Price_Market_Cap_Reg)

# mult reg model using price, market_cap, price change, beta (metric of volatility), and earnings_per_share and pricing_percentage_changes

MultReg_Price_Market_Cap_Na_Omit <- lm(price ~ market_cap + pricing_changes + beta + earnings_per_share + pricing_percentage_changes, data = inputfile_concentrated_na_omit)
summary(MultReg_Price_Market_Cap_Na_Omit)
# better adjusted R squared, beta is not significant

# mult reg model using price, market_cap, price change, earnings_per_share (metric of volatility)
MultReg_Price_Market_Cap_Na_Omit2 <- lm(price ~ market_cap + pricing_changes + earnings_per_share, data = inputfile_concentrated_na_omit)
summary(MultReg_Price_Market_Cap_Na_Omit2)
# earnings per share not significant according to the regression

# mult reg model using price, market_cap, price change, earnings_per_share (metric of volatility)
MultReg_Price_Market_Cap_NA_Omit3 <- lm(price ~ market_cap + pricing_changes + pricing_percentage_changes, data = inputfile_concentrated_na_omit)
summary(MultReg_Price_Market_Cap_NA_Omit3)

# predcitDf values
mean_market_cap2 <- mean(inputfile_concentrated_na_omit$market_cap, na.rm = TRUE)
mean_pricing_changes2 <- mean(inputfile_concentrated_na_omit$pricing_changes, na.rm = TRUE)
mean_pricing_percentage_changes2 <- mean(inputfile_concentrated_na_omit$pricing_percentage_changes, na.rm = TRUE)

predictDF2 <- data.frame(market_cap = mean_market_cap, pricing_changes = mean_pricing_changes, pricing_percentage_changes = mean_pricing_percentage_changes)

# predict function
predict(MultReg_Price_Market_Cap_NA_Omit3, predictDF2)

#####Multi Reg using price_change as outcome, market_cap, earnings_per_share pricing_percentage_changes, and beta as predictors #####
MultiReg_PriceChange_NA_Omit_1 <- lm (pricing_changes ~ market_cap + earnings_per_share + pricing_percentage_changes, data = inputfile_concentrated)
summary(MultiReg_PriceChange_NA_Omit_1)

#Mutli Reg using price_change as outcome, and market_cap, earnings_per_share and beta as predictors
MultiReg_PriceChange_NA_Omit_2 <- lm (pricing_changes ~ market_cap + earnings_per_share + beta, data = inputfile_concentrated)
summary(MultiReg_PriceChange_NA_Omit_2)

#Multi Reg using price_change as outcome, and market_cap, earnings_per_share, and price as predictors
MultiReg_PriceChange_NA_Omit_3 <- lm (pricing_changes ~ market_cap + earnings_per_share + price, data = inputfile_concentrated)
summary(MultiReg_PriceChange_NA_Omit_3)

#Plots (na_omit)

ggplot(inputfile_concentrated_na_omit, aes(x = pricing_changes, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red")

ggplot(inputfile_concentrated_na_omit, aes(x = market_cap, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red")

ggplot(inputfile_concentrated_na_omit, aes(x = earnings_per_share, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red")

ggplot(inputfile_concentrated_na_omit, aes(x =beta, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red")

ggplot(inputfile_concentrated_na_omit, aes(x =pricing_percentage_changes, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red")

ggplot(inputfile_concentrated_na_omit, aes(x = earnings_per_share, y = pricing_changes)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red")