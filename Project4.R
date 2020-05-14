library(tidyquant)
library(tidyverse)
library(quantmod)
library(ggplot2)
library(lubridate)


#---strategy 1

#get stock prices
spy_stock_prices <- c("SPY") %>%
  tq_get(get = "stock.prices",
         from = "1990-01-01")

#get yearly stock returns
spy_stock_returns_yearly <- spy_stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "yearly_returns")

#adding investment growth column
spy_stock_returns_yearly <- spy_stock_returns_yearly %>%
  add_column(investment_growth_amount = 0)

#get the investment growth every year
for(i in 1:length(spy_stock_returns_yearly$investment_growth_amount)){
  if(i==1)
    amt=0
  else
    amt=spy_stock_returns_yearly$investment_growth_amount[i-1]
  spy_stock_returns_yearly$investment_growth_amount[i] = 
    amt + 12000 + (amt+12000) * spy_stock_returns_yearly$yearly_returns[i]
}

View(spy_stock_returns_yearly)




#---------strategy 2

#get monthly stock prices
getSymbols("SPY", from = "1990-01-01", periodicity= "monthly")
spy_stock_prices_strategy2 <- SPY %>% fortify.zoo() %>% as.tibble()

#get monthly stock returns
spy_stock_returns_monthly <- spy_stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "monthly_returns")

#adding investment growth column
spy_stock_returns_monthly <- spy_stock_returns_monthly %>%
  add_column(investment_growth_amount = 0)

# getting sma values of the monthly closing values
sma_avg <- SMA(Cl(spy_stock_prices_strategy2))
spy_stock_prices_strategy2 <- spy_stock_prices_strategy2 %>%
  add_column(Close_sma = sma_avg)


# for first 9 months, since there is no sma due to lack of data
# we will assume $1000 is added each month to savings account
for(i in 1:9){
  if(i == 1)
    spy_stock_returns_monthly$investment_growth_amount[i] = 1000
  else
    spy_stock_returns_monthly$investment_growth_amount[i] = spy_stock_returns_monthly$investment_growth_amount[i-1] + 1000
}


#for 10th month onwards 
for(i in 10:length(spy_stock_prices_strategy2$Close_sma)) {
  
  strategy2_amt <- spy_stock_returns_monthly$investment_growth_amount[i-1]
  if(spy_stock_prices_strategy2$SPY.Close[i] > sma_avg[i]){
    spy_stock_returns_monthly$investment_growth_amount[i] = 
      strategy2_amt +1000 + (strategy2_amt + 1000) * spy_stock_returns_monthly$monthly_returns[i]
  }
  else{
    spy_stock_returns_monthly$investment_growth_amount[i] = strategy2_amt + 1000
  }
  
}


#create dataframe of all results together
spy_stock_returns_monthly <- spy_stock_returns_monthly %>%
  mutate(year = year(date),
         month = month(date))

spy_stock_returns_monthly_fil <- spy_stock_returns_monthly %>%
  filter(month == 12)

spy_stock_returns_monthly_filtered <- rbind(spy_stock_returns_monthly_fil, tail(spy_stock_returns_monthly,1))


spy_final <- cbind(spy_stock_returns_yearly, spy_stock_returns_monthly_filtered)
spy_final <- spy_final[ ,-c(1,5,6,9,10)]

spy_final <- spy_final %>%
  rename(
    strategy1_returns = yearly_returns,
    strategy1_invstmt_growth = investment_growth_amount,
    strategy2_returns = monthly_returns,
    strategy2_invstmt_growth = investment_growth_amount1
  )

#Plots


#Strategy1 yearly returns over 30 year period
spy_stock_returns_yearly %>%
  ggplot(aes(x = date, y = yearly_returns)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Strategy 1 yearly Returns",
       x = "year", y = "Yearly Returns") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)


#Strategy2 monthly returns over 30 year period
spy_stock_returns_monthly %>%
  ggplot(aes(x = date, y = monthly_returns)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Strategy 2 monthly Returns",
       x = "date", y = "Monthly Returns") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

#Strategy 1 and 2 yearly returns
ggplot(spy_final, aes(x=date)) +
  geom_line(aes(y = strategy1_returns), color = "green") +
  geom_line(aes(y = strategy2_returns), color = "red") +
  xlab("Year") + ylab("Yearly returns")


#Strategy2 SMA
chartSeries(SPY)
addSMA(col="white")
