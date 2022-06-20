# Libraries
library(stats)
require(tsDyn)
library(vars)
library(Metrics)
library(data.table)
library(urca)
library(tseries)
library(forecast)
library(plotly)

setwd("C:/Users/thiag/Documents/1 -Mestrado/SprintFinal/CasoReal/")

df__ipca <- readxl::read_xlsx("data.xlsx",sheet = "IPCA")
df__usd <- readxl::read_xlsx("data.xlsx",sheet = "USD")

selected_colums <- c("date","Atual","Price")
df__merge <- merge(x = df__ipca, y = df__usd, by = "date", all.x = TRUE)[,selected_colums]
names(df__merge) <- c("date","IPCA","USDBRL")

pre_filter_start <- "2000-01-01"
pre_filter_end <- "2022-02-01"

df__train = df__merge[(df__merge$date >= pre_filter_start) & (df__merge$date <= pre_filter_end),]

p <- plot_ly(df__train) %>%
  add_trace(x = ~date, y = ~IPCA,mode = "lines",  name = "IPCA") %>%  
  add_trace(x = ~date, y = ~USDBRL, mode = "lines", yaxis = "y2", name = "USDBRL") %>%
  layout(yaxis2 = list(overlaying = "y", side = "right"))
plot(p)


scale_value = max(df__train$IPCA, na.rm = TRUE)/ max(df__train$USDBRL, na.rm = TRUE)
ggplot(df__train) + 
  geom_line(aes(x= date, y= IPCA), color = 'dodgerblue') +
  geom_line(aes(x= date, y = USDBRL*scale_value), stat = 'identity') + 
  scale_y_continuous(sec.axis = sec_axis(~./scale_value)) +


df__train_ts <- xts(df__train[,c("IPCA","USDBRL")],order.by=df__train$date)

df__train_ts <- ts(df__train[,c("IPCA","USDBRL")])

df__train_ts <- ts(df__train[,c("IPCA","USDBRL")],order.by=df__train$date)

plot.ts(df__train_ts)

