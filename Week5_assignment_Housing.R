# Assignment: ASSIGNMENT 5
# Name: Selvaraj, Akila
# Date: 2022-01-14

install.packages('rlang')
install.packages('tidyr')
install.packages("ggplot2")
install.packages("dplyr")

library('tidyr')
library(plyr)
library(dplyr)
library(readxl)
library(purrr)
library(stringr)

getwd()
setwd("G:/Users/a162940/Akila/Work/R/Projects/dsc520-master")

#Read the excel into dataframe

Housing_excel <- read_excel("data/week-7-housing.xlsx")
Housing_df <- data.frame(Housing_excel) 

#Creating new variable Sale Year
Housing_df$Saleyear <- as.integer(format(Housing_df$Sale.Date, "%Y"))


#Using the dplyr package, use the 6 different operations to analyze/transform the data - 
#GroupBy, Summarize, Mutate, Filter, Select, and Arrange - 
#Remember this isn't just modifying data, you are learning about your data also - so play around and start to understand your dataset in more detail

Housing_df %>% select(Sale.Date, Sale.Price) %>% head

# the columns can be specified as a vector of column names as well
Housing_df %>% select(c(Sale.Date, Sale.Price)) %>% head
select(Housing_df,1,7) %>% head


#Filter where Sale.Date >= '2016-01-01' and Sale.Price >= 2000000
Housing_df %>% filter(Sale.Price >= 2000000, Sale.Date >= '2016-01-01') %>% select(1,2)

#Slice -  like rownum 
Housing_df %>% slice(1:5, 8, 10:13)
Housing_df %>% head %>% slice(-1)

#Mutate - create new column Half.Price
Housing_df %>% select(Sale.Date, Sale.Price) %>% head %>% mutate(Half.Price=Sale.Price/2)

#Summarize 
Housing_df %>% select(Sale.Date, Sale.Price) %>% summarize(AvgPrice = mean(Sale.Price), 
                                                           MaxPrice = max(Sale.Price), 
                                                           MedianPrice = median(Sale.Price))

#Group_by
Housing_df %>% select(Saleyear,Sale.Date, Sale.Price) %>% group_by(Saleyear) %>% summarize(MaxPrice=max(Sale.Price))

#Arrange - to sort
Housing_df %>% select(Saleyear,Sale.Date, Sale.Price) %>% group_by(Saleyear) %>% 
  summarize(MaxPrice=max(Sale.Price)) %>% arrange(MaxPrice)

#Using the purrr package - perform 2 functions on your dataset.  You could use zip_n, keep, discard, compact, etc.


Housing_df %>% select(Saleyear,Sale.Date, Sale.Price) %>% group_by(Saleyear) %>% 
  summarize(MaxPrice=max(Sale.Price)) %>% map(mean)

Housing_df %>% select(Saleyear,Sale.Date, Sale.Price) %>% group_by(Saleyear) %>% 
  summarize(MaxPrice=max(Sale.Price)) %>% map_dbl(mean)

keep(Housing_df$sq_ft_lot, ~ .x > 1327090)

discard(Housing_df$sq_ft_lot, ~ .x > 1000)


#Use the cbind and rbind function on your dataset

total_bath <- Housing_df$bath_full_count + Housing_df$bath_half_count + Housing_df$bath_3qtr_count
total_bath

cbind(Housing_df,total_bath)

Housing_first_5rows <- Housing_df  %>% slice(1:5)
Housing_second_5rows <- Housing_df  %>% slice(6:10)

rbind(Housing_first_5rows,Housing_second_5rows)

#Split a string, then concatenate the results back together
#Splitting the address and concatening back

yearList <- str_split(string=Housing_df$Sale.Date, pattern="-")
head(yearList)

addr_first <- str_sub(string=Housing_df$addr_full, start=1, end=10)
head(addr_first)

addr_second <- str_sub(string=Housing_df$addr_full, start=11, end=25)
head(addr_second)

head(addr_first + addr_second)

addr_full <- str_c(addr_first,addr_second)
head(addr_full)

str_c(addr_first,addr_second) %>% head
