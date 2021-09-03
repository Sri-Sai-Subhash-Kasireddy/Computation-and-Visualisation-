# Assignment 1
# Sri Sai Subhash Kasireddy (001510354)
# Sec 1


library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(magrittr)


##############################Problem 1

roster=read_csv("roster.csv")#Used to read data from a CSV file.

x=as.data.frame(str_split(roster$names, ",", simplify = T))#Used to split the data into our required format.

roster$firstname=x$V2 #Assigning values to column
roster$lastname=x$V1
roster$names=NULL

attendance=read_csv("attendance.csv")

roster$firstname=str_trim(roster$firstname) #Removing the blank spaces
roster$lastname=str_trim(roster$lastname)

y=attendance%>%      #This is used to takes the total rows present after grouping them based on firstname and lastname.
  group_by(firstname,lastname)%>%
  summarise(Count=n())%>%
  arrange(desc(Count))



z=as.data.frame(left_join(roster, y)) #Joining the dataframes to see the values that are in common

q=which(is.na(z$Count)) #Used to detect NULL values in the column and get their row index.
z$Count[q]=0

attendance_count=z[with(z, order(-Count)), ]  #To arrange the values in descending order.






##############################Problem 2

#Question 1
wine=read_csv("wine_data.csv")

wine%>%
  group_by(variety)%>%   #Used to get the rows available after grouping.
  summarise(Count=n())%>%
  arrange(desc(Count))%>%
  drop_na()%>%
  head(10)


#Question 2
wine%>%
  group_by(country)%>%   #Used to calculate the average points by grouping them on country.
  summarise(Avg_Points=mean(points))%>%
  arrange(desc(Avg_Points))
  


#Question 3
wine%>%       #Used to calculate the average price by grouping them on province.
  group_by(province)%>%
  summarise(Max_Avg_Price=mean(price))%>%
  arrange(desc(Max_Avg_Price))%>%
  drop_na()%>%
  head(1)

#Question 4

wine%>%
  group_by(province)%>%
  filter(country=="US")%>%
  summarise(Highest_Avg_Price=mean(price))%>%
  arrange(desc(Highest_Avg_Price))%>%
  drop_na()%>%
  head(1)

#Question 5

wine%>%
  #group_by(designation)%>%     #Used to filter data based on the given string data after grouping designation.
  filter(str_detect(designation, "20 Year|20Years|20-Years|20 Years|20-year|Anos|20 yr|20 Yr|20-year|20-Year|20 Anni"))%>%
  summarise(Count=n())%>%
  arrange(desc(Count))%>%
  head()

