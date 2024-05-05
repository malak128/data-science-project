install.packages("reader")
install.packages("dplyr")
library("reader")
library(dplyr)
x<-file.choose()
df=read.csv(x)
df
str(df)
duplicated(df)
if(sum(duplicated(df))>0){
  df=unique(df)
  df
}
sum(duplicated(df))
is.character(df$items)
is.integer(df$count)
is.numeric(df$total)
is.integer(df$rnd)
is.integer(df$age)
is.character(df$city)
is.character(df$paymentType)
is.na(df)
if(sum(is.na(df))>0){
  df=na.omit(df)
}
df[which(df$total %in% outlier),]
df[which(df$age %in% outlier),]
df
str(df)
duplicated(df)
sum(duplicated(df))
df_without=unique(df)
df_without
sum(duplicated(df_without))
is.character(df$items)
is.integer(df$count)
is.numeric(df$total)
is.integer(df$rnd)
is.integer(df$age)
is.character(df$city)
is.character(df$paymentType)
is.na(df)
sum(is.na(df))
outlier= boxplot(df$total)$out
outlier
if(sum(df[which(df$total %in% outlier),])>0){
  df[-which(df$total %in% outlier),]
}
if(sum(df[which(df$age %in% outlier),])>0){
  df[which(df$age %in% outlier),]
}
age_spendingtotal<-df%>%group_by(age)%>%
  summarise(total_spending=sum(total))
install.packages("ggplot2")
library(ggplot2)
ggplot(data = age_spendingtotal, aes(x = age, y = total_spending)) +
  geom_point(color="orchid4") + # Add points to represent the data
  labs(x = "Age", y = "Total Spending") + # Add labels to the axes
  ggtitle("Relationship between Age and Total Spending") # Add a title to the plot
summarise(total_spending = sum(total_spending))

# Create the bar plot
ggplot(age_spendingtotal, aes(x=age, y=total_spending)) +
  geom_bar(stat="identity") +
  labs(title="Total Spending by Age Category", x="Age Category", y="Total Spending")

# Create a histogram to Display the distribution of total spending.
hist(df$total,  # Specify the column containing total spending
     col = "maroon",  # Fill color of bars
     border = "black",  # Color of bar borders
     xlab = "Total Spending",  # X-axis label
     ylab = "Frequency",  # Y-axis label
     main = "Histogram of Total Spending")  # Title

# for displaying each city with its total spending in descending order 
grouped_total <- df %>% group_by(city) %>%
  summarise(totalspending = sum(total) )
print (grouped_total)

#using barplot 
#ndf -> new data frame 
ndf <- grouped_total[order(grouped_total$totalspending , decreasing = TRUE),]
barplot(ndf$totalspending , name = ndf$city ,
        xlab = "city" , ylab = "total spending" , main = "display each city total spending",
        col = "lightpink")

