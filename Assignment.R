# Import tidyverse library.
library(tidyverse)

# Import csv file.
turtle_sales <- read.csv(file.choose(), header=TRUE)

# Call the data frame
turtle_sales

# View the data frame.
View(turtle_sales)

# Determine the working directory.
getwd()

# Examine the summary.
summary(turtle_sales)

# Examine the structure of the data frame.
str(turtle_sales)

# Check the attributes.
attributes(turtle_sales)

# Check dimensions.
dim(turtle_sales)

# Check for NA values.
turtle_sales[is.na(turtle_sales)]

# To search for missing values in a data set.
sum(is.na(turtle_sales))

# Change NA with 0.

# Keep the necessary columns.
turtle_sales2 <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)

# Create a summary.
summary(turtle_sales2)

# Create plots to review and determine insights into the sales data set.
qplot(Product, NA_Sales, colour=Platform, data=turtle_sales2, geom=c('point', 'jitter'))

qplot(Product, EU_Sales, colour=Platform, data=turtle_sales2, geom=c('point', 'jitter'))

qplot(Product, Global_Sales, colour=Platform, data=turtle_sales2, geom=c('point', 'jitter'))

qplot(Product, NA_Sales, colour=Platform, data=turtle_sales2,
      geom='boxplot')

qplot(Product, EU_Sales, colour=Platform, data=turtle_sales2,
      geom='boxplot')

qplot(Product, Global_Sales, colour=Platform, data=turtle_sales2,
      geom='boxplot')

plot(hist(turtle_sales2$NA_Sales))

plot(hist(turtle_sales2$EU_Sales))

plot(hist(turtle_sales2$Global_Sales))

qplot(Platform, data=turtle_sales2, geom='bar')

# Check the following.
qplot(Platform, fill=Product, data=turtle_sales2, geom='bar')

# Not working.
# Can I plot a line chart displaying NA, EU, and global sales?
# Plot
don %>%
  ggplot(turtle_sales2(x=year, y=n, group=name, color=name)) +
  geom_line()

# Plot the bar chart.
plot(turtle_sales2(v, type = "o", col = "red", xlab = "Year", ylab = "Sales", main = "Sales"))

group_by(turtle_sales2$NA_Sales)

df <- turtle_sales2 %>% group_by(Product) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum)))

# View the data frame.
head(as_tibble(df))

install.packages('skimr')
library(skimr)

skim(turtle_sales2)

install.packages('DataExplorer')
library(DataExplorer)

# Create a report the data set.
DataExplorer::create_report(turtle_sales2)

library (moments)

# Keep the necessary columns.
turtle_sales3 <- select(turtle_sales2, -Platform)

head(turtle_sales3)
summary(turtle_sales3)
dim(turtle_sales3)

min(turtle_sales3$NA_Sales)
max(turtle_sales3$NA_Sales)
mean(turtle_sales3$NA_Sales)
median(turtle_sales3$NA_Sales)
sd(turtle_sales3$NA_Sales)
# Calculate Q1 and Q3.
quantile(turtle_sales3$NA_Sales, 0.25)  
quantile(turtle_sales3$NA_Sales, 0.75)
# Calculate IQR.
IQR(turtle_sales3$NA_Sales)  
# Determine the variance.
var(turtle_sales3$NA_Sales)  

min(turtle_sales3$EU_Sales)
max(turtle_sales3$EU_Sales)
mean(turtle_sales3$EU_Sales)
median(turtle_sales3$EU_Sales)
sd(turtle_sales3$EU_Sales)
# Calculate Q1 and Q3.
quantile(turtle_sales3$EU_Sales, 0.25)  
quantile(turtle_sales3$EU_Sales, 0.75)
# Calculate IQR.
IQR(turtle_sales3$EU_Sales)  
# Determine the variance.
var(turtle_sales3$EU_Sales)

min(turtle_sales3$Global_Sales)
max(turtle_sales3$Global_Sales)
mean(turtle_sales3$Global_Sales)
median(turtle_sales3$Global_Sales)
sd(turtle_sales3$Global_Sales)
# Calculate Q1 and Q3.
quantile(turtle_sales3$Global_Sales, 0.25)  
quantile(turtle_sales3$Global_Sales, 0.75)
# Calculate IQR.
IQR(turtle_sales3$Global_Sales)  
# Determine the variance.
var(turtle_sales3$Global_Sales)

# Why the min og Global Sales is higher that NA and EU which are both 0?

# Range = Max - Min.
max(turtle_sales3$NA_Sales) - min(turtle_sales3$NA_Sales)
max(turtle_sales3$EU_Sales) - min(turtle_sales3$EU_Sales)
max(turtle_sales3$Global_Sales) - min(turtle_sales3$Global_Sales)

# Specify the qqnorm function.
# Draw a qqplot using the total_seconds data.
qqnorm(turtle_sales3$NA_Sales,
       col='blue',
       xlab="z Value",
       ylab='NA Sales')
# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(turtle_sales3$NA_Sales,
       col='red',
       lwd=2) 

# Specify the qqnorm function.
# Draw a qqplot using the total_seconds data.
qqnorm(turtle_sales3$EU_Sales,
       col='blue',
       xlab="z Value",
       ylab='EU Sales')
# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(turtle_sales3$EU_Sales,
       col='red',
       lwd=2)

# Specify the qqnorm function.
# Draw a qqplot using the total_seconds data.
qqnorm(turtle_sales3$Global_Sales,
       col='blue',
       xlab="z Value",
       ylab='Global Sales')
# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(turtle_sales3$Global_Sales,
       col='red',
       lwd=2)

# Run a Shapiro-Wilk test:
shapiro.test(turtle_sales3$NA_Sales)
# Check skewness and Kurtosis
skewness(turtle_sales3$NA_Sales)
kurtosis(turtle_sales3$NA_Sales)

# Run a Shapiro-Wilk test:
shapiro.test(turtle_sales3$EU_Sales)
# Check skewness and Kurtosis
skewness(turtle_sales3$EU_Sales)
kurtosis(turtle_sales3$EU_Sales)

# Run a Shapiro-Wilk test:
shapiro.test(turtle_sales3$Global_Sales)
# Check skewness and Kurtosis
skewness(turtle_sales3$Global_Sales)
kurtosis(turtle_sales3$Global_Sales)
# These p-values indicate that we do not have a normal distribution.

# Shall I conduct a t-test z-test?

df1 <- reshape2::melt(turtle_sales3)

# Aggregate.
North_America <- aggregate(NA_Sales~Product, turtle_sales3, sum)
Europe <- aggregate(EU_Sales~Product, turtle_sales3, sum)
Global <- aggregate(Global_Sales~Product, turtle_sales3, sum)
# or group by
# df <- turtle_sales3 %>% group_by(Product) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), 
# list(sum = sum)))
# View(df)

summary(North_America)
summary(Europe)
summary(Global)

#Here I am doing something wrong.
# Specify histogram function.
hist(North_America$Product)
hist(Europe$Product)
hist(Global$Product)

boxplot(North_America$Product)
boxplot(Europe$Product)
boxplot(Global$Product)

library(ggplot2)

# Histogram
# Start with a simple histogram.
ggplot(North_America, aes(x=NA_Sales)) + 
  geom_histogram(bins = 20)

ggplot(Europe, aes(x=EU_Sales)) + 
  geom_histogram(bins = 20)

ggplot(Global, aes(x=Global_Sales)) + 
  geom_histogram(bins = 20)

# Smoothed density plot.
ggplot(North_America, aes(x=NA_Sales)) + 
  geom_density()

ggplot(Europe, aes(x=EU_Sales)) + 
  geom_density()

ggplot(Global, aes(x=Global_Sales)) + 
  geom_density()

# Start with a simple scatterplot.
ggplot(North_America, aes(x=Product, y=NA_Sales)) + 
  geom_point()
# Add jitter and line of best fit.
ggplot(North_America, aes(x=jitter(Product), y=NA_Sales)) + 
  geom_point() +
  geom_smooth(method=lm)
# Scatterplot with no method in geom_smooth() (spline).
ggplot(North_America, aes(x=jitter(Product), y=NA_Sales)) + 
  geom_point() +
  geom_smooth()
# Remove standard error and thicken line.
ggplot(North_America, aes(x=jitter(Product), y=NA_Sales)) + 
  geom_point() +
  geom_smooth(lwd=2, se=FALSE)

ggplot(Europe, aes(x=Product, y=EU_Sales)) + 
  geom_point()
# Add jitter and line of best fit.
ggplot(Europe, aes(x=jitter(Product), y=EU_Sales)) + 
  geom_point() +
  geom_smooth(method=lm)
# Scatterplot with no method in geom_smooth() (spline).
ggplot(Europe, aes(x=jitter(Product), y=EU_Sales)) + 
  geom_point() +
  geom_smooth()
# Remove standard error and thicken line.
ggplot(Europe, aes(x=jitter(Product), y=EU_Sales)) + 
  geom_point() +
  geom_smooth(lwd=2, se=FALSE)

ggplot(Global, aes(x=Product, y=Global_Sales)) + 
  geom_point()
# Add jitter and line of best fit.
ggplot(Global, aes(x=jitter(Product), y=Global_Sales)) + 
  geom_point() +
  geom_smooth(method=lm)
# Scatterplot with no method in geom_smooth() (spline).
ggplot(Global, aes(x=jitter(Product), y=Global_Sales)) + 
  geom_point() +
  geom_smooth()
# Remove standard error and thicken line.
ggplot(Global, aes(x=jitter(Product), y=Global_Sales)) + 
  geom_point() +
  geom_smooth(lwd=2, se=FALSE)

str(turtle_sales2)

# Add a further/third variable as a colour and remove the smoothing line.
ggplot(turtle_sales2, aes(x=Product, y=NA_Sales, col=Platform)) + 
  geom_point()

ggplot(turtle_sales2, aes(x=Product, y=EU_Sales, col=Platform)) + 
  geom_point()

ggplot(turtle_sales2, aes(x=Product, y=Global_Sales, col=Platform)) + 
  geom_point()

# Barplots
# Categorical variable = Platform.
ggplot(turtle_sales2, aes(x=Platform, col=Product)) + 
  geom_bar()

# Comparing platform and product with colour (fill), add a title, and select theme.
ggplot(turtle_sales2, aes(x=Platform, fill=Product)) + 
  geom_bar()+
  ggtitle("Platform by Product") +
  theme_minimal()

# NA.
# Set the data source.
ggplot(data=turtle_sales2, 
        # Add mapping elements.
        mapping=aes(x=Product, y=NA_Sales)) +
  geom_point()

# Use an operator to specify values in the NA-Sales column greater than 15.
# Create a new object, specify the data to pass to the object
# and remove outliers with filter function
new_NA <- filter(turtle_sales2, NA_Sales<15)

# Specify the ggplot function and the geom_point function:  
# Set data source.
ggplot(data=new_NA,
       # Add mapping element.
       mapping=aes(x=Product, y=NA_Sales)) + 
  # Set the colour to red.
  geom_point(color='red',
             # Set the alpha transparency to 0.5.
             alpha=0.5,  
             # Set the point size to 3.
             size=3)

# EU.
ggplot(data=turtle_sales2, 
       mapping=aes(x=Product, y=EU_Sales)) +
  geom_point()

new_EU <- filter(turtle_sales2, EU_Sales<15)

ggplot(data=new_EU,
       mapping=aes(x=Product, y=EU_Sales)) + 
  geom_point(color='blue',
             alpha=0.5,  
             size=3)

# Global.
ggplot(data=turtle_sales2, 
       mapping=aes(x=Product, y=Global_Sales)) +
  geom_point()

new_Global <- filter(turtle_sales2, EU_Sales<40)

ggplot(data=new_Global,
       mapping=aes(x=Product, y=Global_Sales)) + 
  geom_point(color='purple',
             alpha=0.5,  
             size=3)

# I want to use the facet for NA, EU, Global.

# Here I add subtitles and change captions as well as customising the plots' features. I add a theme too.
ggplot(data=turtle_sales,
       mapping=aes(x=Product, y=NA_Sales)) + 
  geom_point(color='red',
             alpha=0.5,  
             size=3) +
  facet_wrap(~Genre) +
  labs(title= "Relationship between product and NA sales",
       subtitle="Sales by genre",
       caption="Source: SQL DataBase",
       x="Products",
       y="Sales") +
  theme_bw()

ggplot(data=turtle_sales,
       mapping=aes(x=Product, y=EU_Sales)) + 
  geom_point(color='blue',
             alpha=0.5,  
             size=3) +
  facet_wrap(~Genre) +
  labs(title= "Relationship between product and EU sales",
       subtitle="Sales by genre",
       caption="Source: SQL DataBase",
       x="Products",
       y="Sales") +
  theme_bw()

ggplot(data=turtle_sales,
       mapping=aes(x=Product, y=Global_Sales)) + 
  geom_point(color='purple',
             alpha=0.5,  
             size=3) +
  facet_wrap(~Genre) +
  labs(title= "Relationship between product and global sales",
       subtitle="Sales by genre",
       caption="Source: SQL DataBase",
       x="Products",
       y="Sales") +
  theme_bw()
