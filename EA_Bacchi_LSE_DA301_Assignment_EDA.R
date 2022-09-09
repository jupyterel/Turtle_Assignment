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
turtle_sales[is.na(turtle_sales)] <- 0

# To search for missing values in a data set.
sum(is.na(turtle_sales))

# Keep the necessary columns.
turtle_sales2 <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)

# Create a summary.
summary(turtle_sales2)

# Create plots to review and determine insights into the sales data set.
# Qplot scatterplots for NA, EU, and Global.
qplot(Product, NA_Sales, colour=Platform, data=turtle_sales2, geom=c('point', 'jitter'))+
  labs(title= "Relationship between product and NA sales",
       subtitle="Sales by Platform",
       caption="Source: SQL DataBase",
       x="Products",
       y="NA Sales") +
  theme_minimal()

qplot(Product, EU_Sales, colour=Platform, data=turtle_sales2, geom=c('point', 'jitter'))+
  labs(title= "Relationship between product and EU sales",
       subtitle="Sales by Platform",
       caption="Source: SQL DataBase",
       x="Products",
       y="EU Sales") +
  theme_minimal()

qplot(Product, Global_Sales, colour=Platform, data=turtle_sales2, geom=c('point', 'jitter'))+
  labs(title= "Relationship between product and Global Sales",
       subtitle="Sales by Platform",
       caption="Source: SQL DataBase",
       x="Products",
       y="Global Sales") +
  theme_minimal()

# Qplot boxplots for NA, EU, and Global.
qplot(Product, NA_Sales, colour=Platform, data=turtle_sales2,
      geom='boxplot')+
  labs(title= "Boxplot of products and NA sales by Platform",
       subtitle="Products by NA Sales",
       caption="Source: SQL DataBase",
       x="Products",
       y="NA Sales") +
  theme_minimal()

qplot(Product, EU_Sales, colour=Platform, data=turtle_sales2,
      geom='boxplot')+
  labs(title= "Boxplot of products and EU sales by Platform",
       subtitle="Products by EU Sales",
       caption="Source: SQL DataBase",
       x="Products",
       y="EU Sales") +
  theme_minimal()

qplot(Product, Global_Sales, colour=Platform, data=turtle_sales2,
      geom='boxplot')+
  labs(title= "Boxplot of products and global sales by Platform",
       subtitle="Products by Global Sales",
       caption="Source: SQL DataBase",
       x="Products",
       y="Global Sales") +
  theme_minimal()

# Histograms for NA, EU, and Global.

plot(hist(turtle_sales2$NA_Sales))
plot(hist(turtle_sales2$EU_Sales))
plot(hist(turtle_sales2$Global_Sales))

# Qplot barplot for sales by platform.
qplot(Platform, data=turtle_sales2, geom='bar')+
  labs(title= "Barchart of Sales by Platform",
       caption="Source: SQL DataBase",
       x="Platform",
       y="Sales") +
  theme_minimal()

turtle_fac <- as.factor(turtle_sales2$Product)

str(turtle_fac)

# Check the following.
qplot(Platform, fill=Product, data=turtle_fac, geom='bar')

# Can I plot a line chart displaying NA, EU, and global sales?
# Plot
# ggplot(data=turtle_sales, aes(x=Year, y=NA_Sales, group=Platform, color=Platform))+
  # geom_line()

# Plot the bar chart.
# Work on this.
# p <- ggplot(turtle_sales, aes(x = Year, y = NA_Sales))+
  # geom_col(aes(fill = Platform), width = 0.7)+
  # coord_flip()
# p

# Aggregate
df <- turtle_sales2 %>% group_by(Product) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum)))
df

# View the data frame.
head(as_tibble(df))

install.packages('skimr')
library(skimr)

skim(df)

install.packages('DataExplorer')
library(DataExplorer)

# Create a report the data set.
DataExplorer::create_report(df)

library(moments)

###########
# Keep the necessary columns.
str(df)

head(df)
summary(df)
dim(df)

min(df$NA_Sales_sum)
max(df$NA_Sales_sum)
mean(df$NA_Sales_sum)
median(df$NA_Sales_sum)
sd(df$NA_Sales_sum)
# Calculate Q1 and Q3.
quantile(df$NA_Sales_sum, 0.25)  
quantile(df$NA_Sales_sum, 0.75)
# Calculate IQR.
IQR(df$NA_Sales_sum)  
# Determine the variance.
var(df$NA_Sales_sum)  

min(df$EU_Sales_sum)
max(df$EU_Sales_sum)
mean(df$EU_Sales_sum)
median(df$EU_Sales_sum)
sd(df$EU_Sales_sum)
# Calculate Q1 and Q3.
quantile(df$EU_Sales_sum, 0.25)  
quantile(df$EU_Sales_sum, 0.75)
# Calculate IQR.
IQR(df$EU_Sales_sum)  
# Determine the variance.
var(df$EU_Sales_sum)

min(df$Global_Sales_sum)
max(df$Global_Sales_sum)
mean(df$Global_Sales_sum)
median(df$Global_Sales_sum)
sd(df$Global_Sales_sum)
# Calculate Q1 and Q3.
quantile(df$Global_Sales_sum, 0.25)  
quantile(df$Global_Sales_sum, 0.75)
# Calculate IQR.
IQR(df$Global_Sales_sum)  
# Determine the variance.
var(df$Global_Sales_sum)

# Range = Max - Min.
max(df$NA_Sales_sum) - min(df$NA_Sales_sum)
max(df$EU_Sales_sum) - min(df$EU_Sales_sum)
max(df$Global_Sales_sum) - min(df$Global_Sales_sum)

# Specify the qqnorm function.
# Draw a qqplot using the total_seconds data.
qqnorm(df$NA_Sales_sum,
       col='blue',
       xlab="z Value",
       ylab='NA Sales')
# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(df$NA_Sales_sum,
       col='red',
       lwd=2) 

# Specify the qqnorm function.
# Draw a qqplot using the total_seconds data.
qqnorm(df$EU_Sales_sum,
       col='blue',
       xlab="z Value",
       ylab='EU Sales')
# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(df$EU_Sales_sum,
       col='red',
       lwd=2)

# Specify the qqnorm function.
# Draw a qqplot using the total_seconds data.
qqnorm(df$Global_Sales_sum,
       col='blue',
       xlab="z Value",
       ylab='Global Sales')
# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(df$Global_Sales_sum,
       col='red',
       lwd=2)

# Run a Shapiro-Wilk test:
shapiro.test(df$NA_Sales_sum)
# Check skewness and Kurtosis
skewness(df$NA_Sales_sum)
kurtosis(df$NA_Sales_sum)

# Run a Shapiro-Wilk test:
shapiro.test(df$EU_Sales_sum)
# Check skewness and Kurtosis
skewness(df$EU_Sales_sum)
kurtosis(df$EU_Sales_sum)

# Run a Shapiro-Wilk test:
shapiro.test(df$Global_Sales_sum)
# Check skewness and Kurtosis
skewness(df$Global_Sales_sum)
kurtosis(df$Global_Sales_sum)
# These p-values indicate that we do not have a normal distribution.

# Specify histogram function.
hist(df$NA_Sales_sum)
hist(df$EU_Sales_sum)
hist(df$Global_Sales_sum)

boxplot(df$NA_Sales_sum)
boxplot(df$EU_Sales_sum)
boxplot(df$Global_Sales_sum)

library(ggplot2)

# Histogram.
# Start with a simple histogram.
ggplot(df, aes(x=NA_Sales_sum)) + 
  geom_histogram(bins = 20)+
  labs(title= "Histogram of NA sales",
       subtitle="NA Sales Distribution",
       caption="Source: SQL DataBase",
       x="NA Sales",
       y="Count of NA Sales") +
  theme_minimal()

ggplot(df, aes(x=EU_Sales_sum)) + 
  geom_histogram(bins = 20)+
  labs(title= "Histogram of EU sales",
       subtitle="EU Sales Distribution",
       caption="Source: SQL DataBase",
       x="EU Sales",
       y="Count of EU Sales") +
  theme_minimal()

ggplot(df, aes(x=Global_Sales_sum)) + 
  geom_histogram(bins = 20)+
  labs(title= "Histogram of global sales",
       subtitle="Global Sales Distribution",
       caption="Source: SQL DataBase",
       x="Global Sales",
       y="Count of Global Sales") +
  theme_minimal()

# Smoothed density plot.
ggplot(df, aes(x=NA_Sales_sum)) + 
  geom_density() +
  labs(title= "Density Plot of NA Sales",
       subtitle="NA Sales Density",
       caption="Source: SQL DataBase",
       x="NA Sales sum",
       y="Density of NA Sales") +
  theme_minimal()

ggplot(df, aes(x=EU_Sales_sum)) + 
  geom_density() +
  labs(title= "Density Plot of EU Sales",
       subtitle="EU Sales Density",
       caption="Source: SQL DataBase",
       x="EU Sales sum",
       y="Density of EU Sales") +
  theme_minimal()

ggplot(df, aes(x=Global_Sales_sum)) + 
  geom_density() +
  labs(title= "Density Plot of Global Sales",
       subtitle="Global Sales Density",
       caption="Source: SQL DataBase",
       x="Global Sales sum",
       y="Density of Global Sales") +
  theme_minimal()

# NA.
# Start with a simple scatterplot.
ggplot(df, aes(x=Product, y=NA_Sales_sum)) + 
  geom_point() +
  labs(title= "Scatterplot of NA Sales by Product",
       caption="Source: SQL DataBase",
       x="Product",
       y="NA Sales Sum") +
  theme_minimal()

# Add jitter and line of best fit.
ggplot(df, aes(x=jitter(Product), y=NA_Sales_sum)) + 
  geom_point() +
  geom_smooth(method=lm) +
  labs(title= "Scatterplot of NA Sales by Product",
       subtitle="Line of best fit and standard error",
       caption="Source: SQL DataBase",
       x="Product",
       y="NA Sales Sum") +
  theme_minimal()

# Scatterplot with no method in geom_smooth() (spline).
ggplot(df, aes(x=jitter(Product), y=NA_Sales_sum)) + 
  geom_point() +
  geom_smooth() +
  labs(title= "Scatterplot of NA Sales by Product",
       subtitle="Smoothed line of best fit and standard error",
       caption="Source: SQL DataBase",
       x="Product",
       y="NA Sales Sum") +
  theme_minimal()

# Remove standard error and thicken line.
ggplot(df, aes(x=jitter(Product), y=NA_Sales_sum)) + 
  geom_point() +
  geom_smooth(lwd=2, se=FALSE) +
  labs(title= "Scatterplot of NA Sales by Product",
       subtitle="Smoothed line of best fit",
       caption="Source: SQL DataBase",
       x="Product",
       y="NA Sales Sum") +
  theme_minimal()

# EU.
ggplot(df, aes(x=Product, y=EU_Sales_sum)) + 
  geom_point() +
  labs(title= "Scatterplot of EU Sales by Product",
       caption="Source: SQL DataBase",
       x="Product",
       y="EU Sales Sum") +
  theme_minimal()

# Add jitter and line of best fit.
ggplot(df, aes(x=jitter(Product), y=EU_Sales_sum)) + 
  geom_point() +
  geom_smooth(method=lm) +
  labs(title= "Scatterplot of EU Sales by Product",
       subtitle="Line of best fit and standard error",
       caption="Source: SQL DataBase",
       x="Product",
       y="EU Sales Sum") +
  theme_minimal()

# Scatterplot with no method in geom_smooth() (spline).
ggplot(df, aes(x=jitter(Product), y=EU_Sales_sum)) + 
  geom_point() +
  geom_smooth() +
  labs(title= "Scatterplot of EU Sales by Product",
       subtitle="Smoothed line of best fit and standard error",
       caption="Source: SQL DataBase",
       x="Product",
       y="EU Sales Sum") +
  theme_minimal()

# Remove standard error and thicken line.
ggplot(df, aes(x=jitter(Product), y=EU_Sales_sum)) + 
  geom_point() +
  geom_smooth(lwd=2, se=FALSE) +
  labs(title= "Scatterplot of EU Sales by Product",
       subtitle="Smoothed line of best fit",
       caption="Source: SQL DataBase",
       x="Product",
       y="EU Sales Sum") +
  theme_minimal()

# Global.
ggplot(df, aes(x=Product, y=Global_Sales_sum)) + 
  geom_point() +
  labs(title= "Scatterplot of Global Sales by Product",
       caption="Source: SQL DataBase",
       x="Product",
       y="Global Sales Sum") +
  theme_minimal()

# Add jitter and line of best fit.
ggplot(df, aes(x=jitter(Product), y=Global_Sales_sum)) + 
  geom_point() +
  geom_smooth(method=lm) +
  labs(title= "Scatterplot of Global Sales by Product",
       subtitle="Line of best fit and standard error",
       caption="Source: SQL DataBase",
       x="Product",
       y="Global Sales Sum") +
  theme_minimal()

# Scatterplot with no method in geom_smooth() (spline).
ggplot(df, aes(x=jitter(Product), y=Global_Sales_sum)) + 
  geom_point() +
  geom_smooth() +
  labs(title= "Scatterplot of Global Sales by Product",
       subtitle="Smoothed line of best fit and standard error",
       caption="Source: SQL DataBase",
       x="Product",
       y="Global Sales Sum") +
  theme_minimal()

# Remove standard error and thicken line.
ggplot(df, aes(x=jitter(Product), y=Global_Sales_sum)) + 
  geom_point() +
  geom_smooth(lwd=2, se=FALSE) +
  labs(title= "Scatterplot of Global Sales by Product",
       subtitle="Smoothed line of best fit",
       caption="Source: SQL DataBase",
       x="Product",
       y="Global Sales Sum") +
  theme_minimal()

str(turtle_sales2)

# Add a further/third variable as a colour and remove the smoothing line.
ggplot(turtle_sales2, aes(x=Product, y=NA_Sales, col=Platform)) + 
  geom_point() +
  labs(title= "Scatterplot of NA Sales by Product",
       subtitle="Platform detail",
       caption="Source: SQL DataBase",
       x="Product",
       y="NA Sales") +
  theme_minimal()

ggplot(turtle_sales2, aes(x=Product, y=EU_Sales, col=Platform)) + 
  geom_point()+
  labs(title= "Scatterplot of EU Sales by Product",
       subtitle="Platform detail",
       caption="Source: SQL DataBase",
       x="Product",
       y="EU Sales") +
  theme_minimal()

ggplot(turtle_sales2, aes(x=Product, y=Global_Sales, col=Platform)) + 
  geom_point() +
  labs(title= "Scatterplot of Global Sales by Product",
       subtitle="Platform detail",
       caption="Source: SQL DataBase",
       x="Product",
       y="Global Sales") +
  theme_minimal()

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
             size=3) +
  ggtitle("NA Sales by Product") +
  xlab('Product') +
  ylab('NA Sales') +
  theme_minimal()

# EU.
ggplot(data=turtle_sales2, 
       mapping=aes(x=Product, y=EU_Sales)) +
  geom_point()

new_EU <- filter(turtle_sales2, EU_Sales<15)

ggplot(data=new_EU,
       mapping=aes(x=Product, y=EU_Sales)) + 
  geom_point(color='blue',
             alpha=0.5,  
             size=3)+
  ggtitle("EU Sales by Product") +
  xlab('Product') +
  ylab('EU Sales') +
  theme_minimal()

# Global.
ggplot(data=turtle_sales2, 
       mapping=aes(x=Product, y=Global_Sales)) +
  geom_point()+
  theme_minimal()

new_Global <- filter(turtle_sales2, Global_Sales<40)

ggplot(data=new_Global,
       mapping=aes(x=Product, y=Global_Sales)) + 
  geom_point(color='purple',
             alpha=0.5,  
             size=3)+
  ggtitle("Global Sales by Product") +
  ggtitle("Global Sales by Product") +
  xlab('Product') +
  ylab('Global Sales') +
  theme_minimal()

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

######################
# Week 6
# The sales department wants to better understand whether there is any relationship between North America, 
# Europe, and global sales. 

# Determine the correlation between the sales columns. 
# Create a model with only one x variable.
model1 <- lm(Global_Sales_sum~NA_Sales_sum, data=df)

# View the model.
model1
# One is the intercept and the other is the slope of the line of best fit.

# View more outputs for the model - the full regression table.
summary(model1)
# The p-value is highly significant, the t value is encouraging as the larger it is the more likely is 
# that the coefficient is significant, and the R-Squared value is very nice as the best is 1. 
# This is a good model.

# Plot the model.
# View residuals on a plot.
plot(model1$residuals)

# Plot the relationship with base R graphics.
plot(df$Global_Sales_sum, df$NA_Sales_sum)
# Add line-of-best-fit.
abline(coefficients(model1))

# Complete a log transformation with dplyr's mutate() function.
dfNA <- mutate(df, 
              logNA=log(NA_Sales_sum))

# View new object with new variable.
head(dfNA)

# Create a new model using logIndex.
model2 <- lm(Global_Sales_sum~logNA, data=dfNA)

# View full regression table.
summary(model2)

# Plot the relationship between year and logIndex.
plot(dfNA$Global_Sales_sum, dfNA$logNA)

# Add a line-of-best fit to existing plot.
abline(coefficients(model2))

######################################
# Create a model with only one x variable.
model3 <- lm(Global_Sales_sum~EU_Sales_sum, data=df)

# View the model.
model3
# One is the intercept and the other is the slope of the line of best fit.

# View more outputs for the model - the full regression table.
summary(model3)
# The p-value is highly significant, the t value is encouraging as the larger it is the more likely is 
# that the coefficient is significant, and the R-Squared value is very nice as the best is 1. 
# This is a good model.

# Plot the model

# View residuals on a plot.
plot(model3$residuals)

# Plot the relationship with base R graphics.
plot(df$Global_Sales_sum, df$EU_Sales_sum)
# Add line-of-best-fit.
abline(coefficients(model3))

# Complete a log transformation with dplyr's mutate() function.
dfEU <- mutate(df, 
              logEU=log(EU_Sales_sum))

# View new object with new variable.
head(dfEU)

# Create a new model using logIndex.
model4 <- lm(Global_Sales_sum~logEU, data=dfEU)

# View full regression table.
summary(model4)

# Plot the relationship between year and logIndex.
plot(dfEU$Global_Sales_sum, dfEU$logEU)

# Add a line-of-best fit to existing plot.
abline(coefficients(model4))

######################

# Create a multiple linear regression model.

# Determine correlation between variables.
cor(df)

# Create a new object and 
# specify the lm function and the variables.
MLRmodel <- lm(Global_Sales_sum~EU_Sales_sum+NA_Sales_sum, data=df)

# Print the summary statistics.
summary(MLRmodel)

# Create a new object and specify the predict function.
predictTest <- predict(MLRmodel, newdata=df,
                      interval='confidence')

# Print the object.
predictTest

plot(predict(MLRmodel), df$Global_Sales_sum, xlab = 'Predicted Global Sales', ylab = 'Observed Global Sales')
abline(a=0, b=1)
# There is a strong positive relation between the predicted and the observed Global Sales, this means that the model I have 
# choosen is the best.

str(predictTest)
col(predictTest)
View(predictTest)
str(df)
# Turn list into data frame.
as.data.frame(predictTest)

# Add Product ID to the new predicted values.
forecast_df <- cbind(predictTest, Product = df$Product)
View(forecast_df)

# Merge the 2 data frames for a quick comparison.
forecast_df_new <- merge(x = df, y = forecast_df, by = "Product", all.x = TRUE)

View(forecast_df_new)