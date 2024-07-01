africa_sun = read.csv("Africa_sunshine.csv")
asia_sun = read.csv("Asia_sunshine.csv")
europe_sun =read.csv("Europe_sunshine.csv")
northA_sun = read.csv("NorthAmerica_sunshine.csv")
oceania_sun = read.csv("Oceania_sunshine.csv")

country_div = read.csv("Country_Divorce.csv")
mentalhealth = read.csv("share_with_mentalhealth.csv")
happy= read.csv("SimpleData.csv")

###HAPPY/POSITIVE AFFECT DATA 
##filter to only keep 2018 data and positive affect by country 
positiveaffect = filter(happy, Year==2018)
positiveaffect = positiveaffect[,c(2,11)]
head(positiveaffect)

## SUN DATA 
## only take the yearly data
africa_sun = africa_sun[,c(1,15)]
asia_sun = asia_sun[,c(1,15)]
europe_sun= europe_sun[,c(1,15)]
northA_sun = northA_sun[,c(1,15)]
oceania_sun = oceania_sun[,c(1,15)]

## get the mean across years 
africasun_agg = aggregate(africa_sun$Year, by=list(africa_sun$Country), FUN = mean)
asiasun_agg = aggregate(asia_sun$Year, by=list(asia_sun$Country), FUN = mean)
europesun_agg = aggregate(europe_sun$Year, by=list(europe_sun$Country), FUN = mean)
northAsun_agg = aggregate(northA_sun$Year, by=list(northA_sun$Country), FUN = mean)
oceaniasun_agg= aggregate(oceania_sun$Year, by=list(oceania_sun$Country), FUN = mean)

## append the different files 
country_sun = rbind(africasun_agg,asiasun_agg,europesun_agg,northAsun_agg,oceaniasun_agg)
View(country_sun)

names(country_sun)[1]<- "Country"
names(country_sun)[2] <- "Sunlight"
View(country_div)

### MENTAL HEALTH DATA 

#get the lastet data per country. year 2017 
mentalhealth2017 = filter(mentalhealth, Year==2017 )
View(mentalhealth2017)

## join data sets together using the biggest one as the base 
country_data = merge(mentalhealth2017, country_sun, by.x= "Entity", by.y="Country" )
country_data = merge(country_data, country_div, by.x= "Entity", by.y= "Country")
country_data = merge(country_data, positiveaffect , by.x= "Entity", by.y= "Country.name")

country_data2 = country_data[,c(1,4,5,6,7,8,10)]
names(country_data2)[2]<-"Mental Health ratio"
country_data2= na.omit(country_data2)

## we have some missing data because some didnt join as they have diff names 
## also removed countries rows with NA just to keep data clean 
## left with 67 countries/observations 

### ANALYSIS 
cor_m= cor(country_data2[2:length(country_data2)])
cor_m
corrplot(cor_m, method="number")

##regression 
positive_lm = lm(Positive.affect ~., data= country_data2[,2:length(country_data2)])
summary(positive_lm)

##CART
library(caret) # for randomly splitting training/test 
library(rpart) # for building CART model
library(rpart.plot)
set.seed(144)

country_tree = rpart(Positive.affect ~., data=country_data2[,2:length(country_data2)])
prp(country_tree)

##calculating R2
pred = predict(country_tree, newdata=country_data2[,2:length(country_data2)])
mean_train = mean(country_data2$Positive.affect)

SSETrain = sum((pred - country_data2$Positive.affect)^2)
SSTTrain = sum((country_data2$Positive.affect - mean_train)^2)
# R2 is 1 minus the ratio of these terms
R2 = 1 - SSETrain/SSTTrain
R2
