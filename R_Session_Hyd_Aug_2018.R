# install packages - packages are collection of functions that makes life of users very useful
install.packages('dplyr')

#dplyr is one of the most popular packages

#load package every time you start a new R session
library('dplyr')

# some key functions in dyplr
#filter() to select cases based on their values.
#arrange() to reorder the cases.
#select() and rename() to select variables based on their names.
#summarise() to condense multiple values to a single value.


# setting working directory from your location read and write.
setwd("D:/R_Session_Hyd/")
# note that the slashes are forward slashes and not backword slashes

#retriving working directory
getwd()

#read CSV in r
mydata <- read.csv("batting_data_cric_info_v2.csv", header = TRUE)

#check the dimensions of the data. Returns number of columns and rows
dim(mydata)

# check the records. 
mydata[1, ]  
mydata[1:10, ]
mydata[5:10, ]


# check first top/bottom 5 records
head(mydata,5)
tail(mydata,5)

# what is the max runs scored on overall career?
max(mydata$Runs)

# filter for Indian Batsmen
filter(mydata, Country == "INDIA" )

#arrange() to reorder the cases.
arrange(mydata, Runs)
arrange(mydata, desc(Runs))
arrange(mydata, Country, desc(Runs))

# filter for Indian Batmen whose ODI debut was after 2000
filter(mydata, Country == "INDIA", Start_Year > 2000 )

#arrange it by Runs
arrange (filter(mydata, Country == "INDIA", Start_Year > 2000 ), desc(Runs))

#want to summarize information country avg run and avg avg
summ <- mydata %>%
select (Runs, Ave) %>%
group_by(mydata$Country) %>%
summarise(Runs = mean(Runs), Ave = mean(Ave))

# want to summarize and have more aggregate variables
summ <- mydata %>%
select (Runs, Ave) %>%
group_by(mydata$Country) %>%
summarise(MinRuns = min(Runs),MeanRuns = mean(Runs),MaxRuns = max(Runs), MeanAve = mean(Ave))


#get player by country who scored the maximum runs
mydata.agg <- aggregate(Runs ~ Country, mydata, max)

# then simply merge with the original
mydata.Runs = merge(mydata.agg, mydata)
# sort data by runs
mydata.Runs[order(-mydata.Runs$Runs),]

#histogram. To look at the vaiabilty in the data. Point aggregates are sometimes misleading
hist(mydata$Runs,xlab = "Runs",col = "yellow",border = "blue")
filter(mydata, Runs > 17000)


#get the player from India who scored the highest runs
filter(merge(mydata.agg,mydata), Country == "INDIA")
# Sachin Tendulkar

# pie chart for the Runs contribution by Indian Players
pie(filter(mydata, Country == "INDIA")$Runs, labels = filter(mydata, Country == "INDIA")$Player)

#get players by country who has highest avg
mydata.agg <- aggregate(Ave ~ Country, mydata, max)
# then simply merge with the original
mydata.Ave = merge(mydata.agg, mydata)
# sort data by Ave
mydata.Ave[order(-mydata.Ave$Ave),]

#histogram
hist(mydata$Ave,xlab = "Ave",col = "yellow",border = "blue")
head(mydata)
filter(mydata, Ave > 55)

#get the player from India who has highest Ave
filter(merge(mydata.agg,mydata), Country == "INDIA")
#Virat kohli

#get players by country who has highest SR
mydata.agg <- aggregate(SR ~ Country, mydata, max)
# then simply merge with the original
mydata.SR = merge(mydata.agg, mydata)
# sort data by SR
mydata.SR[order(-mydata.SR$SR),]


#histogram
hist(mydata$SR,xlab = "SR",col = "yellow",border = "blue")
filter(mydata, SR > 110)

#get the player from India who has higest SR
filter(merge(mydata.agg,mydata), Country == "INDIA")
# V Sehwag

#get players by country who has highest NO
mydata.agg <- aggregate(NO ~ Country, mydata, max)

# then simply merge with the original to obtain corresponding to the palyer with higest no of NO
mydata.NO = merge(mydata.agg, mydata)
# sort data by NO
mydata.NO[order(-mydata.NO$NO),]

#histogram
hist(mydata$NO,xlab = "NO",col = "yellow",border = "blue")
filter(mydata, NO > 70)

#get the player from India who has higest NO
filter(merge(mydata.agg,mydata), Country == "INDIA")
# MS Dhoni


# How do we do decide who is most valuable player for India?
# Mat, Runs, NO, Avg, SR.

# Different folks would want to have more sophisticated ways to calcute impact
# Home-Away | Win - Loss | % proportion of  runs contribution | By batting positions
# we are taking a simplistic approach to give equal weights to 5 different factors

#rank based on Mat
mydata$rank_mat = rank(-mydata$Mat)
#Order by rank
mydata[order(mydata$rank_mat),]

#rank based on runs
mydata$rank_runs = rank(-mydata$Runs)
#Order by rank
mydata[order(mydata$rank_runs),]

#rank based on NO
mydata$rank_no = rank(-mydata$NO) # - is used to identify highest number with low numerical rank
#Order by rank
mydata[order(mydata$rank_no),]

#rank based on Ave
mydata$rank_ave = rank(-mydata$Ave)
#Order by rank
mydata[order(mydata$rank_ave),]


#rank based on SR
mydata$rank_sr = rank(-mydata$SR)
#Order by rank
mydata[order(mydata$rank_sr),]


#creating ranks of ranks
mydata$rank_of_ranks = rank(mydata$rank_mat + mydata$rank_runs + mydata$rank_ave + mydata$rank_sr + mydata$rank_no)
mydata[order(mydata$rank_of_ranks),]

head(mydata[order(mydata$rank_of_ranks),],10)


# most valuable players by countries
mydata.mvp <- aggregate(rank_of_ranks ~ Country, mydata, min)
# then simply merge with the original
mydata.mvp1 = merge(mydata.mvp, mydata)
# sort data by ranks of ranks
mydata.mvp1[order(mydata.mvp1$rank_of_ranks),]


################################### Use Case 2 ######################################
# How to estimate mileage based on car attributes?
# Identify what factors should be considered for estimating car attributes

# import the data
mydata_2 <- read.csv("mpg.csv", header = TRUE)

# check data 
dim(mydata_2)

head(mydata_2)

# need to know the unit measure of dimensions of data being used
#plot with variable name
plot(mydata_2$Weight, mydata_2$Mpg, xlab = "Weight", ylab="Mpg")

# Predict mileage based on car attributes
# Lets take one variable and test the direction of relation

#scatter plot with variable and chart names
scatter.smooth(x=mydata_2$Weight, y=mydata_2$Mpg, main="Mpg ~ Weight")
# check for outlier - park for later

#correlation - test the direction of relationship
cor(mydata_2$Mpg, mydata_2$Weight)

#first iteration
relation <- lm(mydata_2$Mpg ~ mydata_2$Weight)
summary(relation)

# second iteration - length

#scatter plot with variable and chart names
scatter.smooth(x=mydata_2$Length, y=mydata_2$Mpg, main="Mpg ~ Length")

#correlation
cor(mydata_2$Mpg, mydata_2$Length)

relation <- lm(mydata_2$Mpg ~ mydata_2$Length +  mydata_2$Weight, data = mydata_2)
summary(relation)


# third iteration

#scatter plot with variable and chart names
scatter.smooth(x=mydata_2$Horsepower, y=mydata_2$Mpg, main="Mpg ~ Horsepower")

#correlation
cor(mydata_2$Mpg, mydata_2$Horsepower)

relation <- lm(mydata_2$Mpg ~ mydata_2$Length +  mydata_2$Weight +mydata_2$Horsepower, data = mydata_2)
summary(relation)


#Correlation matrix
cor(mydata_2)

relation <- lm(mydata_2$Mpg ~ mydata_2$Length +  mydata_2$Weight +mydata_2$Horsepower +mydata_2$Rearseat, data = mydata_2)
summary(relation)


# Let us see the impact of removing outliers
scatter.smooth(x=mydata_2$Weight, y=mydata_2$Mpg, main="Mpg ~ Weight")


mydata_3 <- filter(mydata_2, Mpg<45)

relation <- lm(mydata_3$Mpg ~ mydata_3$Length +  mydata_3$Weight +mydata_3$Horsepower +mydata_3$Rearseat, data = mydata_3)
summary(relation)


