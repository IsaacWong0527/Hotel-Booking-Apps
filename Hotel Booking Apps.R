##load data 
data = read.csv("SF_listings.csv")
##inspect data
names(data)
summary(data)   
str(data)
summary(data2$price)   
max(as.numeric(data2$price))

##load required libraries
library(tidyverse)
library(gridExtra)
library(dslabs)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra) 


unique(data$availability_365)

#####################################################################################
## filter for only active record, getting listings that have availability in the next year and have had a review since the beginning of 2019
data <- data %>% filter((as.Date(last_review, "%m/%d/%Y") >= "2019-01-01") & !is.na(last_review) & as.numeric(availability_365) > 0 & !is.na(host_is_superhost))

summary(data$price)

##first step in the exploratory analysis is to look at distribution of variables of interest that will serve as our dependent variables in each of their respective models



#summary stats for ratings, which is a total composite score based on the other ratings categories 
summary(data$review_scores_rating)

#histogram with number of ratings
review_plot <- ggplot(data, aes(x = as.numeric(review_scores_rating))) +
  geom_histogram(aes(y = ..count..), binwidth = 3) +
  scale_x_continuous(name = "Composite Rating Score") +
  scale_y_continuous(name = "Count") +
  ggtitle("3. Frequency Histogram of Rating Score")
review_plot

#histogram with number of price 
price_plot <- ggplot(data, aes(x = as.numeric(price))) +
  geom_histogram(aes(y = ..count..), binwidth = 300) +
  scale_x_continuous(name = "Price, in USD") +
  scale_y_continuous(name = "Count") +
  ggtitle("1. Frequency Histogram of Prices")
price_plot

#remove outliers for price and remove na values 
#data <- data %>% filter(as.numeric(data$price)<473)
#data2 %>% drop_na(data, data$price)
data %>% filter(complete.cases(data$price))



#plot without outliers 
price_plot_no_out <- ggplot(data, aes(x = as.numeric(price))) +
  geom_histogram(aes(y = ..count..), binwidth = 20) +
  scale_x_continuous(name = "Price, in USD") +
  scale_y_continuous(name = "Count") +
  ggtitle("2. Frequency Histogram of Prices (no outliers)") +
  xlim(c(0, 473))
price_plot_no_out

#summary stats for price
summary(data$review_scores_rating)

#histogram with number of reviews, with line at outliers cutoff point 
review_plot_out <- ggplot(data, aes(x = as.numeric(number_of_reviews))) +
  geom_histogram(aes(y = ..count..), binwidth = 10) +
  scale_x_continuous(name = "Number of reviews") +
  scale_y_continuous(name = "Count") +
  ggtitle("4. Frequency Histogram of Number of Reviews") +
  geom_vline(xintercept = 230, size = 1, colour = "#FF3721",
             linetype = "dashed")
review_plot_out


# put all 4 ggplots together
grid.arrange(price_plot, price_plot_no_out, review_plot, review_plot_out)


#summary stats for reviews
summary(data$review)


##scatterplot of reviews on price
review_price <- ggplot(data, aes(x=as.numeric(number_of_reviews), y=as.numeric(price))) + 
  geom_point() +
  scale_x_continuous(name = "Number of reviews") +
  scale_y_continuous(name = "Price") +
  ggtitle("5. Number of Reviews vs Price") +
  ylim(c(0, 473))

#scatterplot of log transformation of price and reviews 
log_review_price <- ggplot(data, aes(x = as.numeric(number_of_reviews), y = as.numeric(price))) +
  geom_point() +
  scale_y_log10()+
  ggtitle('6. Number of Reviews vs Price (log)') +
  xlab("Number of Reviews") +
  ylab("log(Price)")

grid.arrange(review_price, log_review_price, nrow = 1)

##scatterplot of no of reviews and average ratings. shows no relationship between number of reviews and ratings. Airbnb is known for the fact that reviews site are overwhelmingly positive.

review_rate_score <- ggplot(data, aes(x=as.numeric(number_of_reviews), y=as.numeric(review_scores_rating))) + 
  geom_point() +
  scale_x_continuous(name = "Number of reviews") +
  scale_y_continuous(name = "Rating Score") +
  ggtitle("7. Number of Reviews vs Rating Score") 

#scatterprice of log transformation of reviews and ratings 
log_review_rate_score <- ggplot(data, aes(x = as.numeric(number_of_reviews), y = as.numeric(review_scores_rating))) +
  geom_point() +
  scale_x_log10()+scale_y_log10()+ggtitle('8. Number of Reviews vs Rating Score (log)') +
  xlab("log(Number of Reviews)") +
  ylab("log(Rating Score)")


grid.arrange(review_rate_score, log_review_rate_score, nrow = 1)


##scatterplot of ratings on price 
rate_score_price <- ggplot(data, aes(x=as.numeric(review_scores_rating), y=as.numeric(price))) + 
  geom_point() +
  scale_x_continuous(name = "Rating Score") +
  scale_y_continuous(name = "Price") +
  ggtitle("9. Rating Score vs Price") 

#scatterprice of log transformation ratings and price 
log_rate_score_price <- ggplot(data, aes(x = as.numeric(review_scores_rating), y = as.numeric(price))) +
  geom_point() +
  scale_x_log10() + scale_y_log10()+ggtitle("10. Rating Score vs Price (log)") +
  xlab("log(Rating Score)") +
  ylab("log(Price)")

##[33] "review_scores_rating"               "review_scores_accuracy"            
##[35] "review_scores_cleanliness"          "review_scores_checkin"             
##[37] "review_scores_communication"        "review_scores_location"            
##[39] "review_scores_value"                

grid.arrange(rate_score_price, log_rate_score_price, nrow = 1)


########################################

##scatterplot of accuracy ratings on price 
accuracy_on_price <- ggplot(data, aes(x=as.numeric(review_scores_accuracy), y=as.numeric(price))) + 
  geom_point()+ggtitle('11. Accuracy Rating vs Price') +
  xlab("Review Scores Accuracy") +
  ylab("Price")
accuracy_on_price

##scatterplot of cleanliness ratings on price 
cleanliness_on_price <- ggplot(data, aes(x=as.numeric(review_scores_cleanliness), y=as.numeric(price))) + 
  geom_point()+ggtitle('12. Cleanliness Rating vs Price') +
  xlab("Review Scores Cleanliness") +
  ylab("Price")
cleanliness_on_price

##scatterplot of checkin ratings on price 
checkin_on_price <- ggplot(data, aes(x=as.numeric(review_scores_checkin), y=as.numeric(price))) + 
  geom_point()+ggtitle('13. Checkin Rating vs Price') +
  xlab("Review Scores Checkin") +
  ylab("Price")
checkin_on_price


##scatterplot of communication ratings on price 
communication_on_price <- ggplot(data, aes(x=as.numeric(review_scores_communication), y=as.numeric(price))) + 
  geom_point()+ggtitle('14. Communication Rating vs Price') +
  xlab("Review Scores Communication") +
  ylab("Price")
communication_on_price


##scatterplot of location  on price 
location_on_price <- ggplot(data, aes(x=as.numeric(review_scores_location), y=as.numeric(price))) + 
  geom_point()+ggtitle('15. Location Rating vs Price') +
  xlab("Review Scores Location") +
  ylab("Price")
location_on_price

##scatterplot of value on price 
value_on_price <- ggplot(data, aes(x=as.numeric(review_scores_value), y=as.numeric(price))) + 
  geom_point()+ggtitle('16. Value Rating vs Price') +
  xlab("Review Scores Value") +
  ylab("Price")
value_on_price

##grid showing little variablility in rating across rating categories 
grid.arrange(accuracy_on_price, cleanliness_on_price, checkin_on_price, communication_on_price, location_on_price, value_on_price, nrow = 3)



########################################################################################

##not enough variability in the different rating categories 

##splitting the data into two groups: 1)highly reviewed listing and 2)listings with average reviews
## put outliers (i.e. highly reviewed listings) in a vector 
outliers <- boxplot(data$number_of_reviews, plot=FALSE)$out

min(outliers)


##couting the number of outliers 
length(outliers)

# Locate rows outliers are in
data[which(data$number_of_reviews %in% outliers),]

##new data frame with outliers removed 
outliers_removed <- data[-which(data$number_of_reviews %in% outliers),]

##histogram with no outliers
outliers_removed_review_plot <- ggplot(outliers_removed, aes(outliers_removed$number_of_reviews)) + 
  geom_histogram()+ggtitle('17.Frequency of Number of Reviews (No Outliers)')+ xlab("Number of Reviews") + ylab("Count")
outliers_removed_review_plot

##box plot of number of reviews with no outliers 
boxplot(outliers_removed$number_of_reviews)

##dataframe of outliers which we will call higly reviewed ads
outliers_only <- data[which(data$number_of_reviews %in% outliers),]
##of the 4130 obs. we have 260 outliers, listings which have number of reviews that are more than 1.5 times the IQR of the 3rd quardrant. We will call these listings highly reveiewed listings and compare characteristic between highly review listings (no. of reviews > xx) and listings with average number of reviews 

##histogram outliers
outliers_review_plot <- ggplot(outliers_only, aes(number_of_reviews)) + 
  geom_histogram() +ggtitle('18.Frequency of Number of Reviews (Outliers Only)') + xlab("Number of Reviews") +ylab("Count")
outliers_review_plot

#combine 17 18 
grid.arrange(outliers_removed_review_plot, outliers_review_plot, nrow = 1)

##creating a new column to designate if listing is highly reviewed 
data <- within(data, is_highly_reviewed <- ifelse(number_of_reviews>=230, 1, 
                                                  ifelse(number_of_reviews<230,0,NA))) 


##comparing distribution of price for highly reviewed vs average reviewed listing 
p9 <- ggplot(data, aes(x=as.factor(is_highly_reviewed),y=as.numeric(price), fill=as.factor(is_highly_reviewed))) +
  geom_boxplot() +
  ggtitle("19.Box Plot of Highly Reviewed VS Not Highly Reviewed Listing") + xlab("Highly ReViewed") + theme(legend.position="bottom") +theme(legend.title = element_blank()) + ylab("Price")
p9

##### starting point for superhost graphs
library(scales)

data_group_superhost <- data %>% group_by(host_is_superhost) %>% 
  summarize(count = n()) %>%  # count records by superhost
  mutate(Percent = count/sum(count))  # find percent of total

ggplot(data_group_superhost, aes(as.factor(host_is_superhost), Percent, fill = as.factor(host_is_superhost))) + 
  geom_bar(stat='identity') + 
  geom_text(aes(label=scales::percent(Percent)), position = position_stack(vjust = .5))+
  scale_y_continuous(labels = scales::percent)+xlab("Superhost or Not")+ theme(legend.position="bottom") +theme(legend.title = element_blank()) + ggtitle("20. Percentage of Superhosts VS Regular Hosts")

##comparing distribution over price for superhost #super host has higher price
p11 <- ggplot(data, aes(x=as.factor(host_is_superhost),y=as.numeric(price), fill= as.factor(host_is_superhost))) + geom_boxplot() +
  ggtitle("21. Distribution of Prices for Listings of Superhosts VS Those of Regular Hosts") + xlab("Superhost or Not") + theme(legend.position="bottom") +theme(legend.title = element_blank()) + ylab("Price")
p11

##look at regularly review vs highly review by superhost penetration 

ggplot(data,aes(x=as.factor(is_highly_reviewed),y=as.numeric(price),group=interaction(host_is_superhost, is_highly_reviewed),fill= as.factor(host_is_superhost)))+
  geom_boxplot(width=0.45,position=position_dodge(width=0.5))+theme_bw()+
  labs(fill="") +
  ggtitle("22.Box plot of Highly Reviewed vs  Not Highly Reviewed by Superhost Status") +
  labs(x = "Highly Reviewed (Y=1,N=0) Split by Host status", y = "Price") + theme(legend.position="bottom") + scale_fill_discrete(labels=c("Not Superhost", "Superhost"))

colnames(data)
library(reshape)



##host_response_time" 
##if.host.location.is.san.fran.1.Yes"
data_group_sanfran <- data %>% group_by(if.host.location.is.san.fran.1.Yes) %>% 
  summarize(count = n()) %>%  # count records by host location is sanfran
  mutate(Percent = count/sum(count))  # find percent of total

ggplot(data_group_sanfran, aes(as.factor(if.host.location.is.san.fran.1.Yes), Percent, fill = as.factor(if.host.location.is.san.fran.1.Yes))) + 
  geom_bar(stat='identity') + 
  geom_text(aes(label=scales::percent(Percent)), position = position_stack(vjust = .5))+
  scale_y_continuous(labels = scales::percent)+xlab("Host Location Is SanFran Or Not")+ theme(legend.position="bottom") +theme(legend.title = element_blank()) + ggtitle("23. Percentage of Host Location Is SanFran VS Not SanFran")

###see sanfran, super host #for both superhost has higher price
ggplot(data,aes(x=as.factor(if.host.location.is.san.fran.1.Yes),y=as.numeric(price),group=interaction(host_is_superhost, if.host.location.is.san.fran.1.Yes),fill= as.factor(host_is_superhost)))+
  geom_boxplot(width=0.45,position=position_dodge(width=0.5))+theme_bw()+
  labs(fill="") +
  ggtitle("24.Box plot of Host Location Is SanFran vs  Not SanFran by Superhost Status") +
  labs(x = "Host Location Is SanFran (Y=1,N=0) Split by Host status", y = "Price") + theme(legend.position="bottom") + scale_fill_discrete(labels=c("Not Superhost", "Superhost"))


##require_guest_phone_verification #for yes, superhost has lower price
ggplot(data,aes(x=as.factor(require_guest_phone_verification),y=as.numeric(price),group=interaction(host_is_superhost, require_guest_phone_verification),fill= as.factor(host_is_superhost)))+
  geom_boxplot(width=0.45,position=position_dodge(width=0.5))+theme_bw()+
  labs(fill="") +
  ggtitle("25.Box plot of Guest Phone Verification Required or Not Required by Superhost Status") +
  labs(x = "Guest Phone Verification Required or Not (Y=1,N=0) Split by Host status", y = "Price") + theme(legend.position="bottom") + scale_fill_discrete(labels=c("Not Superhost", "Superhost"))

##### do 25 in bar graph
ggplot(data,aes(x=as.factor(require_guest_phone_verification),y=as.numeric(price),group=interaction(host_is_superhost, require_guest_phone_verification),fill= as.factor(host_is_superhost)))+
  geom_boxplot(width=0.45,position=position_dodge(width=0.5))+theme_bw()+
  labs(fill="") +
  ggtitle("25.Box plot of Guest Phone Verification Required or Not Required by Superhost Status") +
  labs(x = "Guest Phone Verification Required or Not (Y=1,N=0) Split by Host status", y = "Price") + theme(legend.position="bottom") + scale_fill_discrete(labels=c("Not Superhost", "Superhost"))

##cancellation_policy****
ggplot(data,aes(x=as.factor(cancellation_policy),y=as.numeric(price),group=interaction(host_is_superhost, cancellation_policy),fill= as.factor(host_is_superhost)))+
  geom_boxplot(width=0.45,position=position_dodge(width=0.5))+theme_bw()+
  labs(fill="") +
  ggtitle("26.Box plot of Cancellation Policy Types by Superhost Status") +
  labs(x = "Types of Cancellation Policy Split by Host status", y = "Price") + theme(legend.position="bottom") + scale_fill_discrete(labels=c("Not Superhost", "Superhost"))

###stacked bar chart

#############################################################cancellation_policy****
ggplot(data,aes(x=as.factor(cancellation_policy),y=as.numeric(price),group=interaction(host_is_superhost, cancellation_policy),fill= as.factor(host_is_superhost)))+
  geom_boxplot(width=0.45,position=position_dodge(width=0.5))+theme_bw()+
  labs(fill="") +
  ggtitle("26.Box plot of Cancellation Policy Types by Superhost Status") +
  labs(x = "Types of Cancellation Policy Split by Host status", y = "Price") + theme(legend.position="bottom") + scale_fill_discrete(labels=c("Not Superhost", "Superhost"))


count_policy <- c(
  sum(data$cancellation_policy == 'flexible' & data$host_is_superhost == 0),
  sum(data$cancellation_policy == 'flexible' & data$host_is_superhost == 1),
  sum(data$cancellation_policy == 'moderate' & data$host_is_superhost == 0),
  sum(data$cancellation_policy == 'moderate' & data$host_is_superhost == 1),
  sum(data$cancellation_policy == 'strict' & data$host_is_superhost == 0),
  sum(data$cancellation_policy == 'strict' & data$host_is_superhost == 1),
  sum(data$cancellation_policy == 'strict_14_with_grace_period' & data$host_is_superhost == 0),
  sum(data$cancellation_policy == 'strict_14_with_grace_period' & data$host_is_superhost == 1),
  sum(data$cancellation_policy == 'super_strict_30' & data$host_is_superhost == 0),
  sum(data$cancellation_policy == 'super_strict_30' & data$host_is_superhost == 1),
  sum(data$cancellation_policy == 'super_strict_60' & data$host_is_superhost == 0),
  sum(data$cancellation_policy == 'super_strict_60' & data$host_is_superhost == 1)
)


count_policy2 <- data %>%
  mutate(Gp = 
           case_when(data$cancellation_policy == 'flexible' & data$host_is_superhost == 0 ~ 405,
                     data$cancellation_policy == 'flexible' & data$host_is_superhost == 1 ~ 256,
                     data$cancellation_policy == 'moderate' & data$host_is_superhost == 0 ~ 624,
                     data$cancellation_policy == 'moderate' & data$host_is_superhost == 1 ~ 1094,
                     data$cancellation_policy == 'strict' & data$host_is_superhost == 0 ~ 1,
                     data$cancellation_policy == 'strict' & data$host_is_superhost == 1 ~ 5,
                     data$cancellation_policy == 'strict_14_with_grace_period' & data$host_is_superhost == 0 ~ 866,
                     data$cancellation_policy == 'strict_14_with_grace_period' & data$host_is_superhost == 1 ~ 850,
                     data$cancellation_policy == 'super_strict_30' & data$host_is_superhost == 0 ~ 7,
                     data$cancellation_policy == 'super_strict_30' & data$host_is_superhost == 1 ~ 16,
                     data$cancellation_policy == 'super_strict_60' & data$host_is_superhost == 0 ~ 5,
                     data$cancellation_policy == 'super_strict_60' & data$host_is_superhost == 1 ~ 0))



ggplot(count_policy2, aes(fill=as.factor(host_is_superhost), y=Gp, x=as.factor(cancellation_policy))) + 
  geom_bar(position="dodge", stat="identity", width = 0.5) + 
  labs(fill="") +
  ggtitle("26.Box plot of Cancellation Policy Types by Superhost Status") +
  labs(x = "Types of Cancellation Policy Split by Host status", y = "Count") + theme(legend.position="bottom") + scale_fill_discrete(labels=c("Not Superhost", "Superhost"))

###################################################################################


unique(data$cancellation_policy)


##number of amenitites listed for superhost vs regular host 
p14 <- ggplot(data, aes(x=as.factor(host_is_superhost),y=as.numeric(no.of.amenities.listed), fill= as.factor(host_is_superhost))) +
  geom_bar(stat = "identity", width=0.5) + ggtitle("27. Number of Amenities in Superhosts VS Regular Hosts' Listings") + xlab("Superhost or Not") + theme(legend.position="bottom") +theme(legend.title = element_blank()) + ylab("Number of Amenities")+ scale_fill_discrete(labels=c("Not Superhost", "Superhost"))
#+geom_text(aes(x=as.factor(host_is_superhost),y=as.numeric(no.of.amenities.listed),label=as.numeric(no.of.amenities.listed)), vjust=1.6, color="black", size=3.5)
p14

##accomodates for superhost vs regular host 
p15 <- ggplot(data, aes(x=as.factor(host_is_superhost),y=as.numeric(accommodates), fill=as.factor(host_is_superhost))) +
  geom_bar(stat = "identity", width=0.5) +
  ggtitle("28. Number of Accommodates in Superhosts VS Regular Hosts' Listings")+ylab("Number of Accommodates")  + xlab("Superhost or Not") + theme(legend.position="bottom") +theme(legend.title = element_blank()) + scale_fill_discrete(labels=c("Not Superhost", "Superhost"))
#+geom_text(aes(label=accommodates), vjust=1.6, color="black", size=3.5)
p15

##############################################sanfran / superhosts (2 categorical values)
ggplot(data, aes(x=as.factor(if.host.location.is.san.fran.1.Yes), fill = as.factor(host_is_superhost), labels=TRUE)) + 
  geom_bar()+ggtitle('29.Of Hosts Whose Location Is SanFran, How Many Are Superhosts?')+xlab("Host Location Is SanFran Or Not (Y=1 | N=0)")+ylab("Superhosts") + theme(legend.position="bottom") +theme(legend.title = element_blank())+ scale_fill_discrete(labels=c("Not Superhost", "Superhost"))


count_sh <- c(
  sum(data$if.host.location.is.san.fran.1.Yes == 0 & data$host_is_superhost == 0),
  sum(data$if.host.location.is.san.fran.1.Yes == 0 & data$host_is_superhost == 1),
  sum(data$if.host.location.is.san.fran.1.Yes == 1 & data$host_is_superhost == 0),
  sum(data$if.host.location.is.san.fran.1.Yes == 1 & data$host_is_superhost == 1))


count_sh2 <- data %>%
  mutate(Gp = 
           case_when(if.host.location.is.san.fran.1.Yes == 0 & 
                       host_is_superhost == 0 ~ 380,
                     data$if.host.location.is.san.fran.1.Yes == 0 & 
                       data$host_is_superhost == 1 ~ 204,
                     data$if.host.location.is.san.fran.1.Yes == 1 & 
                       data$host_is_superhost == 0 ~ 1528,
                     data$if.host.location.is.san.fran.1.Yes == 1 & 
                       data$host_is_superhost == 1 ~ 2017))


ggplot(count_sh2, aes(fill=as.factor(host_is_superhost), y=Gp, x=as.factor(if.host.location.is.san.fran.1.Yes))) + 
  geom_bar(position="dodge", stat="identity", width = 0.5) +
  ggtitle('29. Of Hosts Whose Location Is SanFran, How Many Are Superhosts?')+xlab("Host Location Is SanFran Or Not (Y=1 | N=0)")+ylab("Counts") + theme(legend.position="bottom") +theme(legend.title = element_blank())+ scale_fill_discrete(labels=c("Not Superhost", "Superhost"))
##################################################################



##unique neighborhoods
unique(data$neighbourhood_cleansed)

##display average rating by neighborhood 

avg_reviews_by_neighbourhood <- aggregate(number_of_reviews ~ neighbourhood_cleansed, data, mean)
avg_reviews_by_neighbourhood

presidio_subset <- filter(data, data$neighbourhood_cleansed == 'Presidio')

mean_by_neighbourhood <- aggregate(number_of_reviews ~ neighbourhood_cleansed, data, mean )


#summary(Presidio_hill_subset$neighbourhood_cleansed)
## avg of potrero hill, neighborhood with highest average reviews 
#avg = sum(potrero_hill_subset$number_of_reviews)/(length(potrero_hill_subset$number_of_reviews))

#appendix: just to see that it's different for neighborhood
avg_reviews_by_neighbourhood_plot <- ggplot(mean_by_neighbourhood, aes(x=neighbourhood_cleansed, y=number_of_reviews)) + 
  geom_bar(stat="identity", width=0.5, fill="tomato3") + 
  labs(title="30. Average Number of Reviews by Neighborhood", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+xlab("Neighbourhood")+ylab("Number of Reviews")
avg_reviews_by_neighbourhood_plot


##investigate the neighborhood(s) with the highest and lowest avg reviewed 


##average reviews by property type
#first aggegate by property type
avg_reviews_by_property_type <- aggregate(number_of_reviews ~ property_type, data, mean)

#then plot
avg_reviews_by_property_type_plot <- ggplot(avg_reviews_by_property_type, aes(x=reorder(property_type, -number_of_reviews), y=number_of_reviews)) + 
  geom_bar(stat="identity", width=0.5, fill="tomato3") + 
  labs(title="31. Average Number of Reviews by Property Type", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+xlab("Property Type")+ylab("Number of Reviews")
avg_reviews_by_property_type_plot

#appendix: property type: pencentage : to explain why we put into others

data_group_property_type <- data %>% group_by(property_type) %>% 
  summarize(count = n()) %>%  # count records by superhost
  mutate(Percent = count/sum(count))  # find percent of total

ggplot(data_group_property_type, aes(reorder(as.factor(property_type),-Percent), Percent)) + geom_bar(stat='identity', fill="tomato3") + 
  geom_text(aes(label=scales::percent(Percent)), position = position_stack(vjust = 0.8))+
  scale_y_continuous(labels = scales::percent)+xlab("Property Type")+ theme(legend.position="bottom") +theme(legend.position = "none") + ggtitle("32. Percentage of Property Types")+ theme(axis.text.x = element_text(angle=65, vjust=0.6))

#appendix: neighborhood
data_group_neigh <- data %>% group_by(neighbourhood_cleansed) %>% 
  summarize(count = n()) %>%  # count records by superhost
  mutate(Percent = count/sum(count))  # find percent of total

ggplot(data_group_neigh, aes(reorder(as.factor(neighbourhood_cleansed),-Percent), Percent)) + 
  geom_bar(stat='identity', fill="tomato3") + 
  geom_text(aes(label=scales::percent(Percent)), position = position_stack(vjust = 0.8))+
  scale_y_continuous(labels = scales::percent)+xlab("Neighbourhood")+ theme(legend.position="bottom") +theme(legend.position = "none") + ggtitle("32a. Percentage of Neighbourhood")+ theme(axis.text.x = element_text(angle=65, vjust=0.6))


## average rating by room type 

##aggregate by roomtype 
avg_reviews_by_room_type <- aggregate(number_of_reviews ~ room_type, data, mean)

##then plot
avg_reviews_by_room_type_plot <- ggplot(avg_reviews_by_room_type, aes(x=room_type, y=number_of_reviews)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="32. Average Number of Reviews by Room Type", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+xlab("Room Type")+ylab("Number of Reviews")
avg_reviews_by_room_type_plot

######### main variable of interest - reviews 
# histogram of reviews - no 4 
#average reviews left for listing by neighborhood - above

# reviews for super host VS non superhost - the whole box plot for reviews
##host_response_time" 
ggplot(data, aes(x=as.factor(host_response_time),y=as.numeric(number_of_reviews),fill = as.factor(host_response_time))) + geom_boxplot() + ggtitle("33. Box Plot of Number of Reviews Each Response Time")+ylab("Number of Reviews")  + xlab("Host Response Time") + theme(legend.position="bottom") +theme(legend.position = 'none')+ geom_smooth(method = "lm", se=TRUE, aes(group=1))

##host_response_time" 
ggplot(data, aes(x=as.factor(host_response_time),y=as.numeric(review_scores_rating),fill = as.factor(host_response_time))) + geom_boxplot() + ggtitle("33a. Box Plot of Ratings Each Response Time")+ylab("Ratings")  + xlab("Host Response Time") + theme(legend.position="bottom") +theme(legend.position = 'none') + geom_smooth(method = "lm", se=TRUE, aes(group=1))

##extra_people - continuous (didnt do)

##cancellation_policy****
ggplot(data,aes(x=as.factor(cancellation_policy),y=as.numeric(number_of_reviews), fill = as.factor(cancellation_policy)))+
  geom_boxplot(width=0.45,position=position_dodge(width=0.5))+theme_bw()+
  labs(fill="") +
  ggtitle("34. Box plot of Cancellation Policy Types") +
  labs(x = "Types of Cancellation Policy", y = "Number of Reviews") + theme(legend.position="none")



###################################### regression part #############

#regression analysis 
library(tidyverse)
library(gridExtra)
library(dslabs)
library(dplyr)
library(ggplot2)
library(GGally)
library(corrplot)
library(forcats)

data2 = read.csv("SF_listings.csv")
data2$last_review=as.Date(data2$last_review, format = "%m/%d/%Y")
data2$host_since=as.Date(data2$host_since, format = "%m/%d/%Y")
data2$first_review=as.Date(data2$first_review, format = "%m/%d/%Y")
data2$days_since_host=as.numeric(as.Date('2019-07-08')-as.Date(data2$host_since, format = "%m/%d/%Y"))
data2$active_days=as.numeric(data2$last_review-data2$first_review)
filter_test <- data2%>%filter((as.Date(last_review, "%m/%d/%Y") >= "2019-01-01") & !is.na(last_review) & availability_365 > 0)
data2=filter_test


##Since neighbourhood_cleansed has too many levels which makes the final regression less informative, we re-categorized 
##neighbourhood_cleansed,remaining 6 main level and classifying all others as 'Others'. Also, we do the same thing for property_type,
##leaving 3 levels,i.e. apartments, house, and others.


count_neighbourhood<-filter_test%>%
  group_by(neighbourhood_cleansed)%>%
  summarize(no=length(neighbourhood_cleansed))
filter_test$neighbourhood_cleansed<-fct_collapse(data2$neighbourhood_cleansed, 'Others' = as.vector(as.data.frame(count_neighbourhood[count_neighbourhood$no<240,1])$neighbourhood_cleansed),Western_Addition='Western Addition')


count_property<-filter_test%>%
  group_by(property_type)%>%
  summarize(no=length(property_type))
filter_test$property_type<-fct_collapse(data2$property_type, 'Others' = as.vector(as.data.frame(count_property[count_property$no<1000,1])$property_type),'Others'=c('Camper/RV','Hut','Timeshare'))


##Part2: corplot
library(corrplot)

correlation = cor(na.omit(data2[c(6,7,8,14,15,17,18,20,21,22,23,24,25,26,27,31,38,43,45)]))
corrplot(correlation, method='square',type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, sig.level=0.001,insig = "blank",addrect = 2,tl.pos='td',tl.cex=0.8)
##With the variables we interested in, we run a correlation plot. In the plot, price seems to correlated with many different variables including accomodates, beds, cleaning fee.
##For number of reviews, it seem to be highly positively correlated to active days of host,and also negatively correlated with cleaning fee, which is interesting.






###Part 3:regression of # reviews


##When it comes to regression part, we first drop some items that would not make any sense in regression,like id.
##Also, we investigate the items that are highly correlated with others, and drop one out of the pairs.
##Following are the cortests proving the correlation between them.

cor.test(x=as.numeric(filter_test$active_days),y=as.numeric(filter_test$days_since_host))
cor.test(x=filter_test$review_scores_rating,y=filter_test$review_scores_accuracy)
cor.test(x=filter_test$review_scores_rating,y=filter_test$review_scores_cleanliness)
cor.test(x=filter_test$review_scores_rating,y=filter_test$review_scores_checkin)
cor.test(x=filter_test$review_scores_rating,y=filter_test$review_scores_communication)
cor.test(x=filter_test$review_scores_rating,y=filter_test$review_scores_location)
cor.test(x=filter_test$review_scores_rating,y=filter_test$review_scores_value)
cor.test(x=filter_test$bedrooms,y=filter_test$beds)
cor.test(x=filter_test$accommodates,y=filter_test$beds)

## With the clean data, we use step function to fit the best regression for number of reviews
FitAll_review=lm(number_of_reviews~host_response_time+host_response_rate+host_is_superhost+neighbourhood_cleansed+property_type+room_type+accommodates+bathrooms+no.of.amenities.listed+price+security_deposit+cleaning_fee+guests_included+extra_people+minimum_nights+maximum_nights+review_scores_rating+instant_bookable+cancellation_policy+require_guest_phone_verification+calculated_host_listings_count+active_days,data=na.omit(filter_test))

fit.nothing_review=lm(number_of_reviews~1,na.omit(filter_test))

bestfit_review_1=step(fit.nothing_review,direction='forward',scope=formula('FitAll_review'))
bestfit_review_2=step(fit.nothing_review,direction='both',scope=formula('FitAll_review'))
bestfit_review_3=step(FitAll_review,direction='both',scope=formula('fit.nothing_review'))
bestfit_review_4=step(FitAll_review,direction='backward',scope=formula('fit.nothing_review'))
summary(bestfit_review_1)
# summary(bestfit_review_2)
# summary(bestfit_review_3)
#summary(bestfit_review_4)
plot(bestfit_review_1)
##Out of four possible best model,bestfit_3/4 have the lowest ALC,in which guests,minimum nights,availability, active days,neighbourhood, & calculated_host_listings_count are the indicators.
##The result shows that, giving the same neighbourhood, clean fee,and review scores, increase of minimum nights and guests included seem to have negative influence on number of reviews
##It's clear that the more minimum nights required from the customers, the less likely they choose the listing and write a feedback.
##however, the reason why the increse of guests of guests included make the reviews lower is not clear.
##What's more, the increase of active days and availability seem to increase the number of reviews.




###Part 4:regression of # rating
##To further understand how reviews are influenced, we try to find the best regression for rating.

FitAll_ratescore1=lm( review_scores_rating~host_response_time+host_response_rate+host_is_superhost+neighbourhood_cleansed+ property_type+room_type+accommodates+bathrooms+no.of.amenities.listed+price+security_deposit+
                       cleaning_fee+guests_included+extra_people+minimum_nights+maximum_nights+number_of_reviews+
                       instant_bookable+cancellation_policy+require_guest_phone_verification+calculated_host_listings_count
                     +active_days,data=na.omit(filter_test))
fit.nothing_ratescore1=lm(review_scores_rating~1,na.omit(filter_test))

bestfit_ratescore1_1=step(fit.nothing_ratescore1,direction='forward',scope=formula('FitAll_ratescore1'))
bestfit_ratescore1_2=step(fit.nothing_ratescore1,direction='both',scope=formula('FitAll_ratescore1'))
bestfit_ratescore1_3=step(FitAll_ratescore1,direction='both',scope=formula('fit.nothing_ratescore1'))
bestfit_ratescore1_4=step(FitAll_ratescore1,direction='backward',scope=formula('fit.nothing_ratescore1'))

# summary(bestfit_ratescore1_1)
# summary(bestfit_ratescore1_2)
summary(bestfit_ratescore1_3)
# summary(bestfit_ratescore1_4)

##filter here
data3<-filter_test%>%
  filter(host_response_time!='a few days or more')

FitAll_ratescore2=lm( review_scores_rating~host_response_time+host_response_rate+host_is_superhost+neighbourhood_cleansed+
                       property_type+room_type+accommodates+bathrooms+no.of.amenities.listed+price+security_deposit+
                       cleaning_fee+guests_included+extra_people+minimum_nights+maximum_nights+number_of_reviews+
                       instant_bookable+cancellation_policy+require_guest_phone_verification+calculated_host_listings_count
                       +active_days,data=na.omit(data3))
fit.nothing_ratescore2=lm(review_scores_rating~1,na.omit(data3))

bestfit_ratescore2_1=step(fit.nothing_ratescore2,direction='forward',scope=formula('FitAll_ratescore2'))
bestfit_ratescore2_2=step(fit.nothing_ratescore2,direction='both',scope=formula('FitAll_ratescore2'))
bestfit_ratescore2_3=step(FitAll_ratescore2,direction='both',scope=formula('fit.nothing_ratescore2'))
bestfit_ratescore2_4=step(FitAll_ratescore2,direction='backward',scope=formula('fit.nothing_ratescore2'))

# summary(bestfit_ratescore2_1)
# summary(bestfit_ratescore2_2)
# summary(bestfit_ratescore2_3)
summary(bestfit_ratescore2_4)
plot(bestfit_ratescore2_4)
##Out of four possible best model,bestfit_3/4 have the lowest ALC,in which response time,response rate, superhost, number of amenities, extra fee for people, minimum nights, availability are included in the regression.
##holding the number of amenities listed constant, the longer host response time is,the less the rating becomes.
##While at the same time, response rate and being a superhost has a positive influence on rating. Also, an increase in minimum nights might result in better connection between 
##the host and the customers, which could lead to a higher rating. And of course, the higher the rating is, the more likely it get booked.



###Part 5:regression of price
##similarly, we run a step function to find the best regression for price too.
FitAll_price=lm( log(price)~host_response_time+host_response_rate+host_is_superhost+neighbourhood_cleansed+
                   property_type+room_type+accommodates+bathrooms+no.of.amenities.listed+security_deposit+
                   cleaning_fee+guests_included+extra_people+minimum_nights+maximum_nights+number_of_reviews+review_scores_rating+
                   instant_bookable+cancellation_policy+require_guest_phone_verification+calculated_host_listings_count
                   +active_days,data=na.omit(filter_test))

fit.nothing_price=lm(log(price)~1,na.omit(filter_test))

bestfit_price_1=step(fit.nothing_price,direction='forward',scope=formula('FitAll_price'))
bestfit_price_2=step(fit.nothing_price,direction='both',scope=formula('FitAll_price'))
bestfit_price_3=step(FitAll_price,direction='both',scope=formula('fit.nothing_price'))
bestfit_price_4=step(FitAll_price,direction='backward',scope=formula('fit.nothing_price'))
summary(bestfit_price_1)
# summary(bestfit_price_2)
# summary(bestfit_price_3)
summary(bestfit_price_4)
bestfit_price_5=lm(formula = log(price) ~ host_response_rate + room_type + accommodates + 
     bathrooms + security_deposit + minimum_nights + number_of_reviews + 
     review_scores_rating + cancellation_policy + active_days+neighbourhood_cleansed, 
   data = na.omit(filter_test))
summary(bestfit_price_5)
plot(bestfit_price_4)
plot(bestfit_price_1)
plot(bestfit_price_5)
##Out of four possible best model,bestfit_1/2 have the lowest ALC,in which accommodates, availability,extra fee for extra people, superhost and number of reviews are the indicators.
##The result shows that, giving the same number of reviews, increase of extra fee for extra people seem to have positive influence on price, which is for sure.
##The increase of accommodates implys a larger listing, can also lead to incresement in price.
##In the exploratory analysis, we found that superhost has relatively lower price, however, in the regression, being a superhost seems to have a positive influence on the price, holding other factors constant.
##This finding implys that superhosts might have less accommodates, which makes them cheaper than non-superhosts in general.

##regression for superhost
FitAll_superhost<-glm(host_is_superhost~host_response_time+host_response_rate+neighbourhood_cleansed+property_type+room_type+accommodates+bathrooms+no.of.amenities.listed+security_deposit+
                        cleaning_fee+guests_included+extra_people+minimum_nights+maximum_nights+number_of_reviews+review_scores_rating+
                        instant_bookable+cancellation_policy+require_guest_phone_verification+calculated_host_listings_count
                      +active_days+price,data=na.omit(filter_test),family=binomial(link='logit'))
fit.nothing_price=glm(host_is_superhost~1,na.omit(filter_test),family=binomial(link='logit'))
bestfit_superhost_1=step(fit.nothing_price,direction='forward',scope=formula('FitAll_price'))
bestfit_superhost_2=step(fit.nothing_price,direction='both',scope=formula('FitAll_price'))
bestfit_superhost_3=step(FitAll_price,direction='both',scope=formula('fit.nothing_price'))
bestfit_superhost_4=step(FitAll_price,direction='backward',scope=formula('fit.nothing_price'))

summary(bestfit_superhost_1)
# summary(bestfit_superhost_2)
# summary(bestfit_superhost_3)
# summary(bestfit_superhost_4)

#Part 6: map plot of neighbourhood
# map
# install.packages(c('ggmap','mapproj'))
library(ggmap)
library(mapproj)
register_google(key='AIzaSyAGxEl1LEyWQhJA8T120cPTvs42I7A3Uyw')

#clean data for map
data_map <- data2 %>% filter((as.Date(last_review, "%m/%d/%Y") >= "2019-01-01") & !is.na(last_review) & availability_365 > 0)

map_neighbourhood_price<-as.data.frame(data_map%>%
                                         group_by(neighbourhood_cleansed)%>%
                                         summarize(avg_price=round(mean(price),1),avg_reviews=round(mean(number_of_reviews)),count=n(),longitude=mean(longitude),latitude=mean(latitude)))

a<-qmap('San Francisco',zoom=12,maptype='toner-lite')

b <- a+
  geom_point(data = map_neighbourhood_price, aes(x = longitude, y = latitude,size = avg_reviews,col=avg_price) )+
  scale_color_gradient(low = "#F05F57" , high ="#360940",
                       space = "Lab", na.value = "grey50", guide = "colourbar",
                       aesthetics = "colour")+
  ggtitle('Avg reviews & avg price in different neighbourhood')
b  
##In this plot, we show average number of reviews and average price in different neighbourhood. First, the neighbourhood are evenly distributed across San Francisco. And, it seems that the listing in the upper part of San Francisco is more expensive,which might due to a more wealthy enviroment.
#Also, in the graph, most neighbourhoods have the similiar number of reviews except the one at the top.

