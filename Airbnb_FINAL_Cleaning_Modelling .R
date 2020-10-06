


###AIRBNB KAGGLE COMPETITION - ANALYSIS DATA### 


# Read Data 
setwd("/Users/Sushmitha/Desktop/Pace/Spring 2020/Predictive Analytics/predict-price-lala/")

data0 = read.csv('analysisData.csv',stringsAsFactors = F)
data1 = read.csv('scoringData.csv', stringsAsFactors = F)

library(corrplot)
library(plyr)
library(ggplot2)

table(data0$price)

#to clean both the analysis and scoring data together, we can use rbind 
data_bind = rbind(data0,data1)

#After trying to bind the dfs the first time, I realised that we have to make a dummy col for price in scoring data as there aren't equal number of columns.  
data1$price = NA
count(data1$price)


data = subset(data_bind, select = -c(host_acceptance_rate,host_neighbourhood,host_listings_count,host_verifications,street,neighbourhood,city,state,market, smart_location,country_code,country,is_location_exact,square_feet,weekly_price,monthly_price, minimum_minimum_nights,maximum_minimum_nights,minimum_maximum_nights,maximum_maximum_nights, minimum_nights_avg_ntm, maximum_nights_avg_ntm,requires_license,license,jurisdiction_names))
names(data) #removed variables that weren't relevant on first glance 

#Converting variables such as name, summary, notes to the number of characters  

#name 
data$name = nchar(data$name)
count(data$name)

#summary
data$summary = nchar(data$summary)

#space
data$space = nchar(data$space)

#Notes 
data$notes = nchar(data$notes)

#Description
data$description = nchar(data$description)

#neighborhood_overview
data$neighborhood_overview = nchar(data$neighborhood_overview)

#transit
data$transit = nchar(data$transit)

#access
data$access = nchar(data$access)

#house_rules
data$house_rules = nchar(data$house_rules)
colnames(data)

#interaction 
data$interaction = nchar(data$interaction)
count(data$interaction)

#host_about
data$host_about = nchar(data$host_about)
count(data$host_about)

#Checking each variable through ggplot, removing missing values and modifying levels 

box = geom_boxplot(varwidth=T, fill="red")
scatter = geom_point(color="red")  

#host_location ##ADD 
count(data$host_location)

#Tried to see if there was a relationship between hosts who lived in New York and had a listing there with the ones who didn't. For instance, there might have been a possiblity that a host not living in New York would choose to price their listing lower. 
#whether host_location contains 'New York' in it 
host_location_NY = function(s){
  if(grepl("New York", s)){
    return(1)
  }else
    return(0)
}
data$host_location_NY = unlist(lapply(data$host_location,host_location_NY), use.names = F)
count(data$host_location_NY)
data$host_location_NY =as.logical(data$host_location_NY)
ggplot(data, aes(x=host_location_NY,y=price)) + box



#host_since ##ADD  
data$host_since = as.Date(data$host_since)
data$host_since = as.integer(as.Date("2020-4-25") - data$host_since)
data[is.na(data$host_since),]$host_since = mean(data$host_since,na.rm=T) 
ggplot(data, aes(x=host_since,y=price)) + scatter #checking if there's a relationship b/w host_since and price 
ggplot(data, aes(x=host_since,y=number_of_reviews)) + scatter #checking if there s arelationship b/w host_since and number_of_reviews 

#host_response_time -- ##REMOVE##
count(data$host_response_time) #Removing values with more than 20% of missing data 
typeof(data$host_response_time)


#host_response_rate --##REMOVE##
typeof(data$host_response_rate)
count(data$host_response_rate) #Removing values with more than 20% of missing data

#host_is_superhost ##ADD##
typeof(data$host_is_superhost)
count(data$host_is_superhost)
data$host_is_superhost = as.numeric(c("t" = "1", "f" = "0")[data$host_is_superhost]) 
data[is.na(data$host_is_superhost),]$host_is_superhost = 1
data$host_is_superhost =as.logical(data$host_is_superhost)
ggplot(data, aes(x=host_is_superhost,y=price)) + box


#host_total_listings_count #CHECK CORRELATION
typeof(data$host_total_listings_count)

Condense = function(vector,threshold,newName){
  l = levels(as.factor(vector))
  t = table(vector)
  for(i in l){
    if (t[i] < threshold){
      vector[vector == i] <- newName
    }
  }
  return(vector)
}  #to condense the extra levels 


data$host_total_listings_count = Condense(data$host_total_listings_count,1000,5)
data[is.na(data$host_total_listings_count),]$host_total_listings_count = 0
count(data$host_total_listings_count)
data$host_total_listings_count = as.numeric(data$host_total_listings_count)
ggplot(data, aes(x=host_total_listings_count,y=price)) + box


#host_has_profile_pic #CHECK CORRELATION
typeof(data$host_has_profile_pic)
count(data$host_has_profile_pic)
data$host_has_profile_pic = as.numeric(c("t" = "1", "f" = "0")[data$host_has_profile_pic]) 
data[is.na(data$host_has_profile_pic),]$host_has_profile_pic = 0
data$host_has_profile_pic = as.logical(data$host_has_profile_pic)
ggplot(data, aes(x=host_has_profile_pic,y=price)) + box

#host_identity_verified #CHECK CORRELATION
typeof(data$host_has_profile_pic)
count(data$host_identity_verified)
data$host_identity_verified = as.numeric(c("t" = "1", "f" = "0")[data$host_identity_verified]) 
data[is.na(data$host_identity_verified),]$host_identity_verified = 1
data$host_identity_verified = as.logical(data$host_identity_verified)
ggplot(data, aes(x=host_identity_verified,y=price)) + box

  
#neighbourhood_group_cleansed #ADD
count(data$neighbourhood_group_cleansed)
ggplot(data, aes(x=neighbourhood_group_cleansed,y=price)) + box

#zipcode #ADD 
count(data$zipcode)

#correcting zipcode values 
data$zipcode[data$zipcode=="11103-3233"] = 11103
data$zipcode[data$zipcode=="11249\n11249"] = 11249
data$zipcode[data$zpcode=="11385-2308"] = 11385 ###For some odd reason, this one zipcode wouldn't work 
data$zipcode[data$zipcode=="11413-3220"] = 11413
data$zipcode[data$zipcode=="1m"] = 11694
data$zipcode[data$zipcode=="91766"] = 10021
data$zipcode = as.factor(data$zipcode) #converting to factor

table(data$zipcode)

#property_type #ADD 
count(data$property_type)
data$property_type = as.factor(Condense(data$property_type,300,"Other"))
ggplot(data, aes(x=property_type,y=price)) + box


#room_type  #ADD
count(data$room_type)
ggplot(data, aes(x=room_type,y=price)) + box


#accommodates #ADD   
count(data$accommodates)
data$accommodates = as.factor(Condense(data$accommodates,200,"9"))


#bathrooms #ADD
count(data$bathrooms)
ggplot(data, aes(x=bathrooms,y=price)) + scatter
data$bathrooms = Condense(data$bathrooms,40,5)


#bedrooms #ADD
count(data$bedrooms)
data$bedrooms = Condense(data$bedrooms,60,5)
ggplot(data, aes(x=bedrooms,y=price)) + scatter


#beds   
count(data$beds) #COME BACK TO THIS 
data$beds = Condense(data$beds,100,7)
data[is.na(data$beds),]$beds = 7
ggplot(data, aes(x=beds, y=price)) + scatter 



#bed_type #ADD  
typeof(data_TB$bed_type)
count(data_TB$bed_type)
ggplot(data, aes(x=bed_type, y=price)) + scatter 

#Frequency of other bed types is very low in comparision to Real bed. We can combine all the other bed types and real bed into a 1 or 0. 

levels = levels(as.factor(data$bed_type))
table = table(data$bed_type)
for(i in levels){
  if (table[i] < 30000){
    data$bed_type[data$bed_type == i] = "f"
  } else{
    data$bed_type[data$bed_type == i] = "t"
  }
}
table(data$bed_type)
colnames(data)[which(names(data) == "bed_type")] <- "Real_Bed"
data$Real_Bed = as.factor(data$Real_Bed)
count(data$Real_Bed)
ggplot(data, aes(x=Real_Bed, y=price)) + scatter 

#Real_Bed #ADD 



#amenities #COME BACK TO IT  

amenities = data$amenities
library(tm)
amenities_corpus = Corpus(VectorSource(data$amenities))
amenities_corpus = tm_map(x = amenities_corpus, FUN=content_transformer(FUN= function(x)gsub(pattern = '\\.',replacement = '',x)))
library(stringr)
amenities_corpus = tm_map(x = amenities_corpus, FUN=content_transformer(FUN= str_to_title))
amenities_corpus = tm_map(x = amenities_corpus, FUN=content_transformer(FUN= function(x)gsub(pattern = ' ',replacement = '',x)))
amenities_corpus = tm_map(x = amenities_corpus, FUN=content_transformer(FUN= function(x)gsub(pattern = ',',replacement = ' ',x)))
amenities_matrix = as.matrix(DocumentTermMatrix(amenities_corpus,control = list(tolower=FALSE)))
colnames(amenities_matrix) = make.names(colnames(amenities_matrix))
dim(amenities_matrix)
data_with_amenities_dummies = cbind(data, amenities_matrix)
colnames(amenities_matrix)

dim(amenities_matrix)

#Looking at the frequency of the amenities variables and selecting variables with higher frequency  

apply(data_with_amenities_dummies, 2, table)


data = data_with_amenities_dummies



#security_deposit 
count(data$security_deposit) 


#Calculated_Host_listings
count(data$calculated_host_listings_count)
host_listings_1 <- function(s){
  if(s>1){
    return(1)
  }else{
    return(0)
  }
}
data$host_listings_1 = unlist(lapply(data$calculated_host_listings_count,host_listings_1),use.names=FALSE)
#host_listings_1

#cleaning_fee #ADD
count(data$cleaning_fee)
data[is.na(data$cleaning_fee),]$cleaning_fee = mean(data$cleaning_fee,na.rm=T) 



#guests_included  #ADD
count(data$guests_included)
ggplot(data, aes(x=guests_included,y=price)) + scatter
data$guests_included = Condense(data$guests_included,50,8)



#extra_people #COME BACK TO IT 
count(data$extra_people)
ggplot(data, aes(x=extra_people,y=price)) + scatter
data$extra_people = Condense(data$extra_people,100,8)



#minimum_nights #ADD
count(data$minimum_nights)
data$minimum_nights = Condense(data$minimum_nights,3000,4)
ggplot(data, aes(x=minimum_nights,y=price)) + scatter


#maximum_nights #ADD
ggplot(data, aes(x=maximum_nights,y=price)) + scatter
count(data$maximum_nights)


#calendar_updated  #COME BACK TO IT LATER 
table(data$calendar_updated)

#creating function to replace values in calendar_updated 
#Didn't have enough time to wwork on this 

#has_availability ##REMOVE## 
count(data$has_availability)
ggplot(data, aes(x=has_availability,y=price)) + box


#availability_30 #ADD / COME BACK TO LATER 
count(data$availability_30)
ggplot(data, aes(x=availability_30,y=price)) + scatter


#availability_60  #ADD

count(data$availability_60)
ggplot(data, aes(x=availability_60,y=price)) + scatter

#availability_90 #ADD
count(data$availability_90)
ggplot(data, aes(x=availability_90,y=price)) + scatter


#availability_365 #ADD
count(data$availability_365)
ggplot(data, aes(x=availability_365,y=price)) + scatter


#number_of_reviews #ADD
ggplot(data, aes(x=number_of_reviews,y=price)) + scatter


#number_of_reviews_ltm #ADD
ggplot(data, aes(x=number_of_reviews_ltm,y=price)) + scatter


#first_review  #ADD
data$first_review = as.Date(data$first_review)
data$first_review = as.integer(as.Date("2020-4-25") - data$first_review)
ggplot(data, aes(x=first_review,y=price)) + scatter
data[is.na(data$first_review),]$first_review = mean(data$first_review,na.rm=T) 
count(data$first_review)

#last_review #ADD
data$last_review = as.Date(data$last_review)
data$last_review = as.integer(as.Date("2020-4-25") - data$last_review)
ggplot(data, aes(x=last_review,y=price)) + scatter
data[is.na(data$last_review),]$last_review = mean(data$last_review,na.rm=T) 
count(data$last_review)

#review_scores_rating #ADD
ggplot(data, aes(x=review_scores_rating,y=price)) + scatter

#review_scores_accuracy #ADD 
ggplot(data, aes(x=review_scores_accuracy,y=price)) + scatter

#review_scores_cleanliness #ADD 
ggplot(data, aes(x=review_scores_cleanliness,y=price)) + scatter


#review_scores_checkin  #ADD
ggplot(data, aes(x=review_scores_checkin,y=price)) + scatter

#review_scores_communication #ADD 
ggplot(data, aes(x=review_scores_communication,y=price)) + scatter

#review_scores_location #ADD   
ggplot(data, aes(x=review_scores_location,y=price)) + scatter
count(data$review_scores_location)

#review_scores_value #ADD
ggplot(data, aes(x=review_scores_value,y=price)) + scatter

#instant_bookable #ADD      
count(data$instant_bookable)
ggplot(data, aes(x=instant_bookable,y=price)) + box


#is_business_travel_ready ##REMOVE## 
count(data$is_business_travel_ready)
ggplot(data, aes(x=is_business_travel_ready,y=price)) + box


#cancellation_policy #ADD
count(data$cancellation_policy)
data$cancellation_policy = Condense(data$cancellation_policy,20,"strict")
ggplot(data, aes(x=cancellation_policy,y=price)) + box


#require_guest_profile_picture #ADD- CHECK  
count(data$require_guest_profile_picture)
ggplot(data, aes(x=require_guest_profile_picture,y=price)) + box


#require_guest_phone_verification #ADD - CHECK
count(data$require_guest_phone_verification)
ggplot(data, aes(x=require_guest_phone_verification,y=price)) + box


#calculated_host_listings_count #ADD - CHECK
count(data$calculated_host_listings_count)
data$calculated_host_listings_count = Condense(data$calculated_host_listings_count,200,9)
ggplot(data, aes(x=calculated_host_listings_count,y=price)) + scatter


#calculated_host_listings_count_entire_homes #ADD - CHECK 
count(data$calculated_host_listings_count_entire_homes)
data$calculated_host_listings_count_entire_homes = Condense(data$calculated_host_listings_count_entire_homes,100,7)
ggplot(data, aes(x=calculated_host_listings_count_entire_homes,y=price)) + scatter


#calculated_host_listings_count_private_rooms #ADD - CHECK 
count(data$calculated_host_listings_count_private_rooms)
data$calculated_host_listings_count_private_rooms = Condense(data$calculated_host_listings_count_private_rooms,130,9)
ggplot(data, aes(x=calculated_host_listings_count_private_rooms,y=price)) + scatter

#calculated_host_listings_count_shared_rooms #ADD - CHECK 
count(data$calculated_host_listings_count_shared_rooms)
data$calculated_host_listings_count_shared_rooms = Condense(data$calculated_host_listings_count_shared_rooms,100,4)
ggplot(data, aes(x=calculated_host_listings_count_shared_rooms,y=price)) + scatter

#reviews_per_month  #ADD 
count(data$reviews_per_month)
data[is.na(data$reviews_per_month),]$reviews_per_month = 0
ggplot(data, aes(x=reviews_per_month,y=price)) + scatter



#Using cor to check for correlations
numericVars <- which(sapply(data, is.numeric)) 
all_numVar <- data[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") 
cor_data <- as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))
print(cor_data)

colnames(data)

data = subset(data, select = -c(security_deposit, calendar_updated,host_name,host_location,host_response_time,host_response_rate,neighbourhood_cleansed,amenities,has_availability))

table(data$zipcode)
data[is.na(data$zipcode),]$zipcode = 10002

------##---------


#Saving cleaned data
write.csv(data, "clean_data_MAY_3.csv",row.names = F)

#DATA MODELLING
setwd("/Users/Sushmitha/Desktop/Pace/Spring 2020/Predictive Analytics/predict-price-lala/")
data_model = read.csv("clean_data_MAY_3.csv")
colnames(data_model)


#Seperating analysis and scoring data 

#analysis 
ad = data_model[!is.na(data_model$price),]

#scoring
sd = data_model[is.na(data_model$price),] 


#RandomForest Model (TEST)
set.seed(617)
split = sample(1:nrow(ad),nrow(ad)*0.7)
train = ad[split,]
test = ad[-split,]

#install.packages('ranger')
library(ranger)
forest_ranger = ranger(price~.,data=train,num.trees = 1000)
pred = predict(forest_ranger, data =test,num.trees = 1000)
rmse_forest_ranger = sqrt(mean((pred$predictions-test$price)^2)); rmse_forest_ranger
#RMSE test = 63.89976




#USing a boosting model (h2o, gbm) Splitting anlaysis data into test and train#####(https://www.h2o.ai/blog/h2o-gbm-tuning-tutorial-for-r/)


library(caret)
set.seed(200)
split = createDataPartition(y = ad$price,p = 0.80,list = F,groups=1450) 
train = data_model[split,]
test = data_model[-split,]


train.hex <- as.h2o(train, destination_frame = "train.hex")
test.hex <- as.h2o(test, destination_frame = "test.hex")
drops <- c("price","id")


gbm <- h2o.gbm(
  ## standard model parameters
  x = colnames(train[ , !(names(train) %in% drops)]),
  y = "price",
  training_frame = train.hex,
  validation_frame = test.hex,
  ## more trees is better if the learning rate is small enough
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 4000, max_depth = 11, 
  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate=0.008,
  ## sample 80% of rows per tree
  sample_rate = 0.8,
  ## sample 80% of columns per split
  col_sample_rate = 0.8,col_sample_rate_change_per_level = 1,col_sample_rate_per_tree = .5,
  ## fix a random number generator seed for reproducibility
  seed = 1234,
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10, 
  min_split_improvement = 1e-06
)

summary(gbm)
#train RMSE is 34.60474 and test is 56.63617


#building final model 
library(h2o)
h2o.init()
a.hex <- as.h2o(ad, destination_frame = "a.hex")
s.hex <- as.h2o(sd, destination_frame = "s.hex")
drops <- c("price","id")



gbm1 <- h2o.gbm(
  ## standard model parameters
  x = colnames(ad[ , !(names(ad) %in% drops)]),
  y = "price",
  training_frame = a.hex,
  ## more trees is better if the learning rate is small enough
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 4000,
  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate=0.008,
  ## sample 80% of rows per tree
  sample_rate = 0.8,sample_rate_per_class = NULL, 
  ## sample 80% of columns per split
  col_sample_rate = 0.8,col_sample_rate_change_per_level = 1, col_sample_rate_per_tree = .5,
  ## fix a random number generator seed for reproducibility
  seed = 1234,
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10, 
  min_split_improvement = 1e-06
)
summary(gbm1)



View(h2o.varimp(gbm1))

#Submission file creation#####
pred = h2o.predict(gbm1,s.hex)
submissionFile = data.frame(id = sd$id, price = as.vector(pred))
write.csv(submissionFile, 'SM6_submission_5_3.csv',row.names = F)

