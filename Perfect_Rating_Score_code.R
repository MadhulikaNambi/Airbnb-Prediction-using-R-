library(tidyverse)
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(caret)
library(tree)
library(class)
library(stringr)
library(tidyverse)
library(tm)
library(text2vec)
library(SnowballC)
library(glmnet)
library(vip)
library(Matrix)
library(tidytext)
library(textdata)
library(sentimentr)
library(dplyr)
library(randomForest)
library(gbm)
library(ISLR)
library(pROC)
library(ROCR)


#----------------------------------------------------------------------------------------------------------------------------------------
# load dataset
train_x <- read_csv("airbnb_train_x_2023.csv")
train_y <- read_csv("airbnb_train_y_2023.csv")
test_x <- read_csv("airbnb_test_x_2023.csv")


#-----------------------------------------------------------------------------------------------------------------------------------------
# Add flag variable to new_train and new_data to identify test and train data after combining them
train_x$flag <- "train"
test_x$flag <- "test"

# Creating a new column with y variable in test even out the no. of columns in test and train
test_x$perfect_rating_score <- "Dummy"


#-----------------------------------------------------------------------------------------------------------------------------------------
# join the training y to the training x file
# also turn the target variables into factors
train_x_y <- cbind(train_x, train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score)) 

# remove "high_booking_rate" from train_x
train_x_y <- subset(train_x_y, select = -high_booking_rate)

summary(train_x_y)


#-----------------------------------------------------------------------------------------------------------------------------------------
# Using R-Bind, combine new_train and new_data into one data frame to perform cleaning on both
train_test_combined_data <- rbind(train_x_y, test_x)


#-----------------------------------------------------------------------------------------------------------------------------------------
# clean data
removal_dollar_sign <- c("price", "weekly_price", "cleaning_fee", "security_deposit", "extra_people")

new_data <- train_test_combined_data  %>%
  mutate_at(removal_dollar_sign, ~str_replace_all(., pattern="\\$", replacement="")) %>%
  mutate_at(removal_dollar_sign, ~str_replace_all(., pattern=",", replacement="")) %>%
  mutate_at(removal_dollar_sign, ~as.numeric(.))

new_data <- new_data %>%
  mutate(cancellation_policy = as.factor(ifelse(cancellation_policy %in% c( "super_strict_60", "super_strict_30", "strict", "no_refunds"), "strict", cancellation_policy)),
         cleaning_fee = ifelse(is.na(cleaning_fee), mean(cleaning_fee, na.rm = TRUE), cleaning_fee), #Replacing NAs in cleaning_fee and price with 0
         price = ifelse(is.na(price), mean(price, na.rm = TRUE), price), 
         bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm=TRUE), bedrooms),
         bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm=TRUE), bathrooms),
         beds = ifelse(is.na(beds), mean(beds, na.rm=TRUE), beds), 
         host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm=TRUE), host_total_listings_count), 
         host_listings_count = ifelse(is.na(host_listings_count), mean(host_listings_count, na.rm=TRUE), host_listings_count), 
         room_type=as.factor(room_type),
         price_per_person = price/accommodates,
         has_cleaning_fee = as.factor(ifelse(cleaning_fee > 0, "YES", "NO")), 
         bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed", "other")),
         property_category = case_when(
           property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ "apartment",
           property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ "hotel",
           property_type %in% c("Townhouse", "Condominium") ~ "condo",
           property_type %in% c("Bungalow", "House") ~ "house",
           is.na(property_type) ~ "other",
           TRUE ~ as.character(property_type)
         ),
         property_category = as.factor(property_category)
         )
new_data  <- new_data  %>%
  group_by(property_category) %>%
  mutate(median_ppp_ind = median(price_per_person, na.rm= TRUE)) %>%
  ungroup()

new_data  <- new_data  %>%
  mutate(ppp_ind = as.factor(ifelse((price_per_person > median_ppp_ind), "1", "0")),
         host_is_superhost = as.factor(ifelse(is.na(host_is_superhost), "FALSE", host_is_superhost)),
         extra_people = parse_number(as.character(extra_people)),
         charges_for_extra = as.factor(ifelse(extra_people == 0 | is.na(extra_people), "NO", "YES")),
         host_acceptance_rate = parse_number(as.character(host_acceptance_rate)), 
         host_response_rate = parse_number(as.character(host_response_rate)),
         host_acceptance = as.factor(ifelse(is.na(host_acceptance_rate), "MISSING", ifelse(host_acceptance_rate == 100, "ALL", "SOME"))),
         host_response = as.factor(ifelse(is.na(host_response_rate), "MISSING", ifelse(host_response_rate == 100, "ALL", "SOME"))),
         has_min_nights = as.factor(ifelse(minimum_nights > 1, "YES", "NO")),
         host_has_profile_pic = ifelse(is.na(host_has_profile_pic), "FALSE", host_has_profile_pic),
         host_identity_verified = ifelse(is.na(host_identity_verified), "FALSE", host_identity_verified),
         city = as.factor(ifelse(is.na(city),"Not Specified", city)),
         host_location = as.factor(ifelse(is.na(host_location), "Not Specified", host_location)),
         host_neighbourhood = as.factor(ifelse(is.na(host_neighbourhood), "Not Specified", host_neighbourhood)),
         host_response_time = as.factor(ifelse(is.na(host_response_time), "Not Specified", host_response_time)),
         neighbourhood = as.factor(ifelse(is.na(neighbourhood), "Not Specified", neighbourhood)),
         city = as.factor(city),
         city_name = as.factor(city_name),
         country = as.factor(country),
         country_code = as.factor(country_code),
         experiences_offered = as.factor(experiences_offered),
         host_has_profile_pic = as.factor(host_has_profile_pic),
         host_identity_verified = as.factor(host_identity_verified),
         host_location = as.factor(host_location),
         host_neighbourhood = as.factor(host_neighbourhood),
         host_response_time = as.factor(host_response_time),
         neighbourhood = as.factor(neighbourhood),
         smart_location = as.factor(smart_location),
         instant_bookable = as.factor(instant_bookable),
         is_location_exact = as.factor(is_location_exact),
         require_guest_phone_verification = as.factor(require_guest_phone_verification),
         require_guest_profile_picture = as.factor(require_guest_profile_picture),
         requires_license = as.factor(requires_license),
         state = case_when(
           state %in% c("ca", "Ca", "CA") ~ "CA",
           state %in% c("il", "IL") ~ "IL",
           state %in% c("New York", "NY", "ny", "Ny") ~ "NY",
           TRUE ~ state
         ),
         state = as.factor(ifelse(is.na(state), "Not Specified", state)),
  )


#new_data $monthly_price = parse_number(new_data $monthly_price)
#new_data $monthly_price = ifelse(is.na(new_data $monthly_price), 0, new_data $monthly_price)#have to try with mean and median.

new_data  <- new_data  %>% 
  group_by(market) %>% 
  mutate(market_count=n(), 
         market = ifelse(is.na(market),"OTHER", market),
         market = ifelse((market_count < 100), "OTHER", market),
         market = as.factor(market)) %>%
  ungroup()

new_data  <- new_data  %>%
  mutate(access = ifelse(is.na(access), "Not Specified", access),
         host_about = ifelse(is.na(host_about), "Not Specified", host_about))

new_data  <- new_data  %>%
  mutate(interaction = as.character(ifelse(is.na(interaction), "No Interation", interaction)),
         house_rules = as.character(ifelse(is.na(house_rules), "Not Specified", house_rules)),
         neighborhood_overview = as.character(ifelse(is.na(neighborhood_overview), "Not Available", neighborhood_overview)))
new_data <- new_data %>%
  mutate(space = as.character(ifelse(is.na(space), "No Space Details", space)),
         notes = as.character(ifelse(is.na(notes), "No Notes", notes)),
         summary = as.character(ifelse(is.na(summary), "No Summary", summary)))

# Sentiment Analysis
new_data  <- new_data  %>%
  mutate(id = row_number())

sentiment_analysis <- function(my_data){
  #neighbourhood_overview
  col1_sentiments <- my_data %>%
    select(id, neighborhood_overview) %>%
    unnest_tokens(word, neighborhood_overview) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(id) %>%
    summarize(sentiment_score = sum(value)) %>%
    mutate(sentiment = ifelse(sentiment_score > 0, "Positive",
                              ifelse(sentiment_score < 0, "Negative", "Neutral")))
  col1_ids <- unique(col1_sentiments$id)
  
  #interaction
  col2_sentiments <- my_data %>%
    select(id, interaction) %>%
    unnest_tokens(word, interaction) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(id) %>%
    summarize(sentiment_score = sum(value)) %>%
    mutate(sentiment = ifelse(sentiment_score > 0, "Positive",
                              ifelse(sentiment_score < 0, "Negative", "Neutral")))
  col2_ids <- unique(col2_sentiments$id)
  
  #house_rules
  col3_sentiments <- my_data %>%
    select(id, house_rules) %>%
    unnest_tokens(word, house_rules) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(id) %>%
    summarize(sentiment_score = sum(value)) %>%
    mutate(sentiment = ifelse(sentiment_score > 0, "Positive",
                              ifelse(sentiment_score < 0, "Negative", "Neutral")))
  col3_ids <- unique(col3_sentiments$id)
  
  #host_about
  col4_sentiments <- my_data %>%
    select(id, host_about) %>%
    unnest_tokens(word, host_about) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(id) %>%
    summarize(sentiment_score = sum(value)) %>%
    mutate(sentiment = ifelse(sentiment_score > 0, "Positive",
                              ifelse(sentiment_score < 0, "Negative", "Neutral")))
  col4_ids <- unique(col4_sentiments$id)
  
  #access
  col5_sentiments <- my_data %>%
    select(id, access) %>%
    unnest_tokens(word, access) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(id) %>%
    summarize(sentiment_score = sum(value)) %>%
    mutate(sentiment = ifelse(sentiment_score > 0, "Positive",
                              ifelse(sentiment_score < 0, "Negative", "Neutral")))
  col5_ids <- unique(col5_sentiments$id)
  
  col6_sentiments <- my_data %>%
    select(id, space) %>%
    unnest_tokens(word, space) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(id) %>%
    summarize(sentiment_score = sum(value)) %>%
    mutate(sentiment = ifelse(sentiment_score > 0, "Positive",
                              ifelse(sentiment_score < 0, "Negative", "Neutral")))
  col6_ids <- unique(col6_sentiments$id)
  
  col7_sentiments <- my_data %>%
    select(id, notes) %>%
    unnest_tokens(word, notes) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(id) %>%
    summarize(sentiment_score = sum(value)) %>%
    mutate(sentiment = ifelse(sentiment_score > 0, "Positive",
                              ifelse(sentiment_score < 0, "Negative", "Neutral")))
  col7_ids <- unique(col7_sentiments$id)
  
  col8_sentiments <- my_data %>%
    select(id, summary) %>%
    unnest_tokens(word, summary) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(id) %>%
    summarize(sentiment_score = sum(value)) %>%
    mutate(sentiment = ifelse(sentiment_score > 0, "Positive",
                              ifelse(sentiment_score < 0, "Negative", "Neutral")))
  col8_ids <- unique(col8_sentiments$id)
  
  all_ids <- data.frame(id = unique(my_data$id), sentiment_score = 0, stringsAsFactors = FALSE)
  missing_col1_ids <- setdiff(all_ids$id, col1_ids)
  missing_col2_ids <- setdiff(all_ids$id, col2_ids)
  missing_col3_ids <- setdiff(all_ids$id, col3_ids)
  missing_col4_ids <- setdiff(all_ids$id, col4_ids)
  missing_col5_ids <- setdiff(all_ids$id, col5_ids)
  missing_col6_ids <- setdiff(all_ids$id, col6_ids)
  missing_col7_ids <- setdiff(all_ids$id, col7_ids)
  missing_col8_ids <- setdiff(all_ids$id, col8_ids)
  
  missing_col1_sentiments <- data.frame(id = missing_col1_ids, sentiment_score = 0, sentiment = "Not Available", stringsAsFactors = FALSE)
  missing_col2_sentiments <- data.frame(id = missing_col2_ids, sentiment_score = 0, sentiment = "No Interaction", stringsAsFactors = FALSE)
  missing_col3_sentiments <- data.frame(id = missing_col3_ids, sentiment_score = 0, sentiment = "Not Specified", stringsAsFactors = FALSE)
  missing_col4_sentiments <- data.frame(id = missing_col4_ids, sentiment_score = 0, sentiment = "Not Specified", stringsAsFactors = FALSE)
  missing_col5_sentiments <- data.frame(id = missing_col5_ids, sentiment_score = 0, sentiment = "Not Specified", stringsAsFactors = FALSE)
  missing_col6_sentiments <- data.frame(id = missing_col6_ids, sentiment_score = 0, sentiment = "No Space Details", stringsAsFactors = FALSE)
  missing_col7_sentiments <- data.frame(id = missing_col7_ids, sentiment_score = 0, sentiment = "No Notes", stringsAsFactors = FALSE)
  missing_col8_sentiments <- data.frame(id = missing_col8_ids, sentiment_score = 0, sentiment = "No Summary", stringsAsFactors = FALSE)
  
  col1_sentiment_analysis <- rbind(col1_sentiments, missing_col1_sentiments)
  col2_sentiment_analysis <- rbind(col2_sentiments, missing_col2_sentiments)
  col3_sentiment_analysis <- rbind(col3_sentiments, missing_col3_sentiments)
  col4_sentiment_analysis <- rbind(col4_sentiments, missing_col4_sentiments)
  col5_sentiment_analysis <- rbind(col5_sentiments, missing_col5_sentiments)
  col6_sentiment_analysis <- rbind(col6_sentiments, missing_col6_sentiments)
  col7_sentiment_analysis <- rbind(col7_sentiments, missing_col7_sentiments)
  col8_sentiment_analysis <- rbind(col8_sentiments, missing_col8_sentiments)
  
  col1_sentiment_scores <- col1_sentiment_analysis[,c("sentiment", "sentiment_score", "id")]
  col2_sentiment_scores <- col2_sentiment_analysis[,c("sentiment", "sentiment_score", "id")]
  col3_sentiment_scores <- col3_sentiment_analysis[,c("sentiment", "sentiment_score", "id")]
  col4_sentiment_scores <- col4_sentiment_analysis[,c("sentiment", "sentiment_score", "id")]
  col5_sentiment_scores <- col5_sentiment_analysis[,c("sentiment", "sentiment_score", "id")]
  col6_sentiment_scores <- col6_sentiment_analysis[,c("sentiment", "sentiment_score", "id")]
  col7_sentiment_scores <- col7_sentiment_analysis[,c("sentiment", "sentiment_score", "id")]
  col8_sentiment_scores <- col8_sentiment_analysis[,c("sentiment", "sentiment_score", "id")]
  
  col1_sentiment_scores$id <- as.numeric(col1_sentiment_scores$id)
  col2_sentiment_scores$id <- as.numeric(col2_sentiment_scores$id)
  col3_sentiment_scores$id <- as.numeric(col3_sentiment_scores$id)
  col4_sentiment_scores$id <- as.numeric(col4_sentiment_scores$id)
  col5_sentiment_scores$id <- as.numeric(col5_sentiment_scores$id)
  col6_sentiment_scores$id <- as.numeric(col6_sentiment_scores$id)
  col7_sentiment_scores$id <- as.numeric(col7_sentiment_scores$id)
  col8_sentiment_scores$id <- as.numeric(col8_sentiment_scores$id)
  
  col1_sentiment_scores <- col1_sentiment_scores %>%
    arrange(id)
  col2_sentiment_scores <- col2_sentiment_scores %>%
    arrange(id)
  col3_sentiment_scores <- col3_sentiment_scores %>%
    arrange(id)
  col4_sentiment_scores <- col4_sentiment_scores %>%
    arrange(id)
  col5_sentiment_scores <- col5_sentiment_scores %>%
    arrange(id)
  col6_sentiment_scores <- col6_sentiment_scores %>%
    arrange(id)
  col7_sentiment_scores <- col7_sentiment_scores %>%
    arrange(id)
  col8_sentiment_scores <- col8_sentiment_scores %>%
    arrange(id)
  return(list(col1_sentiment_scores, col2_sentiment_scores, col3_sentiment_scores, col4_sentiment_scores, col5_sentiment_scores, col6_sentiment_scores, col7_sentiment_scores, col8_sentiment_scores))
}

result = sentiment_analysis(new_data )
new_data  <- new_data  %>%
  mutate(neighborhood_sentiment = as.factor(result[[1]]$sentiment),
         interaction_sentiment = as.factor(result[[2]]$sentiment),
         house_rules_sentiment = as.factor(result[[3]]$sentiment),
         host_about_sentiment = as.factor(result[[4]]$sentiment),
         access_sentiment = as.factor(result[[5]]$sentiment),
         space_sentiment = as.factor(result[[6]]$sentiment),
         notes_sentiment = as.factor(result[[7]]$sentiment),
         summary_sentiment = as.factor(result[[8]]$sentiment))

new_data $availability_30_change <- new_data $availability_30 - new_data $availability_60
new_data $availability_60_change <- new_data $availability_60 - new_data $availability_90
new_data $availability_90_change <- new_data $availability_90 - new_data $availability_365


#####################
#host verification
#cleaning 
new_data $host_verifications <- gsub("\\[","", new_data $host_verifications)
new_data $host_verifications <- gsub("\\]","", new_data $host_verifications)
new_data $host_verifications <- gsub("\\'","", new_data $host_verifications)

##################
# Extract unique verification methods from host_verifications column
verif_methods <- unique(unlist(strsplit(as.character(new_data$host_verifications), ', ')))
verif_methods <- setdiff(verif_methods, "None")

# Count the number of verification methods for each row
new_data$verif_count <- apply(new_data, 1, function(x) {
  sum(sapply(verif_methods, function(method) grepl(method, x["host_verifications"])))
})


#converting variables to factors
new_data $amenities <- gsub("\\{","", new_data $amenities)
new_data $amenities <- gsub("\\}","", new_data $amenities)
new_data $amenities <- gsub('\\"',"", new_data $amenities)
new_data $amenities <- gsub("\\-","", new_data $amenities)
new_data $amenities <- gsub("\\:","", new_data $amenities)
new_data $amenities <- gsub("\\.","", new_data $amenities)
new_data $amenities <- gsub("\\_"," ", new_data $amenities)
new_data $amenities <- gsub("\\,",", ", new_data $amenities)
new_data $amenities <- gsub("  "," ", new_data $amenities)
new_data $amenities <- gsub("translation missing enhosting amenity 50","", new_data $amenities, fixed = TRUE)
new_data $amenities <- gsub("translation missing enhosting amenity 49","", new_data $amenities, fixed = TRUE)

new_data $amenities <- ifelse(grepl("TV", new_data $amenities) & grepl("Cable TV,", new_data $amenities), 
                             gsub("Cable TV, ", "", new_data $amenities), new_data $amenities)


new_data $amenities <- gsub("(Internet|Wireless Internet|Wifi)", "Wifi", new_data $amenities, ignore.case = TRUE)
new_data $amenities <- gsub("(Air conditioning|Heating)", "Thermostat", new_data $amenities, ignore.case = TRUE)
new_data $amenities <- gsub("(Dryer|Washer)", "Laundry", new_data $amenities, ignore.case = TRUE)
new_data $amenities <- gsub("(Smoke detector|Carbon monoxide detector)", "Smoke detector", new_data $amenities, ignore.case = TRUE)
new_data $amenities <- gsub("(Essentials|Shampoo|Hangers|Hair dryer|Iron)", "Essentials", new_data $amenities, ignore.case = TRUE)
new_data $amenities <- gsub("(Laptop friendly workspace)", "Workplace", new_data $amenities, ignore.case = TRUE)
new_data $amenities <- gsub("(Free parking on premises)", "Free parking", new_data $amenities, ignore.case = TRUE)
new_data $amenities <- gsub("(Safety card|First aid kit)", "First-aid", new_data $amenities, ignore.case = TRUE)
new_data $amenities <- sapply(new_data $amenities, unique)
new_data $amenities <- as.factor(new_data $amenities)

# Extract unique amenities from amenities column
diff_amenities <- unique(unlist(strsplit(as.character(new_data$amenities), ', ')))
diff_amenities <- diff_amenities[nzchar(diff_amenities)]
diff_amenities <- setdiff(diff_amenities, "None")

# Count the number of amenities for each row
new_data$amenity_count <- apply(new_data, 1, function(x) {
  sum(sapply(diff_amenities, function(amenity) grepl(amenity, x["amenities"])))
})

# assume the host_since column is a character string
new_data $host_since <- as.Date(new_data $host_since)

# calculate the number of years since host_since
new_data $host_since_years <- as.numeric(interval(new_data $host_since, Sys.Date()) / years(1))

new_data $host_since_years = ifelse(is.na(new_data $host_since_years), mean(new_data $host_since_years, na.rm=TRUE), new_data $host_since_years)


new_data $host_since_years

sum(is.na(new_data $host_since_years))

new_data $first_review <- as.Date(new_data $first_review)

# calculate the number of years since host_since
new_data $first_review_years <- as.numeric(interval(new_data $first_review, Sys.Date()) / years(1))

new_data $first_review_years = ifelse(is.na(new_data $first_review_years), mean(new_data $first_review_years, na.rm=TRUE), new_data $first_review_years)


new_data $first_review_years

sum(is.na(new_data $first_review_years))

# Transit
# create a new variable called "Cab" and assign 1 if any of the keywords are found in "transit", and 0 otherwise
new_data$Cab <- ifelse(grepl("Cab|taxi|car|uber|lyft", new_data$transit, ignore.case = TRUE), 1, 0)

new_data$Bus <- ifelse(grepl("\\bbus\\b", new_data$transit, ignore.case = TRUE), 1, 0)

new_data$Train <- ifelse(grepl("Train|metro|Subway|Rail", new_data$transit, ignore.case = TRUE), 1, 0)

new_data$Plane <- ifelse(grepl("Airport|airplane|Aeroplan|flight|Plane", new_data$transit, ignore.case = TRUE), 1, 0)

new_data$Bike <- ifelse(grepl("*Bike|Bicycle|Cycle|Scooter", new_data$transit, ignore.case = TRUE), 1, 0)

new_data$count_of_transportation <- rowSums(new_data[, c("Cab", "Bus", "Train", "Plane", "Bike")])

#K-means for latitude longitude
# nearest airbnb's 
library(sp)
library(dplyr)
new_data <- new_data %>% rename(lat = latitude)
new_data <- new_data %>% rename(lon = longitude)

# Create coordinates matrix
coords <- cbind(new_data$lon, new_data$lat)
sp_df <- SpatialPointsDataFrame(coords, new_data)


# Determine the optimal number of clusters using the elbow method
wss <- (nrow(coords)-1)*sum(apply(coords,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(coords, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Set the number of clusters based on the elbow plot
k <- 7

# Perform k-means clustering on the coordinates
km <- kmeans(coords, centers = k)

# Add the cluster assignments to the original dataframe
new_data$cluster <- km$cluster

sum(is.na(new_data$cluster))

#K-means on market
k_market = 7
market_cluster <- kmeans(scale(model.matrix(~market - 1, data = new_data)), centers = k_market, nstart = 20)
new_data $market_cluster <- market_cluster$cluster

#interaction terms
new_data$acc_bed <- new_data$accommodates * new_data$bedrooms
new_data$ava30_ava60 <- new_data$availability_30 * new_data$availability_60
new_data$ava60_ava90 <- new_data$availability_60 * new_data$availability_90
new_data$ava90_ava365 <- new_data$availability_90 * new_data$availability_365
new_data$amen_ver <- new_data$amenity_count * new_data$verif_count


new_data$bath_bed_ratio <- new_data$bathrooms/new_data$bedrooms
new_data <- new_data %>%
  mutate(bath_bed_ratio = ifelse((is.na(bath_bed_ratio) | is.infinite(bath_bed_ratio)), 0, bath_bed_ratio))

avg_bath_bed_ratio = sum(new_data$bathrooms)/sum(new_data$bedrooms)
new_data <- new_data %>%
  mutate(bath_bed_avg_comp = ifelse(bath_bed_ratio >= avg_bath_bed_ratio, TRUE, FALSE))

new_data $ max_min_ratio = new_data$maximum_nights/new_data$minimum_nights
summary(is.infinite(new_data$max_min_ratio))

levels(new_data$city)
which(new_data$city == "11220")
new_data$city[311] = new_data$city_name[311]

levels(as.factor(new_data$country))
levels(as.factor(new_data$country_code))

new_data$is_business_travel_ready <- as.factor(ifelse(is.na(new_data$is_business_travel_ready), "Not Provided", new_data$is_business_travel_ready))
new_data <- new_data %>%
  mutate(license_possession  = case_when(is.na(license) ~ "FALSE",
                                         grepl("pending", license, fixed = TRUE) | grepl("Pending", license, fixed = TRUE) | 
                                           grepl("process", license, fixed = TRUE)   ~ "PENDING",
                                         TRUE ~ "TRUE"),
         license_possession = as.factor(license_possession))

new_data <- new_data %>%
mutate(
  jurisdiction_names = ifelse(is.na(jurisdiction_names), "Other", jurisdiction_names),
  jurisdiction_names = str_trim(gsub("[^[:alnum:]]", " ", jurisdiction_names)),
  jurisdiction_names = str_squish(jurisdiction_names),          #Removing repeated whitespaces
  jurisdiction_names = as.factor(jurisdiction_names))

new_data <- new_data %>%
  group_by(jurisdiction_names) %>% 
  mutate(jur_count = n(),
         jurisdiction_names = if_else(jur_count < 100, "Other",jurisdiction_names)) %>% 
  ungroup() %>%
  mutate(maximum_nights = cut(maximum_nights, breaks = c(0,14,30,1124,Inf),
                     labels = c("0-2 Weeks","2 weeks to a month","1 month to 3 years","More than 3 years")),
         max_nights_labels = maximum_nights)

to_replace <- c("Бруклин","南艾尔蒙地","哈仙达岗","圣地亚哥", "天普市","布鲁克林","沃尔纳特",
                "法拉盛", "波士顿", "波摩纳", "洛杉矶","纽约", "纽约市", "纽约法拉盛",
                "罗兰高地", "艾尔蒙地", "西雅图","聖地亞哥","西科维纳","马里布")

after_replace <- c("Brooklyn","South El Monte","Hacienda Height","San Diego", "Temple City",
                   "Brooklyn", "Walnut", "Flushing", "Boston", "Pomona", "Los Angeles", "New York",
                   "New York", "Flushing", "Rowland Heights", "El Monte", "Seattle", "San Diego","West Covina", "Malibu" )

for(i in 1:length(to_replace)){
  new_data$city <- gsub(to_replace[i],after_replace[i],new_data$city,perl = TRUE)
}

new_data<- new_data %>% 
  group_by(city) %>% 
  mutate(city = trimws(city),
         lat = if_else(is.na(lat),mean(lat,na.rm = T),lat),
         lon = if_else(is.na(lon),mean(lon,na.rm = T),lon)) %>% ungroup()

library(geosphere)
#install.packages('geosphere')

airport_data <- read_csv("airport_domestic.csv")
airport_data <- airport_data %>%
  rename(state = iso_region) %>%
  mutate(state = gsub("-.*", "", state))
airport_data <- airport_data %>%
  select(name, latitude_deg, longitude_deg, state)

airbnb_coords <- new_data %>%
  select(lat, lon,id)

airport_coords <-airport_data %>%
  select(latitude_deg, longitude_deg, name)

# create an empty vector to store the distances
airport_dist <- vector(mode = "numeric", length = nrow(airbnb_coords))

for (i in 1:nrow(airbnb_coords)) {
  airport_dist[i] <- min(distHaversine(p1 = airbnb_coords[i, c("lon", "lat")], 
                                    p2 = airport_coords[, c("longitude_deg", "latitude_deg")]))
}

new_data $ dist_nearest_airport <- airport_dist

wage_data <- read_csv("Wage_domestic.csv")
wage_avg <- wage_data %>% 
  group_by(State) %>% 
  mutate(avg_wage = mean(TotalWages)) %>% 
  ungroup()

# find top 10% high-wage areas
wage_quantile <- quantile(wage_avg$avg_wage, probs = 0.5)
high_wage_areas <- wage_avg %>% 
  filter(avg_wage >= wage_quantile)

dist_higher_wages <- vector(mode = "numeric", length = nrow(airbnb_coords))

for (i in 1:nrow(new_data)) {
  high_wage_dist <- distHaversine(p1 = new_data[i, c("lon", "lat")], 
                             p2 = high_wage_areas[, c("Long", "Lat")])
  dist_higher_wages[i] <- min(high_wage_dist)
}

dist_higher_wages_log <- log(dist_higher_wages)
new_data$dist_higher_wages_log <- dist_higher_wages_log
summary(new_data$dist_higher_wages_log)


new_data$jur_count_log <- log(new_data$jur_count)

#Trying readability analysis on text columns
# Install and load the required package
#install.packages("quanteda.textstats")
library(quanteda.textstats)

new_data$description_readability <- textstat_readability(as.character(new_data$description), measure = "Flesch.Kincaid")$Flesch.Kincaid
summary(new_data$description_readability) #median = 8.114

new_data <- new_data %>%
  mutate(description_readability = ifelse(is.na(description_readability), 8.114, description_readability))
summary(new_data$description_readability) #median = 8.757

new_data <- new_data %>%
  mutate(description_readability_analysis = as.factor(ifelse(description_readability > 8.757, "YES", "NO")))

#HOUSE RULES READABILITY
new_data$house_rules_readability <- textstat_readability(as.character(new_data$house_rules), measure = "Flesch.Kincaid")$Flesch.Kincaid
summary(new_data$house_rules_readability) #median = 8.265

new_data <- new_data %>%
  mutate(house_rules_readability = ifelse(is.na(house_rules_readability), 8.265, house_rules_readability))
summary(new_data$house_rules_readability) #median = 8.265

new_data <- new_data %>%
  mutate(house_rules_readability_analysis = as.factor(ifelse(house_rules_readability > 8.265, "YES", "NO")))


#ACCEESS READABILITY
new_data$access_readability <- textstat_readability(as.character(new_data$access), measure = "Flesch.Kincaid")$Flesch.Kincaid
summary(new_data$access_readability) #median = 8.790

new_data <- new_data %>%
  mutate(access_readability = ifelse(is.na(access_readability), 8.790, access_readability))
summary(new_data$access_readability) #mean = 8.326

new_data <- new_data %>%
  mutate(access_readability_analysis = as.factor(ifelse(access_readability > 8.326, "YES", "NO")))

#EXtra People Allowed
new_data$extra_people_allowed <- as.factor(ifelse(new_data$extra_people > 0, "YES", "NO"))
summary(new_data$extra_people_allowed)

#compare monthly price and price*30 , if monthly is less than price*30 new column value cheap else expensive
summary(new_data$price)

summary(is.na(new_data$monthly_price))


new_data <- new_data %>%
  group_by(market) %>%
  mutate(price = ifelse(price==0, median(price), price))
summary(new_data$monthly_price)

new_data$monthly_price = ifelse(is.na(new_data$monthly_price),new_data$price*30 , new_data$monthly_price)

class(new_data$monthly_price)
new_data$monthly_price <- as.numeric(new_data$monthly_price)
#new_data$monthly_price <- gsub("\\$","", new_data$monthly_price)
#new_data$monthly_price <- gsub("\\,","", new_data$monthly_price)


new_data <- new_data %>% 
  mutate(price_times_30 = price * 30)


new_data <- new_data %>% 
  mutate(price_compare = ifelse(monthly_price <= price_times_30, "cheap", "expensive"),
         price_compare = as.factor(price_compare))
#-----------------------------------------------------------------------------------------------------------------------------------------
# Split new_data into train and test using flag  
train <- subset(new_data, flag == "train")
test <- subset(new_data, flag == "test")

train$perfect_rating_score <- droplevels(train$perfect_rating_score)

summary(train)
summary(test)

#-----------------------------------------------------------------------------------------------------------------------------------------
# select model features
# Define the list of features
features <- c("price_per_person",
              "cleaning_fee",
              "cancellation_policy",
              "beds",
              "host_is_superhost",
              "house_rules_sentiment",
              "bedrooms",
              "room_type",
              "bathrooms",
              "instant_bookable",
              "host_response_time",
              "host_acceptance",
              "host_response",
              "charges_for_extra",
              "host_identity_verified",
              "requires_license",
              "ppp_ind",
              "is_location_exact",
              "require_guest_phone_verification",
              "verif_count",
              "host_since_years",
              "first_review_years",
              "notes_sentiment",
              "summary_sentiment",
              "acc_bed",
              "ava30_ava60",
              "ava60_ava90",
              "ava90_ava365",
              "amen_ver",
              "is_business_travel_ready",
              "license_possession",
              "max_nights_labels",
              "dist_nearest_airport",
              "dist_higher_wages_log",
              "neighborhood_sentiment",
              "jur_count_log",
              "interaction_sentiment",
              "lat",
              "lon",
              "price_compare",
              "perfect_rating_score")

# Select the features in train dataset
perfect_rate_train <- train[, features]

# Select the features in test dataset
perfect_rate_test <- test[, features]
perfect_rate_test <- subset(perfect_rate_test, select = -perfect_rating_score)

summary(perfect_rate_train)
summary(perfect_rate_test)
#-----------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------
#Learning Curve of log model
# define range of training set sizes
train_sizes <- seq(from = 0.1, to = 1, by = 0.15)

# initialize vectors to store TPR and FPR values
tpr_values <- numeric(length(train_sizes))
fpr_values <- numeric(length(train_sizes))
summary(is.na(perfect_rate_train))

# loop over training set sizes
for (i in seq_along(train_sizes)) {
  testing_size <- train_sizes[i]
  train_insts <- sample(nrow(perfect_rate_train), size = floor(testing_size* nrow(perfect_rate_train)))
  perfect_train_subset <- perfect_rate_train[train_insts, ]
  perfect_test_subset <- perfect_rate_train[-train_insts, ]
  
  # fit logistic regression model on training set
  log_model <- glm(perfect_rating_score ~ ., data = perfect_train_subset, family = "binomial")
  
  # make predictions on validation set
  log_preds <- predict(log_model, newdata = perfect_test_subset, type = "response")
  log_class <- as.factor(ifelse(log_preds >= 0.5, "YES", "NO"))
  log_class <- factor(log_class, levels = c("YES", "NO"))
  perfect_test_subset$perfect_rating_score <- factor(perfect_test_subset$perfect_rating_score, levels = c("YES", "NO"))
  log_cm <- confusionMatrix(log_class, perfect_test_subset$perfect_rating_score, positive = "YES")
  log_tpr <- log_cm$byClass["Sensitivity"]
  log_fpr <- 1 - log_cm$byClass["Specificity"]
  
  tpr_values[i] <- log_tpr
  fpr_values[i] <- log_fpr
}

# Plot learning curve
plot(train_sizes*100, tpr_values, type = "b", pch = 16, col = "blue",
     xlab = "Training Set Size", ylab = "TPR", main = "Learning Curve",
     xlim = c(0, max(train_sizes*100)), ylim = c(0, 0.4))
lines(train_sizes*100, tpr_values, type = "b", pch = 16, col = "blue")
points(train_sizes*100, fpr_values, pch = 16, col = "red")
lines(train_sizes*100, fpr_values, type = "b", pch = 16, col = "red")

# Adjust the cex parameter to control the font size in the legend
legend("topright", legend = c("TPR", "FPR"), col = c("blue", "red"), pch = 16, cex = 0.6)

#------------------------------------------------------------------------------------------------------------------------
#Split training into train and test data
train_insts <- sample(nrow(perfect_rate_train), size = round(0.85* nrow(perfect_rate_train)), replace = FALSE)
perfect_train_subset <- perfect_rate_train[train_insts, ]
perfect_test_subset <- perfect_rate_train[-train_insts, ]

#-----------------------------------------------------------------------------------------------------
# log model
log_model <- glm(perfect_rating_score~., data = perfect_train_subset, family = "binomial")
summary(log_model)


log_cutoffs <- seq(0, 1, by = 0.01)
log_tprs <- numeric(length(log_cutoffs))
log_fprs <- numeric(length(log_cutoffs))

log_preds <- predict(log_model, newdata = perfect_test_subset, type = "response")

log_max_tpr <- 0
log_max_fpr <- 0
log_opt_cutoff <- 0
for (i in seq_along(log_cutoffs)) {
  log_classifications <- as.factor(ifelse(log_preds >= log_cutoffs[i], "YES", "NO"))
  log_classifications <- factor(log_classifications, levels = levels(perfect_test_subset$perfect_rating_score))
  log_cm <- confusionMatrix(log_classifications, perfect_test_subset$perfect_rating_score, positive = "YES")
  log_tprs[i] <- ifelse(is.na(log_cm$byClass["Sensitivity"]), 0, log_cm$byClass["Sensitivity"])
  log_fprs[i] <- ifelse(is.na(1 - log_cm$byClass["Specificity"]), 0, 1 - log_cm$byClass["Specificity"])
  
  if (log_tprs[i] >= log_max_tpr && log_fprs[i] < 0.10) {
    log_max_tpr <- log_tprs[i]
    log_max_fpr <- log_fprs[i]
    log_opt_cutoff <- log_cutoffs[i]
  }
}

#Fitting curve for Log Model
# Set xlim and ylim for better clarity
plot(log_cutoffs, log_tprs, type = "l", xlab = "Cutoff", ylab = "True Positive Rate", 
     main = "Fitting Curve for Logistic Regression Model", col = "blue", lwd = 2)
xlim <- range(log_cutoffs)
ylim <- range(log_tprs)
xlim <- c(xlim[1] - 0.1 * diff(xlim), xlim[2] + 0.1 * diff(xlim))
ylim <- c(ylim[1] - 0.1 * diff(ylim), ylim[2] + 0.1 * diff(ylim))
xlim(xlim)
ylim(ylim)
grid(col = "lightgray")


log_classifications <- as.factor(ifelse(log_preds >= log_opt_cutoff, "YES", "NO"))
log_classifications <- factor(log_classifications, levels = levels(perfect_test_subset$perfect_rating_score))
log_cm <- confusionMatrix(log_classifications, perfect_test_subset$perfect_rating_score, positive = "YES")
log_acc <- log_cm$overall["Accuracy"]
log_tpr <- log_cm$byClass["Sensitivity"]
log_fpr <- 1 - log_cm$byClass["Specificity"]
cat("Accuracy of Log Model: ", log_acc, "\n")
cat("TPR of Log Model: ", log_tpr, "\n")
cat("FPR of Log Model: ", log_fpr, "\n")

#training and predictions on final train and test dataset
log_model_final <- glm(perfect_rating_score~., data = perfect_rate_train, family = "binomial")
summary(log_model_final)
log_preds_final <- predict(log_model_final, newdata = perfect_test_subset, type = "response")
log_class_final <- as.factor(ifelse(log_preds_final >= 0.45, "YES", "NO"))
write.table(log_class_final, "logistic_perfect_rating_score_group001.csv", row.names = FALSE)
#-------------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------------------------
#RANDOM FOREST
rf.mod <- randomForest(perfect_rating_score~.,
                        data=perfect_rate_train,
                        subset = train_insts,
                        mtry=7,
                        ntree = 1000, #defaults to 500 trees
                        importance=TRUE) 

#selecting the optimal cutoff
rf_cutoffs <- seq(0, 1, by = 0.01)
rf_tprs <- numeric(length(rf_cutoffs))
rf_fprs <- numeric(length(rf_cutoffs))

rf_probs <- predict(rf.mod, newdata = perfect_test_subset, type = "prob")

rf_max_tpr <- 0
rf_max_fpr <- 0
rf_opt_cutoff <- 0
for (i in seq_along(rf_cutoffs)) {
  rf_pred_class <- as.factor(ifelse(rf_probs[, "YES"] >= rf_cutoffs[i], "YES", "NO"))
  rf_pred_class <- factor(rf_pred_class, levels = levels(perfect_test_subset$perfect_rating_score))
  rf_cm <- confusionMatrix(rf_pred_class, perfect_test_subset$perfect_rating_score, positive = "YES")
  rf_tprs[i] <- rf_cm$byClass["Sensitivity"]
  rf_fprs[i] <- 1 - rf_cm$byClass["Specificity"]
  
  if (rf_tprs[i] > rf_max_tpr && rf_fprs[i] < 0.10) {
    rf_max_tpr <- rf_tprs[i]
    rf_max_fpr <- rf_fprs[i]
    rf_opt_cutoff <- rf_cutoffs[i]
  }
}

cat("Optimal threshold: ", rf_opt_cutoff, "\n")
cat("Max TPR: ", rf_max_tpr, "\n")
cat("Max FPR: ", rf_max_fpr, "\n")


#Classification of the probabilities of perfect_rating_score
rf_pred_class <- ifelse(rf_probs[, "YES"] > rf_opt_cutoff, "YES", "NO")
rf_pred_class <- factor(rf_pred_class, levels = levels(perfect_test_subset$perfect_rating_score))
summary(rf_pred_class)

# Compute confusion matrix
rf_cm <- confusionMatrix(rf_pred_class, perfect_test_subset$perfect_rating_score, positive = "YES")

# Calculate TPR and FPR
rf_acc <- rf_cm$overall["Accuracy"]
rf_tpr <- rf_cm$byClass["Sensitivity"]
rf_fpr <- 1 - rf_cm$byClass["Specificity"]


# Print TPR, FPR, and Accuracy
cat("Accuracy of Random Forest Model: ", rf_acc, "\n")
cat("TPR of Random Forest Model: ", rf_tpr, "\n")
cat("FPR of Random Forest Model: ", rf_fpr, "\n")

#Extraxt predictions into csv for the test dataset
rf.mod <- randomForest(perfect_rating_score~.,
                        data=perfect_rate_train,
                        #subset = train_insts,
                        mtry=7,
                        ntree = 1000, #defaults to 500 trees
                        importance=TRUE) 
rf_probs <- predict(rf.mod, newdata = perfect_rate_test, type = "prob")
rf_pred_class <- as.factor(ifelse(rf_probs[, "YES"] > rf_opt_cutoff, "YES", "NO"))
summary(rf_pred_class)
write.table(rf_pred_class, "random_forest_perfect_rating_score_group001.csv", row.names = FALSE)
#-------------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------------------------------------------
#BAGGING
bag.mod <- randomForest(perfect_rating_score~.,
                        data=perfect_rate_train,
                        subset = train_insts,
                        mtry=40,
                        ntree = 1000, #defaults to 500 trees
                        importance=TRUE) 

#selecting the optimal cutoff
cutoffs <- seq(0, 1, by = 0.01)
tprs <- numeric(length(cutoffs))
fprs <- numeric(length(cutoffs))

bag_probs <- predict(bag.mod, newdata = perfect_test_subset, type = "prob")


max_tpr <- 0
max_fpr <- 0
opt_cutoff <- 0
for (i in seq_along(cutoffs)) {
  bag_pred_class <- as.factor(ifelse(bag_probs[, "YES"] >= cutoffs[i], "YES", "NO"))
  bag_pred_class <- factor(bag_pred_class, levels = levels(perfect_test_subset$perfect_rating_score))
  bag_cm <- confusionMatrix(bag_pred_class, perfect_test_subset$perfect_rating_score, positive = "YES")
  tprs[i] <- bag_cm$byClass["Sensitivity"]
  fprs[i] <- 1 - bag_cm$byClass["Specificity"]
  
  if (tprs[i] > max_tpr && fprs[i] < 0.10) {
    max_tpr <- tprs[i]
    max_fpr <- fprs[i]
    opt_cutoff <- cutoffs[i]
  }
}

cat("Optimal threshold: ", opt_cutoff, "\n")
cat("Max TPR: ", max_tpr, "\n")
cat("Max FPR: ", max_fpr, "\n")


#Classification of the probabilities of perfect_rating_score
bag_pred_class <- ifelse(bag_probs[, "YES"] > opt_cutoff, "YES", "NO")
bag_pred_class <- factor(bag_pred_class, levels = levels(perfect_test_subset$perfect_rating_score))
summary(bag_pred_class)

# Compute confusion matrix
bag_cm <- confusionMatrix(bag_pred_class, perfect_test_subset$perfect_rating_score, positive = "YES")

# Calculate TPR and FPR
bag_acc <- bag_cm$overall["Accuracy"]
bag_tpr <- bag_cm$byClass["Sensitivity"]
bag_fpr <- 1 - bag_cm$byClass["Specificity"]

# Print TPR, FPR, and Accuracy
cat("Accuracy of Bagging Model: ", bag_acc, "\n")
cat("TPR of Bagging Model: ", bag_tpr, "\n")
cat("FPR of Bagging Model: ", bag_fpr, "\n")

#Extraxt predictions into csv for the test dataset
bag.mod <- randomForest(perfect_rating_score~.,
                        data=perfect_rate_train,
                        #subset = train_insts,
                        mtry=40,
                        ntree = 1000, #defaults to 500 trees
                        importance=TRUE) 
bag_probs <- predict(bag.mod, newdata = perfect_rate_test, type = "prob")
bag_pred_class <- as.factor(ifelse(bag_probs[, "YES"] > opt_cutoff, "YES", "NO"))
summary(bag_pred_class)
write.table(bag_pred_class, "bagging_perfect_rating_score_group001.csv", row.names = FALSE)
#-----------------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------------------
# XGBOOST
library(xgboost)

#Creating dummies of new data
xgb_train <- subset(new_data, flag == "train")
xgb_test <- subset(new_data, flag == "test")

xgb_train <- xgb_train[, features]
xgb_test <- xgb_test[, features]

xgb_train$perfect_rating_score <- droplevels(xgb_train$perfect_rating_score)

dummy_dataset <- dummyVars( ~ ., data = xgb_train, fullRank = TRUE)
xgboost_train_data <- data.frame(predict(dummy_dataset, newdata = xgb_train))

dummy_dataset <- dummyVars( ~ ., data = xgb_test, fullRank = TRUE)
xgboost_test_data <- data.frame(predict(dummy_dataset, newdata = xgb_test))

xgboost_test_data <- subset(xgboost_test_data, select = -c(perfect_rating_score.YES, perfect_rating_score.Dummy))

# creating a dataset of 10,000 rows for hyper-parameter and cutoff tuning 
xgb_train_insts <- sample(nrow(xgboost_train_data), size = round(0.85* nrow(xgboost_train_data)), replace = FALSE)
xgb_t <- xgboost_train_data[xgb_train_insts, ]
xgb_v <- xgboost_train_data[-xgb_train_insts, ]

tr_y_num <- xgb_t$perfect_rating_score.YES
va_y_num <- xgb_v$perfect_rating_score.YES
va_y_num_factor <- as.factor(ifelse(va_y_num == 1, "YES", "NO"))  

xgb_t <- subset(xgb_t, select = -perfect_rating_score.YES)
xgb_v <- subset(xgb_v, select = -perfect_rating_score.YES)


train_matrix <- xgb.DMatrix(data = as.matrix(xgb_t), label = tr_y_num)
test_matrix <- xgb.DMatrix(data = as.matrix(xgb_v), label = va_y_num)


#Hyper-parameter tuning of XGBoost
xgb_cutoffs <- seq(0, 1, by = 0.01)
xgb_tprs <- numeric(length(xgb_cutoffs))
xgb_fprs <- numeric(length(xgb_cutoffs))
xgb_accs <- numeric(length(xgb_cutoffs))


xgb_max_tpr <- 0
xgb_max_fpr <- 0
xgb_max_acc <- 0
xgb_final_depth <- 0
xgb_final_nrounds <- 0
xgb_final_eta <- 0
xgb_opt_cutoff <- 0
count <- 0

grid_search <- function(){
  
  #three hyperparameters can possibly really change predictive performance of xgboost (although maybe not)
  depth_choose <- c(2, 5)
  nrounds_choose <- c(100, 500, 1000, 1500)
  eta_choose <- c(.1, .2, .3, 1)
  
  #nested loops to tune these three parameters
  print('depth, nrounds, eta, accuracy')
  for(i in c(1:length(depth_choose))){
    for(j in c(1:length(nrounds_choose))){
      for(k in c(1:length(eta_choose))){
        thisdepth <- depth_choose[i]
        thisnrounds <- nrounds_choose[j]
        thiseta <- eta_choose[k]
        count = count + 1
        
        inner_bst <- xgboost(data = train_matrix, label = tr_y_num, max.depth = thisdepth, eta = thiseta, nrounds = thisnrounds,  objective = "binary:logistic", verbosity = 0, verbose = 0)
        inner_bst_pred <- predict(inner_bst, test_matrix)
        for (m in seq_along(xgb_cutoffs)) {
          inner_bst_classifications <- as.factor(ifelse(inner_bst_pred >= xgb_cutoffs[m], "YES", "NO"))
          xgb_cm <- confusionMatrix(inner_bst_classifications, va_y_num_factor, positive = "YES")
          xgb_tprs[m] <- xgb_cm$byClass["Sensitivity"]
          xgb_fprs[m] <- 1 - xgb_cm$byClass["Specificity"]
          
          if (xgb_tprs[m] >= xgb_max_tpr && xgb_fprs[m] < 0.10) {
            xgb_max_tpr <<- xgb_tprs[m]
            xgb_max_fpr <<- xgb_fprs[m]
            xgb_opt_cutoff <<- xgb_cutoffs[m]
            xgb_max_acc <<- xgb_cm$overall["Accuracy"]
            xgb_final_depth <<- thisdepth
            xgb_final_nrounds <<- thisnrounds
            xgb_final_eta <<- thiseta
          }
        }
        cat("round", count, "is done","TPR:",xgb_max_tpr, "FPR:", xgb_max_fpr, "depth:",  thisdepth, "nrounds:", thisnrounds, "eta:", thiseta, "\n")
    }
  }
    }
  }

grid_search()

print("Final Parameters for XgBoost Model Are: ")
cat("Accuracy of XgBoost Model: ", xgb_max_acc, "\n")
cat("TPR of XgBoost Model: ", xgb_max_tpr, "\n")
cat("FPR of XgBoost Model: ", xgb_max_fpr, "\n")
cat("Depth of XgBoost Model: ", xgb_final_depth, "\n")
cat("Nrounds of XgBoost Model: ", xgb_final_nrounds, "\n")
cat("Eta of XgBoost Model: ", xgb_final_eta, "\n")
cat("Optimal Cutoff of XgBoost Model: ", xgb_opt_cutoff, "\n")

# Predictions using final hyper-parameters of Xgboost model:
#depth = 2
# nrounds = 1000
# eta = 0.1
# opt cutoff = 0.49
inner_bst <- xgboost(data = train_matrix, label = tr_y_num, max.depth = 2, eta = 0.1, nrounds = 1000,  objective = "binary:logistic", verbosity = 0, verbose = 0)

#Trying differet cutoff values upto 4 decimal places
xgb_try_cutoffs <- seq(0, 1, by = 0.0001)
xgb_try_tprs <- numeric(length(xgb_try_cutoffs))
xgb_try_fprs <- numeric(length(xgb_try_cutoffs))

inner_bst_pred <- predict(inner_bst, test_matrix)


max_tpr_xgb_try <- 0
max_fpr_xgb_try <- 0
opt_cutoff_xgb_try <- 0
for (i in seq_along(xgb_try_cutoffs)) {
  inner_bst_classifications <- as.factor(ifelse(inner_bst_pred >= xgb_try_cutoffs[i], "YES", "NO"))
  xgb_cm <- confusionMatrix(inner_bst_classifications, va_y_num_factor, positive = "YES")
  xgb_try_tprs[i] <- xgb_cm$byClass["Sensitivity"]
  xgb_try_fprs[i] <- 1 - xgb_cm$byClass["Specificity"]
  
  if (xgb_try_tprs[i] >= max_tpr_xgb_try && xgb_try_fprs[i] < 0.0979999) {
    max_tpr_xgb_try <- xgb_try_tprs[i]
    max_fpr_xgb_try <- xgb_try_fprs[i]
    opt_cutoff_xgb_try <- xgb_try_cutoffs[i]
  }
}

cat("Optimal threshold: ", opt_cutoff_xgb_try, "\n")
cat("Max TPR: ", max_tpr_xgb_try, "\n")
cat("Max FPR: ", max_fpr_xgb_try, "\n")

inner_bst_classifications <- as.factor(ifelse(inner_bst_pred >= opt_cutoff_xgb_try, "YES", "NO"))
xgb_cm <- confusionMatrix(inner_bst_classifications, va_y_num_factor, positive = "YES")
inner_bst_acc <- xgb_cm$overall["Accuracy"]
inner_bst_tpr <- xgb_cm$byClass["Sensitivity"]
inner_bst_fpr <- 1 - xgb_cm$byClass["Specificity"]
cat("Accuracy of XgBoost Model: ", inner_bst_acc, "\n")
cat("TPR of XgBoost Model: ", inner_bst_tpr, "\n")
cat("FPR of XgBoost Model: ", inner_bst_fpr, "\n")


#Precitions on the final train data to predict 12205 values for perfect_rating_score:
tr_y_num <- xgboost_train_data$perfect_rating_score.YES
xgboost_train_data <- subset(xgboost_train_data, select = -perfect_rating_score.YES)

train_matrix_final <- xgb.DMatrix(data = as.matrix(xgboost_train_data), label = tr_y_num)
test_matrix_final <- xgb.DMatrix(data = as.matrix(xgboost_test_data))


dim(train_matrix_final)
dim(test_matrix_final)

inner_bst_final <- xgboost(data = train_matrix_final, label = tr_y_num, max.depth = 2, eta = 0.1, nrounds = 1000,  objective = "binary:logistic", verbosity = 0, verbose = 0)
inner_bst_pred_final <- predict(inner_bst_final, test_matrix_final)
inner_bst_class_final <- as.factor(ifelse(inner_bst_pred_final >= opt_cutoff_xgb_try, "YES", "NO"))
levels(inner_bst_class_final)
summary(inner_bst_class_final)
write.table(inner_bst_class_final, "xgboost_perfect_rating_score_group001.csv", row.names = FALSE)
#-----------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------
#RIDGE
library(glmnet)

#Creating dummies of new data
ridge_train <- subset(new_data, flag == "train")
ridge_test <- subset(new_data, flag == "test")

ridge_train <- ridge_train[, features]
ridge_test <- ridge_test[, features]

ridge_train$perfect_rating_score <- droplevels(ridge_train$perfect_rating_score)

dummy_dataset_tr <- dummyVars( ~ ., data = ridge_train, fullRank = TRUE)
ridge_train_data <- data.frame(predict(dummy_dataset_tr, newdata = ridge_train))

dummy_dataset_te <- dummyVars( ~ ., data = ridge_test, fullRank = TRUE)
ridge_test_data <- data.frame(predict(dummy_dataset_te, newdata = ridge_test))

ridge_test_data <- subset(ridge_test_data, select = -c(perfect_rating_score.Dummy))

ridge_train_insts <- sample(nrow(ridge_train_data), size = round(0.85* nrow(ridge_train_data)), replace = FALSE)
ridge_t <- ridge_train_data[ridge_train_insts, ]
ridge_v <- ridge_train_data[-ridge_train_insts, ]

ridge_tr_y_num <- ridge_t$perfect_rating_score.YES
ridge_va_y_num <- ridge_v$perfect_rating_score.YES
ridge_va_y_num_factor <- as.factor(ifelse(ridge_va_y_num == 1, "YES", "NO"))  


ridge_train_matrix <- model.matrix(perfect_rating_score.YES ~ ., data = ridge_t)
ridge_valid_matrix <- model.matrix(perfect_rating_score.YES ~ ., data = ridge_v)

# Fit the Ridge regression model using cross-validation to determine the optimal lambda
lambda_grid <- 10^seq(-7, 7, length.out = 100)
fit_ridge <- cv.glmnet(ridge_train_matrix, ridge_tr_y_num, family = "binomial", alpha = 0, lambda = lambda_grid, nfolds = 5)

# Get the optimal lambda value
lambda_opt_ridge <- fit_ridge$lambda.min
cat("The optimal lambda for Ridge-penalized logistic regression is", lambda_opt_ridge, "\n")

#predictions on the test dataset
ridge_preds <- predict(fit_ridge, ridge_valid_matrix, s = lambda_opt_ridge, type = "response")

#Fitting curve for Ridge Model
lambdas <- fit_ridge$lambda
errors <- fit_ridge$cvm
# Set xlim and ylim for better clarity
plot(log(lambdas), errors, type = "l", xlab = "Log of Lambda", ylab = "Cross-Validation Error", 
     main = "Fitting Curve for Ridge Model", col = "blue", lwd = 2)
xlim <- range(log(lambdas))
ylim <- range(errors)
xlim <- c(xlim[1] - 0.1 * diff(xlim), xlim[2] + 0.1 * diff(xlim))
ylim <- c(ylim[1] - 0.1 * diff(ylim), ylim[2] + 0.1 * diff(ylim))
xlim(xlim)
ylim(ylim)
grid(col = "lightgray")

# Convert the predictions back to "YES" or "NO"
ridge_class <- as.factor(ifelse(ridge_preds >= 0.5, "YES", "NO"))

ridge_cm <- confusionMatrix(ridge_class, ridge_va_y_num_factor, positive = "YES")
ridge_acc <- ridge_cm$overall["Accuracy"]
ridge_tpr <- ridge_cm$byClass["Sensitivity"]
ridge_fpr <- 1 - ridge_cm$byClass["Specificity"]
cat("Accuracy of Ridge-penalized logistic regression Model: ", ridge_acc, "\n")
cat("TPR of Ridge-penalized logistic regression Model: ", ridge_tpr, "\n")
cat("FPR of Ridge-penalized logistic regression Model: ", ridge_fpr, "\n")

#Precitions on the final train data to predict 12205 values for perfect_rating_score:
ridge_tr_y_num_final <- ridge_train_data$perfect_rating_score.YES

ridge_train_matrix_final <- model.matrix(perfect_rating_score.YES ~ ., data = ridge_train_data)
ridge_test_matrix_final <- model.matrix(perfect_rating_score.YES ~ ., data = ridge_test_data)

dim(ridge_train_matrix_final)
dim(ridge_test_matrix_final)

fit_ridge_final <- glmnet(ridge_train_matrix_final, ridge_tr_y_num_final, family = "binomial", alpha = 0, lambda = lambda_opt_ridge)

#predictions on the test dataset
ridge_preds_final <- predict(fit_ridge_final, ridge_test_matrix_final, type = "response")

# Convert the predictions back to "YES" or "NO"
ridge_class_final <- as.factor(ifelse(ridge_preds_final >= 0.5, "YES", "NO"))
levels(ridge_class_final)
summary(ridge_class_final)
write.table(ridge_class_final, "ridge_perfect_rating_score_group001.csv", row.names = FALSE)
#---------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------
#LASSO
library(glmnet)

#Creating dummies of new data
lasso_train <- subset(new_data, flag == "train")
lasso_test <- subset(new_data, flag == "test")

lasso_train <- lasso_train[, features]
lasso_test <- lasso_test[, features]

lasso_train$perfect_rating_score <- droplevels(lasso_train$perfect_rating_score)

dummy_dataset_tr <- dummyVars( ~ ., data = lasso_train, fullRank = TRUE)
lasso_train_data <- data.frame(predict(dummy_dataset_tr, newdata = lasso_train))

dummy_dataset_te <- dummyVars( ~ ., data = lasso_test, fullRank = TRUE)
lasso_test_data <- data.frame(predict(dummy_dataset_te, newdata = lasso_test))

lasso_test_data <- subset(lasso_test_data, select = -c(perfect_rating_score.Dummy))

lasso_train_insts <- sample(nrow(lasso_train_data), size = round(0.85* nrow(lasso_train_data)), replace = FALSE)
lasso_t <- lasso_train_data[lasso_train_insts, ]
lasso_v <- lasso_train_data[-lasso_train_insts, ]

lasso_tr_y_num <- lasso_t$perfect_rating_score.YES
lasso_va_y_num <- lasso_v$perfect_rating_score.YES
lasso_va_y_num_factor <- as.factor(ifelse(lasso_va_y_num == 1, "YES", "NO"))  


lasso_train_matrix <- model.matrix(perfect_rating_score.YES ~ ., data = lasso_t)
lasso_valid_matrix <- model.matrix(perfect_rating_score.YES ~ ., data = lasso_v)

# Fit the Ridge regression model using cross-validation to determine the optimal lambda
lambda_grid <- 10^seq(-7, 7, length.out = 100)
fit_lasso <- cv.glmnet(lasso_train_matrix, lasso_tr_y_num, family = "binomial", alpha = 1, lambda = lambda_grid, nfolds = 5)

# Get the optimal lambda value
lambda_opt_lasso <- fit_lasso$lambda.min
cat("The optimal lambda for Lasso-penalized logistic regression is", lambda_opt_lasso, "\n")

#Fitting curve for Ridge Model
lasso_lambdas <- fit_ridge$lambda
lasso_errors <- fit_ridge$cvm
# Set xlim and ylim for better clarity
plot(log(lasso_lambdas), lasso_errors, type = "l", xlab = "Log of Lambda", ylab = "Cross-Validation Error", 
     main = "Fitting Curve for Lasso Model", col = "green", lwd = 2)
xlim <- range(log(lasso_lambdas))
ylim <- range(lasso_errors)
xlim <- c(xlim[1] - 0.1 * diff(xlim), xlim[2] + 0.1 * diff(xlim))
ylim <- c(ylim[1] - 0.1 * diff(ylim), ylim[2] + 0.1 * diff(ylim))
xlim(xlim)
ylim(ylim)
grid(col = "lightgray")

#predictions on the test dataset
lasso_preds <- predict(fit_lasso, lasso_valid_matrix, s = lambda_opt_lasso, type = "response")

# Convert the predictions back to "YES" or "NO"
lasso_class <- as.factor(ifelse(lasso_preds >= 0.5, "YES", "NO"))

lasso_cm <- confusionMatrix(lasso_class, lasso_va_y_num_factor, positive = "YES")
lasso_acc <- lasso_cm$overall["Accuracy"]
lasso_tpr <- lasso_cm$byClass["Sensitivity"]
lasso_fpr <- 1 - lasso_cm$byClass["Specificity"]
cat("Accuracy of Lasso-penalized logistic regression Model: ", lasso_acc, "\n")
cat("TPR of Lasso-penalized logistic regression Model: ", lasso_tpr, "\n")
cat("FPR of Lasso-penalized logistic regression Model: ", lasso_fpr, "\n")

#Precitions on the final train data to predict 12205 values for perfect_rating_score:
lasso_tr_y_num_final <- lasso_train_data$perfect_rating_score.YES

lasso_train_matrix_final <- model.matrix(perfect_rating_score.YES ~ ., data = lasso_train_data)
lasso_test_matrix_final <- model.matrix(perfect_rating_score.YES ~ ., data = lasso_test_data)

dim(lasso_train_matrix_final)
dim(lasso_test_matrix_final)

fit_lasso_final <- glmnet(lasso_train_matrix_final, lasso_tr_y_num_final, family = "binomial", alpha = 1, lambda = lambda_opt_lasso)

#predictions on the test dataset
lasso_preds_final <- predict(fit_lasso_final, lasso_test_matrix_final, type = "response")

# Convert the predictions back to "YES" or "NO"
lasso_class_final <- as.factor(ifelse(lasso_preds_final >= 0.5, "YES", "NO"))
levels(lasso_class_final)
summary(lasso_class_final)
write.table(lasso_class_final, "lasso_perfect_rating_score_group001.csv", row.names = FALSE)
#-------------------------------------------------------------------------------------------------------------------------------------------------------
