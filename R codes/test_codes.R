library(Amelia)
library(dplyr)

#library(sqldf)
master_data <- read.csv("/Users/n2n/RWorkingDirectory/WQD7004_group_project_git/iprop_master_dataframe.csv")

#1. remove null & apartment
master_data <- master_data %>% filter(propertyName != 'NULL')
master_data <- master_data %>% filter(!(propertyType == 'Apartment'))

#2. Data conversion
master_data$lat <- as.numeric(master_data$lat, options(digits = 6))
master_data$lng <- as.numeric(master_data$lng,options(digits = 6))
master_data$bathrooms <-as.numeric(master_data$bathrooms)
master_data$builtUp <-as.numeric(gsub(",","",master_data$builtUp))
master_data$carParks <-as.numeric(master_data$carParks)
master_data$postedDate <-as.Date(master_data$postedDate)
master_data$pricePerSizeUnitBuiltUp<-as.numeric(master_data$pricePerSizeUnitBuiltUp)
master_data$buildingId <-as.numeric(master_data$buildingId)

#3. impute missing values
master_data_imputed <- master_data %>%  
  group_by(district) %>%
  mutate(builtUp = ifelse(is.na(builtUp),mean(builtUp,na.rm=TRUE),builtUp),
         carParks = ifelse(is.na(carParks),round(mean(carParks,na.rm=TRUE),digits = 0),carParks),
         bathrooms = ifelse(is.na(bathrooms),round(mean(bathrooms,na.rm=TRUE),digits = 0),bathrooms),
         lat = ifelse(is.na(lat),round(mean(lat,na.rm=TRUE),digits = 0),lat),
         lng = ifelse(is.na(lng),round(mean(lng,na.rm=TRUE),digits = 0),lng),
         pricePerSizeUnitBuiltUp = ifelse(is.na(pricePerSizeUnitBuiltUp),round(mean(pricePerSizeUnitBuiltUp,na.rm=TRUE),digits = 0),pricePerSizeUnitBuiltUp))

#4. remove outliers
lower_bound <- quantile(master_data_imputed$rental, 0.025)
upper_bound <- quantile(master_data_imputed$rental, 0.975)

outlier_ind <- master_data_imputed %>%
  filter(rental < quantile(master_data_imputed$rental, 0.025) | rental > quantile(master_data_imputed$rental, 0.975))

master_data_clean <-  master_data_imputed %>%
  filter(rental <= quantile(master_data$rental, 0.975) & rental >= quantile(master_data$rental, 0.025))

#replace outliers in carPark column
master_data_clean[master_data_clean$carParks > 5, "carParks"] <- 1

#5. standardize furnishing types
master_data_clean <- master_data_clean %>%
  mutate(furnishing = ifelse(furnishing == "Fully furnished" | furnishing == "Fully Furnished", "Fully Furnished",
                             ifelse(furnishing == "Partly furnished" | furnishing == "Partly Furnished", "Partly Furnished",
                                    ifelse(furnishing == "NULL", "Unknown",
                                           furnishing))))

dim(master_data_clean)

summary(master_data_clean)

unique(master_data_clean$bedrooms)
unique(master_data_clean$furnishing)

#We noticed that there is one record of propertype Apartment, since our study only interested in Condo and Serviced Apartment, we are going to remove this record

master_data_clean <- master_data_clean %>%
  mutate(furnishing=factor(furnishing)) %>%
  mutate(bedrooms=factor(bedrooms)) %>%
  mutate(propertyType=factor(propertyType)) %>%
  mutate(unitType=factor(unitType)) %>%
  mutate(district=factor(district)) 

dim(master_data_clean)

summary(master_data_clean)

#EDA
## Top 10 most expensive

# group_by_district <- aggregate(list(master_data_clean$rental), list(master_data_clean$district), median)
# colnames(group_by_district) <- c("District","Avg_rental_price_by_District")
# group_by_district <- group_by_district[order(group_by_district$Avg_rental_price_by_District, decreasing = TRUE),]
# 
# #reset the row index
# rownames(group_by_district) <- 1:24
# 
# top_10_district <- head(group_by_district, 10)
# last_10_districts <- tail(group_by_district, 10)
# 
# 
# #plot the graph: top 10
# options(repr.plot.width=15, repr.plot.height=11)
# plot1 <- ggplot(data = top_10_district, mapping = aes(x = reorder(District, Avg_rental_price_by_District), y = Avg_rental_price_by_District)) +
#   geom_bar(stat = "identity", alpha = .8, size = 1.5, fill = "navyblue") +
#   labs(x = "District", y = "Average monthly rental price in (Ringgit Malaysia)") +
#   coord_flip() +
#   geom_label(mapping = aes(label = round(Avg_rental_price_by_District, 1)), size = 4, fill = "#F5FFFA", fontface = "bold") + ggtitle("Top 10 most expensive areas in Kuala Lumpur")
# plot1
# 
# #plot the last 10
# options(repr.plot.width=15, repr.plot.height=11)
# plot2 <- ggplot(data = last_10_districts, mapping = aes(x = reorder(District, -Avg_rental_price_by_District), y = Avg_rental_price_by_District)) +
#   geom_bar(stat = "identity", alpha = .8, size = 1.5, fill = "#FF6666") +
#   labs(x = "District", y = "Average monthly rental price in (Ringgit Malaysia)") +
#   coord_flip() +
#   geom_label(mapping = aes(label = round(Avg_rental_price_by_District, 1)), size = 4, fill = "#F5FFFA", fontface = "bold") + ggtitle("Top 10 most affordable areas in Kuala Lumpur")
# plot2
# 
# names(master_data_clean)
# unique(master_data_clean$bedrooms)
# unique(master_data_clean$bathrooms)
# unique(master_data_clean$carParks)
# unique(master_data_clean$bathrooms)

# # min, max, median
# summary_df <- master_data_clean %>% 
#   filter(pricePerSizeUnitBuiltUp < 10) %>%
#   group_by(district) %>%
#   summarize(Min = min(pricePerSizeUnitBuiltUp),
#     Avg = median(pricePerSizeUnitBuiltUp), 
#             Max = max(pricePerSizeUnitBuiltUp)) 
# summary_df <- summary_df %>% arrange(Max)
# 
# #the district name is too long, so I'm going to truncate it
# summary_df[17,1] <- 'Kampung Kerinchi'
# 
# # make district an ordered factor
# summary_df$district <- factor(summary_df$district, levels = summary_df$district)
# 
# p1 <- ggplot(summary_df, mapping = aes(district, Avg, group = 1)) +
#   geom_ribbon(aes(ymin=Min,ymax=Max),color="yellow",fill = "yellow",alpha=0.5) + 
#   geom_line(color="#69b3a2") +
#   labs(x = "District", y = "Price per built up sq. ft (RM)", 
#        title = "Average price per built up size (with max and min price)") +
#   theme(axis.text.x=element_text(angle = 90, hjust = 0))
#   
# 
# #donut chart for built-up size
# library(ggrepel)
# count_by_condo_size = function(min, max){
#   count <- master_data_clean %>% filter(builtUp > min & builtUp <=max) %>% nrow()
#   count
# }
# 
# df_builtup_summary <- data.frame(
#   category=c("<=600 sq.ft.", "600-900 sq.ft", "900-1200 sq.ft", "1200-1500 sq.ft", "1500-1800 sq.ft", ">1800 sq.ft"),
#   count=c(count_by_condo_size(0, 600), 
#           count_by_condo_size(600, 900), 
#           count_by_condo_size(900, 1200), 
#           count_by_condo_size(1200, 1500), 
#           count_by_condo_size(1500, 1800), 
#           count_by_condo_size(1800, max(master_data_clean$builtUp)))
# )
# 
# df_builtup_summary$percentage = df_builtup_summary$count / sum(df_builtup_summary$count)* 100
# df_builtup_summary$ymax = cumsum(df_builtup_summary$percentage)
# df_builtup_summary$ymin = c(0, head(df_builtup_summary$ymax, n = -1))
# 
# donut_plot = ggplot(df_builtup_summary, aes(fill = category, ymax = ymax, ymin = ymin, xmax = 100, xmin = 80)) +
#   geom_rect(colour = "black") +
#   coord_polar(theta = "y") + 
#   xlim(c(0, 100)) +
#   geom_label_repel(aes(label = paste(round(percentage,2),"%"), x = 100, y = (ymin + ymax)/2),inherit.aes = F, show.legend = F, size = 5)+
#   theme(legend.title = element_text(colour = "black", size = 10, face = "bold"), 
#         legend.text = element_text(colour = "black", size = 10), 
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank()) +
#   annotate("text", x = 0, y = 0, size = 10, label = "Built-up Size")
# donut_plot
# 

dim(master_data_clean)
summary(master_data_clean)
str(master_data_clean)

unique(master_data_clean$bedrooms)

library(Amelia)
library(plyr)
library(dplyr)
library(plotly)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(selectr)
library(xml2)
library(rvest)
library(jsonlite)
library(knitr)
library(kableExtra)
library(PerformanceAnalytics)
library(caret)
library(rmarkdown)

master_data_clean <- master_data_clean %>% 
  select(propertyName, rental, pricePerSizeUnitBuiltUp, district, lat, lng, bedrooms, bathrooms, builtUp, carParks, furnishing, buildingId, unitType, propertyType)


dim(master_data_clean)
summary(master_data_clean)

#convert Factors to numeric first
master_data_clean_cor <- master_data_clean[2:14]
master_data_clean_cor <- sapply(master_data_clean_cor, unclass) 
correlations <- cor(master_data_clean_cor)
print(correlations)
#corrplot(correlations, type="upper", order="hclust")


# ==========CORRELATION===============

cor.test(master_data_clean$rental,master_data_clean$bathrooms,method = "pearson")
cor.test(master_data_clean$rental,master_data_clean$bathrooms,method = "kendall")

cor.test(master_data_clean$rental,master_data_clean$pricePerSizeUnitBuiltUp,method = "pearson")
cor.test(master_data_clean$rental,master_data_clean$pricePerSizeUnitBuiltUp,method = "kendall")

cor.test(master_data_clean$rental,master_data_clean$carParks,method = "pearson")
cor.test(master_data_clean$rental,master_data_clean$carParks,method = "kendall")

library(randomForest)

#determine price2
install.packages("ggrepel")


# === Random Forest ======
# median_rental <- median(master_data_clean$rental)
# master_data_clean$rental_class <- ifelse(master_data_clean$rental > median_rental, 1, 0)
# 
# 
# master_data_analysis <- master_data_clean[3:15]
# master_data_analysis$rental_class <- as.character(master_data_analysis$rental_class)
# master_data_analysis$rental_class <- as.factor(master_data_analysis$rental_class)
# rf_model <- randomForest(rental_class ~ ., data=master_data_analysis, proximity=TRUE)
# varImpPlot(rf_model)
# rf_model

#====== Linear regression =====
library(Hmisc)
res2 <- rcorr(as.matrix(master_data_clean_cor))

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res2<-rcorr(as.matrix(master_data_clean_cor))
flattenCorrMatrix(res2$r, res2$P)

# ============

# Filter master_data_clean with Batu Caves, Bukit Jalil, Bukit Bintang only 
new_dataset1 <- master_data_clean %>% 
  filter(district == 'Batu Caves'|district == 'Bukit Jalil'|district == 'Bukit Bintang')

# View only rental_price and prizePerSizeUnitBuildUp at these 3 districts.
data1 <- sqldf("select [rental_price],[pricePerSizeUnitBuiltUp], district from master_data_clean where district in ('Batu Caves','Bukit Jalil','Bukit Bintang')")

data1.sorted <- sqldf("SELECT * FROM data1 ORDER BY rental_price ASC, pricePerSizeUnitBuiltUp DESC")

#Boxplot for data1.sorted
#ggplot(data1.sorted,aes(x=district,y=rental_price,fill=district))+geom_boxplot()

#==========================================

#dataset1
new_dataset1 <- master_data_clean %>% 
  filter(district == 'Batu Caves'|district == 'Bukit Jalil'|district == 'Bukit Bintang')
dataset1_corb <- cor.test(new_dataset1$rental,new_dataset1$bathrooms,method = "pearson")
dataset1_corp <- cor.test(new_dataset1$rental,new_dataset1$pricePerSizeUnitBuiltUp,method = "pearson")
sprintf("Correlation coefficient of rental vs bathrooms: %f", dataset1_corb$estimate)
sprintf("Correlation coefficient of rental vs pricePerSizeUnitBuiltUp: %f", dataset1_corp$estimate)
new_dataset1 <- new_dataset1 %>% select(rental, pricePerSizeUnitBuiltUp, bathrooms)
chart.Correlation(new_dataset1[,c(2,3,4)],histogram=TRUE,pch="19")

#dataset2
new_dataset2 <- master_data_clean %>% 
  filter(district == 'Batu Caves'|district == 'Cheras'|district == 'Kuchai Lama'|district == 'Sri Hartamas'|district == 'Sri Petaling')
dataset2_corb <- cor.test(new_dataset2$rental,new_dataset2$bathrooms,method = "pearson")
dataset2_corp <- cor.test(new_dataset2$rental,new_dataset2$pricePerSizeUnitBuiltUp,method = "pearson")
sprintf("Correlation coefficient of rental vs bathrooms: %f", dataset2_corb$estimate)
sprintf("Correlation coefficient of rental vs pricePerSizeUnitBuiltUp: %f", dataset2_corp$estimate)
new_dataset2 <- new_dataset2 %>% select(rental, pricePerSizeUnitBuiltUp, bathrooms)
chart.Correlation(new_dataset2[,c(2,3,4)],histogram=TRUE,pch="19")

#dataset3
new_dataset3 <- master_data_clean %>% 
  filter(district == 'Sri Hartamas'| district == 'Bangsar'|district == 'Mont Kiara')
dataset3_corb <- cor.test(new_dataset3$rental,new_dataset3$bathrooms,method = "pearson")
dataset3_corp <- cor.test(new_dataset3$rental,new_dataset3$pricePerSizeUnitBuiltUp,method = "pearson")
sprintf("Correlation coefficient of rental vs bathrooms: %f", dataset3_corb$estimate)
sprintf("Correlation coefficient of rental vs pricePerSizeUnitBuiltUp: %f", dataset3_corp$estimate)
new_dataset3 <- new_dataset3 %>% select(rental, pricePerSizeUnitBuiltUp, bathrooms)
chart.Correlation(new_dataset3[,c(2,3,4)],histogram=TRUE,pch="19")

#dataset4
new_dataset4 <- master_data_clean %>% 
  filter(district == 'Batu Caves'|district == 'Cheras'|district == 'Kuchai Lama'|district == 'Sri Hartamas'|district == 'Sri Petaling')
dataset4_corb <- cor.test(new_dataset4$rental,new_dataset4$bathrooms,method = "pearson")
dataset4_corp <- cor.test(new_dataset4$rental,new_dataset4$pricePerSizeUnitBuiltUp,method = "pearson")
sprintf("Correlation coefficient of rental vs bathrooms: %f", dataset4_corb$estimate)
sprintf("Correlation coefficient of rental vs pricePerSizeUnitBuiltUp: %f", dataset4_corp$estimate)
new_dataset4 <- new_dataset4 %>% select(rental, pricePerSizeUnitBuiltUp, bathrooms)
chart.Correlation(new_dataset4[,c(2,3,4)],histogram=TRUE,pch="19")

#combine dataset1 and dataset3
new_dataset <- rbind(new_dataset1,new_dataset3)
dim(new_dataset)

#training set and test set
library(caret)
set.seed(123)
split = 0.70 #define an 70%/30% training/testing split of the dataset.
training_set_index <- createDataPartition(new_dataset$district,p=split,list=FALSE)
trainingset <- new_dataset[training_set_index,]
testset <- new_dataset[-training_set_index,]
dim(trainingset)
dim(testset)

#Model1
linear_model_1_predictor<-lm(rental~bathrooms,data=trainingset)
summary(linear_model_1_predictor)
par(mfrow=c(2,2))
plot(linear_model_1_predictor)

#Model2
linear_model_2_predictor<-lm(rental~pricePerSizeUnitBuiltUp+bathrooms,data=trainingset)
summary(linear_model_2_predictor)
par(mfrow=c(2,2))
plot(linear_model_2_predictor)

#Perform predictions on test set with the models created above
#Predictor1
prediction_model1 <- predict(linear_model_1_predictor, testset)

#Predictor2
prediction_model2 <- predict(linear_model_2_predictor, testset)

#plots
par(mfrow=c(2,2))

plot(testset$rental,type="l",lty=1.8,col="red")
lines(prediction_model1,type="l",col="blue")
plot(prediction_model1,type="l",lty=1.8,col="blue")

plot(testset$rental,type="l",lty=1.8,col="red")
lines(prediction_model2,type="l",col="blue")
plot(prediction_model2,type="l",lty=1.8,col="blue")

#comparison

plot_predictions <- function(linear_model_predictor){
  pred = predict(linear_model_predictor,testset)
  act = testset$rental
  tst = data.frame(district = testset$district,prediction = pred,actual = act)
  tst = mutate(tst,delta = prediction - actual)
  tstvar = round(var(tst$delta))
  plot <- ggplot(tst,aes(x=actual,y=prediction,color=district)) + geom_point()  + geom_abline(intercept = 0, slope = 1, linetype="dashed")
  plot
}

p1 <- plot_predictions(linear_model_1_predictor)
p2 <- plot_predictions(linear_model_2_predictor)
grid.arrange(p1, p2, ncol=2)


plot_predictions <- function(linear_model_predictor, dataset){
  pred = predict(linear_model_predictor,dataset)
  act = dataset$rental
  tst = data.frame(district = dataset$district,prediction = pred,actual = act)
  tst = mutate(tst,delta = prediction - actual)
  tstvar = round(var(tst$delta))
  plot <- ggplot(tst,aes(x=actual,y=prediction,color=district)) + geom_point()  + geom_abline(intercept = 0, slope = 1, linetype="dashed")
  plot
}

p1 <- plot_predictions(linear_model_2_predictor, trainingset)
p2 <- plot_predictions(linear_model_2_predictor, testset)
grid.arrange(p1, p2, ncol=2)

#calculate model accuracy
calculate_accuracy_trainset <- function(linear_model, dataset){
  # Fitted values of train data
  pred_1_train = linear_model$fitted.values
  
  # Store actual price and predicted price in a data frame
  actual_fitted = data.frame(actual=dataset$rental,predicted=pred_1_train)
  
  # Mean of absolute difference between predicted values and actual values
  abs_diff = mean(abs(actual_fitted$actual-actual_fitted$predicted)/actual_fitted$actual)
  
  # % accuracy of the prediction with trainigset1
  accuracy_train = 1 - abs_diff
  
  accuracy_train
}

calculate_accuracy_testset <- function(linear_model, dataset){
  pred_1_test=predict(linear_model,dataset)
  
  # Store actual price and predicted price in a data frame
  actual_fitted_test=data.frame(actual=dataset$rental, predicted=pred_1_test)
  
  # Mean of absolute difference between predicted values and actual values
  abs_diff_test = mean(abs(actual_fitted_test$actual-actual_fitted_test$predicted)/actual_fitted_test$actual)
  
  # % accuracy of the prediction with testset1
  accuracy_test = 1-abs_diff_test
  accuracy_test
}

m1_train <- calculate_accuracy_trainset(linear_model_1_predictor, trainingset)
m1_test <- calculate_accuracy_testset(linear_model_1_predictor, testset)
m2_train <- calculate_accuracy_trainset(linear_model_2_predictor, trainingset)
m2_test <- calculate_accuracy_testset(linear_model_2_predictor, testset)

result <- (c("model", "model 1 predictor", "model 2 predictor",
             "train set", percent(m1_train), percent(m1_test), 
             "test set", percent(m2_train), percent(m2_test)))
accuracy_matrix <- matrix(result, nrow=3, ncol = 3)

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

percent(m2_train)

#Kendall Tau
#We will compute the kendall’s tau correlations for the predictions of the two prediction models above, to check how the rankings of the predicted values correlate with the actual target values in the test set.
kt_for_model1_predictor <- cor(prediction_model1,testset$rental,method = "kendall")
kt_for_model2_predictor <- cor(prediction_model2,testset$rental,method = "kendall")
sprintf("Kendalls tau for model with one predictor: %f", kt_for_model1_predictor)
sprintf("Kendalls tau for model with two predictors: %f", kt_for_model2_predictor)

# significance
dataframe_prediction_one_predictor<-data.frame(prediction_one_predictor=double())
dataframe_prediction_two_predictor<-data.frame(prediction_two_predictor=double())
for (i in 1:50){
  
  linear_model_1_predictor<-lm(rental~bathrooms,data=trainingset)
  linear_model_2_predictor<-lm(rental~pricePerSizeUnitBuiltUp+bathrooms,data=trainingset)
  prediction_for_model_with_1_predictor<-predict(linear_model_1_predictor,testset)
  prediction_for_model_with_2_predictors<-predict(linear_model_2_predictor,testset)
  #Compute the kendall’s tau correlations for the predictions of the models.
  
  newRow_1_predictor<-data.frame(prediction_one_predictor=cor(prediction_for_model_with_1_predictor,testset$rental,method = "kendall"))
  dataframe_prediction_one_predictor<-rbind(dataframe_prediction_one_predictor,newRow_1_predictor)
  newRow_2_predictor<-data.frame(prediction_two_predictor=cor(prediction_for_model_with_2_predictors,testset$rental,method = "kendall"))
  dataframe_prediction_two_predictor<-rbind(dataframe_prediction_two_predictor,newRow_2_predictor)
}

qqnorm(dataframe_prediction_two_predictor$prediction_two_predictor, pch = 1, frame = FALSE)
qqline(dataframe_prediction_two_predictor$prediction_two_predictor, col = "steelblue", lwd = 2)

wilcox.test(dataframe_prediction_one_predictor$prediction_one_predictor,dataframe_prediction_two_predictor$prediction_two_predictor,exact = FALSE)