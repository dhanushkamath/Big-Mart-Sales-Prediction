# Loading the data
train <- read.csv("data/train.csv",header=TRUE)
test  <- read.csv("data/test.csv",header=TRUE)

#combine both the data sets
test.Item_Outlet_Sales <- data.frame(Item_Outlet_Sales=rep(NA,nrow(test)),test[ ,])
data.combined <- rbind(train,test.Item_Outlet_Sales)

n_train <- nrow(train) # The number of training examples.
# understanding the variables
str(data.combined)

#========================================================================================================
# DATA CLEANSING

##################################3
# MISSING VALUE RECTIFICATION
# installing package MICE for treating missing values

library(mice)
library(VIM)


str(data.combined)
summary(data.combined)


# Segregating the Categorical and Continuous variables into 2 different DFs
is.fact <- sapply(data.combined, is.factor)
is.fact <- colnames(data.combined)[is.fact]
data.fact <- data.combined[,is.fact]
data.num <- data.combined[,!colnames(data.combined) %in% is.fact]


# Checking for the number of missing values on the entire dataset. (Convert below code to function)
na_count <- t(data.frame(sapply(data.combined, function(y) sum(length(which(is.na(y)))))))
rownames(na_count)<-NULL
na_count[,-12]
barplot(na_count[,-12]) # Removing the 12th column which is the target variable.
# Only the Item_Weight has missing values


# Segregating rows with missing values from the rest.
missing_data <- data.combined[is.na(data.combined$Item_Weight),]
table(missing_data$Outlet_Establishment_Year)
# This indicates that all products in the year 1985 do not have weights


# Drilling down on the Item Identifier.
# Let's check the weights of products of a particular ID in the complete dataset.
data.combined$Item_Weight[which(data.combined$Item_Identifier=='FDW12')]
data.combined$Item_Weight[which(data.combined$Item_Identifier=='DRI11')]
# This shows that the item weight corresponding to a particular item ID remains the same.


# So the imputation is now easier. Imputing the missing values with the common weight for a particular item ID.
for(i in which(is.na(data.combined$Item_Weight)))
{
  data.combined$Item_Weight[i]=mean(data.combined$Item_Weight[which(data.combined$Item_Identifier==data.combined$Item_Identifier[i])],na.rm = T)
}
data.combined$Item_Weight[which(data.combined$Item_Identifier=='DRH49')]
data.combined$Item_Weight[which(data.combined$Item_Identifier=='DRI11')] # Just checking
# The imputation is now successful
# There are no more missing data points in the dataframe.

table(is.na(data.combined[,-12])) # 12th column is the target variable.
summary(data.combined)



#####################
# OUTLIER TREATMENT
library(ggplot2)
library(stringr)

# Checking the boxplots of each Item Type for checking outliers. (Anything beyond the range
# of 1.5*IQR can be considered an outlier.)
ggplot(train, aes(x=Item_Type,y=Item_Outlet_Sales))+
  geom_boxplot()+xlab("Item Type")+ylab("Item Outlet Sales")
# it is seen that there are many significant outliers in the OutletSales which is the target.

# Checking for Outliers in
ggplot(train, aes(x=Item_Type, y=Item_Weight))+
  geom_boxplot()+xlab("Item Type")+ylab("Item Weight")
boxplot(data.combined$Item_Weight)

# No outliers in Item Weight
ggplot(train, aes(x=Item_Type, y=Item_Visibility))+
  geom_boxplot()+xlab("Item Type")+ylab("Item Visiblity")
boxplot(data.combined$Item_Visibility)

# It is seen that low fat and regular are spelled differently in the dataset
table(data.combined$Item_Fat_Content)
data.combined2 <- data.combined
data.combined$Item_Fat_Content[which(data.combined$Item_Fat_Content=="low fat" | data.combined$Item_Fat_Content=="Low Fat")] <- "LF"
data.combined$Item_Fat_Content[which(data.combined$Item_Fat_Content=="reg" | data.combined$Item_Fat_Content=="Regular")] <- "reg"
data.combined$Item_Fat_Content <- as.factor(as.character(data.combined$Item_Fat_Content))


# Splitting item identifiers into Char and Num
data.combined$Item_Id_Char <-(str_extract(data.combined$Item_Identifier, "[aA-zZ]+"))
data.combined$Item_Id_Char <- as.factor(data.combined$Item_Id_Char)

length(unique(data.combined$Item_Id_Char))
sort(unique(data.combined$Item_Id_Char))
# It is observed that there are 3 main categories - DR, FD and NC
# the 3rd character in these categories varies from A to Z (except in DR)

# Checking similarities of the first identifier DRA12. 
DRA12 <- data.combined[data.combined$Item_Identifier=='DRA12',]
# DRA12 corresponds to the same product, hence MRP is almost the same. Weight, Item Type and
# Item Fat content is the same. However, it is seen that there are missing values in outlet size too
# These missing values are represented using "" (empty string) and not NA, hence they went unnoticed.
# Let's segregate the missing ones from the main dataset.

################################################
# RETURNING TO MISSING VALUE RECTIFICATION
miss_outlet_size <- data.combined[which(data.combined$Outlet_Size==""),]
table(miss_outlet_size$Outlet_Identifier)
# There are 3 outlets with missing outlet sizes :- OUT010, OUT017, OUT045
table(data.combined$Outlet_Size[which(data.combined$Outlet_Identifier=='OUT010')])
table(data.combined$Outlet_Size[which(data.combined$Outlet_Identifier=='OUT017')])
table(data.combined$Outlet_Size[which(data.combined$Outlet_Identifier=='OUT045')])
# all records in the dataset with these outlet identifiers have missing outlet sizes.

# Observing the miss_outlet_size dataframe, there are many grocery stores.
# Checking the outlet sizes of grocery stores
table(data.combined$Outlet_Size[which(data.combined$Outlet_Type=="Grocery Store")])
# All grocery stores in the dataset are small. So it can be safely assumed that all missing outlet sizes
# for grocery stores can be taken as small.
data.combined2 <- data.combined
data.combined$Outlet_Size[which(data.combined$Outlet_Type=="Grocery Store")] <- "Small"

miss_outlet_size <- data.combined[which(data.combined$Outlet_Size==""),] # Refreshing the segregated missing values
table(miss_outlet_size$Outlet_Type)
# Only Supermarket Type1 is left.
# checking for Supermarket Type1
table(data.combined$Outlet_Size[which(data.combined$Outlet_Type=="Supermarket Type1")])
# Supermarket Type1 has DIFFERENT OUTLET SIZES. So, DIRECT IMPUTATION IS NOT POSSIBLE.

# Let's check the next variable in the miss_outlet_size
table(miss_outlet_size$Outlet_Location_Type)
# All of the entries belong to 'Tier 2'
# Checking Outlet Sizes for Tier2 and Supermarket Type1 in main dataset
table(data.combined$Outlet_Size[which(data.combined$Outlet_Location_Type=="Tier 2" & data.combined$Outlet_Type=="Supermarket Type1")])
# All of them are Small.
# So it can be safely assumed that all missing outlet sizes for Tier 2 and
# Supermarket Type1 can be taken as small.
data.combined$Outlet_Size[which(data.combined$Outlet_Location_Type=="Tier 2" & data.combined$Outlet_Type=="Supermarket Type1")] <- "Small"

miss_outlet_size <- data.combined[which(data.combined$Outlet_Size==""),] # Refreshing the segregated missing values
table(miss_outlet_size$Outlet_Type)
data.combined$Outlet_Size<-as.factor(as.character(data.combined$Outlet_Size))
# There are no more missing values for outlet_size

str(data.combined)
# Checking Outliers in categorical variables. (Each Outlier Identifier should only 
# have one particular value of each categorical variable)
table(data.combined$Outlet_Identifier, data.combined$Outlet_Location_Type) # No outliers here
table(data.combined$Outlet_Identifier, data.combined$Outlet_Type)
table(data.combined$Outlet_Identifier, data.combined$Outlet_Size)

# Let's see the distribution of outlet size and outlet type
table(data.combined$Outlet_Size, data.combined$Outlet_Type)
# It is observed that Grocery stores are always small, supermarket type 2 is always medium,
# supermarket type3 is always medium, supermarket type1 can be of any size.
# As it is pretty sparse, lets combine both the dimensions
data.combined$Outlet_Type2<- "NA"
data.combined$Outlet_Type2[which(data.combined$Outlet_Type=='Grocery Store')] <-"GS"
data.combined$Outlet_Type2[which(data.combined$Outlet_Type=='Supermarket Type2')] <-"SMT2"
data.combined$Outlet_Type2[which(data.combined$Outlet_Type=='Supermarket Type3')] <-"SMT3"
data.combined$Outlet_Type2[which(data.combined$Outlet_Type=='Supermarket Type1' & data.combined$Outlet_Size=='High' )] <-"SMT1_H"
data.combined$Outlet_Type2[which(data.combined$Outlet_Type=='Supermarket Type1' & data.combined$Outlet_Size=='Medium' )] <-"SMT1_M"
data.combined$Outlet_Type2[which(data.combined$Outlet_Type=='Supermarket Type1' & data.combined$Outlet_Size=='Small' )] <-"SMT1_S"
data.combined$Outlet_Type2 <- as.factor(data.combined$Outlet_Type2)
table(data.combined$Outlet_Type2)

table(data.combined$Outlet_Establishment_Year)

# Drilling down on one particular item ID, say DRA12
DRA12 <- data.combined[data.combined$Item_Identifier=='DRA12',]
# it's observed that one particular entry of OUT045 has zero visibility, but has generated a considerable
# amount of sale.
# Let's compare visibilities of products in OUT045
summary(data.combined$Item_Visibility[which(data.combined$Outlet_Identifier=='OUT045')])
data.combined$Item_Visibility[which(data.combined$Outlet_Identifier=='OUT045')]
# No anomalies can be observed in visibilities of products in OUT045 

# Let's segregate all products with zero visibilities
zero_vis <- data.combined[which(data.combined$Item_Visibility==0),]
table(zero_vis$Outlet_Identifier)
# Can't observe any trend in the dataframe.

# Let's now climb up one step and segregate items based on the first 3 letters of the 
# item_identifier (like DRA rather than using DRA12)
DRA <- data.combined[which(data.combined$Item_Id_Char=="DRA"),]

# Lets ignore the 3rd character too in char ID
data.combined$Item_Id_Char2<-(str_extract(data.combined$Item_Identifier, "[aA-zZ][aA-zZ]"))
data.combined$Item_Id_Char2<-as.factor(data.combined$Item_Id_Char2)
str(data.combined)

# Lets check DR
DR <- data.combined[which(data.combined$Item_Id_Char2=="FD"),]
table(DR$Item_Type)

# Lets change the year variable so that it's usable in model building
data.combined$Outlet_Establishment_Year<-data.combined$Outlet_Establishment_Year+1999
str(data.combined)

# A better package for describing the summary of continuous variables.
# install.packages("pastecs")
library(pastecs)
options(scipen=100)
options(scipen=2)

stat.desc(data.combined)

#=======================================================================================================
# DATA VISUALIZATION
library(ggplot2)
str(train)

str(data.combined)

# Item_Type is a categorical with 16 possible values. Let's first dive into that
prop.table(table(data.combined$Item_Type))*100

# Plotting a distribution of Item Type
ggplot(train, aes(x=Item_Type))+
  geom_histogram(binwidth=0.3,stat = "count")+xlab("Item Type")+ylab("Total Count")+
  labs(fill="Item Type")

# Plotting a histogram of Item Type filled with Fat Content
ggplot(train, aes(x=Item_Type, fill=Item_Fat_Content))+
  geom_histogram(binwidth=0.3,stat = "count")+xlab("Item Type")+ylab("Total Count")+
  labs(fill="Item Type")
# Regular and low fat are almost 50-50, not all products have all Item Types


# Scatter plot between MRP and Item Sales colored according to the item type
ggplot(train, aes(x=Item_Type,y=Item_Outlet_Sales,color = Outlet_Type))+
  geom_jitter(alpha = 0.6)+xlab("Item Type")+ylab("Item Outlet Sales")+
  labs(fill="Outlet Type")
# It is seen that the grocery stores lie at the bottom indicating lesser overall sales.
# Not all different Item types have all categories of fat types.

# Check the proportion table of Outlet_type
prop.table(table(data.combined$Outlet_Type))*100
# 65 percent are supermarket type1, the rest is almost equally divided. 


# Does item visibility affect sales?
ggplot(train, aes(x=Item_Visibility,y=Item_Outlet_Sales,color = Outlet_Type))+
  geom_jitter()+xlab("Item Visibility")+ylab("Item Outlet Sales")+
  labs(fill="Outlet Type")

# No apparent relation as it all looks random




#========================================================================================
# Building a model
library(h2o)

train_h2o<-data.combined[1:nrow(train),]
test_h2o<-data.combined[-(1:nrow(train)),]

localH2O<-h2o.init(nthreads = 6)

train_h2o<-as.h2o(train_h2o)
test_h2o<-as.h2o(test_h2o)

colnames(train_h2o)

# Highest XGB till now
system.time( 
  xgb1 <- h2o.xgboost(x= c(2,3,4,6,10,14,15),y = 12,
                                       training_frame = train_h2o,
                                       ntrees = 30,
                                       max_depth = 5,
                                       seed = 1122
  )
)


system.time( 
  gbm1 <- h2o.gbm(x= c(2,3,4,6,10,14,15),y = 12,
                      training_frame = train_h2o,
                      ntrees = 50,
                      max_depth = 3,
                      seed = 1122
  )
)



system.time( 
  gbm2 <- h2o.gbm(x= c(1,2,3,4,6,7,8,10,14),y = 12,
                  training_frame = train_h2o,
                  ntrees = 50,
                  max_depth = 3,
                  seed = 1122
  )
)


system.time( 
  gbm2 <- h2o.gbm(x= c(1,7,6,15),y = 12,
                  training_frame = train_h2o,
                  ntrees = 50,
                  max_depth = 3,
                  seed = 1122
  )
)


system.time( 
  gbm3 <- h2o.gbm(x= c(1,7,6,15,14),y = 12,
                  training_frame = train_h2o,
                  ntrees = 55,
                  max_depth = 3,
                  seed = 1122
  )
)
# Highest GBM till now (current highest)
system.time( 
  gbm5 <- h2o.gbm(x= c(1,4,7,6,8,14),y = 12,
                  training_frame = train_h2o,
                  ntrees = 64,
                  max_depth = 3,
                  seed = 1122
  )
)


system.time( 
  gbm6 <- h2o.gbm(x= c(1,4,6,7,8,14),y = 12,
                  training_frame = train_h2o,
                  ntrees = 64,
                  max_depth = 3,
                  seed = 1122
  )
)



colnames(train_h2o)
h2o.varimp_plot(gbm6)


predict.xgb1 <- as.data.frame(h2o.predict(gbm6, test_h2o))
submission <- data.frame(Item_Identifier = test$Item_Identifier, Outlet_Identifier = test$Outlet_Identifier, Item_Outlet_Sales= predict.xgb1)
setnames(submission, "predict","Item_Outlet_Sales")
write.csv(submission, file = "Submission_1_Mar16_0126.csv", row.names =  F)


grid <- h2o.grid("xgboost", x= c(2,3,4,6,10,14,15), y = 12, training_frame = train_h2o,
                 hyper_params = list(ntrees = c(40,45,50),max_depth = c(2,3,4)))
summary(grid)
model_ids <- grid@model_ids
models <- lapply(model_ids, function(id) { h2o.getModel(id)})