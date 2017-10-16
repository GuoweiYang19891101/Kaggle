
## Titanic Project
## Author:Guowei Yang
## Date: 08/28/2017


#----------------------------------------------------------------------------------------------------
## Part 1. Libraries needed

#These packages are for SVM
require(e1071)
require(kernlab)

#These packages are for Decision Tree
require(gmodels)  
require(rattle)   #built under version 3.3.3
require(tree)
require(party)
require(rpart)

#These packages are for data manipulation
require(tibble)
require(dplyr)
require(stringr)

#These packages for data visualization
require(ggplot2)
require(ggthemes)
require(ggjoy)
require(ggforce)
require(scales)
require(grid)
require(gridExtra)
require(corrplot)

#These packages for feature selection
require(caret)

#Package for imputation
require(mice)
require(randomForest)

#----------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------------
## Part 2. Loading data
train <- read.csv("C:/Users/Guowei Yang/Desktop/Kaggle/Titanic/train.csv", header = T, na.strings = c("", " ", "NA"), stringsAsFactors = F)
dim(train)
#891 12
str(train)
# int(ID) int(Survived) int(Pclass) chr(Name) chr(Sex) num(Age) int(SibSp) int(Parch) chr(Ticket) num(Fare) chr(Cabin) chr(Embarked)

test <- read.csv("C:/Users/Guowei Yang/Desktop/Kaggle/Titanic/test.csv", header = T, na.strings = c("", " ", "NA"), stringsAsFactors = F)
dim(test)
#418 11
str(test)
# int(ID) int(Pclass) chr(Name) chr(Sex) num(Age) int(SibSp) int(Parch) chr(Ticket) num(Fare) chr(Cabin) chr(Embarked)

#-----------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------------------
## Part 3. Data initial exploration and visualization
### 3.1 Checking data quality for bad values (any weird value or missing value)

train <- train %>% mutate(
  Survived = factor(Survived),
  Pclass = factor(Pclass),
  Sex = factor(Sex),
  Embarked = factor(Embarked)
)

test <- test %>% mutate(
  Pclass = factor(Pclass),
  Sex = factor(Sex),
  Embarked = factor(Embarked)
)

summary(train)
summary(test)

# Then, I need to combine two data sets into 1 whole data set.

combine <- bind_rows(train, test)
summary(combine)
str(combine)
#1309 12


na_count <- rep(0,ncol(combine))
variable <- colnames(combine)

for(i in 1:ncol(combine)){
  na_count[i] <- sum(is.na(combine[,i]))
}

na_df <- data.frame(variable, na_count)
na_df

# 418 for Survived, 263 for Age, 1 for Fare, 1014 for Cabin, and 2 for Embarked.
# Here, NA values for Survived are the predictions I was going to make in the end.

# Pay attention to these missing values, I would try to impurate them later on.


### 3.2 Exploratory analysis and visualization

# Check the variable one by one 
#Note: character variables are difficult to be analyzed and visualized, so I just chose quantantative/factor variables here.



#Pclass
summary(combine$Pclass)
#There are in total 3 classes, 1 is best, and 3 is the worst. Majority of people live in class 3. (323, 277, 709 respectively in each class)
#Plot for relationship between pessenger classes and survival rate, the better you class is, the more likely you will survive
p_class_fill <- ggplot(combine[1:891,], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat = 'count', position = 'fill') +
  labs(x = 'Pclass') +
  theme(legend.position = "none")+
  theme_few()
p_class_fill

# p_class_dodge <- ggplot(combine[1:891,], aes(x = Pclass, fill = Survived)) +
#   geom_bar(stat = 'count', position = 'dodge') +
#   labs(x = 'Pclass') +
#   theme(legend.position = "none")+
#   theme_few()
# p_class_dodge


#Sex
summary(combine$Sex)
#There are only 2 sexes: female and male. (466 female, 843 male)
#Plot shows that if you are a female, you are more likely to survive.
p_sex_fill <- ggplot(combine[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar(stat = 'count', position  = 'fill') +
  labs(x = 'Sex') +
  theme(legend.position = "none") + 
  theme_few()
p_sex_fill

# p_sex_dodge <- ggplot(combine[1:891,], aes(x = Sex, fill = Survived)) +
#   geom_bar(stat = 'count', position = 'dodge') +
#   labs(x = 'Sex') +
#   theme(legend.position = "none")+
#   theme_few()
# p_sex_dodge


#Age
summary(combine$Age)
#Despite the NA values, the other values are in reasonable distribution except the age range from 5 to 15 (teenegers).
#Plot also shows us that survival rate among all age levels are quite uniformly distributed except for Age from 0 to 10.
# Still, I think I might miss some information because of the plot type I chose.

age_distri <- ggplot(combine, aes(x = Age)) +
  geom_density(fill = "blue") 
age_distri

p_age <- ggplot(combine[1:891,], aes(x = Age, fill = Survived)) +
  geom_histogram() +
  labs(x = 'Age') +
  #facet_grid(.~Sex) +
  theme_few()
p_age

p_age_fill <- ggplot(combine[1:891,], aes(x = Age, fill = Survived)) +
  geom_histogram(position = 'fill') +
  labs(x = 'Age') +
  #facet_grid(.~Sex) +
  theme_few()
p_age_fill



#SibSp
summary(combine$SibSp)
# The number of SibSp varied from 0 to 8, so the family size should also matter to determine if this person could survive.
# The plot shows us that family size of medium is the best compared to those family with too small or too big size.

p_sibsp_fill <- ggplot(combine[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar(stat = 'count', position = 'fill') +
  labs(x = 'SibSp') +
  scale_x_continuous(breaks = c(1:8)) +
  theme(legend.position = "none") +
  theme_few()
p_sibsp_fill


#Parch
summary(combine$Parch)
# The number of Parch varied from 0 to 9, so the family size should also matter to determine if this person could survive.
# The plot shows us that family size of medium is the best compared to those family with too small or too big size.

p_parch_fill <- ggplot(combine[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar(stat = 'count', position = 'fill') +
  labs(x = 'Parch') +
  scale_x_continuous(breaks = c(1:9)) +
  theme(legend.position = "none") +
  theme_few()
p_parch_fill
#Here, I got an interesting inspiration from other posts on Kaggle that variable SibSp and Parch could combine to count the total family size.
#This could go deeper in the following Feather Engineering section.

#Fare
summary(combine$Fare)
#The range of Fare varied from 0 to 512.3 dollars.
#The plot shows that fare is fat-tailed distributed.
#And, the survival rate for Fare above $50 is higher than that for Fare below $50.
#Money is still a good thing to have.

fare_distri <- ggplot(combine, aes(x = Fare)) +
  geom_density(fill = "blue") 
fare_distri
  
p_fare <- ggplot(combine[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(position = 'fill', binwidth = 40) +
  labs(x = 'Fare') +
  theme_few()
p_fare

#-------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------
#Part 4. Feature engineering and missing value imputation
### 4.1 Feature engineering



#I wanted to mine some deeper information based on current variables and try to construct new features that make model work better


# Check character variable one by one
#Character variables are likely to provide some hidden information.



# 1) Name 
#split name first into 3 parts
#Each name has three parts, I tried to split these three parts and get the full name and Title.

string <- combine$Name
head(string)

combine$last_name <- str_extract(string, '(.*,)')
combine$last_name <- gsub(',','',combine$last_name)
combine$first_name <- str_extract(string, '(\\..*)')
combine$first_name <- str_trim(gsub('\\.','',combine$first_name))
combine$title <- gsub('(.*,)|(\\..*)', '', string)

head(combine$first_name)
head(combine$last_name)
head(combine$title)

#Title VS. Survived
table(combine$title)
#combine$title <- as.factor(combine$title)

#All titles are here (based on research from Wikipedia and Google):
# 1) Capt IS a title for ranking in Army (Navy).
# 2) Col IS a title for ranking in Army (Armies). 
# 3) Don IS a title for men in Spanish for respect.
# 4) Dona IS a title for women in Spanish for respect.
# 5) Dr IS a title for academic degree, can be both female and male.
# 6) Jonkheer is a Dutch honorific ofo nobility, most likely to be male.
# 7) Lady is a title for women with a title of honorary.
# 8) Major IS a title for military rank of commisioned officer status.
# 9) Master is a title for young boys and yong men.
# 10) Miss is a title for unmarried women.
# 11) Mlle is a title for unmarried women in French.
# 12) Mme is a title for women equivalent to Mr/Sir.
# 13) Mr IS a title for married men.
# 14) Mrs IS a title for married women.
# 15) Ms IS a title foro women regardless of their marital status.
# 16) Rev IS a title for Christian clergy (men).
# 17) Sir is a title for men regardless of their matiral status.
# 18) the Countess IS a title for the wife of an Earl or Count.



# Next, I need to assign some of them new values based on my understanding of the titles because some of them are redundant.

combine$title <- gsub('Capt','Army', combine$title)
combine$title <- gsub('Col','Army', combine$title)
combine$title <- gsub('Major','Army', combine$title)
combine$title <- gsub('the Countess','Lady', combine$title)
combine$title <- gsub('Dona','Ms', combine$title)

combine$title <- gsub('Don','Sir', combine$title)
combine$title <- gsub('Jonkheer','Sir', combine$title)
combine$title <- gsub('Mlle','Miss', combine$title)
combine$title <- gsub('Mme','Ms', combine$title)
combine$title <- gsub('Rev','Religion', combine$title)

# combine$title[combine$title == 'Capt'] <- 'Army'
# combine$title[combine$title == 'Col'] <- 'Army'
# combine$title[combine$title == 'Major'] <- 'Army'
# combine$title[combine$title == 'Countess'] <- 'Lady'
# combine$title[combine$title == 'Don'] <- 'Sir'
# combine$title[combine$title == 'Dona'] <- 'Ms'
# combine$title[combine$title == 'Mlle'] <- 'Miss'
# combine$title[combine$title == 'Mme'] <- 'Ms'
# combine$title[combine$title == 'Rev'] <- 'Christian'

table(combine$title)

table(combine$Sex, combine$title)
table(combine$Survived, combine$title)

#From the table, I can learn that titles are closely related to sex mainly because titles are also given based on sex. 
#However, titles are also useful because it provided more information than just sex.


# 2) SibSp and Parch 
#Number of siblings, parents and children can add up to become total family size.

combine$FamilySize <- combine$SibSp + combine$Parch + 1
summary(combine$FamilySize)
#size varies from 1 to 11
#Plot for family size and survival rate shows that family size between 2 and 4 is the best for survive .


table(combine$FamilySize)
table(combine$FamilySize, combine$Survived)

p_fsize_fill <- ggplot(combine[1:891,], aes(x = FamilySize, fill = Survived)) +
  geom_bar(stat='count', position = 'fill') +
  scale_x_continuous(breaks = c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()
p_fsize_fill


#Create new feature "Family"


combine$Family <- NA
combine$Family[combine$FamilySize == 1] <- 'single'
combine$Family[combine$FamilySize >= 2 & combine$FamilySize <= 4] <- 'small'
combine$Family[combine$FamilySize >= 5] <- 'large'

combine$Family <- as.factor(combine$Family)
summary(combine$Family)

# Get tables and visualizations of Family and Survived
# Plot shows that small family between 2 and 4 has the best survival rate, even single is better than a large family.
table(combine$Survived, combine$Family)

p_f_fill <- ggplot(combine[1:891,], aes(x = Family, fill = Survived)) +
  geom_bar(stat = 'count', position = 'fill') +
  labs(x = 'Family') +
  theme_few() 
p_f_fill


# 3) Ticket & Cabin (Ticket is not complete yet, but I would remove these two from my model, so it's fine)

#In common sense, tickets and cabins should have correlation with Pclass. Let's take a look at them.
head(combine[,c(3,9,11)], 20)
tail(combine[,c(3,9,11)], 20)

#After carefully consideration, I got the following rule that could match most of the observations in this data set.
# First digit or the letters of tickets could imply the Pclass of this ticket.
# First letter of the Cabin could imply the Pclass of this cabin.
# Here, I will skip Ticket because this variable provided little information.



#Create new subset called "Cabin_known" and figure out the cabin letter's relationship with Pclass and Survived.

cabin_known <- combine[!is.na(combine$Cabin),]
head(cabin_known)
str(cabin_known)
#295 17

cabin_known$ab_cabin <- substr(cabin_known$Cabin, 1, 1)
head(cabin_known$ab_cabin)

table(cabin_known$Pclass, cabin_known$ab_cabin)
table(cabin_known$Survived, cabin_known$ab_cabin)



p_cabin_class <- ggplot(cabin_known, aes(x = ab_cabin, fill = Pclass)) +
  geom_bar(stat = 'count', position = 'fill') +
  labs(x = 'Cabin Letter')
  theme_few()
p_cabin_class

cabin_known_new <- cabin_known[!is.na(cabin_known$Survived),]

p_cabin_survive <- ggplot(cabin_known_new, aes(x = ab_cabin, fill = Survived)) +
  geom_bar(stat = 'count', position = 'fill') +
  labs(x = 'Cabin Letter') +
  theme_few()
p_cabin_survive

#Even though I did some analysis on variable Cabin, I will skip this variable for later model building.



### 4.2 Filling in missing values

# Missing values for Age

set.seed(333)

mice_model <- mice(combine[,!names(combine) %in% c('PassengerID', 'Name','Ticket','Cabin','Family','last_name', 'first_name', 'Survived')], method = 'rf')

mice_output <- complete(mice_model)

par(mfrow=c(1,2))
hist(combine$Age, freq = F, main = 'Age:Original Data', col = 'darkgreen', ylim = c(0, 0.04))
hist(mice_output$Age, freq = F, main = 'Age: MICE Output', col = 'lightgreen', ylim = c(0, 0.04))

#The plots looked great. The missing values are filled.
combine$Age <- mice_output$Age
sum(is.na(combine$Age))

# Missing values for Embarked
combine$Embarked[c(62,830)] <- 'C'

# Missing value for Fare
combine$Fare[c(1044)] <- median(combine[combine$Pclass == '3' & combine$Embarked == 'S', ]$Fare, na.rm = TRUE)

#-------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------

##Part 5. Model building with SVM

combine$title <- as.factor(combine$title)
str(combine)

train_mod <- combine[1:891,c(2,3,5:8,10,12,15:17)]
test_mod <- combine[892:1309,c(2,3,5:8,10,12,15:17)]
head(train_mod)
head(test_mod)


### 5.1 Support Vector Machine (SVM) (Not worked yet so far)

svm_model <- svm(Survived~., data = train_mod)
svm_model

### 5.2 Random Forest

rf_model <- randomForest(Survived~.,data = train_mod)
plot(rf_model)

### 5.3 Logistic Regression




### 5.4 Prediction and submission

pred1 <- predict(svm_model, test_mod)
pred1
pred2 <- predict(rf_model, test_mod)
pred2

solution1 <- data.frame(test$PassengerId, Survived = pred1)
solution2 <- data.frame(test$PassengerId, Survived = pred2)

head(solution2)
colnames(solution2) <- c("PassengerId", "Survived")

write.csv(solution1, 'svm_mod_solution.csv', row.names = F)
write.csv(solution2, 'rf_mod_solution.csv', row.names = F)





