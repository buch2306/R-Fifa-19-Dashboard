library("shiny")
library("shinydashboard")  #
library("tidyverse")
library("dashboard")       
library("ggthemes")
library("DT")
library("lubridate")       
library(dplyr)
require(scales)
#install.packages("gridExtra")
library(gridExtra)
library(sqldf)
library(ggplot2)
library(reshape)
library(ggrepel)
library(stringr)
library(caret)

#setwd("/Users/ruthvik/Desktop/R Final Project")
data <- read.csv("data.csv", header = TRUE, sep = ",")



#str(data)

data$Name <- as.character(data$Name)
data$Photo <- as.character(data$Photo)
data$Nationality <- as.character(data$Nationality)
data$Flag <- as.character(data$Flag)
data$Club <- as.character(data$Club)
data$Club.Logo <- as.character(data$Club.Logo)
data$Loaned.From <- as.character(data$Loaned.From)
data$League <- as.character(data$League)

#for (i in charactercolumns){

# data[i] <- as.character(data[i])
#}
#str(data)


# coercing the columns to date#

datecolumns <- c("Joined")

for (i in datecolumns){
  
  data[[i]] <- as.Date(data[[i]], format ="%d-%b-%y" )
}

#coercing the columns to numeric#

##########function to extract only numbers#####################

# library(stringr)
# 
# numextract <- function(string){ 
#   str_extract(string, "\\-*\\d+\\.*\\d*")
# } 
# 
# data$Value <- numextract(data$Value)
# data$Wage <- numextract(data$Wage)
# data$Weight <- numextract(data$Weight)
# data$Release.Clause <- numextract(data$Release.Clause)
###############################################################

# ##### Cleaning Contract.Valid.Until column is pending#######################
# 
# str(data)
# numericcolumns <- c("Value","Wage","Weight","Release.Clause")
# 
# for (i in numericcolumns){
#   
#   data[[i]] <- as.numeric(data[[i]])
# }
# 
# ### multiplying currency values
# data$Value <- data$Value*1000000
# data$Wage <- data$Wage * 1000
# data$Release.Clause <- data$Release.Clause*1000000

sapply(data, function(x) sum(is.na(x)))

data <- na.omit(data, cols = "Release.Clause")


# finding tenure of player in the club
data$tenure <- round(as.numeric(difftime(as.Date(Sys.Date()), as.Date(data$Joined),
                                         unit="weeks"))/52.25, digits = 0)


# finding out the contract left for player
data$contractleft <- data$Contract.Valid.Until - 2019

#data$Contract.Valid.Until <- NULL

data <- subset(data, data$contractleft >= 0)
rm(datecolumns, i)

data$Preferred.Foot <- droplevels(data$Preferred.Foot)
data$Work.Rate <- droplevels(data$Work.Rate)
data$Body.Type <- droplevels(data$Body.Type)
data$Position <- droplevels(data$Position)

# removing the variables not needed for analysis
d <- data

str(d)
variables <- c("X", "ID", "Photo", "Nationality", "Flag", "Club", "League", "Club.Logo","Value", "Wage", "Special", "Real.Face", 
               "Jersey.Number","Joined", "Loaned.From", "Height", "Weight", "LS",	"ST",	"Position","Contract.Valid.Until",
               "RS",	"LW",	"LF",	"CF",	"RF",	"RW",	"LAM",	"CAM",	"RAM","LM",	"LCM"	,
               "CM",	"RCM",	"RM",	"LWB",	"LDM",	"CDM",	"RDM",	"RWB",	"LB",	"LCB",	
               "CB",	"RCB",	"RB", "Release.Clause")

d <- d[ , !(names(d) %in% variables)]



sapply(d, function(x) sum(is.na(x)))

###### creating dummies for character variables
str(d)

name <- d$Name

d$Name <- NULL


dummies <- dummyVars(ReleaseClause2 ~ . , data = d)
ex <- data.frame(predict(dummies, newdata = d))
names(ex) <- gsub("\\.", "", names(ex))
d <- cbind(d$ReleaseClause2, ex)
names(d)[1] <- "Release Clause"

rm(dummies,ex)

# identifying correlated variables 

descrCor <- cor(d[,2:ncol(d)])
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > 0.80)
summary(descrCor[upper.tri(descrCor)])

highlycorDescr <- findCorrelation(descrCor, cutoff = 0.80)
filteredDescr <- d[,2:ncol(d)][,-highlycorDescr]
descrCor2 <- cor(filteredDescr)

summary(descrCor2[upper.tri(descrCor2)])

d <- cbind(d$`Release Clause` , filteredDescr)
names(d)[1] <- "ReleaseClause"

rm(descrCor,descrCor2,filteredDescr, highCorr, highlycorDescr)


# identifying linear dependencies

ReleaseClause <- d$ReleaseClause

d <- cbind(rep(1,nrow(d)) , d[2:ncol(d)])
names(d)[1] <- "ones"

comboInfo <- findLinearCombos(d)
comboInfo

d<- d[,-comboInfo$remove]

d<- d[,c(2:ncol(d))]
d<- cbind(ReleaseClause, d)

rm(ReleaseClause, comboInfo)

# removing near zero variances 

nzv <- nearZeroVar(d, freqCut =  90/10, uniqueCut = 5  , saveMetrics = TRUE)

d <- d[, c(TRUE,!nzv$nzv[2:ncol(d)])]


# pre process the data

preProcValues <- preProcess(d[,2:ncol(d)], method = c("range"))
d<- predict(preProcValues,d)

rm(nzv, preProcValues)
# Data Partition

set.seed(2019)
inTrain <- createDataPartition(y = d$ReleaseClause, p = 0.70, list = F)
train <- d[inTrain,]
test <- d[-inTrain,]

rm(inTrain)

# traincontrol()

ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = F,
                     summaryFunction = defaultSummary,
                     allowParallel = T)

# train a model 

m1 <- train(ReleaseClause ~ . , 
            data = train,
            method = "lm",
            metric = "RMSE")

summary(m1)


m2 <- train(log(ReleaseClause) ~ . , 
            data = train,
            method = "lm",
            metric = "RMSE")

summary(m2)



y_test <- test$ReleaseClause
y_train <- train$ReleaseClause
d_ReleaseClause <- d$ReleaseClause

p_m1_test <- predict(m1, newdata = test)
p_m2_test <- predict(m2, newdata = test)
p_m1_train <- predict(m1, newdata = train)
p_m2_train <- predict(m2, newdata = train)

p_m1_d <- predict(m1, newdata = d)
p_m2_d <- predict(m2, newdata = d)
predictedvalues <- as.data.frame(exp(p_m2_d))
names(predictedvalues)[1] <- "PredictedValue"

predictedvalues <- cbind(name,d$ReleaseClause , predictedvalues)
names(predictedvalues)[2] <- "ReleaseClause"
predictedvalues$ReleaseClause_M <- paste0("€", predictedvalues$ReleaseClause/1000000, "M")
predictedvalues$PredictedValueM2_M <- paste0("€", predictedvalues$PredictedValue/1000000, "M")


postResample(y_test,p_m1_test)
postResample(log(y_test),p_m2_test)

postResample(y_train,p_m1_train)
postResample(log(y_train), p_m2_train)

predictedvalues <- cbind(predictedvalues, p_m1_d)
predictedvalues$PredictedValueM1_M <- paste0("€", round(predictedvalues$p_m1_d/1000000,1), "M")


# data <- data %>%
#          mutate(Goal_Keeper = if_else(Type %in% "Goal Keeper",1,0),
#                         Defence = if_else(Type %in% "Defender", 1, 0),
#                         Midfielder = if_else(Type %in% "Midfield", 1, 0),
#                         Forward = if_else(Type %in% "Forward", 1, 0))%>% 

data$Goal_Keeper <- ifelse(data$Type %in% "Goal Keeper",1,0)
data$Defence <- ifelse(data$Type %in% "Defender",1,0)
data$Midfielder <- ifelse(data$Type %in% "Midfield",1,0)
data$Forward <- ifelse(data$Type %in% "Forward",1,0)

# 
# paste0("Number of Goal Keeper: ",table(data$Goal_Keeper)[2])
# paste0("Number of Defence: ",table(data$Defence)[2])
# paste0("Number of Midfielder :",table(data$Midfielder)[2])
# paste0("Number of Forward: ",table(data$Forward)[2])
# paste0("Total Players: ", nrow(data))


# Data_clean_1 <- Data_Clean
# 
# Data_clean_1$Value2 <- str_remove_all(Data_clean_1$Value2,"???")
# Data_clean_1$Value2 <- str_replace_all(Data_clean_1$Value2,"K", "000")
# Data_clean_1$Value2 <- str_remove_all(Data_clean_1$Value2,"M")
# 
# Data_clean_1$Value2 <- as.numeric(as.character(Data_clean_1$Value2))

#Data_clean_1 <- Data_clean_1  %>% mutate(Value2 = if_else(Data_clean_1$Value2 < 1000 , Value2 * 1000000, Value2))

p12 <- subset(data,data$League=="Premier League")
P13 <- subset(data,data$League=="Laliga")
P14 <- subset(data,data$League==" Bundesliga")
P15 <- subset(data,data$League=="Serie A")
P16 <- subset(data,data$League=="Ligue 1")

DTL <- function(DT_tab){
  
  Player_1 <- sqldf("select Name, Age,Position, Club 
      from DT_tab 
      where Position='GK' and Overall=(select max(Overall) from DT_tab where Position='GK')")
  
  Player_2 <-sqldf("select Name, Age, Position, Club 
      from DT_tab 
      where Position='RB' and Overall=(select max(Overall) from DT_tab where Position='RB' )")
  
  
  Player_3 <-sqldf("select Name, Age,Position, Club 
      from DT_tab 
      where Position='RCB' and Overall=(select max(Overall) from DT_tab where Position='RCB' )")
  
  Player_4 <-sqldf("select Name, Age,Position, Club 
      from DT_tab 
      where Position='LCB' and Overall=(select max(Overall) from DT_tab where Position='LCB' )")
  
  Player_5 <-sqldf("select Name, Age,Position, Club 
      from DT_tab 
      where Position='LB' and Overall=(select max(Overall) from DT_tab where Position='LB' )")
  
  Player_6 <-sqldf("select Name, Age, Position, Club 
      from DT_tab 
      where Position='RW' and Overall=(select max(Overall) from DT_tab where Position='RW' )")
  Player_6
  Player_7 <-sqldf("select Name, Age, Position, Club 
      from DT_tab 
      where Position='RCM' and Overall=(select max(Overall) from DT_tab where Position='RCM' )")
  
  Player_8<-sqldf("select Name, Age, Position, Club 
       from DT_tab 
      where Position='LCM' and Overall=(select max(Overall) from DT_tab where Position='LCM' )")
  
  Player_9 <-sqldf("select Name, Age,Position, Club 
      from DT_tab 
      where Position='LW' and Overall=(select max(Overall) from DT_tab where Position='LW' )")
  
  
  Player_10 <-sqldf("select Name, Age, Position, Club
      from DT_tab 
      where Position='CF' and Overall=(select max(Overall) from DT_tab where Position='CF' )")
  
  Player_11 <-sqldf("select Name, Age, Position, Club 
       from DT_tab 
      where Position='ST' and Overall=(select max(Overall) from DT_tab where Position='ST' )")
  
  Dream_team=rbind(Player_1,Player_2,Player_3,Player_4,Player_5,Player_6,Player_7,Player_8,Player_9,Player_10,Player_11)
  return(Dream_team)
}

PL_DT= DTL(p12)
LA_DT= DTL(P13)
BU_DT= DTL(P14)
SE_DT= DTL(P15)
LI_DT= DTL(P16)

Gk_Plot=p12  %>% arrange(-Value2) %>% filter(Goal_Keeper == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "seagreen")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Goal Keepers",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Midfielder_Plot=p12  %>% arrange(-Value2) %>% filter(Midfielder == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "steelblue")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Midfielders",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Forward_Plot=p12  %>% arrange(-Value2) %>% filter(Forward == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "khaki")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Forwards",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Defender_Plot=  p12 %>% arrange(-Value2) %>% filter(Defence == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "orchid")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Defenders",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

observ <- function(tb){
  
  
  numofpos <- data.frame(Pos = c("Goal Keeper", "Defence", "Midfielder", "Forward"),
                         Frequency = c(table(tb$Goal_Keeper)[2],table(tb$Defence)[2], table(tb$Midfielder)[2], table(tb$Forward)[2]))
  
  
  OP <- ggplot(numofpos, aes(reorder(Pos, Frequency), Frequency, fill = Pos, label = Frequency))+
    geom_col(show.legend = FALSE)+
    geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
    coord_flip()+
    scale_fill_ordinal()+
    theme_minimal()+
    labs(x = "Position",
         title = "Number of Players")
  return(OP)
}



Gk_Plot_L=P13  %>% arrange(-Value2) %>% filter(Goal_Keeper == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "seagreen")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Goal Keepers",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Midfielder_Plot_L=P13 %>% arrange(-Value2) %>% filter(Midfielder == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "steelblue")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Midfielders",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Forward_Plot_L=P13  %>% arrange(-Value2) %>% filter(Forward == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "khaki")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Forwards",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Defender_Plot_L=  P13 %>% arrange(-Value2) %>% filter(Defence == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "orchid")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Defenders",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())
grid.arrange(Gk_Plot_L,Midfielder_Plot_L,Defender_Plot_L,Forward_Plot_L)

Gk_Plot_B=P14  %>% arrange(-Value2) %>% filter(Goal_Keeper == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "seagreen")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Goal Keepers",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Midfielder_Plot_B=P14 %>% arrange(-Value2) %>% filter(Midfielder == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "steelblue")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Midfielders",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Forward_Plot_B=P14  %>% arrange(-Value2) %>% filter(Forward == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "khaki")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Forwards",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Defender_Plot_B=  P14 %>% arrange(-Value2) %>% filter(Defence == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "orchid")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Defenders",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())
grid.arrange(Gk_Plot_B,Midfielder_Plot_B,Defender_Plot_B,Forward_Plot_B)

Gk_Plot_S=P15  %>% arrange(-Value2) %>% filter(Goal_Keeper == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "seagreen")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Goal Keepers",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Midfielder_Plot_S=P15 %>% arrange(-Value2) %>% filter(Midfielder == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "steelblue")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Midfielders",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Forward_Plot_S=P15  %>% arrange(-Value2) %>% filter(Forward == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "khaki")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Forwards",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Defender_Plot_S=  P15 %>% arrange(-Value2) %>% filter(Defence == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "orchid")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Defenders",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())
grid.arrange(Gk_Plot_S,Midfielder_Plot_S,Defender_Plot_S,Forward_Plot_S)



Gk_Plot_Li=P16 %>% arrange(-Value2) %>% filter(Goal_Keeper == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "seagreen")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Goal Keepers",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Midfielder_Plot_Li=P16 %>% arrange(-Value2) %>% filter(Midfielder == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "steelblue")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Midfielders",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Forward_Plot_Li=P16  %>% arrange(-Value2) %>% filter(Forward == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "khaki")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Forwards",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())

Defender_Plot_Li=  P16 %>% arrange(-Value2) %>% filter(Defence == 1) %>% head(5)%>%
  ggplot(aes(reorder(Name, Value2), Value2, label = paste0(Value2 / 1000000, "M")))+
  geom_col(fill = "orchid")+
  geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
  coord_flip()+
  theme_minimal()+
  labs(subtitle = "Most Valuable Defenders",
       x = "Players",
       y = NULL)+
  theme(axis.text.x=element_blank())
grid.arrange(Gk_Plot_Li,Midfielder_Plot_Li,Defender_Plot_Li,Forward_Plot_Li)


# Pll2 =p12 %>% group_by(Type) %>% summarise(Avg.Overall = mean(Overall, na.rm = TRUE)) %>% 
#   ggplot(aes(reorder(Type, Avg.Overall), Avg.Overall, label = paste0("%", round(Avg.Overall, digits = 2))))+
#   geom_col(fill = "pink")+
#   geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
#   coord_flip()+
#   theme_minimal()+
#   labs(x = "Type",
#        y = "Average Overall")
# 
# Pll3 =P13 %>% group_by(Type) %>% summarise(Avg.Overall = mean(Overall, na.rm = TRUE)) %>% 
#   ggplot(aes(reorder(Type, Avg.Overall), Avg.Overall, label = paste0("%", round(Avg.Overall, digits = 2))))+
#   geom_col(fill = "pink")+
#   geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
#   coord_flip()+
#   theme_minimal()+
#   labs(x = "Type",
#        y = "Average Overall")
# 
# Pll4 =P14 %>% group_by(Type) %>% summarise(Avg.Overall = mean(Overall, na.rm = TRUE)) %>% 
#   ggplot(aes(reorder(Type, Avg.Overall), Avg.Overall, label = paste0("%", round(Avg.Overall, digits = 2))))+
#   geom_col(fill = "pink")+
#   geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
#   coord_flip()+
#   theme_minimal()+
#   labs(x = "Type",
#        y = "Average Overall")
# 
# Pll5 =P15 %>% group_by(Type) %>% summarise(Avg.Overall = mean(Overall, na.rm = TRUE)) %>% 
#   ggplot(aes(reorder(Type, Avg.Overall), Avg.Overall, label = paste0("%", round(Avg.Overall, digits = 2))))+
#   geom_col(fill = "pink")+
#   geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
#   coord_flip()+
#   theme_minimal()+
#   labs(x = "Type",
#        y = "Average Overall")
# 
# Pll6 =P16 %>% group_by(Type) %>% summarise(Avg.Overall = mean(Overall, na.rm = TRUE)) %>% 
#   ggplot(aes(reorder(Type, Avg.Overall), Avg.Overall, label = paste0("%", round(Avg.Overall, digits = 2))))+
#   geom_col(fill = "pink")+
#   geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
#   coord_flip()+
#   theme_minimal()+
#   labs(x = "Type",
#        y = "Average Overall")


trip_scene <- function(w_b){
  rofl =w_b %>% group_by(Type) %>% summarise(Avg.Overall = median(Overall, na.rm = TRUE))
  guy <-paste0(round(rofl[1,2],digits=2),"%")
  return(guy)
}
trip_scene_title <- function(w_b){
  rofl =w_b %>% group_by(Type) %>% summarise(Avg.Overall = median(Overall, na.rm = TRUE))
  return(rofl[1,1])
}

trip_scene_F <- function(w_b){
  rofl =w_b %>% group_by(Type) %>% summarise(Avg.Overall = median(Overall, na.rm = TRUE))
  guy <-paste0(round(rofl[2,2],digits=2),"%")
  return(guy)
}
trip_scene_title_F <- function(w_b){
  rofl =w_b %>% group_by(Type) %>% summarise(Avg.Overall = median(Overall, na.rm = TRUE))
  return(rofl[2,1])
}

trip_scene_G <- function(w_b){
  rofl =w_b %>% group_by(Type) %>% summarise(Avg.Overall = median(Overall, na.rm = TRUE))
  guy <-paste0(round(rofl[3,2],digits=2),"%")
  return(guy)
}
trip_scene_title_G <- function(w_b){
  rofl =w_b %>% group_by(Type) %>% summarise(Avg.Overall = median(Overall, na.rm = TRUE))
  return(rofl[3,1])
}

trip_scene_M <- function(w_b){
  rofl =w_b %>% group_by(Type) %>% summarise(Avg.Overall = median(Overall, na.rm = TRUE))
  guy <-paste0(round(rofl[4,2],digits=2),"%")
  return(guy)
}
trip_scene_title_M <- function(w_b){
  rofl =w_b %>% group_by(Type) %>% summarise(Avg.Overall = median(Overall, na.rm = TRUE))
  return(rofl[4,1])
}

ui <- dashboardPage(
  dashboardHeader(title = "FIFA 19 Search Engine"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "Home"),
                menuItem("League", tabName = "league"),
                menuItem("Team", tabName = "team"),
                menuItem("Player Comparison", tabName = "player"),
                menuItem("Scout System", tabName = "scout"),
                menuItem("Predictive Analytics", tabName = "predictive")
    )),
  dashboardBody(
    mainPanel(
      tabItems(
        tabItem(tabName = "Home", 
                h2("Welcome to FIFA 19 Dashboard"),
                fluidRow(
                  column(width = 6,  imageOutput("Intro")),
                  column(width = 2, imageOutput("pl")),
                  column(width = 2, imageOutput("laliga")),
                  column(width = 2, imageOutput("bundesliga"))),
                fluidRow(box(width = 1.5 , h4("This dashboard helps to compare players, leagues and
                                       teams across different leagues in Europe. This dashboard compares top 5 football leagues across
                                       Europe, based on their value, player ratings and a set of other attributes for the players ")))),
        
        tabItem(tabName = "league",
                fluidRow(
                  box(width = 3,
                      selectInput("Team", label = "Select the League",
                                  choices = list("Premier League",
                                                 "Laliga",
                                                 " Bundesliga",
                                                 "Serie A",
                                                 "Ligue 1"))),
                  # Dynamic infoBoxes
                  
                  valueBoxOutput("val",width = 3),
                  valueBoxOutput("Team_Tot",width = 3),
                  valueBoxOutput("Player_Count",width=3)),
                
                fluidRow(  
                  box( width = 2,
                       actionButton("action",label = "Select", icon("paper-plane"), 
                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                  
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Players", plotOutput("grid")),
                    tabPanel("Operations", valueBoxOutput("grid_1",width=3),valueBoxOutput("grid_40",width=3),valueBoxOutput("grid_41",width=3),valueBoxOutput("grid_42",width=3)),
                    tabPanel("General Summary", plotOutput("grid_2"))
                    
                  )
                ),
                
                fluidRow(
                  DT::dataTableOutput("table_data")
                ),
                
                fluidRow(
                  plotOutput("grid1")
                ),
                
                fluidRow(
                  plotOutput("grid_12")
                )
        ),
        
        tabItem(tabName = "scout",
                fluidRow(
                  
                  #   
                  # tags$style(type='text/css', ".selectize-input { font-size: 32px; line-height: 32px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }"),
                  #   
                  column(4,sliderInput(inputId = 'value', label='Value Range:',
                                       value=c(250000,500000), max=228100000, min=13000))
                  
                  # column(3,offset=2,selectInput(inputId = 'var',label='Choose the Position:', choices= c("Defenders" = "Defender",
                  #                                                                                        'Goal Keepers'='Goal Keeper',
                  #                                                                                        "Forwards" = "Forward",
                  #                                                                                        'Midfielders'='Midfield')))
                  # 
                  #column(3,selectInput(inputId='attr', label='Choose the attribute:',choices = NULL))
                ),
                
                fluidRow(
                  
                  column(4,sliderInput(inputId='age', label='Age Range:', value=c(20,25), max=45, min=16))
                  #column(1,offset=4,actionButton("Enter", "Run Analysis", icon("paper-plane"), 
                  #                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                  
                  
                ),
                
                fluidRow(
                  
                  column(4,checkboxGroupInput(inputId='variable', label='Position', choices = c("Goal Keeper" = "Goal Keeper",
                                                                                                "Defenders" = "Defender",
                                                                                                "Forwards" = "Forward",
                                                                                                'Midfielders'='Midfield')))
                  
                  #column(9,offset=1, plotOutput('graph'))
                  
                ),
                fluidRow(column(4,sliderInput(inputId='range', label='Overall Range:', value=c(20,25), max=94, min=46))),
                DT::dataTableOutput('table', width = "1200px")
        ),
        
        tabItem(tabName = "player" , 
                h2("Player Stats"),
                column(width = 4 ,
                       fluidRow(h2("Player 1:")),
                       fluidRow(htmlOutput("League1_sel")),
                       fluidRow(htmlOutput("Club1_sel")),
                       fluidRow(htmlOutput("Player1_sel")),
                       fluidRow(actionButton("select", "Select", icon("paper-plane"), 
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                       fluidRow(h2("Player 2:")),
                       fluidRow(htmlOutput("League2_sel")),
                       fluidRow(htmlOutput("Club2_sel")),
                       fluidRow(htmlOutput("Player2_sel"))),
                column(width = 8, plotOutput("PlayerComparison", width = "800px", height = "600px"))
        ),
        
        tabItem(tabName = "team", 
                h2("Team Stats"),
                column(width = 4,
                       fluidRow(htmlOutput("League3_sel")),
                       fluidRow(htmlOutput("Club3_sel")),
                       fluidRow(actionButton("select2", "Select", icon("paper-plane"), 
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                ),
                br(),
                plotOutput("teamcharts", height = '550px', width = '1150px')
        ),
        
        tabItem(tabName = "predictive", 
                h2("Predictive Model"),
                splitLayout(
                  fluidRow(box(width=4,
                               selectInput(inputId = 'var',label='Choose the Position:', choices= c("Defenders" = "Defender",
                                                                                                    'Goal Keepers'='Goal Keeper',
                                                                                                    "Forwards" = "Forward",
                                                                                                    'Midfielders'='Midfield')),
                               selectInput(inputId='attr', label='Choose the attribute:',choices = NULL),
                               align="center", actionButton("Enter", "Run Analysis", icon("paper-plane"), 
                                                            style="color: #fff;padding-bottom:5px; background-color: #337ab7; border-color: #2e6da4")),
                           
                           
                           box(width= 8,plotOutput('graph',width="480px")))),
                
                
                
                
                fluidRow(
                  box(width=4, htmlOutput("class"),
                      htmlOutput("selectplayer"),
                      actionButton("predict", "Predict", icon("paper-plane"), 
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                  
                  box(width=8 ,valueBoxOutput('actualvalue1'),
                      valueBoxOutput('predictedvalue1'))
                  
                )
                #align="center",br(),column(4, actionButton("Enter", "Run Analysis", icon("paper-plane"), 
                #style="color: #fff;padding-bottom:5px; background-color: #337ab7; border-color: #2e6da4")),
                
                
                
                
        )
      )
      
    )
    
  )
)




server <- function(input, output, session, text){
  
  ############ Home page code##############
  output$Intro <- renderImage({
    filename <- normalizePath(file.path('www/fifa.jpg'))
    list(src = filename, width = "770px", height = "400px")
  }, deleteFile = FALSE)
  
  output$pl <- renderImage({
    filename <- normalizePath(file.path('www/pl.png'))
    list(src = filename, width = "70px", height = "70px")
  }, deleteFile = FALSE)
  
  output$laliga <- renderImage({
    filename <- normalizePath(file.path('www/laliga.png'))
    list(src = filename, width = "70px", height = "70px")
  }, deleteFile = FALSE)
  output$bundesliga <- renderImage({
    filename <- normalizePath(file.path('www/bundesliga.png'))
    list(src = filename,width = "70px", height = "70px")
  }, deleteFile = FALSE)
  ######Home page code ends###############
  
  ############League code#################
  
  observeEvent(input$action,{
    tr <- subset(data, data$League==input$Team)
    
    output$val <- renderValueBox({
      
      valueBox(paste0("€", round(sum(tr$Value2)/1000000000,2), "B"),
               "Total League Value",  icon = icon("cog"),
               color = "purple"
      )
    })
    
    output$Team_Tot <- renderValueBox({
      lbt_count <- sqldf("select count(distinct(Club)) from tr")
      valueBox(lbt_count,
               "No. of Teams", icon = icon("refresh"),
               color = "purple"
      )
    })
    output$Player_Count <- renderValueBox({
      bt_count <- sqldf("select count(distinct(Name)) from tr")
      valueBox( bt_count,
                "No. of Players", icon = icon("list"),
                color = "purple"
      )
    })
    output$table_data =DT::renderDataTable({
      IUY=DTL(tr)
      datatable(IUY,options = list(scrollX = TRUE,pageLength=3))
    })
    output$grid = renderPlot({
      if (input$Team== "Premier League"){
        grid.arrange(Gk_Plot,Midfielder_Plot,Defender_Plot,Forward_Plot)
        
      }
      else if (input$Team== "Laliga"){
        grid.arrange(Gk_Plot_L,Midfielder_Plot_L,Defender_Plot_L,Forward_Plot_L)
        
      }
      else if (input$Team== " Bundesliga"){
        grid.arrange(Gk_Plot_B,Midfielder_Plot_B,Defender_Plot_B,Forward_Plot_B)
      }
      else if (input$Team== "Serie A"){
        grid.arrange(Gk_Plot_S,Midfielder_Plot_S,Defender_Plot_S,Forward_Plot_S)
      }
      else {
        grid.arrange(Gk_Plot_Li,Midfielder_Plot_Li,Defender_Plot_Li,Forward_Plot_Li)
        
      }
      
      
    })
    
    output$grid_1 <- renderValueBox({
      
      valueBox(trip_scene(tr),
               trip_scene_title(tr),  icon = icon("cog"),
               color = "blue"
      )
    })
    output$grid_40 <- renderValueBox({
      
      valueBox(trip_scene_F(tr),
               trip_scene_title_F(tr),  icon = icon("cog"),
               color = "blue"
      )
    })
    output$grid_41 <- renderValueBox({
      
      valueBox(trip_scene_G(tr),
               trip_scene_title_G(tr),  icon = icon("cog"),
               color = "blue"
      )
    })
    output$grid_42 <- renderValueBox({
      
      valueBox(trip_scene_M(tr),
               trip_scene_title_M(tr),  icon = icon("cog"),
               color = "blue"
      )
    })
    
    output$grid_2 =renderPlot({
      IUY1=observ(tr)
      IUY1
      
    })
    
  }
  )
  
  ##############League code ends#######################
  
  ################ Team tab code
  
  output$League3_sel <- renderUI({
    selectInput("League3", "League:", choices = as.character(unique(data$League)))
  })
  output$Club3_sel <- renderUI({
    data_available4 <- data[data$League == input$League3, "Club"]
    selectInput("Club3", "Club:", choices = as.character(unique(data_available4)))
  })
  
  dataset_team <- eventReactive(input$select2, {
    subset(data,Club == input$Club3) })
  
  output$teamcharts <- renderPlot({
    df1 <- dataset_team()
    
    numofpos <- sqldf("select Type, count(ID) as number
                    from df1 
                    group by Type
                    order by number desc")
    
    numofvalue <- sqldf("select Name, value2
                    from df1
                    order by ReleaseClause2 desc
                    limit 10")
    
    p1 <- ggplot(numofpos, aes(x= reorder(Type, number), y= number, fill = Type)) +
      geom_bar( position = "dodge", stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = number))+
      coord_flip() + labs(x= "Position", y= "Frequency", title = "Number of Players") + scale_y_continuous(labels = comma)
    
    p2<- ggplot(numofvalue, aes(x = reorder(Name, Value2), y = Value2)) +
      geom_bar(position = "dodge", stat = "identity", show.legend = FALSE) +
      geom_col(fill = "seagreen")+
      geom_text(aes(label=paste0("€", Value2 / 1000000, "M")), vjust=1.6, color="black", size=4)+
      coord_flip() + labs(x = "Players", y = "Values", title = "Most Valuable Players") + scale_y_continuous(labels = comma)
    
    p3 <- ggplot(df1, aes(Value2)) + geom_histogram(fill = "blue", bins = 20) + scale_x_continuous(labels = comma)+
      labs(y = "Frequency", title = "Distribution of Player Values")
    
    p4 <- ggplot(df1, aes(x = Type, y = Value2, color = Type)) + geom_boxplot(show.legend = FALSE) + coord_flip()+ ylim(c(0,100000000))+
      labs(title = "Distribution of class values with boxplot")+ scale_y_continuous(labels = comma) + ylab(NULL)+xlab(NULL)
    
    
    grid.arrange(p1,p2,p3,p4)
  })    
  
  
  ###### Team tab code ends
  
  ######Player Comparison code#################
  output$League1_sel <- renderUI({
    selectInput("League1", "League:", choices = as.character(unique(data$League)))
  })
  output$Club1_sel <- renderUI({
    data_available = data[data$League == input$League1, c("Club", "Name")]
    selectInput("Club1", "Club:", choices = as.character(unique(data_available$Club)))
  })
  output$Player1_sel <- renderUI({
    data_available1 <- data[data$Club == input$Club1, "Name"]
    selectInput("Player1", "Player:", choices = as.character(unique(data_available1)), selected = data$Name[1])
  })
  output$League2_sel <- renderUI({
    selectInput("League2", "League:", choices = as.character(unique(data$League)))
  })
  output$Club2_sel <- renderUI({
    data_available2 = data[data$League == input$League2, c("Club", "Name")]
    selectInput("Club2", "Club:", choices = as.character(unique(data_available2$Club)))
  })
  output$Player2_sel <- renderUI({
    data_available3 <- data[data$Club == input$Club2, "Name"]
    selectInput("Player2", "Player:", choices = as.character(unique(data_available3)), selected = data$Name[2])
  })
  
  dataset <- eventReactive(input$select, {
    subset(data, Name == input$Player1| Name == input$Player2, select = c("Name","Crossing", "Finishing", "Dribbling",
                                                                          "Curve", "LongPassing", "BallControl",
                                                                          "SprintSpeed", "Reactions", "ShotPower",
                                                                          "Stamina", "Vision", "LongShots"))
  })
  output$PlayerComparison <- renderPlot({
    
    md <- melt(dataset(), id=(c("Name")))
    
    p <- ggplot(md, aes(x=variable, y=value, fill = Name)) + 
      geom_bar( position = "dodge", stat = "identity", width = 0.75, color = "#2b2b2b", size = 0.05 ) + labs(title = paste0("Attribute Comparison for ", input$Player1, " & ", input$Player2))
    p + facet_wrap( ~ variable, scales="free") + xlab("Value") + ylab("Attribute") 
    
  })
  #############Player comparison code ends####################
  
  #############Scout system code####################
  
  output$table<-DT::renderDataTable({
    table <- subset(data, data$ReleaseClause2>input$value[1] & 
                      data$ReleaseClause2<=input$value[2]&data$Age>input$age[1]&
                      data$Age<=input$age[2]&data$Type==input$variable&
                      data$Overall>input$range[1]&data$Overall<input$range[2], c("Name", "Age", "Nationality", "Overall", "Potential",
                                                                                 "Club", "League", "Value", "Wage", "Preferred.Foot",
                                                                                 "International.Reputation", "Skill.Moves","Contract.Valid.Until",
                                                                                 "Crossing", "Finishing", "Dribbling",
                                                                                 "Curve", "LongPassing", "BallControl",
                                                                                 "SprintSpeed", "Reactions", "ShotPower",
                                                                                 "Stamina", "Vision", "LongShots"))
    table <- arrange(table, -Value)
    datatable(table, options = list(scrollX = TRUE,pageLength=5), extensions = 'AutoFill')
  })
  observe({
    x<-input$var
    
    if (x=='Goal Keeper'){
      
      y=c('GKDiving'='GKDiving','GKHandling'='GKHandling','GKKicking'='GKKicking','GKPositioning'='GKPositioning','GKReflexes'='GKReflexes','Jumping'='Jumping','Agility'='Agility', 'Reactions'='Reactions')
      
      updateSelectInput(session, 'attr',label='Choose the attribute:', choices=y)
      
    }
    
    if (x=='Forward'){
      
      y=c('Crossing'='Crossing','Agility'='Agility','Finishing'='Finishing','HeadingAccuracy'='HeadingAccuracy','Volleys'='Volleys','Dribbling'='Dribbling','Curve'='Curve','ShortPassing'='ShortPassing','FKAccuracy'='FKAccuracy','BallControl'='BallControl','Acceleration'='Acceleration','SprintSpeed'='SprintSpeed','Agility'='Agility','ShotPower'='ShotPower','Strength'='Strength','Penalties'='Penalties','Stamina'='Stamina','LongShots'='Longshots','Aggression'='Aggression')
      
      updateSelectInput(session, 'attr',label='Choose the attribute:', choices=y)
      
    }
    
    if(x=='Defender'){
      y=c('HeadingAccuracy'='HeadingAccuracy','Jumping'='Jumping','Stamina'='Stamina','Strength'='Strength','Aggression'='Aggression','Interceptions'='Interceptions','Marking'='Marking','StandingTackle'='StandingTackle','SlidingTackle'='SlidingTackle')
      updateSelectInput(session, 'attr',label='Choose the attribute:', choices=y)
      
    }
    
    if(x=='Midfield'){
      
      y=c('Crossing'='Crossing','Agility'='Agility','Finishing'='Finishing','Interceptions'='Interceptions','Volleys'='Volleys','Dribbling'='Dribbling','Curve'='Curve','ShortPassing'='ShortPassing','FKAccuracy'='FKAccuracy','BallControl'='BallControl','Acceleration'='Acceleration','SprintSpeed'='SprintSpeed','Agility'='Agility','ShotPower'='ShotPower','Strength'='Strength','Penalties'='Penalties','Stamina'='Stamina','LongShots'='LongShots','Aggression'='Aggression','Vision'='Vision','Composure'='Composure')
      
      updateSelectInput(session, 'attr',label='Choose the attribute:', choices=y)
    }
  })
  observeEvent(input$Enter,{
    
    output$graph<-renderPlot({
      
      data1 <- subset(data, data$Type==input$var)
      x <- which(names(data1)==input$attr)
      
      ggplot(data=data1)+geom_point(aes(x=data1[,x],y=data1$Overall),shape = 16, size = 5, show.legend = FALSE)+xlab(input$attr)+
        ylab('Overall Rating')+theme(plot.title = element_text(size = 30, face = "bold"),axis.text=element_text(size=12))+
        scale_color_gradient(low = "#0091ff", high = "#f0650e")+scale_alpha(range = c(.05, .25))
      
    })
    
  })
  
  #########Scout system code ends###################
  
  #########predictive analytics tab##################
  
  output$class <- renderUI({
    selectInput("type", "Class", choices = as.character(unique(data$Type)))
  })
  output$selectplayer <- renderUI({
    final <- subset(data, Type == input$type, "Name")
    selectInput("pa_player", "Player", choices = as.character(unique(final$Name)))
  })
  
  prediction <- eventReactive(input$predict, {
    subset(predictedvalues, name == input$pa_player ,  c("ReleaseClause_M", "PredictedValueM1_M"))
  })
  
  output$actualvalue1 <- renderValueBox({
    actual <- prediction()
    n <- actual$ReleaseClause_M
    valueBox(value=tags$p(n,style="font-size: 65%;"),"Actual Value",color="green")
  })
  output$predictedvalue1 <- renderValueBox({
    predicted <- prediction()
    m<- predicted$PredictedValueM1_M
    valueBox(value=tags$p(m,style="font-size: 65%;"),"Predicted Value",color="green")
  })
  
  
  
  
  
}


shinyApp(ui = ui, server = server)
