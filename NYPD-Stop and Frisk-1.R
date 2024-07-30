# Final Project   
# Dhairyav J. Shah, Omkar N. Sadekar, Naveen Manuka, Santi K. Chavali

library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(stats)
library(ISLR)
library(ggcorrplot)
library(plotly)
library(caret)
library(lattice)
library(pROC)
library(kableExtra)
library(SmartEDA)
library(heatmaply)
library(stargazer)
library(caTools)
library(leaps)

Nypd <- read_csv("C:\\Users\\shahd\\OneDrive - Northeastern University\\Documents\\MPS ANALYTICS\\Fall Q2\\ALY6015\\Group Assignment\\NYPD_SQF.csv")
str(Nypd)
df <- data.frame(Nypd)
str(df)
dim(df)

#Plot for Missing Data in the Data set
df  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  ggtitle(" Fig no.1 Null values")
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank())

# Creating New variables for analysis

# Creating Decade Col
df$Decade <- df$year - (df$year %% 10)
df$year<-as.factor(df$year)

# Creating Area Col
df <- within(df,{
  Area='N/A'
  Area[pct %in% c(1,5,6,7,9,10,13,14,17,18)]='Manhattan South'
  Area[pct %in% c(19,20,22,23,24,25,26,28,30,32,33,34)]='Manhattan North'
  Area[pct %in% c(40,41,42,43,44,45,46,47,48,49,50,52)]='The Bronx'
  Area[pct %in% c(60,61,62,63,66,67,68,69,70,71,72,76,78)]='Brooklyn South'
  Area[pct %in% c(73,75,77,79,81,83,84,88,90,94)]='Brooklyn North'
  Area[pct %in% c(100,101,102,103,105,106,107,113)]='Queens South'
  Area[pct %in% c(104,108,109,110,111,112,114,115)]='Queens North'
  Area[pct %in% c(120,121,122,123)]='Staten Island'
})


#Summary Statistics of all  Variables
stat_table <- as.data.frame(psych::describe(df))
stat_table <- subset(stat_table, select=c("n","mean","sd")) 
stat_table <- stat_table %>%          
  mutate_if(is.numeric,
            round,
            digits = 1)
kbl(stat_table)%>%kable_styling
capture.output(stat_table,file="Sumstats.doc")

#Summary Statistic by Sex
sum_stat <- df %>% group_by(sex) %>% 
  summarise(stopped=round(mean(stopped),2),
            arrested=round(mean(arrested),2),
            frisked=round(mean(frisked),2),
            searched=round(mean(searched),2),
            summoned=round(mean(summoned),2),
            contraband=round(mean(contrabn),2),
            Weapon = round(mean(weapnfnd),2),
            .groups = 'drop')
kbl(sum_stat)%>%kable_styling()
capture.output(sum_stat,file="Sumstatsbysex.doc")

#Summary Statistics by Race
stat_race <- df%>%group_by(race)%>%
  summarise(stopped=round(mean(stopped),2),
            arrested=round(mean(arrested),2),
            frisked=round(mean(frisked),2),
            searched=round(mean(searched),2),
            summoned=round(mean(summoned),2),
            contraband=round(mean(contrabn),2),
            Weapon = round(mean(weapnfnd),2),
            .groups = 'drop')
kbl(stat_race)%>%kable_styling()
capture.output(stat_race,file="StatsRace.doc")


#Correlation Plot
corr <- select_if(df, is.numeric)
cormat <- round(cor(corr),2)
ggcorrplot(cormat, hc.order = TRUE, type = "lower",
           lab = TRUE)

#EDA
#Total arrests in New York
precint <- df%>%group_by(Area)%>%summarise(arrested=sum(arrested))%>%
  plot_ly(x=~Area, y=~arrested, color=~Area, type = 'bar',text = ~arrested, textposition = 'top', insidetextfont = list(size=10, color = 'black'))%>%
  layout(title= list(text = "Total Arrests in New York",
                     yaxis = list(showgrid = FALSE)))

precint
#People stopped over years
stat_year <- df%>% group_by(year)%>%summarise(arrested=mean(arrested))
ggplot(data=stat_year, aes(x=year, y=arrested, color = "blue")) +
  geom_line() + geom_point()+ 
  scale_color_brewer(palette="Paired")+
  theme_minimal()

#People stopped over years by race
stopped <- df %>% group_by(year,race) %>% summarise(stopped = sum(stopped),.groups = 'drop')
kbl(stopped)%>%kable_styling()
plt <- plot_ly(stopped, x = ~year, y = ~stopped,color = ~race, type='bar',text = ~stopped/1000 , textposition = 'auto', insidetextfont = list(size=10, color = 'black'))%>%
  layout(title= list(text = "People stopped over years"), 
         legend=list(title=list(text='District')), 
         xaxis = list(title = list(text ='Area')),
         yaxis = list(showgrid = FALSE),
         yaxis = list(title = list(text = 'Number of People stopped')), barmode = 'stack')
plt

#Plot for arrested over years by Race
race_arrested <- df%>%group_by(race,year)%>%summarise(arrested = round(mean(arrested),2),.groups='drop')
ggplot(data=race_arrested, aes(x=year, y=arrested, color = race)) + 
  geom_line() + geom_point()+ labs(title="Trend for arrested by Race over years")+
  scale_color_brewer(palette="Paired")+
  theme_minimal()

#Plot for arrested over years by Sex
sex_arrested <- df%>%group_by(sex,year)%>%summarise(arrested = round(mean(arrested),2),.groups='drop')
ggplot(data=sex_arrested, aes(x=year, y=arrested, color = sex)) + 
  geom_line() + geom_point()+ labs(title="Trend for arrested by sex over years")+
  scale_color_brewer(palette="Paired")+
  theme_minimal()

#People frisked over years in districts by race
Area_race <- df %>% group_by(Area,race) %>% summarise(frisked = sum(frisked),.groups = 'drop')%>%
  plot_ly( x = ~Area, y = ~frisked,color = ~race, type='bar',text = ~frisked, textposition = 'center', insidetextfont = list(size=10, color = 'black'))%>%
  layout(title= list(text = "People frisked over years"), 
         legend=list(title=list(text='Race')), 
         xaxis = list(title = list(text ='Year')),
         yaxis = list(showgrid = FALSE),
         yaxis = list(title = list(text = 'Number of People frisked')), barmode = 'stack')
Area_race

# % of people frisked, searched and arrested when stopped

stat <- subset(df, select=c(race,arrested,frisked,stopped,weapnfnd,searched))
B1 <- dplyr::filter(df, race=="B")
stat_B <- subset(B1, select=c(race,arrested,frisked,stopped,weapnfnd,searched))
sum_B <- stat_B %>% summarise(Race ="Black",
                              Total_stops = sum(stopped),
                              frisked = round((sum(frisked)/Total_stops)*100,2),
                              searched = round((sum(searched)/Total_stops)*100,2),
                              arrested = round((sum(arrested)/Total_stops)*100,2),
                              Weapon_found = round(sum(weapnfnd)/Total_stops*100,2))

B_his <- dplyr::filter(df, race=="Q")
Stat_Bh <- subset(B_his, select=c(race,arrested,frisked,stopped,weapnfnd,searched))
sum_Bh <- Stat_Bh %>% summarise(Race ="Black-Hispanic",
                                Total_stops = sum(stopped),
                                frisked = round((sum(frisked)/Total_stops)*100,2),
                                searched = round((sum(searched)/Total_stops)*100,2),
                                arrested = round((sum(arrested)/Total_stops)*100,2),
                                Weapon_found = round(sum(weapnfnd)/Total_stops*100,2))

W <- dplyr::filter(df, race=="W")
Stat_W <- subset(W, select=c(race,arrested,frisked,stopped,weapnfnd,searched))
sum_W <- Stat_W %>% summarise(Race = "White",
                              Total_stops = sum(stopped),
                              frisked = round((sum(frisked)/Total_stops)*100,2),
                              searched = round((sum(searched)/Total_stops)*100,2),
                              arrested = round((sum(arrested)/Total_stops)*100,2),
                              Weapon_found = round(sum(weapnfnd)/Total_stops*100,2))
A <- dplyr::filter(df, race=="A")
Stat_A <- subset(A, select=c(race,arrested,frisked,stopped,weapnfnd,searched))
sum_A <- Stat_A %>% summarise(Race = "Asian",
                              Total_stops = sum(stopped),
                              frisked = round((sum(frisked)/Total_stops)*100,2),
                              searched = round((sum(searched)/Total_stops)*100,2),
                              arrested = round((sum(arrested)/Total_stops)*100,2),
                              Weapon_found = round(sum(weapnfnd)/Total_stops*100,2))
I <- dplyr::filter(df, race=="I")
Stat_I <- subset(I, select=c(race,arrested,frisked,stopped,weapnfnd,searched))
sum_I <- Stat_I %>% summarise(Race = "American Indian/Alaskan Native",
                              Total_stops = sum(stopped),
                              frisked = round((sum(frisked)/Total_stops)*100,2),
                              searched = round((sum(searched)/Total_stops)*100,2),
                              arrested = round((sum(arrested)/Total_stops)*100,2),
                              Weapon_found = round(sum(weapnfnd)/Total_stops*100,2))
P <- dplyr::filter(df, race=="P")
Stat_P <- subset(P, select=c(race,arrested,frisked,stopped,weapnfnd,searched))
sum_P <- Stat_P %>% summarise(Race = "White-Hispanic",
                              Total_stops = sum(stopped),
                              frisked = round((sum(frisked)/Total_stops)*100,2),
                              searched = round((sum(searched)/Total_stops)*100,2),
                              arrested = round((sum(arrested)/Total_stops)*100,2),
                              Weapon_found = round(sum(weapnfnd)/Total_stops*100,2))

total_stat <- rbind(sum_B, sum_Bh, sum_W, sum_A, sum_I, sum_P)
kbl(total_stat)%>%kable_styling
                                                                                                                              
# Predicting the Number of arrests using linear regression

set.seed(123)
trainIndex<- sample(x=nrow(df),size=nrow(df)*0.7)
train<-df[trainIndex,]
test<-df[-trainIndex,]

lm.fit1 <- lm(arrested~.,data=train)
summary(lm.fit1)

lm.fit2 <- lm(arrested~searched+contrabn+weapnfnd+pf_weapn+pf_hcuff,data=train)
summary(lm.fit2)

stargazer(lm.fit2, type="html",out="models.htm")

test$predicted.arrested <- predict(lm.fit2 ,test)
test %>% 
  ggplot(aes(arrested,predicted.arrested)) +
  geom_point(alpha=0.75) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of Arrests') +
  ggtitle("Actual Arrests vs Predicted Arrests")+
  ylab('Predicted value of Arrests') +
  theme_classic()+
  theme(legend.position="none")

# Predicting the Number of arrests using subset analysis in linear regression

Sub <- regsubsets(arrested~., data = df,really.big=T)
reg.summary <- summary(Sub)
reg.summary

reg.summary$cp 
reg.summary$adjr2 
reg.summary$bic

M<-lm(arrested ~ searched + contrabn + pf_weapn+ pf_hcuff+cs_vcrim+
        rf_attir+rf_vcact+rf_rfcmp, data = df)
summary(M)
stargazer(M, type="html",out="models_sub.htm")
# capture.output(M,file = "M_best.doc")

pred_test<- predict(M,new=test)
pred_test
summary(pred_test)



RMSE(test$arrested,pred_test)
R2(test$arrested,pred_test)


# plot actual vs predicted
test %>% 
  ggplot(aes(pred_test,test$arrested)) +
  geom_point(alpha=0.75) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of Arrests') +
  ggtitle("Actual Arrests vs Predicted Arrests Using Subset Analysis")+
  ylab('Predicted value of Arrests') +
  theme_classic()+
  theme(legend.position="none")


# At the 0.05 level of significance, is there
# sufficient evidence to conclude that a difference in mean arrests exists among
# races?

racedf<-subset(df)
race<-racedf[, c('race', 'arrested')]  

# Alpha = 0.05%
 
# H0 = There is a difference in the mean arrests among races.
# Ha = There is no difference in the mean arrests among races.

anova1<-aov(arrested~race,data=race)
summary(anova1)

# Predicting Male or female involed in the incident using logistic regression

df$male<- ifelse (df$sex == "M",1,0)
df2 = subset(df, select = -sex)
set.seed(1234)
sample <- sample.split(df2$male, SplitRatio = 0.7)
train2  <- subset(df2, sample == TRUE)
test2   <- subset(df2, sample == FALSE)

# Logistic Regression Analysis

m<- glm(male~.,data = train2,family = "binomial")
summary(m)

m1<- glm(male~race+stopped+searched+arrested+frisked+summoned+contrabn+weapnfnd+pf_hcuff+cs_objcs+
           cs_descr+cs_casng+ cs_lkout+cs_cloth+cs_vcrim+cs_bulge+rf_vcrim+rf_othsw+rf_rfcmp+rf_verbl+
           rf_knowl+rf_furtv,data = train2,family = "binomial")
summary(m1)
stargazer(m1, type="html",out="models2.htm")

#Creating a Confusion Matrix for the train data set

p.train<-predict(m1,newdata= train2,type = "response")

pred.prob<-as.factor(ifelse(p.train>=0.5,1,0))

confusionMatrix(pred.prob,as.factor(train2$male),positive ="1")

#Creating a Confusion Matrix for the test data set

p.test<-predict(m1,newdata= test2,type = "response")

pred.prob_test<-as.factor(ifelse(p.test>=0.5,1,0))

confusionMatrix(pred.prob_test,as.factor(test2$male),positive = "1")

# Plotting the ROC curve 

roc_curve<-roc(test2$male,p.test)

plot(roc_curve,col = "red", main = "ROC Chart", ylab = "Sensitivity", xlab = "Specificity")

# Calculating AUC

auc_curve<-auc(roc_curve)
auc_curve


