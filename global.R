library(shinydashboard)
library(shiny)
library(googleVis)
library(DT)
library(tidyverse)
library(readr)
library(readxl)

#data

spending2017 <- read_excel("State_Chronic_Conditions_Spend_2017.xlsx", 
                           sheet = "ns", col_types = c("text", "skip", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", 
                                                       "numeric", "numeric", "numeric", "numeric","numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                       "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric","numeric", "numeric",
                                                       "numeric","numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric","numeric", "numeric", "numeric",
                                                       "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric",
                                                       "numeric"))  

spending2017<-spending2017 %>%select(State,Alcohol.Abuse.Spending:Stroke.Spending , everything())                                                       
names(spending2017)
s_spending2017<- spending2017[2:54,1:64]
head(s_spending2017)


df2007 <- read_excel("State_Chronic_Conditions_Spend_All/State_Table_Chronic_Conditions_Utiliz_Spend_2007.xlsx", sheet = "ns")
df2008 <- read_excel("State_Chronic_Conditions_Spend_All/State_Table_Chronic_Conditions_Utiliz_Spend_2008.xlsx", sheet = "ns")
df2009 <- read_excel("State_Chronic_Conditions_Spend_All/State_Table_Chronic_Conditions_Utiliz_Spend_2009.xlsx", sheet = "ns")
df2010 <- read_excel("State_Chronic_Conditions_Spend_All/State_Table_Chronic_Conditions_Utiliz_Spend_2010.xlsx", sheet = "ns")
df2011 <- read_excel("State_Chronic_Conditions_Spend_All/State_Table_Chronic_Conditions_Utiliz_Spend_2011.xlsx", sheet = "ns")
df2012 <- read_excel("State_Chronic_Conditions_Spend_All/State_Table_Chronic_Conditions_Utiliz_Spend_2012.xlsx", sheet = "ns")
df2013 <- read_excel("State_Chronic_Conditions_Spend_All/State_Table_Chronic_Conditions_Utiliz_Spend_2013.xlsx", sheet = "ns")
df2014 <- read_excel("State_Chronic_Conditions_Spend_All/State_Table_Chronic_Conditions_Utiliz_Spend_2014.xlsx", sheet = "ns")
df2015 <- read_excel("State_Chronic_Conditions_Spend_All/State_Table_Chronic_Conditions_Utiliz_Spend_2015.xlsx", sheet = "ns")
df2016 <- read_excel("State_Chronic_Conditions_Spend_All/State_Table_Chronic_Conditions_Utiliz_Spend_2016.xlsx", sheet = "ns")
df2017 <- read_excel("State_Chronic_Conditions_Spend_All/State_Table_Chronic_Conditions_Utiliz_Spend_2017.xlsx", sheet = "ns")

names(df2008)

#m<-rbind(df2007, df2008, df2009,df2010,df2011,df2012,df2013,df2014,df2015,df2016,df2017)

dc <- rbind(df2007, df2008, df2009, df2010, df2011, df2012, df2013, df2014, df2015, df2016, df2017)

dc1=gather(dc,'variables','value',3:65)

Dyads2007 <- read_excel("Clean Table_Chronic_Condition_Dyads_All_Beneficiaries_with_at_least_Two_Chronic_Conditions.xlsx",sheet = "2007", range = "A1:L211")      
Dyads2008 <- read_excel("Clean Table_Chronic_Condition_Dyads_All_Beneficiaries_with_at_least_Two_Chronic_Conditions.xlsx",sheet = "2008", range = "A1:L211") 
Dyads2009 <- read_excel("Clean Table_Chronic_Condition_Dyads_All_Beneficiaries_with_at_least_Two_Chronic_Conditions.xlsx",sheet = "2009", range = "A1:L211")
Dyads2010<- read_excel("Clean Table_Chronic_Condition_Dyads_All_Beneficiaries_with_at_least_Two_Chronic_Conditions.xlsx",sheet = "2010", range = "A1:L211")
Dyads2011 <- read_excel("Clean Table_Chronic_Condition_Dyads_All_Beneficiaries_with_at_least_Two_Chronic_Conditions.xlsx",sheet = "2011", range = "A1:L211")
Dyads2012 <- read_excel("Clean Table_Chronic_Condition_Dyads_All_Beneficiaries_with_at_least_Two_Chronic_Conditions.xlsx",sheet = "2012", range = "A1:L211")
Dyads2013 <- read_excel("Clean Table_Chronic_Condition_Dyads_All_Beneficiaries_with_at_least_Two_Chronic_Conditions.xlsx",sheet = "2013", range = "A1:L211")
Dyads2014 <- read_excel("Clean Table_Chronic_Condition_Dyads_All_Beneficiaries_with_at_least_Two_Chronic_Conditions.xlsx",sheet = "2014", range = "A1:L211")
Dyads2015 <- read_excel("Clean Table_Chronic_Condition_Dyads_All_Beneficiaries_with_at_least_Two_Chronic_Conditions.xlsx",sheet = "2015", range = "A1:L211")
Dyads2016 <- read_excel("Clean Table_Chronic_Condition_Dyads_All_Beneficiaries_with_at_least_Two_Chronic_Conditions.xlsx",sheet = "2016", range = "A1:L211")
Dyads2017 <- read_excel("Clean Table_Chronic_Condition_Dyads_All_Beneficiaries_with_at_least_Two_Chronic_Conditions.xlsx",sheet = "2017", range = "A1:L211")

Dyads= rbind(Dyads2007, Dyads2008, Dyads2009,Dyads2010 ,Dyads2011,Dyads2012,Dyads2013,Dyads2014,Dyads2015,Dyads2016,Dyads2017)

names(Dyads2007)

Dyads<-Dyads%>%select(Dyad.Label,Year, everything())
Dyads[[1]]<-str_replace_all(Dyads[[1]],c(" " = "." , "/" = "." , "'" = "."  ))
Dyads[1]
Dyads<-Dyads%>%select(Dyad.Label,Year, everything())
Dyads[[1]]<-str_replace_all(Dyads[[1]],c(" " = "." , "/" = "." , "'" = "."  ))
Dyads[1]
Dyads1 = gather(Dyads,'variables','value',3:12)
head(Dyads1)

State <- c(spending2017$State[1:54])
State



#selection options
#. drop-down by Allstate
State <- c(spending2017$State[1:54])
State

Years.to.Use <-c(2007:2017)
Years.to.Use
#. drop-down by costs,EDvisists and Hospitalreadimissions
Conditions.Costs.ER.Readmissions <-colnames(spending2017)[-1]
Conditions.Costs.ER.Readmissions

#. drop-down by Dyads
Dyads.Type <- c(Dyads$Dyad.Label)[1:210]
Dyads.Type

#. drop-down by costs or prevalence by Dyads
cost.prevalence <-colnames(Dyads2007)[2:11]
cost.prevalence  

# Percentage of Hospital readmissions
readmin2017 <- spending2017%>%select(State,Alcohol.Abuse.Readmissions)

# Percentage of Hospital readmissions
EDVisits2017 <- spending2017%>%select(everything())

# top 10 costly dyads
Top10Dyads <-Dyads2017%>%select(Dyad.Label,All.Beneficiaries.Spending)%>%
  arrange(desc(All.Beneficiaries.Spending))%>%
  top_n(10, All.Beneficiaries.Spending)


Low10Dyads <-Dyads2017%>%select(Dyad.Label,All.Beneficiaries.Spending)%>%
  arrange(All.Beneficiaries.Spending)

Low10Dyads<-head(Low10Dyads, 10)
Low10Dyads


Top10DyadsM <-Dyads2017%>%select(Dyad.Label,Males.Spending)%>%
  arrange(desc(Males.Spending))%>%
  top_n(10, Males.Spending)
Top10DyadsM

Top10DyadsF <-Dyads2017%>%select(Dyad.Label,Females.Spending)%>%
  arrange(desc(Females.Spending))%>%
  top_n(10, Females.Spending)
Top10DyadsF

Top10Dyadsls <-Dyads2017%>%select(Dyad.Label,Less.than.65.Years.Spending)%>%
  arrange(desc(Less.than.65.Years.Spending))%>%
  top_n(10, Less.than.65.Years.Spending)
Top10Dyadsls

Top10Dyadsalt <-Dyads2017%>%select(Dyad.Label,At.Least.65.Years.Spending)%>%
  arrange(desc(At.Least.65.Years.Spending))%>%
  top_n(10, At.Least.65.Years.Spending)
Top10Dyadsalt


#Top20Dyads[[1]]<-str_replace_all(Top20Dyads[[1]],c(" " = "." , "/" = "." , "'" = "."  ))
#Top20Dyads[[1]]

#dc1%>%filter(variables == input$selected)%>%
# ggplot(aes(x=Year,y=value))+geom_line()
#Dyads1%>%filter(variables == input$dyads)%>%
#  ggplot(aes(x=Year,y=value))+geom_line()


