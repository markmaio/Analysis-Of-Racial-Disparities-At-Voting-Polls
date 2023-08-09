rm(list=ls())
library(dplyr)
library(ggplot2)
setwd("~/Desktop/Poli 281")
data <- read.csv("ces20.csv", stringsAsFactors = TRUE)

# section 2
table(data$wait)
prop.table(table(data$wait))
mean(data$wait)
median(data$wait)

data$more10<-data$wait>2 & data$wait<6

# section 3
library(dplyr)
library(ggplot2)

data_state<-data%>%
  group_by(state)%>%
  summarize(more10_prop = mean(more10))%>%
  arrange(desc(more10_prop))

data_state$state<-factor(data_state$state, levels = data_state$state)

data_state_p<-ggplot(data_state, aes(x=state, y=more10_prop, fill=state)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = 'none', 
        panel.background = element_rect(fill = 'white'))
data_state_p 

# section 4
data_state_region<-data%>%
  group_by(region, state)%>%
  summarize(more10_prop = mean(more10))%>%
  arrange(desc(more10_prop))

data_state_region$state<-factor(data_state_region$state, levels = data_state_region$state)

data_state_region_p<-ggplot(data_state_region, aes(x=state, y=more10_prop, fill=region)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = 'none', 
        panel.background = element_rect(fill = 'white'))
data_state_region_p 

# section 5
data$conserv_vote<-data$vote2020==1

wait_conserv<-data%>%
  group_by(conserv_vote)%>%
  summarize(more10_prop = mean(more10))
wait_conserv

# section 6
data$race_5<-data$race
data$race_5[data$race>4]<-'Other'
data$race_5[data$race==1]<-'White'
data$race_5[data$race==2]<-'Black'
data$race_5[data$race==3]<-'Hispanic'
data$race_5[data$race==4]<-'Asian'

data_race<-data%>%
  group_by(race_5)%>%
  summarize(more10_prop = mean(more10))%>%
  arrange(desc(more10_prop))

data_race$race_5<-factor(data_race$race_5, levels = data_race$race_5)

data_race_p<-ggplot(data_race, aes(x=race_5, y=more10_prop, fill=race_5)) + 
  geom_col() + 
  theme(legend.position = 'none', 
        panel.background = element_rect(fill = 'white'))
data_race_p 

# section 7
data$faminc_4<-""
data$faminc_4[data$faminc==97|is.na(data$faminc)]<-NA
data$faminc_4[data$faminc<5]<-"Less than $40k"
data$faminc_4[data$faminc>4 & data$faminc<9]<-"$40,000-$79,999"
data$faminc_4[data$faminc>8 & data$faminc<13]<-"$80,000-$199,999"
data$faminc_4[data$faminc>12 & data$faminc<97]<-"$200k and up"

data_income<-data%>%
  group_by(faminc_4)%>%
  summarize(more10_prop = mean(more10))%>%
  arrange(desc(more10_prop))

data_income$faminc_4<-factor(data_income$faminc_4, levels=data_income$faminc_4)

data_income_p<-ggplot(data_income, aes(x=faminc_4, y=more10_prop, fill=faminc_4)) + 
  geom_col() + 
  theme(legend.position = 'none', 
        panel.background = element_rect(fill = 'white'))
data_income_p 

# section 8
data_income_race<-data%>%
  group_by(race_5, faminc_4)%>%
  summarize(more10_prop = mean(more10))

data_income_race_p<-ggplot(data_income_race, aes(x=race_5, y=more10_prop)) + geom_col() + theme(panel.background = element_rect(fill = 'white')) + facet_wrap(~faminc_4)
data_income_race_p

# section 9
ggplot(data, aes(x=income_county))+geom_histogram()
quantile(data$income_county, probs=0.95)

data<-subset(data, income_county<=81.152)

data$density<-(data$county_pop/data$land_area)/1000

data$black<-0
data$black[data$race_5=='Black']<-1

data$hispanic<-0
data$hispanic[data$race_5=='Hispanic']<-1

data$asian<-0
data$asian[data$race_5=='Asian']<-1

data$other<-0
data$other[data$race_5=='Other']<-1

data$wait_reg<-data$wait
data$wait_reg[data$wait==6]<-NA

data$faminc_reg<-data$faminc
data$faminc_reg[data$faminc==97]<-NA

model1 <- lm(wait_reg ~ black + hispanic + asian + other, data = data)

model2 <- lm(wait_reg ~ black + hispanic + asian + other + faminc_reg + income_county + density, data = data)

summary(model1)
summary(model2)
