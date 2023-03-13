# question 1: Present a bar graph showing the proportion of states with a change in their cigarette tax in each year from 1970 to 1985.

cig_prop <- subset(final.data, Year >= 1970 & Year <= 1985) %>% 
  arrange(state, Year) %>% 
  group_by(state) %>% 
  mutate(cost_change = tax_state - lag(tax_state, default = first(tax_state))) %>%
  group_by(Year) %>% 
  summarise(prop_change = sum(cost_change != 0) / n_distinct(state))
cigPropPlot<-ggplot(cig_prop, aes(x = Year, y = prop_change)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Year", y = "Proportion of States that Change Taxes", title = "Changes in Cost Proportions by Year") + ylim(0,1)
cigPropPlot
hw3Q1<-ggsave(filename="cigProp.png",cigPropPlot,height=10,width=10)

#2 Plot on a single graph the average tax (in 2012 dollars) on cigarettes and the average price of a pack of cigarettes from 1970 to 2018.
cig_avg <- final.data %>% 
  group_by(Year) %>% 
  summarise(avg_tax = mean(tax_dollar),avg_cost = mean(cost_per_pack))

cigCost<-ggplot(cig_avg,aes(x=Year))+
  geom_line(aes(y=avg_tax,color='Average Tax'))+
  geom_line(aes(y=avg_cost,color='Average Cigarette Cost'))+
  ggtitle("Average tax and cost per year")+
  xlab("Year")+
  ylab("Price")
cigCost  
hw3Q2<-ggsave(filename="avgTaxAndCost.png",cigCost,width=10)

#problem 3

cig_diff <- final.data %>% 
  group_by(state) %>% 
  summarise(diff_cost = last(cost_per_pack) - first(cost_per_pack))

# Sort the states based on the difference in costs and display the top 3 states
top_states <- cig_diff %>% 
  arrange(desc(diff_cost)) %>% 
  head(5)

topStatesDF<-subset(final.data,state=="New York"|state=="District of Columbia"|state=="Connecticut"|state=="Rhode Island"|state=="Massachusetts")
topStatesDF<-topStatesDF %>%
  group_by(state)
cigPerCapita<-ggplot(topStatesDF,aes(x=Year,y=sales_per_capita,color=state,group=state))+
  geom_line()+
  ggtitle("Sales per capita for the 5 highest change in cost states")+
  xlab("Year")+
  ylab("Sales")
cigPerCapita
hw3Q3<-ggsave(filename="topStatesSales.png",cigPerCapita,width=10)
#4

cig_diff <- final.data %>% 
  group_by(state) %>% 
  summarise(diff_cost = last(cost_per_pack) - first(cost_per_pack))

# Sort the states based on the difference in costs and display the top 3 states
bot_states <- cig_diff %>% 
  arrange((diff_cost)) %>% 
  head(5)

botStatesDF<-subset(final.data,state=="Missouri"|state=="Georgia"|state=="North Dakota"|state=="North Carolina"|state=="Tennessee")
botStatesDF<-botStatesDF %>%
  group_by(state)
botCigPerCapita<-ggplot(botStatesDF,aes(x=Year,y=sales_per_capita,color=state,group=state))+
  geom_line()+
  ggtitle("Sales per capita for the 5 lowest change in cost states")+
  xlab("Year")+
  ylab("Sales")
botCigPerCapita
hw3Q4<-ggsave(filename="botStatesSales.png",botCigPerCapita,width=10)

#5

#Both groups of states, those with higher changes in costs and lower, saw a decrease in sales per capita of cigarettes over the years. They both start at similar values but near the end, the sales per capita for the states with less change saw also a higher sales per capita at around 50-75. Those with higher changes of cigarette cost saw a larger decrease with the sales per capita ending around 25.

#ESTIMATING ATEs 6

prob6<-subset(final.data, Year >= 1970 & Year <= 1990)
regres6<-lm(log(sales_per_capita)~log(cost_per_pack),data=prob6)
summary(regres6)
# This shows that as the log of the price of a pack of cigarettes increases, the log of the sales per capita of cigarette packs decreases by .17 packs per capita. This would mean that cigarettes are an elastic good.

#7
library(AER)
prob6$totalTax<-prob6$tax_dollar+prob6$tax_state
regres7<-ivreg(log(sales_per_capita)~log(cost_per_pack) |totalTax ,data=prob6)
summary(regres7)
# These results are similar to the previous, except the effect seems to be stronger when using an instrumental variable. Both coefficients are negative and are statistically significant in both estimates, but when using an IV we see a stronger effect. This makes sense as it takes into account a difference in something external affecting price that does not directly affect a change in consumption.

#8

firstStage1<-lm(cost_per_pack~totalTax,data=prob6)
summary(firstStage1)
reducedForm1<-lm(sales_per_capita~totalTax,data=prob6)
summary(reducedForm1)
#9

prob9<-subset(final.data, Year >= 1991 & Year <= 2015)
regres9<-lm(log(sales_per_capita)~log(cost_per_pack),data=prob9)
summary(regres9)
# This shows that as the log of the price of a pack of cigarettes increases, the log of the sales per capita of cigarette packs decreases by .66 packs per capita. This would mean that cigarettes are an elastic good.


prob9$totalTax<-prob9$tax_dollar+prob9$tax_state
regres9.2<-ivreg(log(sales_per_capita)~log(cost_per_pack) |totalTax ,data=prob9)
summary(regres9.2)
# These results are similar to the previous, except the effect seems to be stronger when using an instrumental variable. Both coefficients are negative and are statistically significant in both estimates, but when using an IV we see a stronger effect. This makes sense as it takes into account a difference in something external affecting price that does not directly affect a change in consumption.



firstStage2<-lm(cost_per_pack~totalTax,data=prob9)
summary(firstStage2)
reducedForm2<-lm(sales_per_capita~totalTax,data=prob9)
summary(reducedForm2)

save.image("homework3Workspace.Rdata")
