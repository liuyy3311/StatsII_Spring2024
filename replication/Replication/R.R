library(stargazer)
library(broom)
#Loading in data
socpoc<-read.csv("E:/1-TCD/Statistic/replication/socpocAPSR.csv", stringsAsFactors = F)

#assign ownership groups
renters<-subset(socpoc, own==0)
owners<-subset(socpoc, own==1)


# Support for 10% supply
supply_7<-lm(city_supply ~ own +scale(ideology)+scale(log(income)) + whitenh  +age + male, subset(socpoc))
summary(supply_7)
# Support for Ban on Neighborhood Development-7
ban_7<-lm(neighborhood_ban ~ own +scale(ideology)+scale(log(income)) + whitenh  +age + male, subset(socpoc))
summary(ban_7)


stargazer(supply_7, ban_7, title="Support for 10% supply, Support for Ban on Neighborhood Development-7", label="neighborhood_ban_7",
           dep.var.caption = "",
          column.labels=c("Full","Full"),
          covariate.labels=c("Homeownership","Ideology","Income, Log","White, Non-Hispanic","Age","Male"),
          omit.stat = c("ser", "f"), digits=2, align=T, type="text",
          initial.zero = F,  font.size = "small",star.cutoffs = NA, omit.table.layout = "n",
          no.space=T, omit=c("name"))



#re-estiamte 
# Define rent categories based on specified thresholds
socpoc$rent_category <- cut(socpoc$zri_city,
                            breaks = c(-Inf, 1000, 2000, Inf),
                            labels = c("Low", "Medium", "High"),
                            include.lowest = TRUE)
# full
re_supply_7<-lm(city_supply ~ own +rent_category+scale(ideology)+scale(log(income)) + whitenh  +age + male, subset(socpoc))
summary(re_supply_7)
re_ban_7<-lm(neighborhood_ban ~  own +rent_category+scale(ideology)+ scale(log(income))+ whitenh  + age + male, subset(socpoc))
summary(re_ban_7)

#Table
stargazer(re_supply_7, re_ban_7, title="Support for 10% supply, Support for Ban on Neighborhood Development-High rent", label="neighborhood_ban_7",
          dep.var.caption = "",
          column.labels=c("Full","Full"),
          covariate.labels=c("Homeownership","Medium rents","High rents","Ideology","Income, Log","White, Non-Hispanic","Age","Male"),
          omit.stat = c("ser", "f"), digits=2, align=T, type="text",
          initial.zero = F,  font.size = "small",star.cutoffs = NA, omit.table.layout = "n",
          no.space=T, omit=c("name"))


