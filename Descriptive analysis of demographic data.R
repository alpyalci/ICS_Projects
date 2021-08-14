library(ggplot2)
library(skimr)
library(dplyr) 
library(formattable)
library(moments)
library(gridExtra)
library(stargazer)
library(knitr)
# Reading and viewing the data set
setwd("~/Desktop/Introductory Case Studies project 1/")
ics1 <- read.table("census_2020_2000.csv", header=TRUE, sep=",")
ics1<-as.data.frame(ics1)
#View(ics1)
head(ics1)
# Data Structure
str(ics1)
sapply(ics1, function(x) length(unique(x)))
# Renaming variables (columns names)
names(ics1)[c(1,7:10)] <- c("Country","TFR","LEB","LEM","LEF")
# Checking for NAs
ics1 %>% 
  filter(is.na(LEB) == TRUE |is.na(LEM) == TRUE | is.na(LEF) == TRUE | is.na(TFR) == TRUE) %>% 
  select(Country, Year)
colSums(is.na(ics1))
ics1 %>% 
  filter(is.na(GENC) == TRUE) %>% 
  select(Country, Year)
# Task (1)     (No NA in 2020 Data)
ics2020 <- ics1[ics1$Year=="2020",c(-2,-3)]
cont_ics2020 <-ics2020[,-(1:4)]
#Measures of Central Tendency & Spread 
summary(cont_ics2020)
stargazer(ics2020,type="text")
stargazer(ics2020)
skimr::partition(skim(cont_ics2020))
# Histograms to visualize Frequency Distributions 
## hist(ics2020$LEM, col="blue",, main="Life Expectancy at Birth for Males (LEM)", xlab = "Avg. no. of years", freq=FALSE)
## hist(ics2020$LEF, col="red",main="Life Expectancy at Birth for Females (LEF)",xlab = "Avg. no. of years", freq=FALSE)
## hist(ics2020$LEB,col="purple",main="Life Expectancy at Birth for Both Sexes (LEB)",xlab = "Avg. no. of years", freq=FALSE)
## hist(ics2020$TFR,main="Total Fertility Rate (TFR)",xlab = "Avg. no. of Children",freq=FALSE,col="darkgrey")

h1<-ggplot(ics2020, aes(ics2020[,5],)) + geom_histogram(aes(y=..density..),fill="lightblue")+xlab("Fertility Rate")
h2<-ggplot(ics2020, aes(ics2020[,6])) + geom_histogram(aes(y=..density..),fill="red")+xlab("Life Expectancy at Birth for Both Sexes")
h3<-ggplot(ics2020, aes(ics2020[,7])) + geom_histogram(aes(y=..density..),fill="purple")+xlab("Life Expectancy at Birth for Males")
h4<-ggplot(ics2020, aes(ics2020[,8])) + geom_histogram(aes(y=..density..))+xlab("Life Expectancy at Birth for Females")


grid.arrange(h1,h2,h3,h4, ncol=2)


geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~text)



with(data = ics2020,
     expr = c(LEM = skewness(x = LEM),
              LEF = skewness(x = LEF),
              LEB = skewness(x = LEB),
              TFR = skewness(x = TFR)))

#Mean of difference of Life Expectancy at Birth between Males & Females
ics2020_diff_SEX <- (ics2020$LEF - ics2020$LEM)
hist(ics2020_diff_SEX,main="Difference of Life Expectancy at Birth between Females & Males",xlab = "Avg. no. of years",freq=FALSE, breaks=10)
mean(ics2020_diff_SEX)
#Task2 
# 4x4 Scatterplot Matrix
pairs(ics2020[,5:8], 
      labels = c("Total Fertility Rate", "Life Expectancy - Both Sexes", "Life Expectancy - Males", "Life Expectancy - Females"),
      main = "Bivariate Correlations among numeric variables",
      upper.panel = NULL)      
# Pearson Correlation Table
db1<-ics2020 %>% select(LEB,TFR,LEM,LEF)
table_cor<-round(cor(db1, method="pearson"),
                 digits = 4)
db4<-as.data.frame(table_cor)
formattable(db4)
formattable(db4) %>% 
  kable("rst",escape = F, caption = "Correlation Table") 
# due to high correlation of LE between sexes, we will proceed with both sexes variable only
#Task3
# Boxplots to visualize homogeneity and heterogeneity
ics2020_grouped <- ics2020 %>% 
  mutate(Region = factor(Region), Subregion = as.factor(Subregion)) %>% 
  arrange(Region) %>% 
  transform(Subregion = reorder(Subregion, order(Region, Subregion))) 
ics2020_grouped %>%
  ggplot(aes(x=Subregion, y=LEB, fill=Region)) +
  geom_boxplot()+
  labs(x="Subregion",y="Life Expectancy at Birth for Both Sexes") +
  coord_flip()
ggsave("BOXPLOT_LifeExpectancy.PDF")
ics2020_grouped %>%
  ggplot(aes(x=Subregion, y=TFR, fill=Region)) +
  geom_boxplot()+
  labs(x="Subregion", y="Total Fertility Rate") +
  coord_flip()
ggsave("BOXPLOT_TotalFertilityRate.PDF")
#Grouped means & medians (Region-wise, Subregion-wise)
b1<-ics2020 %>%
  dplyr::group_by(Region) %>%
  skim(TFR)
b1[-13]
b2<-ics2020 %>%
  dplyr::group_by(Region) %>%
  skim(LEB)
b2[-13]
b3<-ics2020 %>%
  dplyr::group_by(Subregion) %>%
  skim(TFR)
b3[-13]
b4<-ics2020 %>%
  dplyr::group_by(Subregion) %>%
  skim(LEB)
b4
#Feel free to suggest other stuff to summarize
p20<-as.data.frame(ics2020) %>%
  group_by(Region) %>% summarise(mean_fertility=mean(TFR),mean_LifeExpectancy=mean(LEB),median_Fertility=median(TFR),median_LifeExpectancy=median(LEB) ,sd_fertility=sd(TFR),sd_LifeExpectancy=sd(LEB))
p21<-as.data.frame(ics2020) %>%
  group_by(Subregion) %>% summarise(mean_fertility=mean(TFR),mean_LifeExpectancy=mean(LEB),median_Fertility=median(TFR),median_LifeExpectancy=median(LEB) ,sd_fertility=sd(TFR),sd_LifeExpectancy=sd(LEB))

formattable(p20) %>% 
  kable("latex",escape = F, caption = "Regionswise Comparision") 
formattable(p21) %>% 
  kable("latex",escape = F, caption = "Subregionswise Comparision") 
?formattable
#Task4
ics2000 <- ics1[ics1$Year=="2000",c(-2,-3)]
cont_ics2000 <-ics2000[,-(1:4)]
Year_COMP_TFR_ics <- cbind(ics2000$TFR, ics2020$TFR)
Year_COMP_LE_ics <- cbind(ics2000$LEB,ics2020$LEB)
# Mean of Difference of Total Fertility Rate between years 2000 & 2020 (NAs are dropped)
ics_diff_Year_TFR <- (cont_ics2020$TFR - cont_ics2000$TFR)
mean(ics_diff_Year_TFR, na.rm=TRUE)
# Mean of Difference of Life Expectancy at Birth between years 2000 & 2020 (NAs are dropped)
ics_diff_Year_LEB <- (cont_ics2020$LEB - cont_ics2000$LEB)
mean(ics_diff_Year_LEB, na.rm=TRUE)
ics2000 %>%
  dplyr::group_by(Region) %>%
  skim(TFR)
ics2000 %>%
  dplyr::group_by(Region) %>%
  skim(LEB)
ics_task4 <- ics1 %>% 
  mutate(TFR2000 = ifelse(Year == 2000, TFR, 0), 
         TFR2020 = ifelse(Year == 2020, TFR, 0),
         LEB2000 = ifelse(Year == 2000, LEB, 0),
         LEB2020 = ifelse(Year == 2020, LEB, 0)) %>% 
  group_by(Country, Subregion, Region) %>% 
  summarise(TFR2000 = sum(TFR2000),
            TFR2020 = sum(TFR2020),
            LEB2000 = sum(LEB2000),
            LEB2020 = sum(LEB2020)) 
# Scatter plots to visualize the comparison of values: 2000 vs. 2020
ics_task4 %>% 
  ggplot(aes(x = TFR2000, y = TFR2020, color = Region), na.rm = TRUE) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "Grey")+
  expand_limits(x = 0, y = 0) +
  labs(title="Total Fertility Rate: 2000 vs. 2020")+
  xlab("2000")+
  ylab("2020")
ics_task4 %>% 
  ggplot(aes(x = LEB2000, y = LEB2020, color = Region), na.rm = TRUE) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "Grey")+
  expand_limits(x = 40, y = 40) +
  labs(title="Life Expectancy at Birth (Both Sexes): 2000 vs. 2020")+
  xlab("2000")+
  ylab("2020")


###citing packages
citation("dplyr")
citation("skimr")
citation("moments")
citation("formattable")
citation("gridExtra")
citation("ggplot2")
