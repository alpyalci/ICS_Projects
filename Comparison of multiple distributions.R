library(ggplot2)
library(dplyr)
library(gridExtra)
library(stats)
library(formattable)
library(xtable)
setwd("~/Desktop/TU Dortmund/semester2/Introduction to Case Studies/Introductory Case Studies project 2/")
Height_Data <-
  as.data.frame(read.table("Height_Data.csv", header = TRUE, sep = ";"))
Height_Data$Sport <-
  factor(
    Height_Data$Sport,
    levels = c(
      "volleyball",
      "handball",
      "soccer",
      "basketball",
      "ice hockey",
      "water polo"
    )
  )
View(Height_Data)
str(Height_Data)
table(Height_Data$Sport)
#Boxplots, grouped by Sport
Height_Data %>%
  ggplot(aes(x = Sport, y = Height, color = Sport), na.rm = TRUE) +
  geom_boxplot() +
  ylim(168, 215) +
  labs(title = "Heights in different Sports") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Sport") +
  ylab("Height")
ggsave("Heights_of_different_Sports.pdf")
#Normality Assumption: QQ-plots
ggplot(Height_Data, aes(sample = Height, colour =
                          Sport)) +
  stat_qq() + facet_wrap( ~ Sport, nrow = 2) + stat_qq_line()
ggplot(Height_Data, aes(sample = Height)) +
  stat_qq() + stat_qq_line()
#Grouped statistical summary: include sample size!!!!
Grouped_Summary <-
  as.data.frame(Height_Data) %>%
  group_by(Sport) %>% summarise(
    "n"=length(Sport),
    "mean (Height)" = mean(Height),
    "Median (Height)" = median(Height),
    "SD (Height)" = sd(Height)
  )

Grouped_Summary
##Reformatting to text
R_Grouped_Summary <-
  formattable(Grouped_Summary, type = "text")

#Generating Latex code
xtable(R_Grouped_Summary)

#Task(1): ANOVA (Global Test)
Anova_Test <-
  aov(Height ~ Sport , data = Height_Data)
anova(Anova_Test)
xtable(Anova_Test)
#Task(2): Pairwise t-Test + Bonferroni
pairwise.t.test(Height_Data$Height, Height_Data$Sport,p.adj = "none")
pairwise.t.test(Height_Data$Height, Height_Data$Sport, p.adj = "bonf")

       