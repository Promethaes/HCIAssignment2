library(rstatix)
library(ggplot2)
library(ggpubr)
library(rstatix)

set.seed(5)
my_data<-data.frame(immersion = c(rnorm(12,70,10),rnorm(12,75,10),rnorm(12,100,10)), group = gl(3,12, labels = c("sitting","standing","walking")))
my_data

#Test: One-Way ANOVA. Multiple independent groups, testing for significant differences between each

get_summary_stats(group_by(my_data,group))

#Find Outliers
identify_outliers(group_by(my_data,group),immersion)
#No Outliers, first assumption passed

#Test Normality
shapiro_test(group_by(my_data,group),immersion)
#P values are above 0.05, normality achieved

ggqqplot(my_data, "immersion", facet.by = "group")
#
stat.test<-aov(immersion ~ group, data = my_data) %>%tukey_hsd()
ggplot(my_data, aes(group, immersion)) + stat_summary(fun= mean, geom= "bar") + stat_summary(fun.data= mean_sd, geom= "errorbar", width = 0.2) + labs(x = "Group", y = "Immersion") +stat_pvalue_manual(stat.test, label = "p.adj.signif", y.position= c(120, 140, 130))

