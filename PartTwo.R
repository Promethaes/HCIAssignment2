library(rstatix)
library(ez)
library(Rmisc)

sus = read.csv("SUS.csv")

# Normality test
sus %>%
  group_by(Order, Tool) %>%
  shapiro_test(Score) %>%
  print(n = Inf)

# Homogeneity test

lev1 = levene_test(Score ~ Order, data = sus); lev1
lev2 = levene_test(Score ~ Order*Tool, data = sus); lev2

# ANOVA tests

M_AnovaModel = ezANOVA(
  data = sus,
  dv = .(Score),
  wid = .(Participants),
  within = .(Tool),
  between =.(Order),
  detailed = T,
  type = 3
); M_AnovaModel

# Pairwise post-hoc tests

pairwise.t.test(sus$Score, sus$Order, paired = F, p.adjust.method = "bonferroni")
pairwise.t.test(sus$Score, sus$Tool,  paired = T, p.adjust.method = "bonferroni")
pairwise.t.test(
  sus$Score,
  interaction(sus$Order, sus$Tool),
  paired = T,
  p.adjust.method = "bonferroni"
)

# Interaction plot

interaction.plot(
  x.factor= sus$Order,
  trace.factor= sus$Tool,
  response = sus$Score,
  fun = mean,
  type = "b",
  legend = T,
  xlab= "Order",
  ylab="Score",
  trace.label="Tool",
  col = c("#0000FF", "#FF0000")
)

# Summaries

summarySE(sus, measurevar = "Score", groupvars = "Order")
summarySE(sus, measurevar = "Score", groupvars = "Tool")
summarySE(sus, measurevar = "Score", groupvars = c("Order", "Tool"))
