#-----------------------------------PLOTS---------------------------------------
#-------------------------------------------------------------------------------

library(ggplot2)
library(scales)


# 1.0 Plots
# 1.1 Mean Income Plot ( P1: Post Tax Disposable Income)

mn_p13 <- mean_p13_all %>% rename(Mittelwert = statistic)

plotmean_p13 <-ggplot(mn_p13, aes(x=rb010)) +
  geom_line(aes(y=Mittelwert, col = "Mittelwert"))  +
  labs(x = "Jahr", 
       y = "Post-tax disposable income",
       title = "Mittleres Einkommen") +
  
  theme_minimal() 

plotmean_p13

# 1.2 Median Income Plot (P1: Post Tax Disposable Income)

md_p13 <- median_p13_all %>% rename(Median = statistic)

plotmedian_p13 <-ggplot(md_p13, aes(x=rb010)) +
  geom_line(aes(y=Median, col = "Median"))  +
  labs(x = "Jahr", 
       y = "Post-tax disposable income",
       title = "Median Einkommen") +
  
  theme_minimal() 

plotmedian_p13

# 1.3 Gini Plot (P1: Post Tax Disposable Income)

gi_p13 <- gini_p13_all %>% rename(Gini = statistic)

plotgini_p13<-ggplot(gi_p13, aes(x=rb010)) +
  geom_line(aes(y=Gini, col = "Gini"))  +
  labs(x = "Jahr", 
       y = "Post-tax disposable income",
       title = "Gini") +
  
  theme_minimal() 

plotgini_p13

# 1.4 Top 10% Plot (P1: Post Tax Disposable Income)

tp10_p11 <- top10_p11_all %>% mutate(rb010 = 2004:2017)
tp10_p11 <- tp10_p11 %>% rename(Top10 = inc_p1_1)

plottop10_p11 <- ggplot(tp10_p11, aes(x=rb010)) + 
  geom_line(aes(y=Top10, col = "Anteil Top 10%")) + 
  labs(title="Anteil der Top 10% am Gesamtbruttoeinkommen",
       y ="Anteil am Gesamteinkommen in %",
       x = "Jahre",
       col="Legende") +
  
  theme_classic()

plottop10_p11

top10_p11_all

# 1.5 Poverty Rate Plot

arop <- data.frame(pr_p1$value, pr_p1$threshold)
arop <- arop %>% mutate (rb010 = 2006:2017)

arop <- arop %>% rename(Top10 = inc_p1_1)

plotarop <- ggplot(arop, aes(x=rb010)) + 
  geom_line(aes(y=arop$value, col = "Armutsgefaehrdung")) + 
  labs(y = "Armutsgefaehrdung in %",
       x = "Jahre",
       col ="Legende",
       title ="Armutsgefaehrdungsquote") +
  scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014, 2016, 2017)) +
  scale_y_continuous(breaks=c(10, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15))

plotarop

pr_p1
