#-----------------------------INDICATORS----------------------------------------
#-------------------------------------------------------------------------------

library(survey)
library(convey)
library(srvyr)

# Creating Survey Objects-------------------------------------------------------

svy.p1 <- svydesign(ids =  ~ id_h,
                    strata = ~rb020,
                    weights = ~rb050,
                    data = silc.pos.p1) %>% convey_prep()

svy.p2 <- svydesign(ids =  ~ id_h, 
                    strata = ~rb020,
                    weights = ~rb050,
                    data = silc.pos.p2) %>% convey_prep()


# 1.0 Indicators for P1 (Eurostat)----------------------------------------------

# 1.1 Indicators For Pre Tax Factor Income (Canberra: Primary Income)-----------

# 1.1.1 Mean
mean_p11 <- svymean(~inc_p1_1,svy.p1)
mean_p11

mean_p11_all <- svyby(~inc_p1_1, ~rb010, svy.p1,
                       svymean, keep.var = FALSE)
mean_p11_all


# 1.1.2 Median
median_p11 <- svyquantile(~inc_p1_1, svy.p1, quantile=c(0.5))
median_p11

median_p11_all <- svyby(~inc_p1_1, ~rb010, svy.p1,
                          svyquantile, quantile=c(0.5), keep.var = FALSE)
median_p11_all

# 1.1.3 Gini
gini_p11 <- svygini(~inc_p1_1, svy.p1)
gini_p11

gini_p11_all <- svyby(~inc_p1_1, ~rb010, svy.p1,
                      svygini, keep.var = FALSE)
gini_p11_all

# 1.1.4 P80/P20-Ratio
p80p20_p11 <- svyqsr(~inc_p1_1,svy.p1)
p80p20_p11

p80p20_p11_all <- svyby(~inc_p1_1, ~rb010, svy.p1,
                      svyqsr, keep.var = FALSE)
p80p20_p11_all

#1.1.5 Top 10% share
top10_p11 <- svytotal(~inc_p1_1,subset(svy.p1, inc_p1_1>=
            as.numeric(svyquantile(~inc_p1_1, svy.p1, quantile=c(0.9)))),
                       na.rm=TRUE)/svytotal(~inc_p1_1, svy.p1 ,na.rm=TRUE)
top10_p11

svy.p11.top <- subset(svy.p1, inc_p1_1 >= as.numeric(
  svyquantile(~inc_p1_1, svy.p1, quantile=c(0.9))))

top0.1_p11 <- svyby(~inc_p1_1, ~rb010, svy.p11.top, svytotal)

topall_p11 <- svyby(~inc_p1_1, ~rb010, svy.p1, svytotal)

top10_p11_all <- top0.1_p11 / topall_p11
top10_p11_all


# 1.2 Indicators For Pre Tax National Income------------------------------------

# 1.2.1 Mean
mean_p12 <- svymean(~inc_p1_2,svy.p1)
mean_p12

mean_p12_all <- svyby(~inc_p1_2, ~rb010, svy.p1,
                      svymean, keep.var = FALSE)
mean_p12_all


# 1.2.2 Median
median_p12 <- svyquantile(~inc_p1_2, svy.p1, quantile=c(0.5))
median_p12

median_p12_all <- svyby(~inc_p1_2, ~rb010, svy.p1,
                        svyquantile, quantile=c(0.5), keep.var = FALSE)
median_p12_all

# 1.2.3 Gini
gini_p12 <- svygini(~inc_p1_2, svy.p1)
gini_p12

gini_p12_all <- svyby(~inc_p1_2, ~rb010, svy.p1,
                      svygini, keep.var = FALSE)
gini_p12_all

# 1.2.4 P80/P20-Ratio
p80p20_p12 <- svyqsr(~inc_p1_2,svy.p1)
p80p20_p12

p80p20_p12_all <- svyby(~inc_p1_2, ~rb010, svy.p1,
                        svyqsr, keep.var = FALSE)
p80p20_p12_all

#1.2.5 Top 10% share
top10_p12 <- svytotal(~inc_p1_2,subset(svy.p1, inc_p1_2>=
              as.numeric(svyquantile(~inc_p1_2, svy.p1, quantile=c(0.9)))),
                      na.rm=TRUE)/svytotal(~inc_p1_2, svy.p1 ,na.rm=TRUE)
top10_p12

svy.p12.top <- subset(svy.p1, inc_p1_2 >= as.numeric(
  svyquantile(~inc_p1_2, svy.p1, quantile=c(0.9))))

top0.1_p12 <- svyby(~inc_p1_2, ~rb010, svy.p12.top, svytotal)

topall_p12 <- svyby(~inc_p1_2, ~rb010, svy.p1, svytotal)

top10_p12_all <- top0.1_p12 / topall_p12
top10_p12_all

# 1.3 Indicators For Post Tax Disposable Income---------------------------------

# 1.3.1 Mean
mean_p13 <- svymean(~inc_p1_3,svy.p1)
mean_p13

mean_p13_all <- svyby(~inc_p1_3, ~rb010, svy.p1,
                      svymean, keep.var = FALSE)
mean_p13_all


# 1.3.2 Median
median_p13 <- svyquantile(~inc_p1_3, svy.p1, quantile=c(0.5))
median_p13

median_p13_all <- svyby(~inc_p1_3, ~rb010, svy.p1,
                        svyquantile, quantile=c(0.5), keep.var = FALSE)
median_p13_all

# 1.3.3 Gini
gini_p13 <- svygini(~inc_p1_3, svy.p1)
gini_p13

gini_p13_all <- svyby(~inc_p1_3, ~rb010, svy.p1,
                      svygini, keep.var = FALSE)
gini_p13_all


# 1.3.4 P80/P20-Ratio
p80p20_p13 <- svyqsr(~inc_p1_3,svy.p1)
p80p20_p13

p80p20_p13_all <- svyby(~inc_p1_3, ~rb010, svy.p1,
                        svyqsr, keep.var = FALSE)
p80p20_p13_all

#1.3.5 Top 10% share
top10_p13 <- svytotal(~inc_p1_3,subset(svy.p1, inc_p1_1>=
                as.numeric(svyquantile(~inc_p1_3, svy.p1, quantile=c(0.9)))),
                      na.rm=TRUE)/svytotal(~inc_p1_3, svy.p1 ,na.rm=TRUE)
top10_p13

svy.p13.top <- subset(svy.p1, inc_p1_3 >= as.numeric(
  svyquantile(~inc_p1_3, svy.p1, quantile=c(0.9))))

top0.1_p13 <- svyby(~inc_p1_3, ~rb010, svy.p13.top, svytotal)

topall_p13 <- svyby(~inc_p1_3, ~rb010, svy.p1, svytotal)

top10_p13_all <- top0.1_p13 / topall_p13
top10_p13_all


# 2.0 Indicators for P2 (wid.world)---------------------------------------------

# 2.1 Indicators For Pre Tax Factor Income (Canberra: Primary Income)-----------

# 2.1.1 Mean
mean_p21 <- svymean(~inc_p2_1,svy.p2)
mean_p21

mean_p21_all <- svyby(~inc_p2_1, ~rb010, svy.p2,
                      svymean, keep.var = FALSE)
mean_p21_all


# 2.1.2 Median
median_p21 <- svyquantile(~inc_p2_1, svy.p2, quantile=c(0.5))
median_p21

median_p21_all <- svyby(~inc_p2_1, ~rb010, svy.p2,
                        svyquantile, quantile=c(0.5), keep.var = FALSE)
median_p21_all

# 2.1.3 Gini
gini_p21 <- svygini(~inc_p2_1, svy.p2)
gini_p21

gini_p21_all <- svyby(~inc_p2_1, ~rb010, svy.p2,
                      svygini, keep.var = FALSE)
gini_p21_all

# 2.1.4 P80/P20-Ratio
p80p20_p21 <- svyqsr(~inc_p2_1,svy.p2)
p80p20_p21

p80p20_p21_all <- svyby(~inc_p2_1, ~rb010, svy.p2,
                        svyqsr, keep.var = FALSE)
p80p20_p21_all

#1.1.5 Top 10% share
top10_p21 <- svytotal(~inc_p2_1,subset(svy.p2, inc_p2_1>=
                as.numeric(svyquantile(~inc_p2_1, svy.p2, quantile=c(0.9)))),
                      na.rm=TRUE)/svytotal(~inc_p2_1, svy.p2 ,na.rm=TRUE)
top10_p21

svy.p21.top <- subset(svy.p2, inc_p2_1 >= as.numeric(
  svyquantile(~inc_p2_1, svy.p2, quantile=c(0.9))))

top0.1_p21 <- svyby(~inc_p2_1, ~rb010, svy.p21.top, svytotal)

topall_p21 <- svyby(~inc_p2_1, ~rb010, svy.p2, svytotal)

top10_p21_all <- top0.1_p21 / topall_p21
top10_p21_all


# 2.2 Indicators For Pre Tax National Income------------------------------------

# 2.2.1 Mean
mean_p22 <- svymean(~inc_p2_2,svy.p2)
mean_p22

mean_p22_all <- svyby(~inc_p2_2, ~rb010, svy.p2,
                      svymean, keep.var = FALSE)
mean_p22_all


# 2.2.2 Median
median_p22 <- svyquantile(~inc_p2_2, svy.p2, quantile=c(0.5))
median_p22

median_p22_all <- svyby(~inc_p2_2, ~rb010, svy.p2,
                        svyquantile, quantile=c(0.5), keep.var = FALSE)
median_p22_all

# 2.2.3 Gini
gini_p22 <- svygini(~inc_p2_2, svy.p2)
gini_p22

gini_p22_all <- svyby(~inc_p2_2, ~rb010, svy.p2,
                      svygini, keep.var = FALSE)
gini_p22_all

# 2.2.4 P80/P20-Ratio
p80p20_p22 <- svyqsr(~inc_p2_2,svy.p2)
p80p20_p22

p80p20_p22_all <- svyby(~inc_p2_2, ~rb010, svy.p2,
                        svyqsr, keep.var = FALSE)
p80p20_p22_all

#2.2.5 Top 10% share
top10_p22 <- svytotal(~inc_p2_2,subset(svy.p2, inc_p2_2>=
                as.numeric(svyquantile(~inc_p2_2, svy.p2, quantile=c(0.9)))),
                      na.rm=TRUE)/svytotal(~inc_p2_2, svy.p2 ,na.rm=TRUE)
top10_p22

svy.p22.top <- subset(svy.p2, inc_p2_2 >= as.numeric(
  svyquantile(~inc_p2_2, svy.p2, quantile=c(0.9))))

top0.1_p22 <- svyby(~inc_p2_2, ~rb010, svy.p22.top, svytotal)

topall_p22 <- svyby(~inc_p2_2, ~rb010, svy.p2, svytotal)

top10_p22_all <- top0.1_p22 / topall_p22
top10_p22_all


# 2.3 Indicators For Post Tax Disposable Income---------------------------------

# 2.3.1 Mean
mean_p23 <- svymean(~inc_p2_3,svy.p2)
mean_p23

mean_p23_all <- svyby(~inc_p2_3, ~rb010, svy.p2,
                      svymean, keep.var = FALSE)
mean_p23_all


# 2.3.2 Median
median_p23 <- svyquantile(~inc_p2_3, svy.p2, quantile=c(0.5))
median_p23

median_p23_all <- svyby(~inc_p2_3, ~rb010, svy.p2,
                        svyquantile, quantile=c(0.5), keep.var = FALSE)
median_p23_all

# 2.3.3 Gini
gini_p23 <- svygini(~inc_p2_3, svy.p2)
gini_p23

gini_p23_all <- svyby(~inc_p2_3, ~rb010, svy.p2,
                      svygini, keep.var = FALSE)
gini_p23_all

# 2.3.4 P80/P20-Ratio
p80p20_p23 <- svyqsr(~inc_p2_3,svy.p2)
p80p20_p23

p80p20_p23_all <- svyby(~inc_p2_3, ~rb010, svy.p2,
                        svyqsr, keep.var = FALSE)
p80p20_p23_all

#2.3.5 Top 10% share
top10_p23 <- svytotal(~inc_p2_3,subset(svy.p2, inc_p2_3>=
              as.numeric(svyquantile(~inc_p2_3, svy.p2, quantile=c(0.9)))),
                      na.rm=TRUE)/svytotal(~inc_p2_3, svy.p2 ,na.rm=TRUE)
top10_p23

svy.p23.top <- subset(svy.p2, inc_p2_3 >= as.numeric(
  svyquantile(~inc_p2_3, svy.p2, quantile=c(0.9))))

top0.1_p23 <- svyby(~inc_p2_3, ~rb010, svy.p23.top, svytotal)

topall_p23 <- svyby(~inc_p2_3, ~rb010, svy.p2, svytotal)

top10_p23_all <- top0.1_p23 / topall_p23
top10_p23_all


## 3.0 Poverty Rate - At risk of poverty (60% of mean income)

library(laeken)

pr_p1_af <- arpr(silc.pos.p1$inc_p1_3, weights=silc.pos.p1$rb050, 
           years=silc.pos.p1$rb010)
pr_p1_af

pr_p1_bef <- arpr(silc.pos.p1$inc_p1_1, weights=silc.pos.p1$rb050, 
                 years=silc.pos.p1$rb010)
pr_p1_bef


#arop_test <-arpr(silc.rph$hy020, weights=silc.rph$rb050, 
                 #years=silc.rph$rb010)

#arop_test

pr_p2 <- arpr(silc.pos.p2$inc_p2_3, weights=silc.pos.p2$rb050, 
            years=silc.pos.p2$rb010)
pr_p2

# 3.1 Poverty Rate - include different ages 


data_pr1 <- silc.pos.p1 %>% subset(age<18)
data_pr2 <- silc.pos.p1 %>% subset(age >= 18 & age <= 64)
data_pr3 <- silc.pos.p1 %>% subset(age>64)


# after transfers 
# 3.1.1 At risk of poverty rate: Ages 0-17 
arpr_pr1 <- arpr(data_pr1$inc_p1_3, weights=data_pr1$rb050, 
                 years=data_pr1$rb010)
arpr_pr1

# 3.1.2 At risk of poverty rate: Ages 18-64
arpr_pr2 <- arpr(data_pr2$inc_p1_3, weights=data_pr2$rb050, 
                 years=data_pr2$rb010)
arpr_pr2

# 3.1.3 At risk of poverty rate: Ages 65+
arpr_pr3 <- arpr(data_pr3$inc_p1_3, weights=data_pr3$rb050, 
                 years=data_pr3$rb010)
arpr_pr3


#before transfers
# 3.2.1 At risk of poverty rate: Ages 0-17 
arpr_pr1_bef <- arpr(data_pr1$inc_p1_1, weights=data_pr1$rb050, 
                 years=data_pr1$rb010)
arpr_pr1_bef

# 3.2.2 At risk of poverty rate: Ages 18-64
arpr_pr2_bef <- arpr(data_pr2$inc_p1_1, weights=data_pr2$rb050, 
                 years=data_pr2$rb010)
arpr_pr2_bef

# 3.2.3 At risk of poverty rate: Ages 65+
arpr_pr3_bef <- arpr(data_pr3$inc_p1_1, weights=data_pr3$rb050, 
                 years=data_pr3$rb010)
arpr_pr3_bef




