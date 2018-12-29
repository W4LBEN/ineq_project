#----------------------------INCOME AGGREGATIONS--------------------------------
#-------------------------------------------------------------------------------

# 1.0 P1 (EUROSTAT) - Whole Sample & Equal Share Of Ressources Within Household-

# 1.1 Pre Tax Factor Income (Canberra: Primery Income)--------------------------

# Sum up personal income
silc.rph <- silc.rph %>% 
  mutate(inc_p1 = py010g + py050g + py080g + py021g)

# Sum up personal income of household
silc.rph <- silc.rph %>% group_by(id_h) %>% 
  mutate(sum_inc_p1 = sum(inc_p1))

# Equivalised household income per person
silc.rph <- silc.rph %>% 
  mutate(inc_p1_1 = ((sum_inc_p1 + hy110g + hy040g + hy090g) / hx050))

# 1.2 Pre Tax National Income---------------------------------------------------

# Sum up personal income (unemployment benefits & old-age benefits)
silc.rph <- silc.rph %>% 
  mutate(inc_p2 = py090g + py100g)

# Sum up personal income of household
silc.rph <- silc.rph %>% group_by(id_h) %>%
  mutate(sum_inc_p2 = sum(inc_p2))

# Equivalised household income per person
silc.rph <- silc.rph %>%
  mutate(inc_p1_2 = inc_p1_1 + (sum_inc_p2 / hx050))

# 1.3 Post Tax Disposable Income------------------------------------------------

# Sum up personal income (received transfers p-file)
silc.rph <- silc.rph %>% 
  mutate(inc_p3 = py110g + py120g + py130g + py140g)

# Sum up personal income of household
silc.rph <- silc.rph %>% group_by(id_h) %>%
  mutate(sum_inc_p3 = sum(inc_p3))

# Equivalised household income per person
silc.rph <- silc.rph %>%
  mutate(inc_p1_3 = (inc_p1_2 + 
                    (sum_inc_p3 + hy050g + hy060g + hy070g + hy080g 
                      - hy120g - hy130g - hy140g) / hx050))


silc.rph$hy020/silc.rph$hx050



# 2.0 P2 (WID.WORLD) - Partial Sharing Of Ressources and only >= 20 years------- 

# n: remaining number of household members & only persons >= 20 years
silc.rph20 <- silc.rph %>% 
  filter(age >= 20) %>% add_count(id_h)

# 2.1 Pre Tax Factor Income-----------------------------------------------------
silc.rph20 <- silc.rph20 %>%
  mutate(inc_p2_1 = py010g + py021g + py050g + py080g + 
           (hy110g + hy040g + hy090g) / n)

# 2.2 Pre Tax National Income--------------------------------------------------
silc.rph20 <- silc.rph20 %>%
  mutate(inc_p2_2 = inc_p2_1 + py090g + py100g)

# 2.3 Post Tax Disposable Income------------------------------------------------
silc.rph20 <- silc.rph20 %>%
  mutate(inc_p2_3 = inc_p2_2 + py110g + py120g + py130g + py140g + 
           (hy050g + hy060g + hy070g + hy080g 
            - hy120g - hy130g - hy140g)/n)



# 3.0 For useful results -> subsetting -> restrict samples to positive incomes--

silc.pos.p1 <- silc.rph %>% filter(inc_p1_1 > 0, inc_p1_2 > 0, inc_p1_3 > 0)
silc.pos.p2 <- silc.rph20 %>% filter(inc_p2_1 > 0, inc_p2_2 > 0, inc_p2_3 > 0)   




