################# Economics of Inequality#####################


library(dplyr)
library(survey)
library(convey)
library(tidyverse)

# 1.0 Setup Data Portugal-----------------------------------------------------------
  #Data Connection (!!DONT PUSH WITH PASSWORD INSIDE)

pg <- src_postgres(dbname="datacube", host="ineq.wu.ac.at", user='lvineq',
                   password = '', options="-c search_path=silc")

if(!exists("pg")) {
  
  if(!exists("password")) {password <- readline("Password:")}
  pg <- src_postgres(dbname = "datacube", host = "ineq.wu.ac.at",
                     user = "lvineq", 
                     password = password, 
                     options = "-c search_path=silc")
} else {
  message("Connection pg already exists.")
}

# 1.1 Download Data-------------------------------------------------------------

silc.p <- tbl(pg, "pp") %>%
  filter(pb020 == 'PT') %>%
  select(pb010, pb020, pb030, py010g, py020g, py050g, py080g, py090g,
         py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

silc.h <- tbl(pg, "hh") %>%
  filter(hb020 == 'PT') %>%
  select(hb010, hb020, hb030, hy010, hy020, hy030g, hy040g, hy050g, hy060g,
         hy070g, hy080g, hy090g, hy110g, hy120g, hy130g, hy140g, hx040,
         hx050) %>% 
  collect(n = Inf)

silc.r <- tbl(pg, "rr") %>% 
  filter(rb020 == 'PT') %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)


silc.d <- tbl(pg, "dd") %>%
  filter(db020 == 'PT') %>%
  dplyr:: select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

# 2.0 Company Car (PY021G)------------------------------------------------------
# Variable PY021G for the years 2007 - 2013 (no PY021G for previous years)

c07p <- tbl(pg, "c07p") %>% filter(pb020 == 'PT') %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c08p <- tbl(pg, "c08p") %>% filter(pb020 == 'PT') %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c09p <- tbl(pg, "c09p") %>% filter(pb020 == 'PT') %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c10p <- tbl(pg, "c10p") %>% filter(pb020 == 'PT') %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c11p <- tbl(pg, "c11p") %>% filter(pb020 == 'PT') %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c12p <- tbl(pg, "c12p") %>% filter(pb020 == 'PT') %>% 
  select(pb010, pb030,py021g) %>% collect(n = Inf)

c13p <- tbl(pg, "c13p") %>% filter(pb020 == 'PT') %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c14p <- tbl(pg, "c14p") %>% filter(pb020 == 'PT') %>% 
  select(pb010, pb020, pb030, py010g, py020g, py050g, py080g, py090g,
          py100g, py110g, py120g, py130g, py140g, px030, py021g) %>% 
  collect(n = Inf)

c15p <- tbl(pg, "c15p") %>% filter(pb020 == 'PT') %>% 
  select(pb010, pb020, pb030, py010g, py020g, py050g, py080g, py090g,
         py100g, py110g, py120g, py130g, py140g, px030, py021g) %>% 
  collect(n = Inf)

c16p <- tbl(pg, "c16p") %>% filter(pb020 == 'PT') %>% 
  select(pb010, pb020, pb030, py010g, py020g, py050g, py080g, py090g,
         py100g, py110g, py120g, py130g, py140g, px030, py021g) %>% 
  collect(n = Inf)

c17p <- tbl(pg, "c17p") %>% filter(pb020 == 'PT') %>% 
  select(pb010, pb020, pb030, py010g, py020g, py050g, py080g, py090g,
         py100g, py110g, py120g, py130g, py140g, px030, py021g) %>% 
  collect(n = Inf)


# merge c07p - c17p to variable cxxp
cxxp <- bind_rows(c07p, c08p, c09p, c10p, c11p, c12p, c13p, c14p, c15p,
                  c16p, c17p)

# now merge cxxp with silc.p 
silc.p <- full_join(silc.p, cxxp) 


# 3.0 Download Data for the years 2014 - 2017 
# P-File

#silc.p14 <- tbl(pg, "c14p") %>%
 # filter(pb020 == 'PT') %>%
  #select(pb010, pb020, pb030, py010g, py020g, py021g, py050g, py080g, 
   #      py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  #collect(n = Inf)

#silc.p15 <- tbl(pg, "c15p") %>%
 # filter(pb020 == 'PT') %>%
  #select(pb010, pb020, pb030, py010g, py020g, py021g, py050g, py080g, 
   #      py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  #collect(n = Inf)

#silc.p16 <- tbl(pg, "c16p") %>%
 # filter(pb020 == 'PT') %>%
  #select(pb010, pb020, pb030, py010g, py020g, py021g, py050g, py080g, 
   #      py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  #collect(n = Inf)

#silc.p17 <- tbl(pg, "c17p") %>%
 # filter(pb020 == 'PT') %>%
  #select(pb010, pb020, pb030, py010g, py020g, py021g, py050g, py080g, 
   #      py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  #collect(n = Inf)

# H-File

silc.h14 <- tbl(pg, "c14h") %>%
  filter(hb020 == 'PT') %>%
  select(hb010, hb020, hb030, hy010, hy020, hy030g, hy040g, 
         hy050g, hy060g, hy070g, hy080g, hy090g, hy110g, 
         hy120g, hy130g, hy140g, hx040, hx050) %>%
  collect(n = Inf)

silc.h15 <- tbl(pg, "c15h") %>%
  filter(hb020 == 'PT') %>%
  select(hb010, hb020, hb030, hy010, hy020, hy030g, hy040g, 
         hy050g, hy060g, hy070g, hy080g, hy090g, hy110g, 
         hy120g, hy130g, hy140g, hx040, hx050) %>%
  collect(n = Inf)

silc.h16 <- tbl(pg, "c16h") %>%
  filter(hb020 == 'PT') %>%
  select(hb010, hb020, hb030, hy010, hy020, hy030g, hy040g, 
         hy050g, hy060g, hy070g, hy080g, hy090g, hy110g, 
         hy120g, hy130g, hy140g, hx040, hx050) %>%
  collect(n = Inf)

silc.h17 <- tbl(pg, "c17h") %>%
  filter(hb020 == 'PT') %>%
  select(hb010, hb020, hb030, hy010, hy020, hy030g, hy040g, 
         hy050g, hy060g, hy070g, hy080g, hy090g, hy110g, 
         hy120g, hy130g, hy140g, hx040, hx050) %>%
  collect(n = Inf)

cxxh <- bind_rows(silc.h14, silc.h15, silc.h16, silc.h17)
silc.h <- full_join(silc.h, cxxh)

# R-File

silc.r14 <- tbl(pg, "c14r") %>%
  filter(rb020 == 'PT') %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)


silc.r15 <- tbl(pg, "c15r") %>%
  filter(rb020 == 'PT') %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)


silc.r16 <- tbl(pg, "c16r") %>%
  filter(rb020 == 'PT') %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)

silc.r17 <- tbl(pg, "c17r") %>%
  filter(rb020 == 'PT') %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)

cxxr <- bind_rows(silc.r14, silc.r15, silc.r16, silc.r17)
silc.r <- full_join(silc.r, cxxr)

# D-File

silc.d14 <- tbl(pg, "c14d") %>%  
  filter(db020 == 'PT') %>% 
  dplyr::select(db010, db020, db030, db040, db090) %>% 
  collect(n = Inf)

silc.d15 <- tbl(pg, "c15d") %>% 
  filter(db020 == 'PT') %>%
  dplyr::select(db010, db020, db030, db040, db090) %>% 
  collect(n = Inf)

silc.d16 <- tbl(pg, "c16d") %>% 
  filter(db020 == 'PT') %>%
  dplyr::select(db010, db020, db030, db040, db090) %>% 
  collect(n = Inf)

silc.d17 <- tbl(pg, "c16d") %>% 
  filter(db020 == 'PT') %>%
  dplyr::select(db010, db020, db030, db040, db090) %>% 
  collect(n = Inf)

cxxd <- bind_rows(silc.d14, silc.d15, silc.d16, silc.d17)
silc.d <- full_join(silc.d, cxxd)


# Exclude observations with personal id = NA in p and r-file
#silc.p <- silc.p %>% drop_na(pb030)
#silc.r <- silc.r %>% drop_na(rb030)


# generate car variable 
silc.p$car <- silc.p$py020g
silc.p$car <- ifelse(silc.p$pb010 > 2006, silc.p$py021g, silc.p$car)

# 4.0 Merge and rename variables of dataset-------------------------------------

# 4.1 Rename rb030, pb030 to id_p
silc.r <- silc.r %>% rename(id_p = rb030)
silc.p <- silc.p %>% rename(id_p = pb030)
silc.d <- silc.d %>% rename(id_p = db030)

# 4.2 merge silc.r and silc.p
silc.rp <- left_join(silc.r, silc.p)

# 4.3 Create age, household ID, gender variables
silc.rp <- silc.rp %>% 
  mutate(age = rb010 - rb080,
         gender = factor(rb090, labels = c('Male','Female')),
         id_h = paste0(rb010, rx030)) 

# 4.4 Create unique IDs for merging, merge country and household ID h & d
silc.h <- silc.h %>% mutate(id_h = paste0(hb010, hb030))

#silc.d <- silc.d %>% mutate(id_h = paste0(db010, db030))

# 4.5 Merge silc.rp and silc.h
silc.rph <- full_join(silc.rp, silc.h, by = c("id_h", "rb010" = "hb010", 
                                              "rx030" = "hb030"))

# 4.6 Convert NA's in silc.rph to 0 for aggregation issues 
silc.rph[is.na(silc.rph)] <- 0









