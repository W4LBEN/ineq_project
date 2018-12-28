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
  select(pb010, pb020, pb030, py010g, py020g, py050g, py080g, py090g, py100g, py110g,
         py120g, py130g, py140g, px030) %>%
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
  select(db010, db020, db030, db040, db090) %>%
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

# merge c07p - c13p to variable cxxp
cxxp <- bind_rows(c07p, c08p, c09p, c10p, c11p, c12p, c13p)

# now merge cxxp with silc.p 
silc.p <- left_join(silc.p, cxxp %>% select(py021g, pb010, pb030))


# 3.0 Download Data for the years 2014 - 2017 
# P-File

silc.p14 <- tbl(pg, "c14p") %>%
  filter(pb020 == 'PT') %>%
  select(pb010, pb020, pb030, py010g, py020g, py021g, py050g, py080g, 
         py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

silc.p15 <- tbl(pg, "c15p") %>%
  filter(pb020 == 'PT') %>%
  select(pb010, pb020, pb030, py010g, py020g, py021g, py050g, py080g, 
         py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

silc.p16 <- tbl(pg, "c16p") %>%
  filter(pb020 == 'PT') %>%
  select(pb010, pb020, pb030, py010g, py020g, py021g, py050g, py080g, 
         py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

silc.p17 <- tbl(pg, "c17p") %>%
  filter(pb020 == 'PT') %>%
  select(pb010, pb020, pb030, py010g, py020g, py021g, py050g, py080g, 
         py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

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

# D-File

silc.d14 <- tbl(pg, "c14d") %>%  
  filter(db020 == 'PT') %>% 
  select(db010, db020, db030, db040, db090) %>% 
  collect(n = Inf)

silc.d15 <- tbl(pg, "c15d") %>% 
  filter(db020 == 'PT') %>%
  select(db010, db020, db030, db040, db090) %>% 
  collect(n = Inf)

silc.d16 <- tbl(pg, "c16d") %>% 
  filter(db020 == 'PT') %>%
  select(db010, db020, db030, db040, db090) %>% 
  collect(n = Inf)

silc.d17 <- tbl(pg, "c16d") %>% 
  filter(db020 == 'PT') %>%
  select(db010, db020, db030, db040, db090) %>% 
  collect(n = Inf)

# Merge Datasets ( - 2013) & (2014 - 2017)
silc.p <- bind_rows(silc.p, silc.p14, silc.p15, silc.p16, silc.p17)
silc.h <- bind_rows(silc.h, silc.h14, silc.h15, silc.h16, silc.h17)
silc.r <- bind_rows(silc.r, silc.r14, silc.r15, silc.r16, silc.r17)
silc.d <- bind_rows(silc.d, silc.d14, silc.d15, silc.d16, silc.d17)

# 4.0 Merge and rename variables of dataset
# 4.1 Rename rb030 & pb030 to id_p
silc.r <- silc.r %>% rename(personal_id = rb030)
silc.p <- silc.p %>% rename(personal_id = pb030)

# 4.2 Merge silc.r with silc.p
silc.rp <- left_join(silc.r, silc.p)

# 4.3 Create age, household ID, gender variables
silc.rp <- silc.rp %>% 
  mutate(age = rb010 - rb080,
         gender = factor(rb090, labels = c('Male','Female')),
         id_h = paste0(rb020, rx030, rb010)) 

# 4.4 Create unique IDs for merging, merge country and household ID (h&d)
silc.h <- silc.h %>% mutate(id_h = paste0(hb010, hb020, hb030))
silc.d <- silc.d %>% mutate(id_h = paste0(db010, db020, db030))

# 4.5 Merge silc.rp with silc.h
silc.rph <- left_join(silc.rp, silc.h)

# 4.6 Convert NA's in silc.rph to 0 for aggregation issues 
silc.rph[is.na(silc.rph)] <- 0
