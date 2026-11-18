library(readxl)
library(fixest)
library(dplyr)

fil <- file.choose()
df <- read_excel(fil, sheet = "Data formler")
names(df) <- c("Ticker","Ar","Periodslut","LagPB","Efter","EfterLagPB",
               "KassaBelopp","TA","Utdelningar","Aterkop","EK","NI",
               "Omsattning","SkuldEK","Borsvarde","Tom",
               "Kassa","Utbet","ROE","ROA")
df <- df %>% mutate(across(c(Ar, LagPB, Efter, Kassa, Utbet, ROE, ROA), as.numeric))

plac <- df %>% filter(Ar <= 2023) %>% mutate(PlaceboEfter = as.numeric(Ar == 2023))

did_plac <- list(
  Kassa         = feols(Kassa ~ PlaceboEfter * LagPB, data = plac, cluster = ~Ticker),
  Utbetalningar = feols(Utbet ~ PlaceboEfter * LagPB, data = plac, cluster = ~Ticker),
  ROE           = feols(ROE   ~ PlaceboEfter * LagPB, data = plac, cluster = ~Ticker),
  ROA           = feols(ROA   ~ PlaceboEfter * LagPB, data = plac, cluster = ~Ticker)
)
etable(did_plac, digits = 4)