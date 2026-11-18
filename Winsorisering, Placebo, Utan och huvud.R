library(readxl)
library(fixest)
library(dplyr)

df <- read_excel(file.choose(), sheet = "Data formler")
names(df) <- c("Ticker", "Ar", "Periodslut", "LagPB", "Efter", "EfterLagPB",
               "KassaBelopp", "TA", "Utdelningar", "Aterkop", "EK", "NI",
               "Omsattning", "SkuldEK", "Borsvarde", "Tom",
               "Kassa", "Utbet", "ROE", "ROA")

sig <- c("***" = 0.01, "**" = 0.05, "*" = 0.10)

h1 <- feols(Kassa ~ Efter * LagPB, data = df, cluster = ~Ticker)
h2 <- feols(Utbet ~ Efter * LagPB, data = df, cluster = ~Ticker)
h3 <- feols(ROE   ~ Efter * LagPB, data = df, cluster = ~Ticker)
h4 <- feols(ROA   ~ Efter * LagPB, data = df, cluster = ~Ticker)
etable(h1, h2, h3, h4, digits = 4, signif.code = sig)

plac <- df %>%
  filter(Ar <= 2023) %>%
  mutate(PlaceboEfter = as.numeric(Ar == 2023))

p1 <- feols(Kassa ~ PlaceboEfter * LagPB, data = plac, cluster = ~Ticker)
p2 <- feols(Utbet ~ PlaceboEfter * LagPB, data = plac, cluster = ~Ticker)
p3 <- feols(ROE   ~ PlaceboEfter * LagPB, data = plac, cluster = ~Ticker)
p4 <- feols(ROA   ~ PlaceboEfter * LagPB, data = plac, cluster = ~Ticker)
etable(p1, p2, p3, p4, digits = 4, signif.code = sig)

wins <- function(x) pmin(pmax(x, quantile(x, 0.01)), quantile(x, 0.99))
sapply(df[c("Kassa", "Utbet", "ROE", "ROA")], quantile, c(0.01, 0.99))

w  <- df %>% mutate(across(c(Kassa, Utbet, ROE, ROA), wins))
w1 <- feols(Kassa ~ Efter * LagPB, data = w, cluster = ~Ticker)
w2 <- feols(Utbet ~ Efter * LagPB, data = w, cluster = ~Ticker)
w3 <- feols(ROE   ~ Efter * LagPB, data = w, cluster = ~Ticker)
w4 <- feols(ROA   ~ Efter * LagPB, data = w, cluster = ~Ticker)
etable(w1, w2, w3, w4, digits = 4, signif.code = sig)

u  <- df %>% filter(Ticker != "5707 JT Equity")
u1 <- feols(Kassa ~ Efter * LagPB, data = u, cluster = ~Ticker)
u2 <- feols(Utbet ~ Efter * LagPB, data = u, cluster = ~Ticker)
u3 <- feols(ROE   ~ Efter * LagPB, data = u, cluster = ~Ticker)
u4 <- feols(ROA   ~ Efter * LagPB, data = u, cluster = ~Ticker)
etable(u1, u2, u3, u4, digits = 4, signif.code = sig)
