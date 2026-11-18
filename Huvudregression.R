library(readxl)
library(fixest)

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