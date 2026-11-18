library(readxl)
library(fixest)
library(dplyr)
df <- read_excel(file.choose(), sheet = "Data formler")
names(df) <- c("Ticker", "Ar", "Periodslut", "LagPB", "Efter", "EfterLagPB",
               "KassaBelopp", "TA", "Utdelningar", "Aterkop", "EK", "NI",
               "Omsattning", "SkuldEK", "Borsvarde", "Tom",
               "Kassa", "Utbet", "ROE", "ROA")

sig <- c("***" = 0.01, "**" = 0.05, "*" = 0.10)
pre <- df %>%
  filter(Ar <= 2023) %>% 
  mutate(Ar2022 = as.numeric(Ar == 2022),
         Ar2023 = as.numeric(Ar == 2023))

t1 <- feols(Kassa ~ LagPB + Ar2022 + Ar2023 + LagPB:Ar2022 + LagPB:Ar2023,
            data = pre, cluster = ~Ticker)
t2 <- feols(Utbet ~ LagPB + Ar2022 + Ar2023 + LagPB:Ar2022 + LagPB:Ar2023,
            data = pre, cluster = ~Ticker)
t3 <- feols(ROE ~ LagPB + Ar2022 + Ar2023 + LagPB:Ar2022 + LagPB:Ar2023,
            data = pre, cluster = ~Ticker)
t4 <- feols(ROA ~ LagPB + Ar2022 + Ar2023 + LagPB:Ar2022 + LagPB:Ar2023,
            data = pre, cluster = ~Ticker)

etable(t1, t2, t3, t4, digits = 4, signif.code = sig)

wald(t1, keep = "LagPB:Ar")
wald(t2, keep = "LagPB:Ar")
wald(t3, keep = "LagPB:Ar")
wald(t4, keep = "LagPB:Ar")