
library(tidyverse)
library(readxl)
data6 <- read_excel("data.xlsx")
# Codebook:

# pres_* = antibiotic prescriptions by dentists
# kir_* = surgical dental treatments
# paro_* = periodontal treatments
# jh_* = root canal treatments
# kaynnit_* = dental visits per 1000 inhabitants
# ikä_* = average age
# morbidity_* = morbidity index
# pop_* = population
# Region = wellbeing service counties


# Table 1

#convert to per 100,000 inhabitants
data6$pres_per_2019 = data6$pres_2019 / data6$pop_2019 * 100000
data6$pres_per_2020 = data6$pres_2020 / data6$pop_2020 * 100000
data6$pres_per_2021 = data6$pres_2021 / data6$pop_2021 * 100000
data6$pres_per_2022 = data6$pres_2022 / data6$pop_2022 * 100000
data6$pres_per_2023 = data6$pres_2023 / data6$pop_2023 * 100000
data6$pres_per_2024 = data6$pres_2024 / data6$pop_2024 * 100000
data6$pres_per_2025 = data6$pres_2025 / data6$pop_2025 * 100000


sd( data6$pres_per_2019) / mean(data6$pres_per_2019)*100
sd( data6$pres_per_2020) / mean(data6$pres_per_2020)*100
sd( data6$pres_per_2021) / mean(data6$pres_per_2021)*100
sd( data6$pres_per_2022) / mean(data6$pres_per_2022)*100
sd( data6$pres_per_2023) / mean(data6$pres_per_2023)*100
sd( data6$pres_per_2024) / mean(data6$pres_per_2024)*100
sd( data6$pres_per_2025) / mean(data6$pres_per_2025)*100

sd( data6$pres_per_2019)
sd( data6$pres_per_2020)
sd( data6$pres_per_2021)
sd( data6$pres_per_2022)
sd( data6$pres_per_2023)
sd( data6$pres_per_2024)
sd( data6$pres_per_2025) 

max(data6$pres_per_2019) / min(data6$pres_per_2019)
max(data6$pres_per_2020) / min(data6$pres_per_2020)
max(data6$pres_per_2021) / min(data6$pres_per_2021)
max(data6$pres_per_2022) / min(data6$pres_per_2022)
max(data6$pres_per_2023) / min(data6$pres_per_2023)
max(data6$pres_per_2024) / min(data6$pres_per_2024)
max(data6$pres_per_2025) / min(data6$pres_per_2025)

# Table 2
# dental visits
m_nb <- MASS::glm.nb(
  pres_2019 ~ log(kaynnit_2019) + offset(log(pop_2019)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2020 ~ log(kaynnit_2020) + offset(log(pop_2020)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2021 ~ log(kaynnit_2021) + offset(log(pop_2021)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2022 ~ log(kaynnit_2022) + offset(log(pop_2022)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2023 ~ log(kaynnit_2023) + offset(log(pop_2023)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2024 ~ log(kaynnit_2024) + offset(log(pop_2024)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2025 ~ log(kaynnit_2025) + offset(log(pop_2025)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

# Surgical
m_nb <- MASS::glm.nb(
  pres_2019 ~ log(kir_2019/pop_2019) + offset(log(pop_2019)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2020 ~ log(kir_2020/pop_2020) + offset(log(pop_2020)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2021 ~ log(kir_2021/pop_2021) + offset(log(pop_2021)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2022 ~ log(kir_2022/pop_2022) + offset(log(pop_2022)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2023 ~ log(kir_2023/pop_2023) + offset(log(pop_2023)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2024 ~ log(kir_2024/pop_2024) + offset(log(pop_2024)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2025 ~ log(kir_2025/pop_2025) + offset(log(pop_2025)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

# Periodontal 
m_nb <- MASS::glm.nb(
  pres_2019 ~ log(paro_2019/pop_2019) + offset(log(pop_2019)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2020 ~ log(paro_2020/pop_2020) + offset(log(pop_2020)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2021 ~ log(paro_2021/pop_2021) + offset(log(pop_2021)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2022 ~ log(paro_2022/pop_2022) + offset(log(pop_2022)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2023 ~ log(paro_2023/pop_2023) + offset(log(pop_2023)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2024 ~ log(paro_2024/pop_2024) + offset(log(pop_2024)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2025 ~ log(paro_2025/pop_2025) + offset(log(pop_2025)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

# root canal treatments
m_nb <- MASS::glm.nb(
  pres_2019 ~ log(jh_2019/pop_2019) + offset(log(pop_2019)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2020 ~ log(jh_2020/pop_2020) + offset(log(pop_2020)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2021 ~ log(jh_2021/pop_2021) + offset(log(pop_2021)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2022 ~ log(jh_2022/pop_2022) + offset(log(pop_2022)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2023 ~ log(jh_2023/pop_2023) + offset(log(pop_2023)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2024 ~ log(jh_2024/pop_2024) + offset(log(pop_2024)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

m_nb <- MASS::glm.nb(
  pres_2025 ~ log(jh_2025/pop_2025) + offset(log(pop_2025)),
  data = data6
)
m_nb$coef*10
confint(m_nb)*10

# Morbidity index
m_nb <- MASS::glm.nb(
  pres_2019 ~ I(morbidity_2019/10) + offset(log(pop_2019)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

m_nb <- MASS::glm.nb(
  pres_2020 ~ I(morbidity_2020/10) + offset(log(pop_2020)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

m_nb <- MASS::glm.nb(
  pres_2021 ~ I(morbidity_2021/10)  + offset(log(pop_2021)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

m_nb <- MASS::glm.nb(
  pres_2022 ~ I(morbidity_2022/10) + offset(log(pop_2022)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

m_nb <- MASS::glm.nb(
  pres_2023 ~ I(morbidity_2023/10) + offset(log(pop_2023)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

# Morbidity index
m_nb <- MASS::glm.nb(
  pres_2019 ~ I(morbidity_2019/10) + offset(log(pop_2019)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

m_nb <- MASS::glm.nb(
  pres_2020 ~ I(morbidity_2020/10) + offset(log(pop_2020)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

m_nb <- MASS::glm.nb(
  pres_2021 ~ I(morbidity_2021/10)  + offset(log(pop_2021)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

m_nb <- MASS::glm.nb(
  pres_2022 ~ I(morbidity_2022/10) + offset(log(pop_2022)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

m_nb <- MASS::glm.nb(
  pres_2023 ~ I(morbidity_2023/10) + offset(log(pop_2023)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

# age

# Morbidity index
m_nb <- MASS::glm.nb(
  pres_2019 ~ I(ikä_2019/10) + offset(log(pop_2019)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

m_nb <- MASS::glm.nb(
  pres_2020 ~ I(ikä_2020/10) + offset(log(pop_2020)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

m_nb <- MASS::glm.nb(
  pres_2021 ~ I(ikä_2021/10)  + offset(log(pop_2021)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

m_nb <- MASS::glm.nb(
  pres_2022 ~ I(ikä_2022/10) + offset(log(pop_2022)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

m_nb <- MASS::glm.nb(
  pres_2023 ~ I(ikä_2023/10) + offset(log(pop_2023)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

m_nb <- MASS::glm.nb(
  pres_2024 ~ I(ikä_2024/10) + offset(log(pop_2024)),
  data = data6
)
exp(m_nb$coef)
exp(confint(m_nb))

# proportion variation explained analyses 
# data to long format
df_long <- data6[,c(1:54,74)] %>%
  pivot_longer(
    cols = -c(Region, Hyvinvointialue),
    names_to = c("indicator", "year"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(year)
  ) %>%
  pivot_wider(
    names_from  = indicator,
    values_from = value
  )


# Base model 2019-2025: year + offset, random intercept for county
m0 <- glmmTMB(
  pres ~ factor(year) + offset(log(pop)) + (1 | Region),
  family = nbinom2,
  data = df_long,
  REML=T
)

# Model with DENTAL VISITS added
m1 <- glmmTMB(
  pres ~ factor(year)  + I(kaynnit)+  offset(log(pop)) + (1 | Region),
  family = nbinom2,
  data = df_long,
  REML=T
)

# Extract county-level random intercept variance
var0 <- as.numeric(VarCorr(m0)$cond$Region[1])
var1 <- as.numeric(VarCorr(m1)$cond$Region[1])

prop_explained <- (var0 - var1) / var0
prop_explained

# Model with SURGICAL TREATMENTS added
m1 <- glmmTMB(
  pres ~ factor(year)  + I(kir/pop)+  offset(log(pop)) + (1 | Region),
  family = nbinom2,
  data = df_long,
  REML=T
)

# Extract county-level random intercept variance
var0 <- as.numeric(VarCorr(m0)$cond$Region[1])
var1 <- as.numeric(VarCorr(m1)$cond$Region[1])

prop_explained <- (var0 - var1) / var0
prop_explained

# Model with PERIODONTAL TREATMENTS added
m1 <- glmmTMB(
  pres ~ factor(year)  + I(paro/pop)+  offset(log(pop)) + (1 | Region),
  family = nbinom2,
  data = df_long,
  REML=T
)

# Extract county-level random intercept variance
var0 <- as.numeric(VarCorr(m0)$cond$Region[1])
var1 <- as.numeric(VarCorr(m1)$cond$Region[1])

prop_explained <- (var0 - var1) / var0
prop_explained


# Model with ROOT CANAL TREATMENTS added
m1 <- glmmTMB(
  pres ~ factor(year)  + I(jh/pop)+  offset(log(pop)) + (1 | Region),
  family = nbinom2,
  data = df_long,
  REML=T
)

# Extract county-level random intercept variance
var0 <- as.numeric(VarCorr(m0)$cond$Region[1])
var1 <- as.numeric(VarCorr(m1)$cond$Region[1])

prop_explained <- (var0 - var1) / var0
prop_explained

# Base model 2019-2023: year + offset, random intercept for county
m0 <- glmmTMB(
  pres ~ factor(year) + offset(log(pop)) + (1 | Region),
  family = nbinom2,
  data = df_long[df_long$year < 2024,],
  REML=T
)

# Model with MORBIDITY INDEX added
m1 <- glmmTMB(
  pres ~ factor(year)  + I(morbidity)+  offset(log(pop)) + (1 | Region),
  family = nbinom2,
  data = df_long[df_long$year < 2024,],
  REML=T
)

# Extract county-level random intercept variance
var0 <- as.numeric(VarCorr(m0)$cond$Region[1])
var1 <- as.numeric(VarCorr(m1)$cond$Region[1])

prop_explained <- (var0 - var1) / var0
prop_explained

# Base model 2019-2024: year + offset, random intercept for county
m0 <- glmmTMB(
  pres ~ factor(year) + offset(log(pop)) + (1 | Region),
  family = nbinom2,
  data = df_long[df_long$year < 2025,],
  REML=T
)

# Model with MORBIDITY INDEX added
m1 <- glmmTMB(
  pres ~ factor(year)  + I(ikä)+  offset(log(pop)) + (1 | Region),
  family = nbinom2,
  data = df_long[df_long$year < 2025,],
  REML=T
)

# Extract county-level random intercept variance
var0 <- as.numeric(VarCorr(m0)$cond$Region[1])
var1 <- as.numeric(VarCorr(m1)$cond$Region[1])

prop_explained <- (var0 - var1) / var0
prop_explained

# Figure 1

library(ggpubr)

# 1) Enter data
df <- tribble(
  ~antibiotic, ~`2012`, ~`2013`, ~`2014`, ~`2015`, ~`2016`, ~`2017`, ~`2018`, ~`2019`, ~`2020`, ~`2021`, ~`2022`, ~`2023`, ~`2024`, ~`2025`,
  "Phenoxymethylpenicillin", 2967, 3064, 2993, 2979, 2727, 2382, 2231, 2137, NA,  NA,  NA,  NA,  NA,  NA,
  "Amoxicillin",              707,  797,  847,  914,  920,  878,  894,  955,  862, 1018, 1030, 1016,  982,  998,
  "Cefalexin",                550,  638,  667,  729,  742,  676,  658,  665,  592,  626,  603,  567,  534,  503,
  "Metronidazole",            377,  420,  422,  434,  402,  388,  357,  354,  312,  315,  299,  288,  278,  267,
  "Clindamycin",              341,  353,  336,  290,  261,  212,  184,  175,  146,  140,  130,  116,   96,   87,
  "Amoxicillin + clavulanic acid", 172, 190, 205, 241, 230, 186, 171, 165, 137, 131, 129, 118, 116, 117,
  "Doxycycline",              103,  106,   90,   83,   75,   52,   43,   42,   36,   32,   31,   29,   26,   22,
  "Other",                    142,  128,  106,   93,   81,   59,   49,   40,   30,   26,   26,   23,   22,   19,
  "Total (excl. phenoxymethylpenicillin)",   2393, 2632, 2674, 2784, 2712, 2453, 2357, 2396, 2116, 2287, 2249, 2158, 2054, 2009,
  "Total (incl. phenoxymethylpenicillin)", 5359, 5697, 5667, 5763, 5439, 4835, 4588, 4533, NA,  NA,  NA,  NA,  NA,  NA
)

# 2) Long format
df_long <- df %>%
  pivot_longer(cols = matches("^\\d{4}$"),
               names_to = "year",
               values_to = "rate_per_100k") %>%
  mutate(year = as.integer(year))

# 3) Create legend labels with delta: "Name (+/-n, +/-p%)"
deltas <- df_long %>%
  group_by(antibiotic) %>%
  summarise(
    start = rate_per_100k[year == 2012][1],
    end_year = max(year[!is.na(rate_per_100k)]),
    end = rate_per_100k[year == end_year][1],
    .groups = "drop"
  ) %>%
  mutate(
    diff = end - start,
    pct  = 100 * diff / start,
    legend_label = paste0(antibiotic, " (", sprintf("%+.0f%%", pct), ")")
  )

legend_map <- setNames(deltas$legend_label, deltas$antibiotic)

# Plot A: exclude totals (your k)
k <- df_long %>%
  filter(!antibiotic %in% c("Total (excl. phenoxymethylpenicillin)", "Total (incl. phenoxymethylpenicillin)")) %>%
  ggplot(aes(x = year, y = rate_per_100k, color = antibiotic)) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 1.8, na.rm = TRUE) +
  labs(
    x = "Year",
    y = "Dentist antibiotic prescriptions\nper 100,000 inhabitants",
    color = "Antibiotic (% change)"
  ) +
  scale_color_discrete(labels = legend_map) +
  theme_minimal(base_size = 12) +
  xlim(c(2012, 2025)) +
  scale_x_continuous(breaks = c(2012, 2015, 2020, 2025))+
  theme(
    legend.position = "right",
    legend.box.spacing = unit(0, "pt"),  # gap between plot and legend
    legend.margin = margin(0, 0, 0, 0),   # padding inside legend box
    panel.grid.minor.x = element_blank()
  )



# Plot B: totals only (your k2)
k2 <- df_long %>%
  filter(antibiotic %in% c("Total (excl. phenoxymethylpenicillin)", "Total (incl. phenoxymethylpenicillin)")) %>%
  ggplot(aes(x = year, y = rate_per_100k, color = antibiotic)) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 1.8, na.rm = TRUE) +
  labs(
    x = "Year",
    y = "Dentist antibiotic prescriptions\nper 100,000 inhabitants",
    color = "Total (% 2012 to 2025)"
  ) +
  scale_color_discrete(labels = legend_map) +
  theme_minimal(base_size = 12) +
  xlim(c(2012, 2025)) +
  scale_x_continuous(breaks = c(2012, 2015, 2020, 2025)) +
  ylim(c(0, 6000)) +
  theme(legend.title = element_blank())+
  theme(
    legend.position = "right",
    legend.box.spacing = unit(0, "pt"),  # gap between plot and legend
    legend.margin = margin(0, 0, 0, 0),   # padding inside legend box,
    panel.grid.minor.x = element_blank()
  )

p <- ggarrange(k, k2, nrow = 2, labels = "AUTO", align = "hv")
ggsave("figure1AB.tiff", p, dpi = 700, compression = "lzw", units = "cm", width = 22, height = 22)


# Figure 2 and Supplementary figures

library(mapsFinland)
library(sf)
library(tidyverse)
library(readxl)

data6 <- read_excel("data.xlsx")
data("hyvinvointialueet2023")
hyvinvointialueet2023=hyvinvointialueet2023 %>% arrange(nimi)
data6 <- data6[order(data6$Hyvinvointialue), ]

# convert all variables to per 100,000 inhabitants format
data6$pres_per_2019 = data6$pres_2019 / data6$pop_2019 * 100000
data6$pres_per_2020 = data6$pres_2020 / data6$pop_2020 * 100000
data6$pres_per_2021 = data6$pres_2021 / data6$pop_2021 * 100000
data6$pres_per_2022 = data6$pres_2022 / data6$pop_2022 * 100000
data6$pres_per_2023 = data6$pres_2023 / data6$pop_2023 * 100000
data6$pres_per_2024 = data6$pres_2024 / data6$pop_2024 * 100000
data6$pres_per_2025 = data6$pres_2025 / data6$pop_2025 * 100000

data6$kir_per_2019 = data6$kir_2019 / data6$pop_2019 * 100000
data6$kir_per_2020 = data6$kir_2020 / data6$pop_2020 * 100000
data6$kir_per_2021 = data6$kir_2021 / data6$pop_2021 * 100000
data6$kir_per_2022 = data6$kir_2022 / data6$pop_2022 * 100000
data6$kir_per_2023 = data6$kir_2023 / data6$pop_2023 * 100000
data6$kir_per_2024 = data6$kir_2024 / data6$pop_2024 * 100000
data6$kir_per_2025 = data6$kir_2025 / data6$pop_2025 * 100000

data6$jh_per_2019 = data6$jh_2019 / data6$pop_2019 * 100000
data6$jh_per_2020 = data6$jh_2020 / data6$pop_2020 * 100000
data6$jh_per_2021 = data6$jh_2021 / data6$pop_2021 * 100000
data6$jh_per_2022 = data6$jh_2022 / data6$pop_2022 * 100000
data6$jh_per_2023 = data6$jh_2023 / data6$pop_2023 * 100000
data6$jh_per_2024 = data6$jh_2024 / data6$pop_2024 * 100000
data6$jh_per_2025 = data6$jh_2025 / data6$pop_2025 * 100000

data6$paro_per_2019 = data6$paro_2019 / data6$pop_2019 * 100000
data6$paro_per_2020 = data6$paro_2020 / data6$pop_2020 * 100000
data6$paro_per_2021 = data6$paro_2021 / data6$pop_2021 * 100000
data6$paro_per_2022 = data6$paro_2022 / data6$pop_2022 * 100000
data6$paro_per_2023 = data6$paro_2023 / data6$pop_2023 * 100000
data6$paro_per_2024 = data6$paro_2024 / data6$pop_2024 * 100000
data6$paro_per_2025 = data6$paro_2025 / data6$pop_2025 * 100000

data6$kaynnit_2019 = data6$kaynnit_2019 * 100
data6$kaynnit_2020 = data6$kaynnit_2020 * 100
data6$kaynnit_2021 = data6$kaynnit_2021 * 100
data6$kaynnit_2022 = data6$kaynnit_2022 * 100
data6$kaynnit_2023 = data6$kaynnit_2023 * 100
data6$kaynnit_2024 = data6$kaynnit_2024 * 100
data6$kaynnit_2025 = data6$kaynnit_2025 * 100
names(data6)

hyvinvointialueet2023 = cbind(hyvinvointialueet2023,data6[,c("pres_per_2019","pres_per_2025",
                                                             "kir_per_2019","kir_per_2025",
                                                             "jh_per_2019","jh_per_2025",
                                                             "paro_per_2019","paro_per_2025",
                                                             "ikä_2019","ikä_2024",
                                                             "morbidity_2019","morbidity_2023",
                                                             "kaynnit_2019","kaynnit_2025",
                                                             "Hyvinvointialue")])



library(ggplot2)
library(ggpubr)
library(scales)

min_val <- 85000
max_val <- 120000          # adjust to your max
breaks_seq <- seq(min_val, max_val, by = 10000)
summary(hyvinvointialueet2023$kaynnit_2025)

p1 <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = kaynnit_2019)) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    name = "Dental visits\nper 100,000 inhabitants",
    limits = c(min_val, max_val),
    breaks = breaks_seq,        # ticks only at 0,1000,2000,...
    labels = label_comma()      # "1,000", "2,000", ...
  ) +
  theme_minimal() +
  ggtitle("A                                2019")

min_val <- 65000
max_val <- 105000         # adjust to your max
breaks_seq <- seq(min_val, max_val, by = 10000)

p2 <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = kaynnit_2025)) +
  scale_fill_distiller(
    palette = "Oranges",
    direction = 1,
    name = "Dental visits\nper 100,000 inhabitants",
    limits = c(min_val, max_val),
    breaks = breaks_seq,
    labels = label_comma()
  ) +
  theme_minimal() +
  ggtitle("B                                  2025")

ggarrange(p1,p2)

p_visits= ggarrange(p1,p2) 


min_val <- 2000
max_val <- 6000          # adjust to your max
breaks_seq <- seq(min_val, max_val, by = 1000)


p1 <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = pres_per_2019)) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    name = "Antibiotic prescriptions\nper 100,000 inhabitants",
    limits = c(min_val, max_val),
    breaks = breaks_seq,        # ticks only at 0,1000,2000,...
    labels = label_comma()      # "1,000", "2,000", ...
  ) +
  theme_minimal() +
  ggtitle("A                                2019")

min_val <- 500
max_val <- 3100          # adjust to your max
breaks_seq <- seq(min_val, max_val, by = 500)

p2 <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = pres_per_2025)) +
  scale_fill_distiller(
    palette = "Oranges",
    direction = 1,
    name = "Antibiotic prescriptions\nper 100,000 inhabitants",
    limits = c(min_val, max_val),
    breaks = breaks_seq,
    labels = label_comma()
  ) +
  theme_minimal() +
  ggtitle("B                                  2025")

ggarrange(p1,p2)

p_2= ggarrange(p1,p2) 


min(hyvinvointialueet2023$morbidity_2023)
max(hyvinvointialueet2023$morbidity_2023)
min_val <- 60
max_val <- 130          # adjust to your max
breaks_seq <- seq(min_val, max_val, by = 20)

#morbidity
p1_morbidity <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = morbidity_2019)) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    name = "Morbidity index",
    limits = c(min_val, max_val),
    breaks = breaks_seq,        # ticks only at 0,1000,2000,...
    labels = label_comma()      # "1,000", "2,000", ...
  ) +
  theme_minimal() +
  ggtitle("A                                2019")

min_val <- 60
max_val <- 130          # adjust to your max
breaks_seq <- seq(min_val, max_val, by = 20)

p2_morbidity <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = morbidity_2023)) +
  scale_fill_distiller(
    palette = "Oranges",
    direction = 1,
    name = "Morbidity index",
    limits = c(min_val, max_val),
    breaks = breaks_seq,
    labels = label_comma()
  ) +
  theme_minimal() +
  ggtitle("B                                  2023")


min(hyvinvointialueet2023$ikä_2019)
max(hyvinvointialueet2023$ikä_2019)
min_val <- 35
max_val <- 52          # adjust to your max
breaks_seq <- seq(min_val, max_val, by = 5)

#age
p1_age <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = ikä_2019)) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    name = "Average age",
    limits = c(min_val, max_val),
    breaks = breaks_seq,        # ticks only at 0,1000,2000,...
    labels = label_comma()      # "1,000", "2,000", ...
  ) +
  theme_minimal() +
  ggtitle("A                                2019")

min_val <- 35
max_val <- 52          # adjust to your max
breaks_seq <- seq(min_val, max_val, by = 5)

p2_age <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = ikä_2024)) +
  scale_fill_distiller(
    palette = "Oranges",
    direction = 1,
    name = "Average age",
    limits = c(min_val, max_val),
    breaks = breaks_seq,
    labels = label_comma()
  ) +
  theme_minimal() +
  ggtitle("B                                  2024")



min(hyvinvointialueet2023$jh_per_2025)
max(hyvinvointialueet2023$jh_per_2025)
min_val <- 3000
max_val <- 10500          # adjust to your max
breaks_seq <- seq(min_val, max_val, by = 2000)

# JH
p1_jh <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = jh_per_2019)) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    name = "Root canal treatments\nper 100,000 inhabitants",
    limits = c(min_val, max_val),
    breaks = breaks_seq,        # ticks only at 0,1000,2000,...
    labels = label_comma()      # "1,000", "2,000", ...
  ) +
  theme_minimal() +
  ggtitle("A                                2019")


p2_jh <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = jh_per_2025)) +
  scale_fill_distiller(
    palette = "Oranges",
    direction = 1,
    name = "Root canal treatments\nper 100,000 inhabitants",
    limits = c(min_val, max_val),
    breaks = breaks_seq,
    labels = label_comma()
  ) +
  theme_minimal() +
  ggtitle("B                                  2025")


min(hyvinvointialueet2023$paro_per_2019)
max(hyvinvointialueet2023$paro_per_2019)
min_val <- 16000
max_val <- 28500          # adjust to your max
breaks_seq <- seq(min_val, max_val, by = 4000)

# paro
p1_paro <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = paro_per_2019)) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    name = "Periodontal treatments\nper 100,000 inhabitants",
    limits = c(min_val, max_val),
    breaks = breaks_seq,        # ticks only at 0,1000,2000,...
    labels = label_comma()      # "1,000", "2,000", ...
  ) +
  theme_minimal() +
  ggtitle("A                                2019")


p2_paro <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = paro_per_2025)) +
  scale_fill_distiller(
    palette = "Oranges",
    direction = 1,
    name = "Periodontal treatments\nper 100,000 inhabitants",
    limits = c(min_val, max_val),
    breaks = breaks_seq,
    labels = label_comma()
  ) +
  theme_minimal() +
  ggtitle("B                                  2025")

min(hyvinvointialueet2023$kir_per_2025)
max(hyvinvointialueet2023$kir_per_2025)
min_val <- 4000
max_val <- 15500          # adjust to your max
breaks_seq <- seq(min_val, max_val, by = 3000)
# kir
p1_kir <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = kir_per_2019)) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    name = "Surgical dental treatments\nper 100,000 inhabitants",
    limits = c(min_val, max_val),
    breaks = breaks_seq,        # ticks only at 0,1000,2000,...
    labels = label_comma()      # "1,000", "2,000", ...
  ) +
  theme_minimal() +
  ggtitle("A                                2019")


p2_kir <- hyvinvointialueet2023 %>% 
  arrange(hyvinvoint) %>% 
  ggplot() +
  geom_sf(aes(fill = kir_per_2025)) +
  scale_fill_distiller(
    palette = "Oranges",
    direction = 1,
    name = "Surgical dental treatments\nper 100,000 inhabitants",
    limits = c(min_val, max_val),
    breaks = breaks_seq,
    labels = label_comma()
  ) +
  theme_minimal() +
  ggtitle("B                                  2025")

age= ggarrange(p1_age,p2_age)
morbidity= ggarrange(p1_morbidity,p2_morbidity)
jh= ggarrange(p1_jh,p2_jh)
paro= ggarrange(p1_paro,p2_paro)
kir= ggarrange(p1_kir,p2_kir)

ggsave(plot=p_2,"Figure2.tiff",dpi=700,width = 20,height=20,units="cm",compression="lzw")
ggsave(plot=jh,"Figure_S1.tiff",dpi=700,width = 20,height=30,units="cm",compression="lzw")
ggsave(plot=paro,"Figure_S2.tiff",dpi=700,width = 20,height=20,units="cm",compression="lzw")
ggsave(plot=kir,"Figure_S3.tiff",dpi=700,width = 20,height=30,units="cm",compression="lzw")
ggsave(plot=age,"Figure_S4.tiff",dpi=700,width = 20,height=30,units="cm",compression="lzw")
ggsave(plot=morbidity,"Figure_S5.tiff",dpi=700,width = 20,height=20,units="cm",compression="lzw")
ggsave(plot=p_visits,"Figure_S6.tiff",dpi=700,width = 20,height=30,units="cm",compression="lzw")
