Sys.setlocale(locale="no_NO")

library(WDI)
library(tidyverse)
library(ggplot2)

df_gdp_wdi <- WDI(
  country = "all",
  indicator = c('gdppc' = "NY.GDP.PCAP.PP.KD"),
  start = 2000,
  end = 2019,
  extra = TRUE
)

df_gdp <- df_gdp_wdi %>%
  mutate(year = as.numeric(year)) %>%
  filter(iso3c != "", income != "Aggregates") %>%
  drop_na(gdppc) %>%
  group_by(country, year) %>%
  slice(which.max(gdppc)) %>%
  ungroup()

df_gdp <- df_gdp %>%
  group_by(country) %>%
  filter(year == min(year)) %>%
  select(country, gdppc0 = gdppc) %>%
  left_join(df_gdp, by = c("country")) %>%
  ungroup()

df_gdp <- df_gdp %>%
  group_by(country) %>%
  mutate(
    gdpgrowth = (log(gdppc) - lag(log(gdppc))) * 100,
    avg_gdpgrowth = mean(gdpgrowth, na.rm = TRUE)
  ) %>%
  arrange(desc(year)) %>%
  slice(1) %>%
  select(-status,
         -lastupdated,
         -capital,
         -longitude,
         -latitude,
         -lending,
         -gdpgrowth) %>%
  ungroup()

df_edu_wdi <- WDI(
  country = "all",
  indicator = c('educ' = "BAR.SCHL.15UP"),
  start = 2000,
  end = 2019,
  extra = TRUE
)

df_edu <- df_edu_wdi %>%
  drop_na(educ) %>%
  mutate(educ = as.numeric(educ)) %>%
  select(country, iso2c, iso3c, year, educ, region, income) %>%
  group_by(iso3c) %>%
  mutate(avg_educ = mean(educ)) %>%
  ungroup() %>%
  select(-year,-educ) %>%
  distinct(country, .keep_all = TRUE)

df_nsy_wdi <- WDI(
  country = "all",
  indicator = c('nsy' = "NY.ADJ.NNAT.GN.ZS"),
  start = 2000,
  end = 2015,
  extra = TRUE
)

df_nsy <- df_nsy_wdi %>%
  select(country, region, income, iso2c, iso3c, year, nsy) %>%
  arrange(iso3c, year) %>%
  drop_na(nsy, iso3c, region) %>%
  filter(region != "Aggregates") %>%
  mutate(nsy = as.numeric(nsy, na.rm = TRUE)) %>%
  group_by(country) %>%
  mutate(avg_nsy = mean(nsy, na.rm = TRUE)) %>%
  select(-year,-nsy) %>%
  distinct(country, .keep_all = TRUE) %>%
  ungroup()

df_lf_wdi <- WDI(
  country = "all",
  indicator = c('lf' = "JI.TLF.TOTL"),
  start = 2000,
  end = 2019,
  extra = TRUE
)

df_lf <- df_lf_wdi %>%
  select(country, region, income, iso3c = iso2c, year, lf) %>%
  drop_na(lf) %>%
  filter(lf != 0) %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  mutate(deltaYear = year - lag(year),
         growth = log(lf) - log(lag(lf))) %>%
  drop_na(deltaYear, growth) %>%
  mutate(n = growth / deltaYear, avg_n = mean(n, na.rm = TRUE)) %>%
  filter(year == max(year)) %>%
  ungroup()

df_rest_wdi <- WDI(
  country = "all",
  indicator = c(
    'poptot' = "SP.POP.TOTL",
    'gi' = "NE.GDI.FTOT.KD.ZG",
    'gx' = "NE.EXP.GNFS.KD.ZG",
    'nry' = "NY.ADJ.DRES.GN.ZS",
    'p' = "SP.POP.GROW"
  ),
  start = 2000,
  end = 2019,
  extra = TRUE
)

df_rest <- df_rest_wdi %>%
  drop_na(iso3c, gi, gx, nry) %>%
  filter(region != "Aggregates") %>%
  select(-longitude,
         -latitude,
         -capital,
         -lending,
         -status,
         -lastupdated) %>%
  group_by(iso3c) %>%
  mutate(
    avg_p = mean(p, na.rm = TRUE),
    avg_gi = mean(gi, na.rm = TRUE),
    avg_gx = mean(gx, na.rm = TRUE),
    avg_nry = mean(nry, na.rm = TRUE)
  ) %>%
  filter(year == max(year)) %>%
  ungroup(iso3c)

df_growth <-
  df_gdp %>%
  inner_join(select(df_edu, iso3c, avg_educ), by = "iso3c") %>%
  inner_join(select(df_nsy, iso3c, avg_nsy), by = "iso3c") %>%
  inner_join(select(df_lf, iso3c, avg_n), by = "iso3c") %>%
  inner_join(select(df_rest, iso3c, poptot, avg_p, avg_gi, avg_gx, avg_nry),
             by = "iso3c") %>%
  mutate(ln_gdppc0 = log(gdppc0),
         ln_gdppc = log(gdppc),
         avg_n = avg_n * 100) %>%
  select(-year)

#NOrske bokstaver
Sys.setlocale(locale="no_NO")

# Velg ut de variabler du vil ha med i tabellen. Her er et eksempel (du skal ta med alle variabler som du har med i den empiriske analysen)
df <- subset(df_growth, select = c("avg_gdpgrowth","avg_n",  "avg_p", "avg_nsy", "avg_nry", "avg_gi", "avg_gx", "avg_educ"))
# Gi beskrivende navn til variablene (i samme rekkefølge som de ligger i datasettet)
labs <- c("Gj.snitlig årlig vekstrate i BNP pc 2000-2019 (%)",
          "Gj.snitlig vekstrate i arbeidskraft (%)", 
          "Gj.snitlig årlig befolkningsvekst (%)",
          "Gj.snitlig sparing (%)",
          "Gj.snitlig årlig redukjsonsrate naturressurser(%) " ,
          "Gj.snitlig årlig vekstrate i inviseringer (%)" ,
          "Gj.snitlig årlig vekstrate i eksport (%)",
          "Gj.snitlig antall år utdanning (%) ")

library(summarytools)
suppressPackageStartupMessages(library(stargazer))
suppressPackageStartupMessages(library(vtable))

# Lag tabellen
st(df, labels=labs,
   summ = list(
     c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'), # Beskriv hvilken statistikk du ønsker å vise
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Gj.snitt','SD','Min','Maks') # Gi navn til kolumnene
   ))

suppressPackageStartupMessages(library(scales))
library(ggplot2)

df_growth2019_n <- df_growth[complete.cases(df_growth$region,df_growth$poptot, df_growth$avg_p, df_growth$gdppc),] #Her tar jeg vekk land som mangler observasjoner. Dette gjør grafen penere.  

plot1 <- ggplot(df_growth2019_n, aes(x = avg_p , y = ln_gdppc, na.rm = TRUE)) +
  xlab("Befolkningsvekst") + # Beskrivelse for x-akselen
  ylab("BNP per innbygger 2019") + # Beskrivelse for y-akselen
  theme_minimal(base_size = 14, base_family = "Georgia") + # Tekststørrelse og font
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + # Størrelse (farge) på bobblene avhenger befolkningsstørrelse (region)
  scale_x_continuous(labels = dollar)  + # Legg til et dollar tegn til y-akselen
  scale_size_area(guide = "none", max_size = 14) + #Ta vekk legend for befolkningsstørrelse
  theme(legend.text = element_text(size = 10,color="black"))+  # Bestem font-størrelse på legend
  scale_colour_manual(values = rainbow(9)) +# Velg farger til bobblene
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))+
  scale_y_continuous(trans = 'log2', labels = dollar, breaks=c(500, 2000, 8000, 32000, 120000)) + # logaritmere BNP pc og velg hvilke "ticks" som skal vises
  scale_x_continuous(breaks=c(-1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5 )) # Velg vilke "ticks" som skal vises på x-akselen
plot1

######Figur 2#
plot2<-ggplot(df_growth2019_n, aes(x = avg_nsy, y = ln_gdppc, color = region)) +
  geom_point(aes(size = poptot, color = region), alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE) +
  scale_size_area(guide = "none", max_size = 14) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8)) +
  labs(x = "Gjennomsnittlig sparing for perioden 2000-2015",
       y = "BNP per innbygger i 2019 (log)",
       title = "Forholdet mellom sparing og BNP per innbygger i 2019") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))
plot2
###

######Figur 3#
plot3 <- ggplot(df_growth2019_n, aes(x = avg_p, y = ln_gdppc, color = region)) +
  geom_point(aes(size = poptot), alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE) +
  scale_size_area(guide = "none", max_size = 14) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8)) +
  labs(x = "Befolkningsvekst",
       y = "BNP per innbygger i 2019 (log)",
       title = "Forholdet mellom befolkningsvekst og BNP per innbygger i 2019") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))
plot3

###

#####Figur 4#
plot4 <- ggplot(df_growth2019_n, aes(x = avg_educ, y = ln_gdppc, color = region)) +
  geom_point(aes(size = poptot, color = region), alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE) +
  scale_size_area(guide = "none", max_size = 14) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8)) +
  labs(x = "Gjennomsnittlig utdanning for perioden 2000-2015",
       y = "BNP per innbygger i 2019 (log)",
       title = "Forholdet mellom humankapital og BNP per innbygger i 2019") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))
plot4
###

####Figur 5#
plot5<- ggplot(df_growth2019_n, aes(x = avg_nsy, y = avg_gdpgrowth, color = region)) +
  geom_point(aes(size = poptot, color = region), alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE) +
  scale_size_area(guide = "none", max_size = 14) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8)) +
  labs(x = "Sparing som andel av BNI (Netto)",
       y = "Årlig vekstrate i BNP per innbygger (log)",
       title = "Sammenhengen mellom årlig vekstrate i BNP per innbygger og sparing") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9)) 
plot5
###

####Figur 6#
plot6 <- ggplot(df_growth2019_n, aes(x = avg_educ, y = avg_gdpgrowth, color = region)) +
  geom_point(aes(size = poptot), alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE) +
  scale_size_area(guide = "none", max_size = 14) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8)) +
  labs(x = "Utdanningsnivå",
       y = "Vekstrate i BNP per innbygger i 2019",
       title = "Forholdet mellom utdanning og økonomisk vekst") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9)) 
plot6

###

#test-test#
suppressPackageStartupMessages(library(sjPlot))
suppressPackageStartupMessages(library(sjmisc))
suppressPackageStartupMessages(library(sjlabelled))

#regresjon
model1 <- lm(ln_gdppc0  ~ avg_nsy + avg_educ + avg_n + avg_p + avg_nry + avg_gi + avg_gx, data= df_growth)
summary_table <- summary(model1)$coefficients
tab_model(model1)

df <- df_growth[complete.cases( df_growth$avg_gi, df_growth$avg_n),]

Q1gi <- quantile(df$avg_gi, .25 )
Q3gi <- quantile(df$avg_gi, .75)
IQRgi <- IQR(df$avg_gi)

Q1n <- quantile(df$avg_n, .25 )
Q3n <- quantile(df$avg_n, .75)
IQRn <- IQR(df$avg_n)

no_outliers <- subset(df, df$avg_gi > (Q1gi - 1.5*IQRgi) & df$avg_gi < (Q3gi + 1.5*IQRgi) &  df$avg_n > (Q1n - 1.5*IQRn) & df$avg_n < (Q3n + 1.5*IQRn))
dim(no_outliers)
#####

#####SOLOW BAS, SS- FIGUR####
# 
# Parametere for Solow-modellen
# Installer og last inn nødvendige pakker

# Parametre
#####
s <- 0.3       # sparing
n <- 0.02      # vekstrate i arbeidskraft
alpha <- 0.5   # elastisitet av kapital
A <- 1         # teknologisk nivå

# Steady-state kapital per arbeider
k_ss <- (s * A / n)^(1 / (1 - alpha))

# Kapital per arbeider (k) og produksjon per arbeider (y) verdier
k <- seq(0, 2 * k_ss, length.out = 100)
y <- A * k^alpha

# Investering per arbeider (s*y) og erstatningsinvesteringer (n*k)
investment <- s * y
replacement <- n * k


# Dataframe for plotting
df <- data.frame(k = k, investment = investment, replacement = replacement)

# Coordinates for vertical lines
x1 <- 100
x2 <- 350
y1 <- s * A * x1^alpha
y2 <- n * x2

# Plot
p <- ggplot(df, aes(k)) +
  geom_line(aes(y = investment, color = "Investeringer/arbeider(s*y)")) +
  geom_line(aes(y = replacement, color = "Investeringer for erstattning(n*k)   ")) +
  geom_segment(aes(x = 0, xend = k_ss, y = n * k_ss, yend = n * k_ss),
               linetype = "dashed", color = "blue", size = 0.6) +
  geom_segment(aes(x = k_ss, xend = k_ss, y = 0, yend = n * k_ss),
               linetype = "dashed", color = "blue", size = 0.6) +
  geom_segment(aes(x = x1, xend = x1, y = 0, yend = y1),
               color = "green", size = 0.8) +
  geom_segment(aes(x = x2, xend = x2, y = 0, yend = y2),
               color = "green", size = 0.8) +
  geom_text(aes(x = 0.65 * k_ss, y = 0.7 * (s * (0.5 * k_ss)^alpha), label = "->"),
            color = "red", size = 4, fontface = "bold") +
  geom_text(aes(x = 1.40 * k_ss, y = 1.05 * (s * (1.5 * k_ss)^alpha), label = "<-"),
            color = "red", size = 4, fontface = "bold") +
  geom_text(aes(x = 0.80 * k_ss, y = 0.9 * (s * (0.5 * k_ss)^alpha), label = "->"),
            color = "red", size = 4, fontface = "bold") +
  geom_text(aes(x = 1.3 * k_ss, y = 1 * (s * (1.5 * k_ss)^alpha), label = "<-"),
            color = "red", size = 4, fontface = "bold") +
  labs(title = "Steady State, tilstand i Solow-modellen",
       x = "Kapital per arbeider (k)",
       y = "Investeringer per arbeider",
       color = "Legend") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  coord_cartesian(ylim = c(0, 6.8))

print(p)

#####
