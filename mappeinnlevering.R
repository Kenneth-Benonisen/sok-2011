# Nødvendige pakker -------------
library(plyr)
library(tidyverse)
library(WDI)
library(countrycode)
library(vtable)
library(scales)
library(sjPlot)
library(sjmisc)
library(sjlabelled)


# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")

# Innhenting og rydding av data. -----
start_time <- Sys.time()
df_gdp_wdi <- WDI(
  country = "all",
  indicator = c('gdppc' = "NY.GDP.PCAP.PP.KD", # BNP per innbygger (PPP)
                'educ' = "BAR.SCHL.15UP",      # Gjennomsnittlig antall år i skole (befolkning 15+)
                'nsy' = "NY.ADJ.NNAT.GN.ZS",   # Sparing som andel av BNI (netto)
                'lf' = "JI.TLF.TOTL",          # Størrelse på arbeidskraften
                'poptot' = "SP.POP.TOTL",      # Befolkningsstørrelse
                'gi' = "NE.GDI.FTOT.KD.ZG",    # Årlig vekstrate i investeringer
                'gx' = "NE.EXP.GNFS.KD.ZG",    # Årlig vekstrate i eksport
                'nry' = "NY.ADJ.DRES.GN.ZS",   # Årlig reduksjonsrate i naturressurser
                'p' = "SP.POP.GROW"),          # Vekstrate i befolkningen
  start = 2000,
  end = 2019,
  extra = TRUE
)

# Gdp dataframe:
df_gdp <- df_gdp_wdi %>% 
  filter(iso3c != "", income != "Aggregates", !is.na(gdppc)) %>%
  group_by(country,year) %>%
  slice(which.max(gdppc)) %>% 
  ungroup()

df_gdp <- df_gdp %>% 
  group_by(country) %>% 
  filter(year == min(year)) %>% 
  select(country,gdppc0 = gdppc) %>% 
  left_join(df_gdp, by = c("country")) %>% 
  ungroup()

df_gdp <- df_gdp %>% 
  arrange(iso3c, year) %>% 
  ddply("iso3c", mutate,
        gdpgrowth = c(NA,diff(log(gdppc)))*100) %>% 
  mutate(gdpgrowth = as.numeric(gdpgrowth, na.rm = T)) %>% 
  ddply("iso3c", mutate,
        avg_gdpgrowth = mean(gdpgrowth, na.rm = T)) %>% 
  slice_max(year) %>% 
  select(country,
         iso2c,
         iso3c,
         region,
         income,
         year,
         gdppc,
         gdppc0,
         avg_gdpgrowth) %>% 
  distinct(country, .keep_all = T)



# Edu dataframe:
df_educ <- df_gdp_wdi %>%
  drop_na(educ) %>%
  mutate(educ = as.numeric(educ)) %>%
  select(country, iso2c, iso3c, year, educ, region, income) %>%
  ddply("iso3c", transform,
        avg_educ = mean(educ, na.rm = TRUE)) %>% 
  select(-year,
         -educ) %>% 
  distinct(country, .keep_all = TRUE)

# Nsy dataframe: 
df_nsy <- df_gdp_wdi %>% 
  select(country,region,income,iso2c,iso3c,year,nsy) %>% 
  arrange(iso3c, year) %>%
  drop_na(nsy,iso3c,region) %>%
  filter(region != "Aggregates") %>% 
  ddply("iso3c", transform,
        avg_nsy = mean(nsy, na.rm = TRUE)) %>% 
  select(-year,
         -nsy) %>% 
  distinct(iso3c, .keep_all = T)

# Labor force dataframe: 
df_lf <-  df_gdp_wdi %>% 
  select(country, iso3c = iso2c, year, lf) %>%
  drop_na(lf) %>% 
  filter(lf != 0) %>%
  arrange(iso3c, year) %>% 
  ddply("iso3c", transform,
        deltayear = c(NA,diff(year)),
        lf_growth = c(NA,diff(log(lf)))) %>% 
  na.omit(deltayear, lf_growth) %>%  # Ta vekk observasjoner som mangler data
  ddply("iso3c", transform,
        n = lf_growth / deltayear) %>% 
  ddply("iso3c", transform, # Nødt til å ha en ekstra ddplyr. 
        avg_n = mean(n, na.rm = T)) %>% 
  distinct(iso3c, .keep_all = T) %>% # Fjerne identiske variabler
  arrange(iso3c)

# Rest of data:
df_rest <- df_gdp_wdi %>% 
  drop_na(iso3c,gi,gx,nry) %>%
  filter(region != "Aggregates") %>%
  select(country,iso2c,iso3c,year,region,income,poptot,p,gi,gx,nry) %>%
  ddply("iso3c", mutate,
        avg_p = mean(p, na.rm = TRUE),
        avg_gi = mean(gi, na.rm = TRUE),
        avg_gx = mean(gx, na.rm = TRUE),
        avg_nry = mean(nry, na.rm = TRUE)) %>%
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  slice(n())


# # Henter frem gross national income per capita in constant 2017 dollar.
# df_gni_ppp <-WDI(
#   country = "all",
#   indicator = c('gni_ppp'="NY.GNP.PCAP.PP.KD"),
#   start = 2000,
#   end = 2000,
#   extra = F, # det å sette "extra = TRUE" fører til at vi laster inn ekstra informasjon som vi kan benytte seinere (f.eks. variabelen "region")
#   cache = NULL,
#   latest = NULL,
#   language = "en")
# 
# # legger opp til at US er baseline for indeksen.
# us_gni <- df_gni_ppp[df_gni_ppp$iso2c == "US", "gni_ppp"]
# 
# # Indekserer alle land sitt GNI pc mot US.
# df_gni_ppp <- df_gni_ppp %>%
#   filter(iso3c != "") %>%
#   na.omit() %>%
#   mutate(gni_index = gni_ppp / us_gni * 100)

# Samle alt av data til en ferdig sortert dataframe. 
df_growth <- df_gdp %>%
  inner_join(select(df_educ, iso3c, avg_educ), by = "iso3c") %>%
  inner_join(select(df_nsy, iso3c, avg_nsy), by = "iso3c") %>%
  inner_join(select(df_lf, iso3c, avg_n), by = "iso3c") %>%
  # inner_join(select(df_gni_ppp, iso3c, gni_index), by ="iso3c") %>%
  inner_join(select(df_rest, iso3c, poptot, avg_p, avg_gi, avg_gx, avg_nry),
             by = "iso3c") %>%
  mutate(ln_gdppc0 = log(gdppc0),
         ln_gdppc = log(gdppc),
         avg_n = avg_n * 100) %>% 
  slice_max(year) %>% 
  select(country,
         region,
         income,
         iso2c,
         iso3c,
         year,
         poptot,
         gdppc,
         gdppc0,
         avg_gdpgrowth,
         avg_n,
         avg_p,
         avg_nsy,
         avg_nry,
         avg_gi,
         avg_gx,
         avg_educ,
         # gni_index,
         ln_gdppc,
         ln_gdppc0)


# Kopi av dataframe for å hente ut deskriptiv data med outliers. 
df_growth_with_outliners <- df_growth 
# tibble(df_growth)

# Fjerne ekstremverdiene for et mer nøyaktig resultat.
Q1gi <- quantile(df_growth$avg_gi, .25 )
Q3gi <- quantile(df_growth$avg_gi, .75)
IQRgi <- IQR(df_growth$avg_gi)

Q1n <- quantile(df_growth$avg_n, .25 )
Q3n <- quantile(df_growth$avg_n, .75)
IQRn <- IQR(df_growth$avg_n)

no_outliers <- subset(df_growth, df_growth$avg_gi > (Q1gi - 1.5*IQRgi) & 
                        df_growth$avg_gi < (Q3gi + 1.5*IQRgi) &  
                        df_growth$avg_n > (Q1n - 1.5*IQRn) & 
                        df_growth$avg_n < (Q3n + 1.5*IQRn))
dim(no_outliers)

# Analyse av data. -------

# Korrelasjonslyse uten outliers, denne brukes i oppgave teksten. 
cor_df_no_outliers <- cor(no_outliers[c("avg_gdpgrowth", "avg_nsy", "avg_educ", "avg_nry", "avg_gi", "avg_n", "avg_p", "avg_gx")])
cor_df_no_outliers <- cor_df_no_outliers[1,2:8]
print(round(cor_df_no_outliers,4))



# Korrelasjonslyse med outliers. 
cor_df_growth_with_outliers <- cor(df_growth_with_outliners[c("avg_gdpgrowth", "avg_nsy", "avg_educ", "avg_nry", "avg_gi", "avg_n", "avg_p", "avg_gx")])
cor_df_growth_with_outliers <- cor_df_growth_with_outliers[1,2:8]
print(round(cor_df_growth_with_outliers,4))

# Tabell 1 med variabler uten outliers. 
table_df_no_outliers <- subset(no_outliers, select = c("avg_gdpgrowth",
                                                       "ln_gdppc", 
                                                       "ln_gdppc0",
                                                       "avg_educ",
                                                       "avg_nsy", 
                                                       "avg_nry", 
                                                       "avg_gi", 
                                                       "avg_n", 
                                                       "avg_p", 
                                                       "avg_gx"
                                                       ))
# Gi beskrivende navn til variablene, pc = per capita
labs <- c("Gjennomsnitlig årlig vekstrate i BNP pc 2000-2019 (%)",
            "Logaritmisk vekstrate for BNP per capita (2019)",
            "Logaritmisk vekstrate for BNP per capita (2000)",
            "Gjennomsnittlig humankapital (år på skole)",
            "Gjennomsnittlig årlig vekstrate i sparing",
            "Gjennomsnittlig årlig reduksjonsrate i naturressurser",
            "Gjennomsnittlig årlig vekstrate i investeringer",
            "Gjennomsnittlig årlig vekstrate i arbeidskraft",
            "Gjennomsnittlig årlig befolkningsvekst (%)",
            "Gjennomsnittlig årlig vekstrate i eksport") 

# Lag tabellen
st(table_df_no_outliers, labels=labs, title = "Summary statistics without outliers",
   summ = list(
     c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Gjennomsnitt','SD','Min','Maks') 
   ))

# Tabell 2 av variabler med outliers. 
table_df_with_outliers <- subset(df_growth_with_outliners, select = c("avg_gdpgrowth",
                                                                      "ln_gdppc", 
                                                                      "ln_gdppc0",
                                                                      "avg_educ",
                                                                      "avg_nsy", 
                                                                      "avg_nry", 
                                                                      "avg_gi", 
                                                                      "avg_n", 
                                                                      "avg_p", 
                                                                      "avg_gx"))
# Gi beskrivende navn til variablene, pc = per capita
labs_2 <- c("Gjennomsnitlig årlig vekstrate i BNP pc 2000-2019 (%)",
            "Logaritmisk vekstrate for BNP per capita (2019)",
            "Logaritmisk vekstrate for BNP per capita (2000)",
            "Gjennomsnittlig humankapital (år på skole)",
            "Gjennomsnittlig årlig vekstrate i sparing",
            "Gjennomsnittlig årlig reduksjonsrate i naturressurser",
            "Gjennomsnittlig årlig vekstrate i investeringer",
            "Gjennomsnittlig årlig vekstrate i arbeidskraft",
            "Gjennomsnittlig årlig befolkningsvekst (%)",
            "Gjennomsnittlig årlig vekstrate i eksport") 

# Lag tabellen
st(table_df_with_outliers, labels=labs, title = "Summary statistics with outliers",
   summ = list(
     c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Gjennomsnitt','SD','Min','Maks') 
   ))



# Regresjonsanalyse. 
regresjon_no_outliers <- lm(avg_gdpgrowth ~ avg_nsy + avg_n + avg_gi + avg_gx + avg_p + avg_educ + avg_nry, data = no_outliers)
summary(regresjon_no_outliers)


# regresjon_with_outliers <- lm(avg_gdpgrowth ~ avg_nsy + avg_n + avg_gi + avg_gx + avg_p + avg_educ + avg_nry, data = df_growth_with_outliners)
# summary(regresjon_with_outliers)

# sjekk <- lm(ln_gdppc ~ avg_nsy + avg_n + avg_gi + avg_gx + avg_p + avg_educ + avg_nry, data = no_outliers)
# summary(sjekk)


# Presenterer regresjonsanalysen i en tabell.
tab_model(regresjon_no_outliers,
          pred.labels = c("(Intercept)",
                          "Gjennomsnittlig sparing",
                          "Gjennomsnittlig arbeidskraft",
                          "Gjennomsnittlig investering",
                          "Gjennomsnittlig eksport",
                          "Gjennomsnittlig befolkningsvekst",
                          "Gjennomsnittlig humankapital",
                          "Gjennomsnittlig naturressurs"),
          dv.labels = c("Gjennomsnittlig vekstrate BNP per innbygger 2000-2019 (%)"),
          string.pred = "Coeffcients",
          string.p = " P-value",
          string.stat = "t-value",
          show.se = T,
          show.ci = F,
          show.stat = F
          )



tab_model(sjekk,
          pred.labels = c("(Intercept)",
                          "Gjennomsnittlig sparing",
                          "Gjennomsnittlig arbeidskraft",
                          "Gjennomsnittlig investering",
                          "Gjennomsnittlig eksport",
                          "Gjennomsnittlig befolkningsvekst",
                          "Gjennomsnittlig humankapital",
                          "Gjennomsnittlig naturressurs",
                          "Log BNP per innbygger (2000)"),
          dv.labels = c("Log BNP per innbygger 2000-2019 (%)"),
          string.pred = "Coeffcients",
          string.p = " P-value",
          string.stat = "t-value",
          show.se = T,
          show.ci = F,
          show.stat = F
)



# Plot av div. variabler. --------------

no_outliers %>% 
  ggplot(aes(avg_p, ln_gdppc, na.rm = T)) +
  xlab("Befolkningsvekst") + # Beskrivelse for x-akselen
  ylab("BNP per innbygger 2019") + # Beskrivelse for y-akselen
  theme_classic(base_size = 14) + # Tekststørrelse
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + # Størrelse (farge) på bobblene avhenger befolkningsstørrelse (region)
  #geom_smooth(method = "lm", se = F) +
  theme(legend.position = "bottom")+
  scale_size_area(guide = "none", max_size = 14) + #Ta vekk legend for befolkningsstørrelse
  scale_y_continuous(trans = 'log2', breaks=c(seq(0,15,1))) + # logaritmere BNP pc og hvilke "ticks" som skal vises
  scale_x_continuous(breaks=c(seq(-1,5,0.5))) #  "ticks" som skal vises på x-akselen

no_outliers %>% 
  ggplot(aes(avg_nsy, ln_gdppc, na.rm = T)) +
  xlab("Netto-sparing") + 
  ylab("BNP per innbygger 2019") + 
  theme_classic(base_size = 14) + 
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + 
  #geom_smooth(method = "lm", se = F) +
  theme(legend.position = "bottom")+
  scale_size_area(guide = "none", max_size = 14) + 
  scale_y_continuous(trans = 'log2', breaks=c(seq(0,15,1))) + 
  scale_x_continuous(breaks=c(seq(-10,30,5)))   

no_outliers %>% 
  ggplot(aes(avg_educ, ln_gdppc, na.rm = T)) +
  xlab("Humankapital (år på skolen)") + 
  ylab("BNP per innbygger 2019") + 
  theme_classic(base_size = 14) + 
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + 
  #geom_smooth(method = "lm", se = F) +
  theme(legend.position = "bottom")+
  scale_size_area(guide = "none", max_size = 14) + 
  scale_y_continuous(trans = 'log2', breaks=c(seq(0,15,1))) + 
  scale_x_continuous(breaks=c(seq(0,15,3))) 

no_outliers %>% 
  ggplot(aes(avg_nsy, avg_gdpgrowth, na.rm = T)) +
  xlab("Netto-sparing") + 
  ylab("Årlig vesktrate i BNP") + 
  theme_classic(base_size = 14) + 
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + 
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "bottom")+
  scale_size_area(guide = "none", max_size = 14) + 
  scale_y_continuous(trans = 'log2', breaks=c(0)) + 
  scale_x_continuous(breaks=c(seq(-15,30,10))) 

no_outliers %>% 
  ggplot(aes(avg_educ, avg_gdpgrowth, na.rm = T)) +
  xlab("Humankapital (år på skolen)") + 
  ylab("Årlig vesktrate i BNP") + 
  theme_classic(base_size = 14) + 
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + 
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "bottom")+
  scale_size_area(guide = "none", max_size = 14) + 
  scale_y_continuous(trans = 'log2', breaks=c(30)) +
  scale_x_continuous(breaks=c(seq(0,15,1))) 


end_time <- Sys.time()
# end_time - start_time 
# Sjekke tiden det tar for å hente inn data og prosessere det, for moro skyld. 
cat("Time elapsed:", round(end_time-start_time,2), "mins")



