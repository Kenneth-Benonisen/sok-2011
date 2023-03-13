
# Nødvendige pakker -------------
library(dplyr)
library(plyr)
library(tidyverse)
library(WDI)
library(countrycode)
library(vtable)
library(scales)

# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")


# 1. BNP per innbyggere (alle år) og initial nivå på BNP per innbyggere. WDI-variabel =  "NY.GDP.PCAP.PP.KD". ----------------------------
# Velg startår = 2000 og sluttår = 2019
df_gdp0<-WDI(
  country = "all",
  indicator = c('gdppc'="NY.GDP.PCAP.PP.KD"),  
  start = 2000,
  end = 2019,
  extra = TRUE, # det å sette "extra = TRUE" fører til at vi laster inn ekstra informasjon som vi kan benytte seinere (f.eks. variabelen "region")
  cache = NULL,
  latest = NULL,
  language = "en"
)



df_gdp <- subset(df_gdp0, select = c(country, region, income, iso2c, iso3c, year, gdppc) )
df_gdp$iso3c[df_gdp$iso3c ==""] <- NA # Fører inn NA for radene som er blank.
df_gdp <- df_gdp[complete.cases( df_gdp$gdppc, df_gdp$iso3c),] # Ta vekk observasjoner som mangler data på gdppc og iso3c. 
df_gdp = df_gdp  %>%  
  mutate(year = as.numeric(year)) # Se til at year er en numerisk variabel.

# Noen land har flere observasjoner for samme år (f.eks afghanistan år 2010). Vi ønsker å ha én observasjon per land og år. 
df_gdp <- df_gdp[!duplicated(df_gdp[c("iso3c", "year", max("gdppc"))]), ]  %>%
  arrange(iso3c, year) # Ta vekk duplikater for land og år, behold observasjonen med størst gdppc (denne regelen kan diskuteres)

# Lag et datasett med Y0 (nivå på BNP per innbyggere i år 2000)
df_gdp2000  <- df_gdp %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>% #Behold den første observasjonen for BNP per innbyggere (Y0)
  slice(1) %>%
  ungroup()

df_gdp2000 = subset(df_gdp2000, select = -c(year) ) # Slett unødvendige variabler
df_gdp2000 <-   plyr:: rename(df_gdp2000,c("gdppc" = "gdppc0")) # Gi variabeln et nytt navn slik at vi kan identifisere den i datasetet. 

df_gdp <- left_join(df_gdp,df_gdp2000, by=c("country", "iso2c", "iso3c", "region", "income")) # Sett sammen data for BNP per innbygger alle år, med BNP per innbygger år 2000.






# 2. Humankapital (gjennomsnittlig antall år i skole blant befolkningen eldre enn 15 år). WDI-variabel = BAR.SCHL.15UP    -----------------
df_educ0<-WDI(
  country = "all",
  indicator = c('educ'="BAR.SCHL.15UP"),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_educ <- subset(df_educ0, select = c(country, region, income, iso2c, iso3c, year, educ) ) %>%
  arrange(iso3c, year) #Behold nødvendige variabler

df_educ <- df_educ[complete.cases(df_educ$educ),] %>%
  arrange(iso3c, year) # Slett observasjoner med manglende data

df_educ = df_educ %>%  
  arrange(iso3c, year) %>%  # Sorter etter Iso-kode og år. 
  mutate(educ = as.numeric(educ, na.rm = TRUE)) %>% # Se til at variabelen er numerisk
  ddply("iso3c",transform, avg_educ=mean(educ, na.rm = TRUE))  # Beregne gjennomsnittlig år i skole for tidsperioden 2000 - 2019 for hvert land, basert på tilgjenglig data (vil være 2000.2005,2010)

df_educ <- subset(df_educ, select = c(country, region, income, iso2c, iso3c, avg_educ)) # Her tar jeg vekk variabelen "year". Jeg gjør dette fordi vi bare har en observasjon på utdanning per land. Vi ønsker å bruke denne verdi for alle år. 
df_educ <- df_educ[!duplicated(df_educ[c("iso3c")]), ]  %>%
  arrange(iso3c) # Ta vekk duplikater for hvert land.





# 3. Gjennomsnittlig sparing for perioden 2000-2015 (lagg fordi det kan ta litt tid for sparing å bli til investering)   -----------------------
df_nsy0<-WDI(
  country = "all",
  indicator = c( 'nsy'="NY.ADJ.NNAT.GN.ZS"),  
  start = 2000,
  end = 2015,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_nsy <- subset(df_nsy0, select = c(country, region, income, iso2c, iso3c, year, nsy) ) %>%  arrange(iso3c, year) #Behold nødvendige variabler
df_nsy <- df_nsy[complete.cases(df_nsy$nsy),] %>%  arrange(iso3c, year) # Slett observasjoner med manglende data


df_nsy = df_nsy %>%  
  arrange(iso3c, year) %>%  # Sorter etter Iso-kode og år. 
  mutate(nsy = as.numeric(nsy, na.rm = TRUE)) %>% # Se til at variabelen er numerisk
  ddply("iso3c",transform,
        avg_nsy=mean(nsy, na.rm = TRUE))  # Beregne gjennomsnittlig år i skole for tidsperioden 2000 - 2019 for hvert land, basert på tilgjenglig data (vil være 2000.2005,2010)

df_nsy <- subset(df_nsy, select = c(country, region, income, iso2c, iso3c, avg_nsy)) # Her tar jeg vekk variabelen "year". Jeg gjør dette fordi vi bare har en observasjon på utdanning per land. Vi ønsker å bruke denne verdi for alle år. 
df_nsy <- df_nsy[!duplicated(df_nsy[c("iso3c")]), ]  %>%  arrange(iso3c) # Ta vekk duplikater for hvert land.





# 4. Vekst i arbeidskraften (n)   ----------------------------------------------
df_lf0<-WDI(
  country = "all",
  indicator = c('lf'="JI.TLF.TOTL"),  # lf = labor force
  start = 2000,
  end = 2019,
  extra = TRUE, 
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_lf <- subset(df_lf0, select = c(country, region, income, iso2c, year, lf) ) %>%  arrange(iso2c, year) # velg ut relevante variabler
df_lf <-   plyr:: rename(df_lf,c("iso2c" = "iso3c")) # variabelen som identifiserer land med kode er feil i datasetet. Dette korrigerer dette
df_lf [df_lf == 0]<-NA
df_lf <- df_lf[complete.cases(df_lf$iso3c, df_lf$lf),] # Ta vekk observasjoner som mangler data på lf og iso3c. 
df_lf = df_lf  %>%  
  mutate(year = as.numeric(year)) # Se til at year er en numerisk variabel. 

df_lf <- df_lf[!duplicated(df_lf[c("iso3c", "year")]), ]  %>%  arrange(iso3c, year) # Ta vekk duplikater for land og år

# Ta fram vekstraten i arbeidskraften (n). Vi har ikke data for hvert år i alle land. 
# For å beregne gjennomsnittlig årlig vekst må vi lage en variabel som måler antallet tidsperioder mellom hver observasjon.
df_n = df_lf %>%  
  arrange(iso3c, year) %>%  # Sorter på år og land
  ddply("iso3c",transform,
        t=c(NA,diff(year)),
        lf_growth=c(NA,diff(log(lf)))) #Vekstrate uten hensyn til tidsintervall

df_n <- df_n[complete.cases(df_n$t, df_n$lf_growth),] # Ta vekk observasjoner som mangler data på t

#Nå kan vi ta fram årlig vekstrate
df_n = df_n %>%  
  mutate(t = as.numeric(t)) %>%   
  mutate(lf_growth = as.numeric(lf_growth))
df_n <- transform(df_n, n =lf_growth/t)

# gjennomsnittlig vekstrate i arbeidskraften for hvert land
df_n <- df_n %>% # 
  ddply("iso3c",transform,
        avg_n=mean(n, na.rm = TRUE)) #Gjennomsnittlig årlig vekstrate i arbeidskraften

df_n <- subset(df_n, select = c(iso3c, avg_n) )
df_n <- df_n[!duplicated(df_n["iso3c"]), ]  %>%  arrange(iso3c) # Ta vekk duplikater for land





# 5. Lag et datasett som inneholder BNP data, utdanningsdata, sparing, og arbeidskraftsdata    --------------------------

df <- left_join(df_gdp, df_educ, by=c("country", "iso2c", "iso3c", "region", "income"))
df <- left_join(df, df_nsy, by=c("country", "iso2c", "iso3c", "region", "income"))
df <- left_join(df, df_n, by="iso3c")
df <- subset(df, select = c(country, region, income, iso2c, iso3c, year, gdppc, gdppc0, avg_educ, avg_nsy, avg_n)) # Behold nødvendige variabler

# Mange observasjoner representerer aggregerte regioner. Vi ønsker å ta vekk disse. Det finnes helt sikkert en bedre måte å gjøre dette på. Dette er den måten jeg kom på.
df <- df  %>%  filter(iso2c!='1A' & iso2c !='1W' & iso2c != '4E' & iso2c != '7E' & iso2c !='8S'
                      & iso2c !='B8' & iso2c !='EU' & iso2c !='F1' & iso2c !='OE' & iso2c !='S1' & iso2c !='S2' & iso2c !="S3" 
                      & iso2c !='S4' & iso2c !='T2' & iso2c !='T3' & iso2c !='T4' & iso2c !='T5' & iso2c !='T6' & iso2c !='T7' 
                      & iso2c !='V1' & iso2c !='V2' & iso2c !='V3' & iso2c !='V4' & iso2c !='XC' & iso2c !='XD' & iso2c !='XE' 
                      & iso2c !='XF' & iso2c !='XG' & iso2c !='XH' & iso2c !='XI' & iso2c !='XJ' & iso2c !='XL' & iso2c !='XM' 
                      & iso2c !='XN' & iso2c !='XO' & iso2c !='XP' & iso2c !='XQ' & iso2c !='XT' & iso2c !='XU' & iso2c !='Z4' 
                      & iso2c !='Z7' & iso2c !='ZF'& iso2c !='ZG'  & iso2c !='ZH' & iso2c !='ZI'  & iso2c !='ZJ'  & iso2c !='ZQ'  
                      & iso2c !='ZT'  & iso2c !='Z7')  %>% arrange(iso3c, year)





# 6. Lag et datasett for resterende variabler.   -------------------------------

df_rest0<-WDI(
  country = "all",
  indicator = c('poptot'="SP.POP.TOTL", 'gi'="NE.GDI.FTOT.KD.ZG", 'gx'="NE.EXP.GNFS.KD.ZG", 'nry'="NY.ADJ.DRES.GN.ZS", 'p'="SP.POP.GROW" ),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)


#df_rest0<-df_rest0 %>% mutate_all(na_if,"") # feilmelding på koden. kommentert ut. 
df_rest <- df_rest0[complete.cases( df_rest0$iso3c),]  %>%  arrange(iso2c) 


# Ta vekk observasjoner som ikke representerer land.
df_rest <- df_rest  %>%  filter(iso2c!='1A' & iso2c !='1W' & iso2c != '4E' & iso2c != '7E' & iso2c !='8S'
                                & iso2c !='B8' & iso2c !='EU' & iso2c !='F1' & iso2c !='OE' & iso2c !='S1' & iso2c !='S2' & iso2c !="S3" 
                                & iso2c !='S4' & iso2c !='T2' & iso2c !='T3' & iso2c !='T4' & iso2c !='T5' & iso2c !='T6' & iso2c !='T7' 
                                & iso2c !='V1' & iso2c !='V2' & iso2c !='V3' & iso2c !='V4' & iso2c !='XC' & iso2c !='XD' & iso2c !='XE' 
                                & iso2c !='XF' & iso2c !='XG' & iso2c !='XH' & iso2c !='XI' & iso2c !='XJ' & iso2c !='XL' & iso2c !='XM' 
                                & iso2c !='XN' & iso2c !='XO' & iso2c !='XP' & iso2c !='XQ' & iso2c !='XT' & iso2c !='XU' & iso2c !='Z4' 
                                & iso2c !='Z7' & iso2c !='ZF'& iso2c !='ZG'  & iso2c !='ZH' & iso2c !='ZI'  & iso2c !='ZJ'  & iso2c !='ZQ'  
                                & iso2c !='ZT'  & iso2c !='Z7')  %>% arrange(iso3c, year) 

df_rest <- subset(df_rest, select = c("country", "region", "income", "iso3c", "iso2c", "year", "poptot", "p", "nry", "gi", "gx"))
df_all <- left_join(df, df_rest, by=c("country", "region", "income", "iso2c", "iso3c", "year"))

# Lag en rekkefølge til variablene slik at det er enklere å få en oversikt over datamaterialet.
col_order <- c("country",  "region", "income", "iso3c", "iso2c", "year", "gdppc", "gdppc0", "poptot", "p", "avg_n", "avg_nsy", "nry", "gi", "gx", "avg_educ")
df_all <- df_all[, col_order]



# Ta fram vekstraten og gjennomsnitt for resterende variabler
df_growth0 = df_all %>%  
  arrange(iso3c, year) %>%  # Sorter på år og land
  ddply("iso3c",transform,
        gdpgrowth=c(NA,diff(log(gdppc)))*100) %>%   # Årlig vekstrate i gdppc for hvert land
  mutate(gdpgrowth = as.numeric(gdpgrowth, na.rm = TRUE)) %>% # 
  ddply("iso3c",transform,
        avg_gdpgrowth=mean(gdpgrowth, na.rm = TRUE), #Gjennomsnittlig årlig vekstrate i BNP per innbygger for hvert land i perioden
        avg_gi=mean(gi, na.rm = TRUE), # Gjennomsnittlig årlig vekstrate i investeringer for hvert land  i perioden
        avg_nry=mean(nry, na.rm = TRUE),  # Gjennomsnittlig årlig vekstrate (negativ) i naturressurser for hvert land  i perioden
        avg_gx=mean(gx, na.rm = TRUE),  # Gjennomsnittlig årlig vekstrate i eksport for hvert land  i perioden
        avg_p=mean(p, na.rm = TRUE))  # Gjennomsnittlig årlig vekstrate i befolkningen for hvert land  i perioden

# View(df_growth0)
# df_growth0 <-  df_growth0 %>% mutate_all(na_if,"") # kommentert ut pga. error ved bruk av funksjonen. 
df_growth <- df_growth0[complete.cases( df_growth0$country, df_growth0$income, df_growth0$iso3c, df_growth0$avg_gdpgrowth, df_growth0$gdppc0, df_growth0$avg_n, df_growth0$avg_p, df_growth0$avg_nsy, df_growth0$avg_nry,df_growth0$avg_gi, df_growth0$avg_gx, df_growth0$avg_educ),] # Ta vekk land som mangler data 


df_growth <- subset(df_growth, select = c("country",  "region", "income", "iso3c", "iso2c","year", "poptot", "gdppc", "gdppc0", "avg_gdpgrowth", "avg_n", "avg_p", "avg_nsy", "avg_nry", "avg_gi", "avg_gx", "avg_educ"))

# Lage datasettet du vil bruke til analysen din
df_growth2019  <- df_growth %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>% 
  slice(n()) %>% # Behold den SISTE observasjonen for hvert land
  ungroup()
head(df_growth2019)


# Lag en variabel som er logaritmen av BNP per innbygger (enklere tolkning og presser sammen fordelingen)
df_growth2019$gdppc <-as.numeric(df_growth2019$gdppc)
df_growth2019$ln_gdppc<-log(df_growth2019$gdppc) 
df_growth2019$ln_gdppc0<-log(df_growth2019$gdppc0) 



# Analysedel til resultat  -----------------------------------------------------

# avg_gdpgrowth = Gjennomsnittlig årlig vekstrate i BNP per innbygger for hvert land i perioden
# avg_nsy = gj.snitt netto-sparing
# avg_educ = gj. humankapital (år i skole)
# avg_nry = Gjennomsnittlig årlig vekstrate (negativ) i naturressurser for hvert land
# avg_gi = Gjennomsnittlig årlig vekstrate i investeringer for hvert land
# avg_n = gj.snitt vekstrate i arbeidskraft.
# avg_p = Gjennomsnittlig årlig vekstrate i befolkningen for hvert land
# avg_gx = Gjennomsnittlig årlig vekstrate i eksport for hvert land
# ln_gdppc0 = log bnp for 2000. 

# Fjerne ekstremverdiene for et mer nøyaktig resultat. 
df <- df_growth2019[complete.cases( df_growth2019$avg_gi, df_growth2019$avg_n),]

Q1gi <- quantile(df$avg_gi, .25 )
Q3gi <- quantile(df$avg_gi, .75)
IQRgi <- IQR(df$avg_gi)

Q1n <- quantile(df$avg_n, .25 )
Q3n <- quantile(df$avg_n, .75)
IQRn <- IQR(df$avg_n)

no_outliers <- subset(df, df$avg_gi > (Q1gi - 1.5*IQRgi) & df$avg_gi < (Q3gi + 1.5*IQRgi) &  df$avg_n > (Q1n - 1.5*IQRn) & df$avg_n < (Q3n + 1.5*IQRn))
dim(no_outliers)

# Analyse av data.
# +1 perfekt positiv lineær relasjon, -1 perfekt negativ lineær relasjon, 0 ingen relasjon.
cor_df_growth2019 <- cor(df_growth2019[c("ln_gdppc", "avg_nsy", "avg_educ", "avg_nry", "avg_gi", "avg_n", "avg_p", "avg_gx", "ln_gdppc0")])
cor_BNP_growth2019 <- cor_df_growth2019[1,2:9]
print(round(cor_BNP_growth2019,4))

# regresjon
regresjon_df_growth2019 <- lm(ln_gdppc ~ avg_nsy + avg_educ + avg_nry + avg_gi + avg_n + avg_p + avg_gx + ln_gdppc0, data = df_growth2019)
summary(regresjon_df_growth2019)



# Velg ut de variabler du vil ha med i tabellen. Her er et eksempel (du skal ta med alle variabler som du har med i den empiriske analysen)
table_df_growth2019 <- subset(df_growth2019, select = c("avg_gdpgrowth","ln_gdppc", "avg_nsy", "avg_educ", "avg_nry", "avg_gi", "avg_n", "avg_p", "avg_gx", "ln_gdppc0"))
# Gi beskrivende navn til variablene (i samme rekkefølge som de ligger i datasettet) # pc = per capita
labs <- c("Gjennomsnitlig årlig vekstrate i BNP pc 2000-2019 (%)",
          "Logaritmisk vekstrate for BNP pc (2019)",
          "Gjennomsnittlig netto-sparing",
          "Gjennomsnittlig humankapital (år på skole)",
          "Gjennomsnittlig årlig vekstrate (negativ) i naturressurser",
          "Gjennomsnittlig årlig vekstrate i investeringer",
          "Gjennomsnittlig vekstrate i arbeidskraft",
          "Gjennomsnittlig årlig befolkningsvekst (%)",
          "Gjennomsnittlig årlig vekstrate i eksport",
          "Logaritmisk vekstrate for BNP pc (2000)") 

# Lag tabellen
st(table_df_growth2019, labels=labs, title = "Summary statistics with outliers",
   summ = list(
     c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'), # Beskriv hvilken statistikk du ønsker å vise
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Gjennomsnitt','SD','Min','Maks') # Gi navn til kolumnene
   ))




# Analyse av data uten ekstremobservasjoner. 
cor_df_no_outliers <- cor(no_outliers[c("ln_gdppc", "avg_nsy", "avg_educ", "avg_nry", "avg_gi", "avg_n", "avg_p", "avg_gx", "ln_gdppc0")])
cor_BNP_no_outliers <- cor_df_no_outliers[1,2:9]
print(round(cor_BNP_no_outliers,4))

# regresjon
regresjon_no_outliers <- lm(ln_gdppc ~ avg_nsy + avg_educ + avg_nry + avg_gi + avg_n + avg_p + avg_gx + ln_gdppc0, data = no_outliers)
summary(regresjon_no_outliers)

# Velg ut de variabler du vil ha med i tabellen. Her er et eksempel (du skal ta med alle variabler som du har med i den empiriske analysen)
table_df_no_outliers <- subset(no_outliers, select = c("avg_gdpgrowth","ln_gdppc", "avg_nsy", "avg_educ", "avg_nry", "avg_gi", "avg_n", "avg_p", "avg_gx", "ln_gdppc0"))
# Gi beskrivende navn til variablene (i samme rekkefølge som de ligger i datasettet) # pc = per capita
labs <- c("Gjennomsnitlig årlig vekstrate i BNP pc 2000-2019 (%)",
          "Logaritmisk vekstrate for BNP pc (2019)",
          "Gjennomsnittlig netto-sparing",
          "Gjennomsnittlig humankapital (år på skole)",
          "Gjennomsnittlig årlig vekstrate (negativ) i naturressurser",
          "Gjennomsnittlig årlig vekstrate i investeringer",
          "Gjennomsnittlig vekstrate i arbeidskraft",
          "Gjennomsnittlig årlig befolkningsvekst (%)",
          "Gjennomsnittlig årlig vekstrate i eksport",
          "Logaritmisk vekstrate for BNP pc (2000)") 

# Lag tabellen
st(table_df_no_outliers, labels=labs, title = "Summary statistics without outliers",
   summ = list(
     c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Gjennomsnitt','SD','Min','Maks') 
   ))




# plotting ---------------------------------------------------------------------



df_growth2019_n <- df_growth2019[complete.cases(df_growth2019$region,df_growth2019$poptot, df_growth2019$avg_p, df_growth2019$gdppc),] #Her tar jeg vekk land som mangler observasjoner. Dette gjør grafen penere.  

plot1 <- ggplot(df_growth2019_n, aes(avg_p, ln_gdppc, na.rm = T)) +
  xlab("Befolkningsvekst") + # Beskrivelse for x-akselen
  ylab("BNP per innbygger 2019") + # Beskrivelse for y-akselen
  theme_classic(base_size = 14) + # Tekststørrelse
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + # Størrelse (farge) på bobblene avhenger befolkningsstørrelse (region)
  scale_size_area(guide = "none", max_size = 14) + #Ta vekk legend for befolkningsstørrelse
  scale_y_continuous(trans = 'log2', breaks=c(0)) + # logaritmere BNP pc og hvilke "ticks" som skal vises
  scale_x_continuous(breaks=c(seq(-1,5,0.5))) #  "ticks" som skal vises på x-akselen
plot1


plot2 <- ggplot(df_growth2019_n, aes(avg_nsy, ln_gdppc, na.rm = T)) +
  xlab("Netto-sparing") + 
  ylab("BNP per innbygger 2019") + 
  theme_classic(base_size = 14) + 
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + 
  scale_size_area(guide = "none", max_size = 14) + 
  scale_y_continuous(trans = 'log2', breaks=c(0)) + 
  scale_x_continuous(breaks=c(seq(-10,30,5)))   
plot2


plot3 <- ggplot(df_growth2019_n, aes(avg_educ, ln_gdppc, na.rm = T)) +
  xlab("Humankapital (år på skolen)") + 
  ylab("BNP per innbygger 2019") + 
  theme_classic(base_size = 14) + 
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + 
  scale_size_area(guide = "none", max_size = 14) + 
  scale_y_continuous(trans = 'log2', breaks=c(0)) + 
  scale_x_continuous(breaks=c(seq(0,15,3))) 
plot3


plot4 <- ggplot(df_growth2019_n, aes(avg_nsy, avg_gdpgrowth, na.rm = T)) +
  xlab("Netto-sparing") + 
  ylab("Årlig vesktrate i BNP") + 
  theme_classic(base_size = 14) + 
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + 
  scale_size_area(guide = "none", max_size = 14) + 
  scale_y_continuous(trans = 'log2', breaks=c(0)) + 
  scale_x_continuous(breaks=c(seq(-10,30,5))) 
plot4


plot5 <- ggplot(df_growth2019_n, aes(avg_educ, avg_gdpgrowth, na.rm = T)) +
  xlab("Humankapital (år på skolen)") + 
  ylab("Årlig vesktrate i BNP") + 
  theme_classic(base_size = 14) + 
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + 
  scale_size_area(guide = "none", max_size = 14) + 
  scale_y_continuous(trans = 'log2', breaks=c(30)) 
plot5


