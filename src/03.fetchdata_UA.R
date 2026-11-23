# script gerecupereerd uit projectmappen IP
# licht aangepast, bevraagt twee datafiles uit IP

library(sf)
library(tidyverse)
library(lubridate)
library(diffobj)

options(stringsAsFactors = FALSE)

#### alternatief & scenario ####  

Alternatieven.UA <- c("REF", "C1", "C3")
Scenario <- "A0CN"

######## START LOOP ######## 
for (i in 1:length(Alternatieven.UA)) {

Alternatief.UA <- Alternatieven.UA[i]

#### input & output ####
Name.in <- str_c("INBO_out", Alternatief.UA, Scenario, "diffuse_IP_TEMP.csv", sep = "_")
Data.in <- str_c("../../../Data/Data_UA/", Name.in)
GDB.path <- "../../../../Basis/Boxes.gdb"

# data_out <- str_c("../../../Data/Data_processed/", "data_UA_", alternatief_UA, "_", scenario, "_IP_TEMP.csv")
Data.out <- str_c("../../../Data/Data_processed/", "data_UA_", Alternatief.UA, "_", Scenario, "_maand_IP_TEMP.csv")


#### ophalen data ####
# data die veranderen per alternatief en scenario
Data.UA <- read_csv(Data.in)

Layer.name <- "Scheldeboxes_IP_TEMP_metadata"
# Read spatial data and drop the geometry column
Metadata.UA <- st_read(dsn = GDB.path, layer = Layer.name) %>% 
  st_drop_geometry() %>% 
  rename(box = boxes)


#### data ecosysteem ####
Data.UA <- Data.UA %>%
  mutate(date = as_date(realtimes.C),
         year = year(date),
         month = month(date),
         day = day(date),
         date = as.character(date)) %>%
    select(-realtimes.C)

Data.UA %>% distinct(year)

Data.UA <- Data.UA %>% filter(year <= 2050)

# pivot_longer  
Data.UA <- Data.UA %>%
pivot_longer(cols = -c(date, year:day),
               names_to = "var_box",
               values_to = "value")
# 3697650 records

# pivot_wider per variabele
Data.UA <- Data.UA %>%
  separate(var_box, c("var", "box")) %>%
  mutate(box = as.numeric(box)) %>%
  pivot_wider(names_from = var,
              values_from = value) %>%
  right_join(Metadata.UA)
# door de right_join vallen heel wat data weg (alles data binnen boxen 0, en 65 -> 74 )
# 118690 records

#### grafiekjes ####
print(
  Data.UA %>%
  mutate(date = ymd(date)) %>%
  filter(month == 5,
         box %in% c(1,21,41,51,61,75)) %>%
  ggplot(aes(day, Sal)) +
  geom_line(aes(color = ordered(year))) +
  scale_y_continuous(limits = c(-1,30)) +
  facet_wrap(~box, labeller = "label_both")
)

print(Data.UA %>%
  mutate(date = ymd(date)) %>%
  filter(month == 5,
         box %in% c(1,21,41,51,61,75)) %>%
  ggplot(aes(day, O2)) +
  geom_line(aes(color = ordered(year))) +
  scale_y_log10(limits =c(0.03,1.5)) +
  facet_wrap(~box, labeller = "label_both")
)

print(Data.UA %>%
  mutate(date = ymd(date)) %>%
  filter(month == 5,
         box %in% c(1,21,41,51,61,75)) %>%
  ggplot(aes(day, Temp)) +
  geom_line(aes(color = ordered(year))) +
  scale_y_continuous(limits = c(10,21)) +
  facet_wrap(~box, labeller = "label_both")
)

print(Data.UA %>%
  mutate(date = ymd(date)) %>%
  filter(month == 5,
         box %in% c(1,21,41,51,61,75)) %>%
  ggplot(aes(day, Sed)) +
  geom_line(aes(color = ordered(year))) +
  scale_y_continuous(limits = c(0,300)) +
  facet_wrap(~box, labeller = "label_both")
)

print(Data.UA %>%
  mutate(date = ymd(date)) %>%
  filter(month == 5,
         box %in% c(1,21,41,51,61,75)) %>%
  ggplot(aes(day, ZooC)) +
  geom_line(aes(color = ordered(year))) +
  scale_y_continuous(limits = c(0,0.2)) +
  facet_wrap(~box, labeller = "label_both")
)

print(Data.UA %>%
  mutate(date = ymd(date)) %>%
  filter(month == 5,
         box %in% c(1,21,41,51,61,75)) %>%
  ggplot(aes(day, ResidenceTime)) +
  geom_line(aes(color = ordered(year))) +
  scale_y_continuous(limits = c(0,200)) +
  facet_wrap(~box, labeller = "label_both")
)

print(Data.UA %>%
  mutate(date = ymd(date)) %>%
  filter(month == 5,
         box %in% c(1,21,41,51,61,75)) %>%
  ggplot(aes(day, ResidenceTime)) +
  geom_line(aes(color = ordered(year))) +
  scale_y_log10(limits = c(0.05,200)) +
  facet_wrap(~box, labeller = "label_both")
)

print(Data.UA %>%
  mutate(date = ymd(date)) %>%
  filter(box %in% c(1,21,41,51,61,75),
         # box %in% c(1,6,11,16,21,26,28,29,30),
         month == 5) %>%
  ggplot(aes(day, 1/ResidenceTime)) +
  geom_line(aes(color = ordered(year))) +
  # scale_y_continuous(limits = c(0,15)) +
  facet_wrap(~box, labeller = "label_both")
)


#### data per maand, vanaf grens BE-NL ####
Data.UA.maand <- Data.UA %>%
  mutate(
    # ZooC = ZooC*12.0107, # Moleculair gewicht C  : 12.0107 g/mol
    DIN = NH4 + NO3,
    Verplaatsing_std = 1/ResidenceTime) %>%
  group_by(box, X, Y, AfstMondingCentroide, AfstMondingBovengrens, year, month) %>%
  summarise(across(c(Temp, Sal, O2, Chla, TOC, PhytoC, DiatC, DetC, DIN, Sed, ZooC, ResidenceTime, Verplaatsing_std), 
                   ~ mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  rename(Temperatuur = Temp,
         Salinity = Sal,
         Oxygen = O2,
         SPM = Sed) %>%
  arrange(box, year, month) %>%
  mutate(Salinity = if_else(Salinity < 0, 0, Salinity),
         AfstMondingOndergrens = case_when(box == 75 ~ 0,
                                           TRUE ~ lead(AfstMondingBovengrens, n = n_distinct(year, month))),
         Aslengte = AfstMondingBovengrens - AfstMondingOndergrens,
         Retentie = ResidenceTime/Aslengte,
         Verplaatsing = Aslengte*Verplaatsing_std)  %>%
  filter(box < 64 | box == 75)
# hiermee wordt de meest stroomafwaartse box 64 weg gefilterd (was enkel nodig voor de ondergrens van box 63) 
# 3840 records

# 2 grafiekjes:
print(
  ggplot(Data.UA.maand,
       aes(box, Temperatuur, color = factor(year))) +
  geom_line() +
  facet_wrap(~ month, labeller = "label_both")
)

print(ggplot(Data.UA.maand,
       aes(box, Temperatuur, color = factor(month))) +
  geom_line() +
  facet_wrap(~ year)
)

#### koppeling met KRW/OMES ####
Path.shape <- file.path("Q:/Projects/PRJ_Schelde/GIS_Zeeschelde/Referentiedata/gebiedsindelingen",
                       "ScheldevalleiSalzone_KRW_OMES_2022.shp")
KRW.OMES.shp <- read_sf(Path.shape)
st_crs(KRW.OMES.shp)

Data.UA.maand.sf <- st_as_sf(Data.UA.maand, coords = c("X", "Y"))
st_crs(Data.UA.maand.sf) <- 31370

Data.UA.maand <- Data.UA.maand.sf %>%
  st_join(KRW.OMES.shp) %>% 
  st_drop_geometry()

# check:
summary(Data.UA.maand)
# 240 NA's
Data.UA.maand %>% filter(is.na(Naam)) %>% distinct(box) %>% pull()
Data.UA.maand %>% filter(box %in% c(1:11)) %>% select(box, SalZone, Naam, Omes, KRWzone) %>% distinct()
# check in ArcMap: de XY coordinaten vallen net buiten de begrenzingen van KRW/OMES
# dit is weinig elegant -> zorgen dat XY coordinaten goed staan voor het runnen van het script

# als voorlopige oplossing handmatig aanvullen:
Data.UA.maand <- Data.UA.maand %>%
  rename(OMES = Omes) %>%
  mutate(SalZone = case_when(box %in% c(2, 4, 10) ~ "Zoet korte verblijftijd",
                             box %in% c(75) ~ "Oligohalien",
                             TRUE ~ SalZone),
         Naam = case_when(box == 2 ~ "Ringvaart",
                          box %in% c(4, 10) ~ "Zeeschelde",
                          box %in% c(75) ~ "Rupel",
                          TRUE ~ Naam),
         OMES = case_when(box == 2 ~ "Ringvaart",
                          box == 4 ~ "19",
                          box == 10 ~ "18",
                          box %in% c(75) ~ "Rupel",
                          TRUE ~ OMES),
         KRWzone = case_when(box %in% c(2, 4, 10) ~ "Zeeschelde I",
                             box %in% c(75) ~ "Zeeschelde III + Rupel",
                             TRUE ~ KRWzone))
summary(Data.UA.maand)
table(Data.UA.maand$Naam)
table(Data.UA.maand$OMES)
# voor de trajecten Gent-Melle & Durme is het box nr = 0 en hiervoor werden geen data aangeleverd

#### wegschrijven data ####
# write_delim(data.UA, data_out, delim = ";")
write_delim(Data.UA.maand, Data.out, delim = ";")
  
}
