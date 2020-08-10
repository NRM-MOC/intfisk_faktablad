## Detta skript läser in och kombinerar data från de tre övervakningsprogrammen
## resultatet sparas som fig_data.csv


library(tidyverse)
library(readxl)

## Helper funs & tables

name_loc <- function(loc){
  str_replace(loc, "FJBA", "Fjällbacka") %>% 
    str_replace("HOLM", "Holmöarna") %>% 
    str_replace("KVFJ", "Kvädöfjärden") %>% 
    str_replace("TORH", "Torhamn")
}

loc_table <- tribble(
  ~LOC, ~locality,
  "FJBA", "Fjällbacka",
  "FJBAV", "Fjällbacka, vår",
  "FJBAA", "Fjällbacka, aug",
  "FJBAO", "Fjällbacka, okt",
  "TORH", "Torhamn",
  "KVFJ", "Kvädöfjärden",
  "HOLM", "Holmöarna",
  "JM", "Kvädöfjärden"
)

name_genus <- function(gen){
  str_replace(gen, "PERC", "Abborre") %>% 
    str_replace("ZOAR", "Tånglake") %>% 
    str_replace("BEST", "Fiskbestånd") %>% 
    str_replace("INDE", "Index, helfångst")
}

gift_koder <- tibble(var = c("ALDR", "TOTV", "TOTL", "FPRC", "KOND", 
                             "AG", "AL", "AS", "CD", "CR", "CU", "HG", "NI", "PB", "SE", "ZN", 
                             "DDT", "DDD", "DDE", "AHCH", "BHCH", "LINDA", "HCB", "CB28", 
                             "CB52", "CB101", "CB118", "CB153", "CB180", "PCBsum6", "CB77", 
                             "CB81", "CB126", "CB169", "CB123", "CB114", "CB105", "CB138", 
                             "CB167", "CB156", "CB157", "CB189", "CBEQ05", "CBEQV", "TCDF", 
                             "PECDF1", "PECDF2", "HXCDF1", "HXCDF2", "HXCDF3", "HXCDF4", "HPCDF1", 
                             "HPCDF2", "OCDF", "TCDD", "PECDD", "HXCDD1", "HXCDD2", "HXCDD3", 
                             "HPCDD", "OCDD", "TCDDEQ05", "TCDDEQV", "TCDDEQVW", "TEQ05lw", 
                             "TEQ05ww", "TEQ98lw", "TEQ98ww", "BDE28", "BDE47", "BDE99", "BDE100", 
                             "BDE153", "BDE154", "HBCD", "PBDEsum5", "PFHXA", "PFHPA", "PFOA", 
                             "PFNA", "PFDA", "PFUNDA", "PFDODA", "PFTRDA", "PFTEDA", "PFPEDA", 
                             "LPFBS", "PFHXS", "LPFOS", "BPFOS", "PFOS", "LPFDS", "PFDS", 
                             "LFOSA", "BFOSA", "FOSA", "MBT", "DIBT", "TBT", "MPT", "DIPT", 
                             "TPT", "MOT", "DIOT"), 
                     axis_label = c(("\u00C5lder, \u00E5r"), 
                                    ("Vikt, g"), 
                                    ("Längd, cm"), 
                                    ("Fett, % i muskel"), 
                                    ("Konditionsfaktor, K"), 
                                    "Koncentration, \u00b5g/g tv lever", 
                                    "Koncentration, \u00b5g/g tv lever", 
                                    "Koncentration, \u00b5g/g tv lever", 
                                    "Koncentration, \u00b5g/g tv lever", 
                                    "Koncentration, \u00b5g/g tv lever", 
                                    "Koncentration, \u00b5g/g tv lever", 
                                    "Koncentration, ng/g vv muskel", 
                                    "Koncentration, \u00b5g/g tv lever", 
                                    "Koncentration, \u00b5g/g tv lever",
                                    "Koncentration, \u00b5g/g tv lever", 
                                    "Koncentration, \u00b5g/g tv lever", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, \u00b5g/g vv", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, \u00b5g/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel",
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g fv muskel", 
                                    "Koncentration, pg/g vv muskel", 
                                    "Koncentration, pg/g fv", 
                                    "Koncentration, pg/g vv", 
                                    "Koncentration, pg/g fv", 
                                    "Koncentration, pg/g vv", 
                                    "Koncentration, ng/g fv muskel", 
                                    "Koncentration, ng/g fv muskel", 
                                    "Koncentration, ng/g fv muskel", 
                                    "Koncentration, ng/g fv muskel", 
                                    "Koncentration, ng/g fv muskel", 
                                    "Koncentration, ng/g fv muskel", 
                                    "Koncentration, ng/g fv muskel", 
                                    "Koncentration, ng/g vv", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever", 
                                    "Koncentration, ng/g vv lever")) %>% 
  mutate(fig_order = 1:n(), start_zero = !(var %in%c("ALDR", "TOTV", "TOTL", "FPRC", "KOND")), fig_order = ifelse(var == "CB138", 26.5, fig_order))

## Provfiske

provfiske_path <- "data/Copy of Intfisk_DataKlab_NM2020423.xlsx"

provfisk_arsvariabler <- read_excel(provfiske_path, sheet = "Årsvariabler", guess_max = 10000) %>% 
  select(c("År", "LOC", "Artantal", "Diversitet", "Trofi", "AbborreL90", "TanglakeTidigtdoda", "TanglakeSentdoda", "TanglakeMissbildade", "TanglakeOnormala")) %>% 
  rename(year = År) %>% 
  pivot_longer(cols = c("Artantal", "Diversitet", "Trofi", "AbborreL90", "TanglakeTidigtdoda", "TanglakeSentdoda", "TanglakeMissbildade", "TanglakeOnormala"), 
               names_to = "var", values_to = "value")

provfisk_dagsvariabler <- read_excel(provfiske_path, sheet = "Dagsvariabler", guess_max = 10000) %>% 
  select(c("År", "Fiskedag", "LOC", "SalinitetF", "SiktdjupF")) %>% 
  rename(year = År) %>% 
  pivot_longer(cols = c("SalinitetF", "SiktdjupF"), names_to = "var", values_to = "value")

provfisk_sasongsvariabler <- read_excel(provfiske_path, sheet = "Säsongsvariabler", guess_max = 10000) %>% 
  select(c("År", "Mån", "Dag", "LOC", "TemperaturS", "SiktdjupS")) %>% 
  rename(year = År) %>% 
  pivot_longer(cols = c("TemperaturS", "SiktdjupS"), names_to = "var", values_to = "value")

provfisk_stationsvariabler <- read_excel(provfiske_path, sheet = "Stationsvariabler", guess_max = 10000) %>% 
  select(c("År", "Station", "LOC", "TemperaturF", "Abborre", "Karpf/Mesop", "Strandkrabba", "Totalfangst", "Torsk", "Tanglake",  "Al", "Rovfisk")) %>% 
  rename(year = År) %>% 
  pivot_longer(cols = c("TemperaturF", "Abborre", "Karpf/Mesop", "Strandkrabba", "Totalfangst", "Torsk", "Tanglake", "Al", "Rovfisk"),
               names_to = "var", values_to = "value")

provfisk_individvariabler <- read_excel(provfiske_path, sheet = "Individvariabler", guess_max = 100000) %>% 
  select(c("År", "Individ", "LOC", "GENUS", "LangdAr1", "LangdAr2", "LangdAr3", "LangdAr4", "LangdAr5", "LangdAr6", "ESI", "Kondition")) %>% 
  rename(year = År) %>% 
  mutate(GENUS = ifelse((year == 2019) & is.na(GENUS), "ZOAR", GENUS)) %>% # Fix missing genus
  pivot_longer(cols = c("LangdAr1", "LangdAr2", "LangdAr3", "LangdAr4", "LangdAr5", "LangdAr6", "ESI", "Kondition"), 
               names_to = "var", values_to = "value")

fisk_koder <- read_excel("data/Copy of Intfisk_DataKlab_NM20200331.xlsx", sheet = "Enheter") %>% 
  select(c("Variabel", "Matris (Rubrik på grafen?)", "Enhet (y-axeln?)")) %>% 
  rename(var = Variabel, var_name = `Matris (Rubrik på grafen?)`, axis_label = `Enhet (y-axeln?)`) %>% 
  mutate(fig_order = 1:n())

provfisk_data <- bind_rows(provfisk_arsvariabler, provfisk_dagsvariabler, provfisk_sasongsvariabler, provfisk_stationsvariabler, provfisk_individvariabler) %>% 
  select(year, LOC, GENUS, var, value) %>% 
  left_join(fisk_koder, by = "var") %>%
  mutate(var = var_name) %>% 
  filter(!is.na(value)) %>% 
  filter(!((var == "Längd vid 5 års ålder") & (LOC == "FJBAO"))) %>% 
  mutate(fig_order = fig_order + 2000,
         GENUS = name_genus(GENUS),
         fig_title = ifelse(is.na(GENUS), var, paste(var, "hos", tolower(GENUS))),
         fig_title = str_replace(fig_title, "linefeed", "\n"),
         fig_title = ifelse((LOC == "KVFJ") & str_detect(fig_title, "Vattentemperatur"), paste0(fig_title, " "), fig_title),
         start_zero = FALSE
  )

## Fiskhälsa

health_path <- "data/Kopia av Kustfisk_hälsa 1988-2019 ver 200331 figurtitlar fix.xlsx"

health_koder <- read_excel(health_path, sheet = 1, skip = 5) %>% 
  select(var = Variabler, axis_label = Enhet, fig_title = Titel) %>% 
  mutate(fig_order = 1:n() + 1000)

health_data <- read_excel(health_path, sheet = 2, guess_max = 5000) %>% 
  mutate(year = as.numeric(År)) %>% 
  filter(Säsong == "Höst") %>% 
  select(-`Provtagningsdatum yyyymmdd`, -fisk_nr, -Säsong, -År) %>% 
  pivot_longer(-c("GENUS", "LOC", "year", "Kön"), names_to = "var", values_to = "value", values_drop_na = TRUE) %>% 
  filter(((Kön == 0) | (var %in% c("EROD", "GST", "GSI", "Vtg")))) %>% 
  left_join(health_koder, by = "var") %>% 
  mutate(fig_title = paste(fig_title, "hos", tolower(name_genus(GENUS))),
         fig_title = ifelse((var %in% c("EROD", "GST", "GSI", "Vtg")) & (GENUS == "ZOAR"), 
                            str_replace(fig_title, "tånglake", ifelse(Kön == 0, "hontånglake", "hantånglake")), fig_title),
         fig_title = ifelse((var %in% c("EROD", "GST", "GSI", "Vtg")) & (GENUS == "PERC"), 
                            str_replace(fig_title, "abborre", ifelse(Kön == 0, "honabborre", "hanabborre")), fig_title),
         start_zero = var %in% c("EROD", "GR", "GST", "Katalas"),
         var = fig_title) %>% 
  select(-Kön)

## Miljögifter

gift_data <- read_csv("data/aggdata.csv", guess_max = 6000) %>% 
  select(year = YEAR, LOC,  value, var, gen, label) %>% 
  filter(LOC %in% c("FJBA", "KVFJ", "HOLM"), gen %in% c("ZOAR", "PERC"), !((LOC == "HOLM") & (gen == "ZOAR"))) %>%
  left_join(gift_koder, by = "var") %>% 
  select(year, value, var = label, LOC, GENUS = gen, axis_label, fig_order, start_zero) %>% 
  left_join(read.csv("rubriker.csv", sep = ";"), by = c("var" = "eng")) %>% 
  mutate(GENUS = name_genus(GENUS),
         fig_title = ifelse(is.na(GENUS), swe, paste(swe, "hos", ifelse(swe %in% c("Ålder", "Vikt", "Längd", "Fetthalt", "Konditionsfaktor"), 
                                                                        paste("analyserad", tolower(GENUS)), tolower(GENUS)))),
         fig_title = str_replace(fig_title, "linefeed", "\n"),
         fig_order = fig_order + 3000) %>% 
  filter(!(str_detect(var, "BDE") & (year == 1999))) %>% 
  select(-swe)

## Sammanställning

fig_data <- bind_rows(provfisk_data, health_data, gift_data) %>% 
  left_join(loc_table, by = "LOC")  %>% 
  arrange(fig_order) %>% 
  group_by(year, locality, axis_label, fig_title, fig_order, start_zero) %>% 
  summarise(value = mean(value)) # Aggregera med årligt aritmetiskt medelvärde (miljögifter redan aggregerade med geometriskt)

write_csv(fig_data, "fig_data.csv")
