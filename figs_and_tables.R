library(tidyverse)
source("functions.R", encoding = "utf-8")

## Läs in data sammanställt av pred_data.R

fig_data <- read_csv("fig_data.csv") %>% 
  mutate_if(is.character, ~ifelse(is.na(.x), "", .x)) %>%
  group_by(fig_title, locality) %>% 
  filter(n() > 5 | str_detect(fig_title, "Salinitet")) %>% # Tar bort serier med färre än 6 års observationer (undantaget Salinitet)
  ungroup() %>% 
  arrange(fig_order)

## data skall innehålla följande kolumner
# value: mätvärde
# year: år vid mätning
# locality: namn på lokal
# axis_label: text på figurens y-axel (t.ex. enhet)
# fig_title: figurens titel, en figur genereras för varje unik titel
# fig_order: eventuellt ordningsnummer på figur
# start_zero: antar värdet TRUE om y-axeln skall börja vid 0, annars FALSE


## all_figs nedan blir en tabell med kolumner
# fig_path: relativ sökväg där figur skall sparas (byt ".png" mot t.ex. ".pdf" om figur skall sparas i detta format)
# gg_fig: figur som ggplot-objekt (skapas av funktionen hav_fig)
# fig_order: ordningsnummer
all_figs <- fig_data %>%
  mutate(fig_path = paste0("figs/", 
                           stringi::stri_trans_general(fig_title, "Latin-ASCII") %>% 
                             str_remove_all( "[^a-z A-Z]") %>% str_replace_all(" ", "_")
                           , ".png")) %>% 
  group_by(fig_path) %>% 
  nest() %>% 
  mutate(gg_fig = map(data, hav_fig), 
         fig_order = map_dbl(data, ~pull(.x, fig_order) %>% mean())) %>% 
  ungroup()

# spara figurerna på disk
walk2(all_figs$fig_path, all_figs$gg_fig, 
      ~ggsave(filename = .x, plot = .y, 
              width = 7.5, height = 7.5, 
              units = "cm", scale = 2.6, type = "cairo")
)

# spara en lista över sökvägar och figurordning
all_figs %>% select(fig_path, fig_order) %>% 
  write_csv(path = "figpaths.csv")


# all_tables blir en tabell som sammanfattar de linjäre regresionerna (kolumner title, locality, trend, ci, p_value, r2)
all_tables <- fig_data %>% 
  group_by(fig_title) %>% 
  nest() %>% 
  mutate(tab = map(data, hav_tab),
         tab = map2(fig_title, tab, ~bind_cols(title = .x, .y))) %>% 
  pull(tab) %>% 
  bind_rows()

# spara tabellen
write_csv(all_tables, "tables.csv")


