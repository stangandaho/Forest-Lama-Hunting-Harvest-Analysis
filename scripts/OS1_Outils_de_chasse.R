# load necessary package
source("scripts/setup.R")
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


# import data
dynamic_dt <- read_xlsx(path = "datasets/Cleaned_Data/Questionnaire_sur_la_dynamique_Spatio-temporelle.xlsx")


## Technique de chasse avant
tech_chasse_avant <- dynamic_dt %>% 
  select(22:32) %>% 
  mutate(across(.cols = all_of(1:11), .fns = bin_to_chr))

colnames(tech_chasse_avant) <- gsub("Outils de chasse_|_Avant|_", "", colnames(tech_chasse_avant) )
tca_list <- list()

for (col in colnames(tech_chasse_avant)) {
  
  tca <- tech_chasse_avant[, col] %>% 
    summarise(n = n(), .by = col) %>% 
    mutate(Pratique = colnames(.)[1]) %>% 
    rename(Utilise = 1) %>% 
    relocate(Pratique, .before = 1) %>% 
    mutate(prop = round(n*100/sum(n), 2))
  
  tca_list[[col]] <- tca
  
}

tca_tbl <- bind_rows(tca_list) %>% 
  mutate(Temps = "Avant")

## Technique de chasse Audjourd'hui
tech_chasse_auj <- dynamic_dt %>% 
  select(17:20) %>% 
  mutate(across(.cols = all_of(1:4), .fns = bin_to_chr))

colnames(tech_chasse_auj) <- gsub("Outils de chasse_|_Aujourd'hui|_", "", colnames(tech_chasse_avant) )
tcauj_list <- list()

for (col in colnames(tech_chasse_auj)) {
  
  tcauj <- tech_chasse_auj[, col] %>% 
    summarise(n = n(), .by = col) %>% 
    mutate(Pratique = colnames(.)[1]) %>% 
    rename(Utilise = 1) %>% 
    relocate(Pratique, .before = 1) %>% 
    mutate(prop = round(n*100/sum(n), 2))
  
  tcauj_list[[col]] <- tcauj
  
}

tcauj_tbl <- bind_rows(tcauj_list) %>% 
  mutate(Temps = "Aujourd'hui")

tech_chasse_tbl <- bind_rows(tca_tbl, tcauj_tbl)
# Transformer le temps en facteur 
tech_chasse_tbl$Temps <- factor(tech_chasse_tbl$Temps, levels = c("Avant", "Aujourd'hui"))

tech_chasse_tbl %>% 
  ggplot()+
  geom_col(mapping = aes(x = Pratique, y = prop, fill = Utilise, group = Temps))+
  facet_wrap(facets = ~ Temps)+
  geom_text(mapping = aes(x = Pratique, y = prop, 
                          label = paste0(prop, "%"), group = Temps),
            position = position_stack(vjust = 0.5),
            size = 8, color = "white", family = "mm", 
            angle = 90)+
  scale_fill_manual(values = color_plt[c(1, 3)])+
  labs(x = "Outils de chasse", fill = "")+
  theme(
    # axis
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 18, family = "mm", colour = color_plt[4], 
                             angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_text(size = 20, family = "msb", 
                                margin = margin(t = .8, b = 1, unit = "lines")),
    # panel
    panel.background = element_rect(fill = NA, colour = color_plt[1]),
    # strip
    strip.background = element_rect(fill = color_plt[1], colour = color_plt[1]),
    strip.text = element_text(colour = color_plt[4], 
                              family = "msb", size = 25,
                              margin = margin(t = .8, b = .8, unit = "lines")),
    # Legende
    legend.text = element_text(size = 18, family = "mm", colour = color_plt[4]),
    legend.key.height = unit(.1, units = "lines"),
    legend.key.width = unit(1.5, units = "lines"),
    legend.key.spacing.y = unit(.8, units = "lines"),
  )
ggsave(filename = "plots/OS1_outils_de_chasse.jpeg", width = 20, height = 12, dpi = 100)

