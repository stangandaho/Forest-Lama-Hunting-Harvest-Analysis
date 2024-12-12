# load necessary package
source("scripts/setup.R")
source("scripts/utils.R")
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


# import data
dynamic_dt <- read_xlsx(path = "datasets/Cleaned_Data/Questionnaire_sur_la_dynamique_Spatio-temporelle.xlsx")

## nombre de chasse avant versus maintenant
nbr_chass_avant <- as.numeric(gsub(" fois", "", dynamic_dt$`Nombre de chasse avant`))
nbr_chass_maint <- as.numeric(gsub(" fois", "", dynamic_dt$`Nombre de chasse Now`))
rela_dt <- tibble(avant = nbr_chass_avant, maint = nbr_chass_maint)


## verifier la relation entre nombre de chasse avant et maintenant
cor(nbr_chass_avant, nbr_chass_maint, method = "spearman") # coefficient = 0.6697788
cor.test(nbr_chass_avant, nbr_chass_maint, method = "spearman") # rho = 0.6697788 p-value = 5.161e-05

## model linear pour quantifier l'effet nbr_chass_avant sur nbr_chass_main
nbr_chass_mdl <- glm(formula = maint ~ avant, data = rela_dt, family = "poisson")
summary(nbr_chass_mdl)

## Afficher la relation sur un plot
ggplot(data = rela_dt)+
  geom_point(mapping = aes(x = avant, y = maint), 
             color = color_plt[4], size = 2)+
  geom_smooth(mapping = aes(x = avant, y = maint), method = "glm", 
              fill = color_plt[1], color = color_plt[4])+
  labs(x = "\nNombre de chasse avant", y = "Nombre de chasse maintenant")+
  theme(
    # Axes
    axis.title = element_text(size = 20, color = color_plt[1], family = "msb"),
    axis.text = element_text(size = 18, color = color_plt[3], family = "mm"),
    axis.line = element_line(colour = color_plt[3], linewidth = 1.5)
  )

ggsave(filename = "plots/OS1_nombre_chasse_avant_maint.jpeg", 
       width = 15, height = 15, units = "cm", dpi = 200)

## Occurrence des pratiques de chasse
pratique_chasse <- dynamic_dt %>% 
  select(`2. Quelles sont les pratiques traditionnelles que vous utilisez avant ou pendant la chasse ?`) %>% 
  rename(pratique_chasse = 1) %>% 
  count(pratique_chasse) %>% 
  mutate(prop = (n/sum(n))*100,
         x = rep("A", 4)) %>% 
  arrange(prop)

pratique_chasse$pratique_chasse <- factor(pratique_chasse$pratique_chasse, 
                                             levels = pratique_chasse$pratique_chasse)
  
ggplot(data = pratique_chasse)+
  geom_col(mapping = aes(x = x, y = prop, 
                         fill = pratique_chasse))+
  theme_void()+
  geom_text(mapping = aes(x = x, y = prop, 
                          label = paste0(round(prop, 1), "%"),
                          group = pratique_chasse),
            position = position_stack(vjust = 0.5),
            size = 18, family = "msb", color = "white")+
  labs(fill = "")+
  scale_fill_manual(values = color_plt[1:4])+
  coord_flip()+
  theme(
    #legend
    legend.position = c(0.5, 0.92),
    legend.direction = "horizontal",
    legend.text = element_text(size = 45, color = color_plt[4], family = "mm",
                               margin = margin(r = 2.5, l = 0.1, unit = "lines")),
    legend.key.height = unit(2, units = "lines"),
    legend.key.width = unit(0.5, units = "lines")
  )
ggsave(filename = "plots/OS1_pratique_tradi_avant_chasse.jpeg", width = 22, height = 5, dpi = 200)

## Occurrence des periodes de chasse
periode_chasse <- dynamic_dt %>% 
  select(`Période de chasse`) %>% 
  count(`Période de chasse`) %>% 
  mutate(prop = (n/sum(n))*100) %>% 
  arrange(prop)
### Saison sèche ==> 20%
### Saison pluvieuse ==> 23.3%
### Toutes les saisons ==> 17  56.7%

## Chasse fructueuse depend t-elle d'une saison de chasse?
periode_fructueuse <- dynamic_dt %>% 
  select(`Période de chasse`, `La période de chasse et la période fructueuse ont-t-elles changées dans le temps ?`) %>% 
  rename(fructeuse = 2) %>% 
  filter(fructeuse != "N/A") %>% 
  table() %>% 
  fisher.test()

### h0: no association, ==> independence if p < 0.05. In our case, p-value = 0.4597

## Verifier la dependance entre la modalite des variables par probabilite conditionelle
cond_proba(data = periode_fructueuse, by_row = T)

## Animaux chasses
anim_chass <- dynamic_dt %>% 
  select(63:64)


especes_list <- list()
nombre_list <- list()
for (r in 1:nrow(anim_chass)) {
  especes <- strsplit(anim_chass$`Espèces tuées`, split = ",")
  nombre <- strsplit(anim_chass$Nombre, split = ",")
  
  if (all(especes[[r]] != "NA")) {
    correct_esp <- especes[[r]]
    correct_num <- nombre[[r]]
    
    for (sp in 1:length(correct_esp)) {
      especes_list[[paste0(r, "_", correct_esp[sp])]] <- correct_esp[sp] # each
      nombre_list[[paste0(r, "_", correct_esp[sp])]] <- correct_num[sp] # corresponding number
    }
    
  }
  
}

anim_tbl <- tibble(especes = unlist(especes_list), nombre = unlist(nombre_list)) %>% 
  mutate(especes = trimws(especes),
         nombre = as.numeric(trimws(nombre)),
         especes = case_when(especes == "Pangolin genette" ~ "Pangolin", 
                             especes == "Achou" ~ "Rat géant",
                             TRUE ~ especes)) %>% 
  group_by(especes) %>% 
  summarise(nombre = sum(nombre, na.rm = TRUE)) %>% 
  filter(nombre != 0) %>% 
  mutate(nombre = floor(nombre),
         prop = round(nombre*100/sum(nombre), 2)) %>% 
  arrange(prop)

anim_tbl$especes <- factor(anim_tbl$especes, levels = anim_tbl$especes)

anim_tbl %>% 
  mutate(label_pos = case_when(especes == "Céphalophe de walter" ~ prop + 50, TRUE ~ prop + 25)) %>% 
  ggplot()+
  geom_col(mapping = aes(x = especes, y = prop, fill = prop), show.legend = FALSE)+
  scale_fill_gradientn(colours = color_plt[1:4])+
  ylim(-50, 100)+
  geom_text(mapping = aes(x = especes, y = label_pos, 
                          label = paste0(especes,"\n(", prop, "%)")),
            angle = calc_angle(1:nrow(anim_tbl)),
            family = "msb", size = 10, lineheight = 0.3)+
  coord_polar()+
  theme_void()
ggsave(filename = "plots/OS1_especes_tuees.jpeg", width = 16, height = 16, units = "cm")
