###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Summary statistics - Nb studies per Category of traits - Aspalter database
### Author: Julie Gauzere May 2026
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)


### 3 main categories of traits:
# •	Growth: growth, allocation, physiology, anatomy/morphology, leaf/root chemical composition, vegetative phenology
# •	Reproduction: reproductive traits, seed traits, seed germination, reproductive phenology
# •	Defense: survival, pest interaction, biotic stress tolerance, abiotic stress tolerance, damage traits, hydraulics, defence chemistry, response to experimental treatments (dry, hot)
#~~ REMOVE: "Plasticity indices" and "Other" categories

# load data ---------------------------------------------------------------
dummy <- readxl::read_xlsx(path = "S1_Data_extractions.xlsx", sheet = 1)


# Age classes defined by Aspalter
levels(factor(dummy$Age.class))
# [1] "cutting"                       "juvenile tree"                 "juvenile tree and mature tree"
# [4] "mature tree"                   "multiple age groups"           "not stated"                   
# [7] "seedling"                      "seedling and juvenile tree"


# List and open all the .txt files containing the list of traits per main category (Growth, Defense, Reproduction)
chemin_dossier <- getwd()
fichiers <- list.files(path = paste0(chemin_dossier, "/Category_Growth_Repro_Defense"), 
                       pattern = "\\.txt$", 
                       full.names = TRUE)

df_final <- fichiers %>% 
  set_names(tools::file_path_sans_ext(basename(.))) %>% 
  map(readLines) %>% 
  stack() %>% 
  rename(Trait = values, Categorie = ind)

head(df_final)



## Add the new Category to the dummy file
# merge the two files
fichier_final <- dummy %>% 
  left_join(df_final, by = "Trait")

View(fichier_final)


traits_sans_categorie <- dummy %>% 
  anti_join(df_final, by = "Trait") %>% 
  select(Trait, Group.y) %>% 
  distinct()

View(traits_sans_categorie)



## Keep only one line per study, trait and age of measurement
# Note: the Aspalter database contains a lot of duplicated lines per type of analysis, covariate considered, etc....
table_unique <- fichier_final %>%
  distinct(Paper.ID, Trait, Age, .keep_all = TRUE)
nrow(table_unique) # n=1885


## Define new age classes based on the "Age.class" level of Aspalter but correcting for some mistakes detected in the database
table_unique[which(as.numeric(table_unique$Age) <5 & table_unique$Categorie == "Reproduction"), "Age.class"] <- "seed"
table_unique[which(as.numeric(table_unique$Age) >=5 & table_unique$Categorie == "Reproduction"), "Age.class"] <- "mature tree"
table(table_unique$Age.class, useNA = "ifany")

# we remove the ambiguous age classes of the database
table_unique_class = table_unique[-which(table_unique$Age.class %in% c("multiple age groups", "cutting", "not stated")),]

# we account for traits measured at multiple age classes (e.g., "juvenile and mature tree", "seedling and juvenile tree")
table_unique_class <- table_unique_class %>%
  mutate(Age.class = str_replace(Age.class, "juvenile and mature tree", "seedling and juvenile tree")) %>%
  separate_rows(Age.class, sep = " and ")

## This table gives the number of studies per age class
table(table_unique_class$Age.class, useNA = "ifany")

## Table of the number of studies per age class and category of trait
NbStudy_trait_age =as.data.frame.matrix(table(table_unique_class$Categorie, table_unique_class$Age.class, useNA = "ifany"))
# remove empty Category (Plasticity indices, others...)
NbStudy_trait_age= NbStudy_trait_age[-which(rownames(NbStudy_trait_age) == "NA."),]

# Create a data frame for the graphics with ggplot2
Tab_Freq_trait_age = as.data.frame(table(table_unique_class$Categorie, table_unique_class$Age.class, useNA = "ifany"))
names(Tab_Freq_trait_age) = c("Category","Age", "Number_study")
# remove empty levels
Tab_Freq_trait_age = Tab_Freq_trait_age[which(!is.na(Tab_Freq_trait_age$Category)),]
# sort some variables by levels for graphical representations (Age, Category)
Tab_Freq_trait_age$Age <- factor(Tab_Freq_trait_age$Age, levels = c("seed", "seedling", "juvenile tree", "mature tree"))
Tab_Freq_trait_age$Category <- factor(Tab_Freq_trait_age$Category, levels = c("Growth","Defense", "Reproduction" ))



## Plot p and table "NbStudy_trait_age" were used to make Figure 2
p <- ggplot(Tab_Freq_trait_age, aes(x = Category, y = Number_study, fill = Age)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  labs(x= "", y = "Number of studies", fill = "Age class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p, filename = "NbStudies_Trait_Age_3class.pdf",
       device = "pdf",
       height = 10, width = 14, units = "in")




####################################
## Analysing the results per species

nrow(table_unique_class) # n= 1839
names(table_unique_class)
levels(as.factor(table_unique_class$Species))
# [1] "A.alba"           "A.platanoides"    "A.pseudoplatanus" "B.pendula"        "B.pubescens"      "C.sativa"         "C.sempervirens"   "F.alnus"        
# [9] "F.sylvatica"      "J.excelsa"        "L.decidua"        "P.abies"          "P.brutia"         "P.halepensis"     "P.nigra"          "P.padus"        
# [17] "P.pinaster"       "P.spinosa"        "P.sylvestris"     "Q.frainetto"      "Q.ilex"           "Q.petraea"        "Q.pubescens"      "Q.robur"       
# [25] "Q.suber"          "T.cordata"
Tab_Species = as.data.frame(table(table_unique_class$Species, useNA = "ifany"))
names(Tab_Species)= c("Species", "Nb_studies")


# Importance in terms of traits x age measured
pA = ggplot(Tab_Species, aes(x = reorder(Species, -Nb_studies), y = Nb_studies)) +
  geom_bar(stat = "identity") +
  labs(title = "Total number of traits analysed per species",
    x= "", y = "Number of traits analysed") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# the importance of the species in terms of total number of traits analysed: F. sylvatica, P. sylvestris, Q. ilex


# in terms of number of studies
table_unique_Sp <- fichier_final %>%
  # On garde une ligne unique par étude, par trait, ET par donnée d'âge (certains traits sont mesurés à différents âges)
  distinct(Paper.ID, Species, .keep_all = TRUE)
nrow(table_unique_Sp) # 252 studies in total

Tab_Species_all = as.data.frame(table(table_unique_Sp$Species, useNA = "ifany"))
names(Tab_Species_all)= c("Species", "Nb_studies")

pB = ggplot(Tab_Species_all, aes(x = reorder(Species, -Nb_studies), y = Nb_studies)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of studies per species",
       x= "", y = "Number of studies") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# the importance of the species in terms of total number of traits analysed: F. sylvatica, P. sylvestris, P. abies
# consistent with the figure of the Aspalter paper

pC= ggarrange(pA, pB, ncol = 1, nrow = 2, align="v")

ggsave(pC, filename = "NbStudies_per_Species.pdf",
       device = "pdf",
       height = 12, width = 10, units = "in")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Focus on two main species, Fagus sylvatica and Pinus sylvestris

nrow(table_unique_class) # n= 1839
table_unique_class_Fs = table_unique_class[which(table_unique_class$Species == "F.sylvatica"),]
nrow(table_unique_class_Fs) # n= 512

table_unique_class_Ps = table_unique_class[which(table_unique_class$Species == "P.sylvestris"),]
nrow(table_unique_class_Ps) # n= 201


### Q1: how many studies analysed one versus multiple category of traits ?
etudes_categories <- table_unique_class_Fs %>%
  group_by(Paper.ID) %>%
  summarise(nb_categories = n_distinct(Categorie)) %>%
  ungroup()

resultat_synthese <- etudes_categories %>%
  mutate(type_etude = if_else(nb_categories == 1, "Une seule catégorie", "Plusieurs catégories")) %>%
  dplyr::count(type_etude, name = "nombre_detudes")

print(resultat_synthese)


####### Venn Diagram
install.packages("ggVennDiagram")
library("ggVennDiagram")


#~ FAGUS SYLVATICA
liste_venn <- table_unique_class_Fs %>%
  group_by(Categorie) %>%
  summarise(etudes = list(unique(Paper.ID))) %>%
  deframe() # Transforme le data.frame en une liste nommée
# remove the element "NA"
liste_venn[[4]] <- NULL

# 2. Tracer le diagramme de Venn
pv_fs = ggVennDiagram(liste_venn, label_alpha = 0, show.legend = FALSE) +
  scale_fill_gradient(low = "#F4F7F6", high = "#3A86C8") +
  labs(
    title = "Biological integration",
    subtitle = "Numbers of studies analysing one or multiple life-history traits",
    caption = "Fagus sylvatica"
  ) +
  theme(legend.position = "none", plot.caption = element_text(size = 12, face = "italic", color = "gray30"))

ggsave(pv_fs, filename = "VennDiagramm_Fsylvatica_3classes.pdf",
       device = "pdf",
       height = 10, width = 15, units = "in")


#~ PINUS PINASTER
liste_venn <- table_unique_class_Ps %>%
  group_by(Categorie) %>%
  summarise(etudes = list(unique(Paper.ID))) %>%
  deframe() # Transforme le data.frame en une liste nommée
# remove the element "NA"
liste_venn[[4]] <- NULL

# 2. Tracer le diagramme de Venn
pv_pp = ggVennDiagram(liste_venn, label_alpha = 0, show.legend = FALSE) +
  scale_fill_gradient(low = "#F4F7F6", high = "#3A86C8") +
  labs(
    title = "Biological integration",
    subtitle = "Numbers of studies analysing one or multiple life-history traits",
    caption = "Pinus pinaster"
  ) +
  theme(legend.position = "none", plot.caption = element_text(size = 12, face = "italic", color = "gray30"))

ggsave(pv_pp, filename = "VennDiagramm_Ppinaster_3classes.pdf",
       device = "pdf",
       height = 10, width = 15, units = "in")




 
