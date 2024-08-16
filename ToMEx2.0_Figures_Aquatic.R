#Manuscript Figures
#Author: Leah Thornton Hampton

#Libraries
library(tidyverse)
library(wesanderson)
library(calecopal)
library(clipr)
library(scales)
library(ggpubr)
library(grid)

#Data Import
data <- readRDS("aoc_setup_tomex2.RDS") 

####Database Stats####

#Total Publications

pubs <- as.data.frame(unique(data$doi))

#Counts of Study Types

study_types <- data %>%
  group_by(doi, exp_type_f) %>%
  summarise()

#Counts of Data Types

acute_chronic <- data %>%
  filter(acute.chronic_f == "Acute")

vivo <- data %>% 
  filter(vivo_f == "In Vitro")

species <- data %>%
  group_by(species_f, env_f) %>%
  summarise()

effect_metrics <- data %>%
  mutate(db = ifelse(source == "ToMEx 2.0", "ToMEx 2.0", "ToMEx 1.0")) %>%
  mutate(author_year = paste0(authors," et al., ", year)) %>% 
  filter(effect.metric %in% c("EC50", "LC50", "EC10", "IC50", "EC20", "LC20")) %>%
  group_by(db, exposure.route, doi, author_year, env_f, org_f, species_f, effect.metric, lvl1_f, lvl2_f, lvl3_f) %>%
  summarise()

write_clip(effect_metrics)

con <- data %>% 
  filter(source != "ToMEx 2.0") %>%
  filter(treatments >= 3)

poly <- data %>% 
  filter(source != "ToMEx 2.0") 

#### Data Tidying ####

data <- data %>% 
  mutate(poly_f = case_when(
    poly_f == "Biopolymer" ~ "BIO",
    poly_f == "Polystyrene" ~ "PS",
    poly_f == "Not Reported" ~ "NR",
    poly_f == "Polyamide" ~ "PA",
    poly_f == "Polycarbonate" ~ "PC",
    poly_f == "Polyethylene" ~ "PE",
    poly_f == "Polyethylene Terephthalate" ~ "PET",
    poly_f == "Polyethylene vinyl acetate" ~ "PEVA",
    poly_f == "Polyisoprene" ~ "PI",
    poly_f == "Polylactic Acid" ~ "PLA",
    poly_f == "Polymethylmethacrylate" ~ "PMMA",
    poly_f == "Polypropylene" ~ "PP",
    poly_f == "Polyurethane" ~ "PU",
    poly_f == "Polyvinylchloride" ~ "PVC",
    poly_f == "High Density Polyethylene" ~ "HDPE",
    poly_f == "Low Density Polyethylene" ~ "LDPE",
    poly_f == "Medium Density Polyethylene" ~ "MDPE",
    poly_f == "Mix - See Original Study" ~ "MIX",
    poly_f == "Poly(Styrene-co-acrylonitrile)" ~ "PS-CO-ACN",
    poly_f == "Polyoxymethylene" ~ "POM",
    poly_f == "Polytetrafluoroethylene" ~ "PTFE",
    poly_f == "Polyvinyl Acetate" ~ "PVA",
    poly_f == "Polyvinylchloride/vinylacetate co-polymer" ~ "PVC-VA",
    poly_f == "Sodium Polyacrylate" ~ "SP",
    poly_f == "Starch/Polybutylene Adipate Terephthalate/Polylactic Acid" ~ "S/PBAT/PLA",
    poly_f == "Tire Wear" ~ "TW"))

data1 <- data %>%  
  mutate(db = ifelse(source == "ToMEx 2.0", "ToMEx 2.0", "ToMEx 1.0")) %>% 
  filter(db == "ToMEx 1.0") %>% 
  group_by(size_f) %>% 
  mutate(size_obs = n()) %>% 
  ungroup() %>% 
  group_by(poly_f) %>% 
  mutate(poly_obs = n()) %>% 
  ungroup() %>% 
  group_by(shape_f) %>% 
  mutate(shape_obs = n()) %>% 
  ungroup() %>% 
  group_by(org_f) %>% 
  mutate(org_obs = n()) %>% 
  ungroup() %>% 
  group_by(exposure.route) %>% 
  mutate(er_obs = n()) %>% 
  ungroup() %>% 
  group_by(lvl1_f) %>% 
  mutate(lvl1_obs = n()) %>% 
  ungroup()

data2 <- data %>%
  add_column(db = "ToMEx 2.0") %>% 
  group_by(size_f) %>% 
  mutate(size_obs = n()) %>% 
  ungroup() %>% 
  group_by(poly_f) %>% 
  mutate(poly_obs = n()) %>% 
  ungroup() %>% 
  group_by(shape_f) %>% 
  mutate(shape_obs = n()) %>% 
  ungroup() %>% 
  group_by(org_f) %>% 
  mutate(org_obs = n()) %>% 
  ungroup() %>% 
  group_by(exposure.route) %>% 
  mutate(er_obs = n()) %>% 
  ungroup() %>% 
  group_by(lvl1_f) %>% 
  mutate(lvl1_obs = n()) %>% 
  ungroup()

#### Size Category ####

size1 <- data1 %>% 
  group_by(db, size_f, size_obs) %>% 
  summarise() %>% 
  filter(!is.na(size_f)) %>% 
  filter(!size_f == "Not Reported")

size2 <- data2 %>% 
  group_by(db, size_f, size_obs) %>% 
  summarise()%>% 
  filter(!is.na(size_f))%>% 
  filter(!size_f == "Not Reported")

size <- full_join(size1, size2) %>% 
  ungroup() %>% 
  mutate(db = as.factor(db))

size_plot <- size %>%
  ggplot(aes(fill=db,x=reorder(size_f, size_obs),y=size_obs,label= size_obs))+
  theme_bw()+
  theme(legend.position="top", text = element_text(size = 20), axis.text.x = element_blank())+
  geom_bar(stat = "identity", position = "dodge", color = "black")+
  scale_fill_manual(values = c("#009E73", "#56B4E9"))+
  labs(fill = "Database", x = "Size Category", y = element_blank())+
  scale_y_continuous(expand = c(0,0), limits = c(0,7500))+
  coord_flip()

plot(size_plot)

#### Shape Category ####

shape1 <- data1 %>% 
  group_by(db, shape_f, shape_obs) %>% 
  summarise() %>% 
  filter(!is.na(shape_f)) %>% 
  filter(!shape_f == "Not Reported")

shape2 <- data2 %>% 
  group_by(db, shape_f, shape_obs) %>% 
  summarise()%>% 
  filter(!is.na(shape_f))%>% 
  filter(!shape_f == "Not Reported")

shape <- full_join(shape1, shape2) %>% 
  ungroup() %>% 
  mutate(db = as.factor(db))

shape_plot <- shape %>%
  ggplot(aes(fill=db,x=reorder(shape_f, shape_obs),y=shape_obs,label=shape_obs))+
  theme_bw()+
  theme(legend.position="none", text = element_text(size = 20))+
  geom_bar(stat = "identity", position = "dodge", color = "black")+
  scale_fill_manual(values = c("#009E73", "#56B4E9"))+
  labs(x = "Morphology", y = "Data Points")+
  scale_y_continuous(expand = c(0,0), limits = c(0,7500))+
  coord_flip()

# plot(shape_plot)

#### Polymer Category ####

poly1 <- data1 %>% 
  group_by(db,poly_f,poly_obs) %>% 
  summarise() %>% 
  filter(!is.na(poly_f)) %>% 
  filter(!poly_f=="NR")

poly2 <- data2 %>% 
  group_by(db,poly_f,poly_obs) %>% 
  summarise()%>% 
  filter(!is.na(poly_f))%>% 
  filter(!poly_f =="NR")

poly <- full_join(poly1,poly2) %>% 
  ungroup() %>% 
  mutate(db=as.factor(db))

poly_plot1 <- poly %>%
  filter(poly_f %in% c("PS", "PE", "PVC")) %>% 
  ggplot(aes(fill=db, x=reorder(poly_f, poly_obs),y=poly_obs,label=poly_obs))+
  theme_bw()+
  theme(legend.position="top", text=element_text(size = 20))+
  geom_bar(stat="identity", position ="dodge", color ="black")+
  scale_fill_manual(values = c("#009E73", "#56B4E9"))+
  labs(x=element_blank(), y =element_blank(), fill = "Database")+
  scale_y_continuous(expand=c(0,0), limits = c(0,7500))+
  coord_flip()

plot(poly_plot1)

poly_plot2 <- poly %>%
  filter(!poly_f %in% c("PS", "PE", "PVC")) %>% 
  ggplot(aes(fill=db, x=reorder(poly_f, poly_obs),y=poly_obs,label=poly_obs))+
  theme_bw()+
  theme(legend.position="none", text=element_text(size = 20))+
  geom_bar(stat="identity", position ="dodge", color ="black")+
  scale_fill_manual(values = c("#009E73", "#56B4E9"))+
  labs(x="Polymer", y ="Data Points")+
  scale_y_continuous(expand=c(0,0), limits = c(0,525))+
  coord_flip()

plot(poly_plot2)

#combine poly plots
poly_plot <- ggarrange(poly_plot1, poly_plot2, heights = c(0.2, 1), ncol = 1, align = "v")

plot(poly_plot)

annotate_figure(poly_plot, left = textGrob("Polymer", rot = 90, hjust = -0.2, gp = gpar(cex = 1.8)))

ggsave("Figure1A.png", plot = poly_plot, width = 10, height = 14, units = "in")

#combine size and shape plot
size_shape <- ggarrange(size_plot, shape_plot, labels = c('A','B'), common.legend = T, 
                       font.label = list(size = 30), ncol = 1, align = "v")

plot (size_shape)

ggsave("Figure1B.png", plot = size_shape, width = 10, height = 14, units = "in")


#### Taxa ####

org1 <- data1 %>% 
  group_by(db,org_f,org_obs) %>% 
  summarise() 

org2 <- data2 %>% 
  group_by(db,org_f,org_obs) %>% 
  summarise()

org <- full_join(org1,org2) %>% 
  ungroup() %>% 
  mutate(db=as.factor(db))

org_plot <- org %>%
  ggplot(aes(fill=db, x=reorder(org_f, org_obs),y=org_obs,label=org_obs))+
  theme_bw()+
  theme(legend.position="top", text=element_text(size = 20), axis.text.x = element_blank())+
  geom_bar(stat="identity", position ="dodge", color ="black")+
  scale_fill_manual(values = c("#009E73", "#56B4E9"))+
  labs(fill = "Database", x="Organism Group", y =element_blank())+
  scale_y_continuous(expand=c(0,0), limits=c(0,7500))+
  coord_flip()

plot(org_plot)

#### Endpoints ####

endpt1 <- data1 %>% 
  group_by(db,lvl1_f,lvl1_obs) %>% 
  summarise() 

endpt2 <- data2 %>% 
  group_by(db,lvl1_f,lvl1_obs) %>% 
  summarise()

endpt <- full_join(endpt1,endpt2) %>% 
  ungroup() %>% 
  mutate(db=as.factor(db))

endpt_plot <- endpt %>%
  ggplot(aes(fill=db, x=reorder(lvl1_f, lvl1_obs),y=lvl1_obs,label=lvl1_obs))+
  theme_bw()+
  theme(legend.position="top", text=element_text(size = 20))+
  geom_bar(stat="identity", position ="dodge", color ="black")+
  scale_fill_manual(values = c("#009E73", "#56B4E9"))+
  labs(fill = "Database", x="Broad Endpoint Category", y ="Data Points")+
  scale_y_continuous(expand=c(0,0), limits=c(0,7500))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  coord_flip()

plot(endpt_plot)

ggsave("Figure3.png", plot = endpt_plot, width = 10, height = 7, units = "in")

#### Red Criteria Plots ####

red_crit <- data %>% 
  mutate(db = ifelse(source == "ToMEx 2.0", "ToMEx 2.0", "ToMEx 1.0")) %>%
  select(rowid, db, tech.a1, tech.a2, tech.a3, tech.a4, tech.a5, tech.a6, risk.b1) %>% 
  pivot_longer(!c(rowid, db), names_to = "criteria", values_to = "score", values_drop_na = TRUE)

red_crit1 <- red_crit %>% 
  filter(db == "ToMEx 1.0") %>% 
  group_by(criteria, score) %>% 
  mutate(obs = n()) %>% 
  ungroup() %>% 
  group_by(db, criteria, score, obs) %>% 
  summarise()
  
red_crit2 <- red_crit %>% 
  group_by(criteria, score) %>% 
  mutate(obs = n()) %>% 
  ungroup() %>% 
  group_by(criteria, score, obs) %>% 
  summarise() %>% 
  add_column(db = "ToMEx 2.0")

red_crit_all <- full_join(red_crit1, red_crit2) 

red_crit_all <- red_crit_all %>% 
  mutate(score = if_else(score == 2, "Pass", "Fail")) %>% 
  mutate(criteria = case_when(
    criteria == "risk.b1" ~ "≥3 Test Concentrations",
    criteria == "tech.a1" ~ "Test Media Reported",
    criteria == "tech.a2" ~ "Exposure Route Reported",
    criteria == "tech.a3" ~ "Test Species Reported",
    criteria == "tech.a4" ~ "Sample Size Reported",
    criteria == "tech.a5" ~ "Control Group Reported",
    criteria == "tech.a6" ~ "Exposure Duration Reported")) %>% 
  ungroup() %>% 
  add_row(db = "ToMEx 1.0", criteria = "Exposure Duration Reported", score = "Fail", obs = 0) %>% 
  add_row(db = "ToMEx 1.0", criteria = "Exposure Route Reported", score = "Fail", obs = 0) %>% 
  add_row(db = "ToMEx 1.0", criteria = "Test Media Reported", score = "Fail", obs = 0) %>% 
  add_row(db = "ToMEx 1.0", criteria = "Test Species Reported", score = "Fail", obs = 0) 

red_crit_plot <- red_crit_all %>%
  ggplot(aes(fill=score,x=criteria,y=obs,label= obs))+
  theme_bw()+
  theme(legend.position="top", strip.text = element_text(size=15), text = element_text(size = 15))+
  scale_fill_manual(values = c("#CC79A7", "#56B4E9"))+
  geom_bar(stat = "identity", position = "fill", color = "black")+
  labs(fill = "Criteria Met?", x = element_blank(), y = "Proportion of Data Points")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(labels = wrap_format(12))+
  facet_wrap(~db, ncol = 1)

plot(red_crit_plot)

ggsave("Figure5.png", plot = red_crit_plot, width = 8, height = 6, units = "in")
