#Manuscript Figures
#Author: Leah Thornton Hampton

#Libraries
library(tidyverse)
library(ggpubr)
library(scales)

#Data Import
data <- readRDS("human_setup_tomex2.RDS")

#### Database Stats ####

#Total Publications

pubs <- as.data.frame(unique(data$doi))

#Counts of Study Types

study_types <- data %>%
  group_by(doi, exp_type_f) %>%
  summarise()

#Counts of Data Types

vivo <- data %>% 
  filter(vivo_h_f == "In Vitro")

species <- data %>%
  group_by(species_h_f) %>%
  summarise()

polymers <- data %>%
  group_by(poly_h_f) %>%
  summarise()

#### Data Tidying ####

data <- data %>%
  mutate(poly_h_f = case_when(
  poly_h_f == "Biopolymer" ~ "BIO",
  poly_h_f == "Polystyrene" ~ "PS",
  poly_h_f == "Not Reported" ~ "NR",
  poly_h_f == "Polyamide" ~ "PA",
  poly_h_f == "Polycarbonate" ~ "PC",
  poly_h_f == "Polyethylene" ~ "PE",
  poly_h_f == "Polyethylene Terephthalate" ~ "PET",
  poly_h_f == "Polyethylene vinyl acetate" ~ "PEVA",
  poly_h_f == "Polyisoprene" ~ "PI",
  poly_h_f == "Polylactic Acid" ~ "PLA",
  poly_h_f == "Polymethylmethacrylate" ~ "PMMA",
  poly_h_f == "Polypropylene" ~ "PP",
  poly_h_f == "Polyurathane" ~ "PU",
  poly_h_f == "Polyvinylchloride" ~ "PVC",
  poly_h_f == "High Density Polyethylene" ~ "PE",
  poly_h_f == "Low Density Polyethylene" ~ "PE",
  poly_h_f == "Medium Density Polyethylene" ~ "PE",
  poly_h_f == "Mix - See Original Study" ~ "MIX",
  poly_h_f == "Poly(Styrene-co-acrylonitrile)" ~ "PS-CO-ACN",
  poly_h_f == "Polyoxymethylene" ~ "POM",
  poly_h_f == "Polytetrafluoroethylene" ~ "PTFE",
  poly_h_f == "Polyvinyl Acetate" ~ "PVA",
  poly_h_f == "Polyvinylchloride/vinylacetate co-polymer" ~ "PVC-VA",
  poly_h_f == "Sodium Polyacrylate" ~ "SP",
  poly_h_f == "Starch/Polybutylene Adipate Terephthalate/Polylactic Acid" ~ "S/PBAT/PLA",
  poly_h_f == "Tire Rubber" ~ "TW"))

data1 <- data %>%  
  mutate(db = ifelse(source == "ToMEx 2.0", "ToMEx 2.0", "ToMEx 1.0")) %>% 
  filter(db == "ToMEx 1.0") %>% 
  group_by(size_h_f) %>% 
  mutate(size_obs = n()) %>% 
  ungroup() %>% 
  group_by(poly_h_f) %>% 
  mutate(poly_obs = n()) %>% 
  ungroup() %>% 
  group_by(shape_h_f) %>% 
  mutate(shape_obs = n()) %>% 
  ungroup() %>% 
  group_by(species_h_f) %>% 
  mutate(sp_obs = n()) %>% 
  ungroup() %>% 
  group_by(lvl1_h_f) %>% 
  mutate(lvl_obs = n()) %>% 
  ungroup()

data2 <- data %>%
  add_column(db = "ToMEx 2.0") %>% 
  group_by(size_h_f) %>% 
  mutate(size_obs = n()) %>% 
  ungroup() %>% 
  group_by(poly_h_f) %>% 
  mutate(poly_obs = n()) %>% 
  ungroup() %>% 
  group_by(shape_h_f) %>% 
  mutate(shape_obs = n()) %>% 
  ungroup() %>% 
  group_by(species_h_f) %>% 
  mutate(sp_obs = n()) %>% 
  ungroup() %>% 
  group_by(lvl1_h_f) %>% 
  mutate(lvl_obs = n()) %>% 
  ungroup()


#### Size Category ####

size1 <- data1 %>% 
  group_by(db, size_h_f, size_obs) %>% 
  summarise() %>% 
  filter(!is.na(size_h_f)) %>% 
  filter(!size_h_f == "Not Reported")

size2 <- data2 %>% 
  group_by(db, size_h_f, size_obs) %>% 
  summarise()%>% 
  filter(!is.na(size_h_f))%>% 
  filter(!size_h_f == "Not Reported")

size <- full_join(size1, size2) %>% 
  ungroup() %>% 
  mutate(db = as.factor(db))

size_plot <- size %>%
  ggplot(aes(fill=db,x=reorder(size_h_f, size_obs),y=size_obs,label= size_obs))+
  theme_bw()+
  theme(legend.position="top", text = element_text(size = 20), axis.text.x = element_blank())+
  geom_bar(stat = "identity", position = "dodge", color = "black")+
  scale_fill_manual(values = c("thistle", "orchid4"))+
  labs(fill = "Database", x = "Size Category", y = element_blank())+
  scale_y_continuous(expand = c(0,0), limits = c(0,6200))+
  coord_flip()

plot(size_plot)

#### Shape Category ####

shape1 <- data1 %>% 
  group_by(db, shape_h_f, shape_obs) %>% 
  summarise() %>% 
  filter(!is.na(shape_h_f)) %>% 
  filter(!shape_h_f == "Not Reported")

shape2 <- data2 %>% 
  group_by(db, shape_h_f, shape_obs) %>% 
  summarise()%>% 
  filter(!is.na(shape_h_f))%>% 
  filter(!shape_h_f == "Not Reported")

shape <- full_join(shape1, shape2) %>% 
  ungroup() %>% 
  mutate(db = as.factor(db))

shape_plot <- shape %>%
  ggplot(aes(fill=db,x=reorder(shape_h_f, shape_obs),y=shape_obs,label=shape_obs))+
  theme_bw()+
  theme(legend.position="none", text = element_text(size = 20), axis.text.x = element_blank())+
  geom_bar(stat = "identity", position = "dodge", color = "black")+
  scale_fill_manual(values = c("thistle", "orchid4"))+
  labs(x = "Morphology", y = element_blank())+
  scale_y_continuous(expand = c(0,0), limits = c(0,6200))+
  coord_flip()

plot(shape_plot)

#### Polymer Category ####

poly1 <- data1 %>% 
  group_by(db,poly_h_f,poly_obs) %>% 
  summarise() %>% 
  filter(!is.na(poly_h_f)) %>% 
  filter(!poly_h_f=="NR")

poly2 <- data2 %>% 
  group_by(db,poly_h_f,poly_obs) %>% 
  summarise()%>% 
  filter(!is.na(poly_h_f))%>% 
  filter(!poly_h_f =="NR")

poly <- full_join(poly1,poly2) %>% 
  ungroup() %>% 
  mutate(db=as.factor(db)) 
  # filter(poly_obs>=20) #anything less than 20 obs filtered out

poly_plot <- poly %>%
  ggplot(aes(fill=db, x=reorder(poly_h_f, poly_obs),y=poly_obs,label=poly_obs))+
  theme_bw()+
  theme(legend.position="none", text=element_text(size = 20))+
  geom_bar(stat="identity", position ="dodge", color ="black")+
  scale_fill_manual(values = c("thistle", "orchid4"))+
  labs(x="Polymer", y ="Data Points")+
  scale_y_continuous(expand=c(0,0), limits=c(0,6200))+
  coord_flip()

plot(poly_plot)

#combine plots
particles <- ggarrange(size_plot, shape_plot, poly_plot, labels = c('A','B','C'), font.label = list(size = 30), common.legend = T, ncol = 1, align = "v")

plot(particles)

ggsave("Figures/PARTICLE_H.png", plot = particles, width = 10, height = 21, units = "in")

#### Endpoints ####

endpt1 <- data1 %>% 
  group_by(db,lvl1_h_f,lvl_obs) %>% 
  summarise() 

endpt2 <- data2 %>% 
  group_by(db,lvl1_h_f,lvl_obs) %>% 
  summarise()

endpt <- full_join(endpt1,endpt2) %>% 
  ungroup() %>% 
  mutate(db=as.factor(db))

endpt_plot <- endpt %>%
  ggplot(aes(fill=db, x=reorder(lvl1_h_f, lvl_obs),y=lvl_obs,label=lvl_obs))+
  theme_bw()+
  theme(legend.position="top", text=element_text(size = 15))+
  geom_bar(stat="identity", position ="dodge", color ="black")+
  scale_fill_manual(values = c("thistle", "orchid4"))+
  labs(fill = "Database", x="Broad Endpoint Category", y ="Data Points")+
  scale_y_continuous(expand=c(0,0), limits=c(0,2200))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  coord_flip()

plot(endpt_plot)

ggsave("Figures/ENDPT_H.png", plot = endpt_plot, width = 10, height = 7, units = "in")

#### Red Criteria Plots ####

red_crit <- data %>% 
  mutate(db = ifelse(source == "ToMEx 2.0", "ToMEx 2.0", "ToMEx 1.0")) %>%
  select(db, particle.1, particle.2, particle.3, particle.4,
         design.3, design.4, design.6, design.7, design.9, design.10, design.11,
         risk.2, risk.3, risk.5) %>% 
pivot_longer(!c(db), names_to = "Criteria", values_to = "score", values_drop_na = TRUE)%>% 
mutate(Criteria = case_when(Criteria == "particle.1" ~ "Particle Size",
                            Criteria == "particle.2" ~ "Particle Shape",
                            Criteria == "particle.3" ~ "Polymer Type",
                            Criteria == "particle.4" ~ "Particle Source",
                            Criteria == "particle.5" ~ "Surface Chemistry",
                            Criteria == "particle.6" ~ "Chemical Purity",
                            Criteria == "particle.7" ~ "Microbial Contamination",
                            Criteria == "design.1" ~ "Concentration Units",
                            Criteria == "design.2" ~ "Particle Stability",
                            Criteria == "design.3" ~ "Test Vehicle",
                            Criteria == "design.4" ~ "Admin. Dose",
                            Criteria == "design.5" ~ "Homogeneity of Exposure",
                            Criteria == "design.6" ~ "Admin. Route",
                            Criteria == "design.7" ~ "Test Species",
                            Criteria == "design.8" ~ "Feeding/Housing Conditions",
                            Criteria == "design.9" ~ "Sample Size",
                            Criteria == "design.10" ~ "Exposure Duration",
                            Criteria == "design.11" ~ "Controls",
                            Criteria == "design.12" ~ "Replicates",
                            Criteria == "design.13" ~ "Internal Dose Confirmation",
                            Criteria == "risk.1" ~ "Statistical Analysis",
                            Criteria == "risk.2" ~ "Endpoints",
                            Criteria == "risk.3" ~ "Dose Response",
                            Criteria == "risk.4" ~ "Concentration Range",
                            Criteria == "risk.5" ~ "Effect Thresholds",
                            Criteria == "risk.6" ~ "Test Particle Relevance")) %>% 
  mutate(score = ifelse(score == 0, "Fail", "Pass"))

red_crit1 <- red_crit %>% 
  filter(db == "ToMEx 1.0") %>% 
  group_by(Criteria, score) %>% 
  mutate(obs = n()) %>% 
  ungroup() %>% 
  group_by(db, Criteria, score, obs) %>% 
  summarise()

red_crit2 <- red_crit %>% 
  group_by(Criteria, score) %>% 
  mutate(obs = n()) %>% 
  ungroup() %>% 
  group_by(Criteria, score, obs) %>% 
  summarise() %>% 
  add_column(db = "ToMEx 2.0")

red_crit_all <- full_join(red_crit1, red_crit2) 

red_crit_all <- red_crit_all %>% 
  ungroup() %>% 
  add_row(db = "ToMEx 1.0", Criteria = "Admin. Dose", score = "Fail", obs = 0) %>% 
  add_row(db = "ToMEx 1.0", Criteria = "Admin. Route", score = "Fail", obs = 0) %>% 
  add_row(db = "ToMEx 2.0", Criteria = "Admin. Route", score = "Fail", obs = 0) %>% 
  add_row(db = "ToMEx 1.0", Criteria = "Controls", score = "Fail", obs = 0) %>%  
  add_row(db = "ToMEx 2.0", Criteria = "Endpoints", score = "Fail", obs = 0) %>% 
  add_row(db = "ToMEx 1.0", Criteria = "Endpoints", score = "Fail", obs = 0) %>%  
  add_row(db = "ToMEx 1.0", Criteria = "Exposure Duration", score = "Fail", obs = 0) %>%
  add_row(db = "ToMEx 1.0", Criteria = "Particle Shape", score = "Fail", obs = 0) %>% 
  add_row(db = "ToMEx 1.0", Criteria = "Particle Size", score = "Fail", obs = 0) %>% 
  add_row(db = "ToMEx 2.0", Criteria = "Particle Size", score = "Fail", obs = 0) %>% 
  add_row(db = "ToMEx 1.0", Criteria = "Particle Source", score = "Fail", obs = 0) %>% 
  add_row(db = "ToMEx 2.0", Criteria = "Particle Source", score = "Fail", obs = 0) %>% 
  add_row(db = "ToMEx 1.0", Criteria = "Polymer Type", score = "Fail", obs = 0) %>% 
  add_row(db = "ToMEx 2.0", Criteria = "Polymer Type", score = "Fail", obs = 0) %>%
  add_row(db = "ToMEx 1.0", Criteria = "Test Species", score = "Fail", obs = 0) %>%
  add_row(db = "ToMEx 1.0", Criteria = "Test Vehicle", score = "Fail", obs = 0) 
  
red_crit_plot <- red_crit_all %>%
  ggplot(aes(fill=score,x=Criteria,y=obs,label= obs))+
  theme_bw()+
  theme(legend.position="top", strip.text = element_text(size=12), text = element_text(size = 12))+
  scale_fill_manual(values = c("thistle", "orchid4"))+
  geom_bar(stat = "identity", position = "fill", color = "black")+
  labs(fill = "Criteria Met?", x = element_blank(), y = "Proportion of Data Points")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(labels = wrap_format(10))+
  facet_wrap(~db, ncol = 1)

plot(red_crit_plot)

ggsave("Figures/CRITERIA_H.png", plot = red_crit_plot, width = 10, height = 6, units = "in")


