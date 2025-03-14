####### MASTER DATA TIDYING ########
###### 04-25-2024 #####
### This script replaces the need to run RDAmaker.R asnd the ToMEx2.0_Data_Tidying_functions.R scripts individually ##
### Another version of this script is available which combines the data tidying and alignments to do Monte Carlo ###
getwd()
#### SOURCE SCRIPTS ####
source("RDAmaker.R") #get ToMEx1.0 fxn

# RDA_maker_function(aoc = read_csv("AquaticOrganisms_Clean_final.csv", guess_max = 10000) %>% rowid_to_column(),
#                    beta_log10_body_length =  0.9341, # Jâms, et al 2020 Nature paper
#                    body_length_intercept = 1.1200, # Jâms, et al 2020 Nature paper
#                    H_W_ratio = 0.67, # H:W ratio across all environments/particle types
#                    R.ave.water.marine = 0.77, # average length to width ratio of microplastics in marine environment (Kooi et al. 2021)
#                    R.ave.water.freshwater = 0.67, # average length to width ratio of microplastics in freshwater environment (Kooi et al. 2021)
#                    R.ave.sediment.marine = 0.75, # average length to width ratio of microplastics in marine environment (Kooi et al. 2021)
#                    R.ave.sediment.freshwater = 0.70, # average length to width ratio of microplastics in freshwater environment (Kooi et al. 2021)
#                    p.ave.marine = 1.10, #average density in marine surface water
#                    alpha.marine = 2.07, #table s4 for marine surface water. length
#                    a.sa.marine = 1.50, #marine surface area power law
#                    a.v.marine = 1.48, #a_V for marine surface water volume
#                    a.m.marine = 1.32, # upper limit fora_m for mass for marine surface water in table S4 
#                    a.ssa.marine = 1.98, # A_SSA for marine surface water
#                    p.ave.freshwater = 1.04, #average density in freshwater surface water
#                    alpha.freshwater = 2.64, #table s4 for freshwater surface water. length
#                    a.sa.freshwater = 2.00, #freshwater surface area power law
#                    a.v.freshwater = 1.68, #a_V for freshwater surface water volume
#                    a.m.freshwater = 1.65, # upper limit fora_m for mass for freshwater surface water in table S4 
#                    a.ssa.freshwater = 2.71)

source("ToMEx2.0_Onboarding/ToMEx2.0_Data_Tidying.R") 

# tomex2.0_tidying_function(aoc_setup = readRDS("aoc_setup.RDS"),
#                           beta_log10_body_length =  0.9341, # Jâms, et al 2020 Nature paper
#                           body_length_intercept = 1.1200, # Jâms, et al 2020 Nature paper
#                           H_W_ratio = 0.67, # H:W ratio across all environments/particle types
#                           R.ave.water.marine = 0.77, # average length to width ratio of microplastics in marine environment (Kooi et al. 2021)
#                           R.ave.water.freshwater = 0.67, # average length to width ratio of microplastics in freshwater environment (Kooi et al. 2021)
#                           R.ave.sediment.marine = 0.75, # average length to width ratio of microplastics in marine environment (Kooi et al. 2021)
#                           R.ave.sediment.freshwater = 0.70, # average length to width ratio of microplastics in freshwater environment (Kooi et al. 2021)
#                           p.ave.marine = 1.10, #average density in marine surface water
#                           alpha.marine = 2.07, #table s4 for marine surface water. length
#                           a.sa.marine = 1.50, #marine surface area power law
#                           a.v.marine = 1.48, #a_V for marine surface water volume
#                           a.m.marine = 1.32, # upper limit fora_m for mass for marine surface water in table S4 
#                           a.ssa.marine = 1.98, # A_SSA for marine surface water
#                           p.ave.freshwater = 1.04, #average density in freshwater surface water
#                           alpha.freshwater = 2.64, #table s4 for freshwater surface water. length
#                           a.sa.freshwater = 2.00, #freshwater surface area power law
#                           a.v.freshwater = 1.68, #a_V for freshwater surface water volume
#                           a.m.freshwater = 1.65, # upper limit fora_m for mass for freshwater surface water in table S4 
#                           a.ssa.freshwater = 2.71
#                           )

### DONE! ### (seriously, that's all you need to do. The Monte Carlo script is more complicated... don't worry! lol)

library(crayon)
cat(green("ToMEx 1 and 2 datasets prepared and RDS files saved in main folder!"))
