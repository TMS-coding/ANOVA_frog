# Packages ----
library(tidyverse)
library(janitor)
#___________________----
# Read data----
frog_data <- read.csv("frogs_messy_data.csv")
#_______________----
# Clean data ----
# clean up column names
frog_data_clean <- janitor::clean_names(frog_data) %>%
  rename("13" = temperature13,
         "18" = temperature18,
         "25" = temperature25,
         frogspawn_id = `frogspawn_sample_id`) %>% 
  pivot_longer(`13`:`25`, names_to="temperature", values_to="days") %>% 
  drop_na(days)
# check data is in a tidy format
head(frog_data_clean)
# check variable names
colnames(frog_data_clean)
# check for duplication
frog_data_clean %>% 
  duplicated() %>% 
  sum()
# check for typos - by looking at impossible values
frog_data_clean %>% 
  summarise(min=min(temperature, na.rm=TRUE), 
            max=max(temperature, na.rm=TRUE))
frog_data_clean %>% 
  summarise(min=min(days, na.rm=TRUE), 
            max=max(days, na.rm=TRUE))
# check for typos by looking at distinct characters/values
frog_data_clean %>% 
  distinct(temperature)
frog_data_clean %>% 
  distinct(days)
# missing values
frog_data_clean %>% 
  is.na() %>% 
  sum()
# quick summary
summary(frog_data_clean)
#_____________________----
# Analysis----
# ANALYSIS ----
lsmodel_frogs <- lm(days ~ temperature, data = frog_data_clean)
summary(lsmodel_frogs)
anova(lsmodel_frogs)
broom::tidy(lsmodel_frogs, conf.int = T)
#___________________________----
# Plotting ----

boxpplot_frog <- ggplot(data=frog_data_clean, aes(x=temperature, y=days))+
  geom_boxplot(aes(fill=temperature),
               alpha = 0.5,
               width = 0.5)+
  geom_jitter(aes(colour=temperature),
              width=0.2)+
  theme_minimal()+
  theme(legend.position = "none")


