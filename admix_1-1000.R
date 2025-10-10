##################################
## plot pcangsd -admix for COYE ##
##################################

###########################################################
# Analyses with eigenvectors 1-9 and alpha of 0 and 10000 #
###########################################################

# new general run
rm(list=ls()) #clears all variables
objects() # clear all objects
graphics.off() #close all figures

# Load libs
# plotting
library(tidyverse)
library(ggthemes)
library(patchwork)
library(cowplot)

### setwd
setwd("/Volumes/ruegglab/Genetic_and_Environmental_data/comparative_datasets_Joan/COYE/loco-pipe/pcangsd_admix/")
## read in individual to population map 
inds <- read_tsv("../docs/COYE_breeding_sample_table_sp.tsv") %>%
  dplyr::select(sample_name, Population)

# Global
## read in Q values
K2_eig1_alpha0 <- read.table("COYE_pcangsd_admix_eig1_K2_1_alpha0.admix.2.Q",col.names=c("1","2"))
K3_eig2_alpha0 <- read.table("COYE_pcangsd_admix_eig2_K3_1_alpha0.admix.3.Q",col.names=c("1","2","3"))
K4_eig3_alpha0 <- read.table("COYE_pcangsd_admix_eig3_K4_1_alpha0.admix.4.Q",col.names=c("1","2","3","4"))
K5_eig4_alpha0 <- read.table("COYE_pcangsd_admix_eig4_K5_1_alpha0.admix.5.Q",col.names=c("1","2","3","4","5"))
K6_eig5_alpha0 <- read.table("COYE_pcangsd_admix_eig5_K6_1_alpha0.admix.6.Q",col.names=c("1","2","3","4","5","6"))
K7_eig6_alpha0 <- read.table("COYE_pcangsd_admix_eig6_K7_1_alpha0.admix.7.Q",col.names=c("1","2","3","4","5","6","7"))
K8_eig7_alpha0 <- read.table("COYE_pcangsd_admix_eig7_K8_1_alpha0.admix.8.Q",col.names=c("1","2","3","4","5","6","7","8"))
K9_eig8_alpha0 <- read.table("COYE_pcangsd_admix_eig8_K9_1_alpha0.admix.9.Q",col.names=c("1","2","3","4","5","6","7","8","9"))
K10_eig9_alpha0 <- read.table("COYE_pcangsd_admix_eig9_K10_1_alpha0.admix.10.Q",col.names=c("1","2","3","4","5","6","7","8","9","10"))

K2_eig1_alpha10000 <- read.table("COYE_pcangsd_admix_eig1_K2_alpha10000.admix.2.Q",col.names=c("1","2"))
K3_eig2_alpha10000 <- read.table("COYE_pcangsd_admix_eig2_K3_alpha10000.admix.3.Q",col.names=c("1","2","3"))
K4_eig3_alpha10000 <- read.table("COYE_pcangsd_admix_eig3_K4_alpha10000.admix.4.Q",col.names=c("1","2","3","4"))
K5_eig4_alpha10000 <- read.table("COYE_pcangsd_admix_eig4_K5_alpha10000.admix.5.Q",col.names=c("1","2","3","4","5"))
K6_eig5_alpha10000 <- read.table("COYE_pcangsd_admix_eig5_K6_alpha10000.admix.6.Q",col.names=c("1","2","3","4","5","6"))
K7_eig6_alpha10000 <- read.table("COYE_pcangsd_admix_eig6_K7_alpha10000.admix.7.Q",col.names=c("1","2","3","4","5","6","7"))
K8_eig7_alpha10000 <- read.table("COYE_pcangsd_admix_eig7_K8_alpha10000.admix.8.Q",col.names=c("1","2","3","4","5","6","7","8"))
K9_eig8_alpha10000 <- read.table("COYE_pcangsd_admix_eig8_K9_alpha10000.admix.9.Q",col.names=c("1","2","3","4","5","6","7","8","9"))
K10_eig9_alpha10000 <- read.table("COYE_pcangsd_admix_eig9_K10_alpha10000.admix.10.Q",col.names=c("1","2","3","4","5","6","7","8","9","10"))

pop_alpha <- as.tibble(inds$Population) %>%
  rename(loc=value) %>%
  distinct() %>%
  dplyr::mutate(population = case_when(
    loc == "COYE_CA_Olema" ~ "A",
    loc == "COYE_CA_Oceanside" ~ "B",
    loc == "COYE_WA_Graham" ~ "C",
    loc == "COYE_AK_MitkofIsland" ~ "D",
    loc == "COYE_NV_CaveCreek" ~ "E",
    loc == "COYE_AB_JasperNationalPark" ~ "F",
    loc == "COYE_BC_BC1" ~ "G",
    loc == "COYE_MT_SeelyLake" ~ "H",
    loc == "COYE_WY_KeyholeBandingStation" ~ "I",
    loc == "COYE_WY_AtlatnicCity" ~ "J",
    loc == "COYE_NM_Roswell" ~ "K",
    loc == "COYE_AZ_AZ1" ~ "L",
    loc == "COYE_KS_JunctionCity" ~ "M",
    loc == "COYE_KY_Harrodsburg" ~ "N",
    loc == "COYE_MI_Augusta" ~ "O",
    loc == "COYE_QUE_Normandin" ~ "P",
    loc == "COYE_ON_Hilliardton" ~ "Q",
    loc == "COYE_NY_Hilton" ~ "R",
    loc == "COYE_PA_Schnecksville" ~ "S",
    loc == "COYE_NB_BurpeeWildlifeRefuge" ~ "T",
    loc == "COYE_NC_AshleyHeights" ~ "U",
    loc == "COYE_AL_Midway" ~ "V",
    loc == "COYE_FL_LakePlacid" ~ "W"))  

K2_eig1_alpha0$sampleID <- inds$sample_name
K2_eig1_alpha0$loc <- inds$Population
K2_eig1_alpha0 <- K2_eig1_alpha0 %>% gather(key=popGroup,value=prob,1:2) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K2_eig1_alpha0$population <- as.factor(K2_eig1_alpha0$population)

K3_eig2_alpha0$sampleID <- inds$sample_name
K3_eig2_alpha0$loc <- inds$Population
K3_eig2_alpha0 <- K3_eig2_alpha0 %>% gather(key=popGroup,value=prob,1:3) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K3_eig2_alpha0$population <- as.factor(K3_eig2_alpha0$population)

K4_eig3_alpha0$sampleID <- inds$sample_name
K4_eig3_alpha0$loc <- inds$Population
K4_eig3_alpha0 <- K4_eig3_alpha0 %>% gather(key=popGroup,value=prob,1:4) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K4_eig3_alpha0$population <- as.factor(K4_eig3_alpha0$population)

K5_eig4_alpha0$sampleID <- inds$sample_name
K5_eig4_alpha0$loc <- inds$Population
K5_eig4_alpha0 <- K5_eig4_alpha0 %>% gather(key=popGroup,value=prob,1:5) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K5_eig4_alpha0$population <- as.factor(K5_eig4_alpha0$population)

K6_eig5_alpha0$sampleID <- inds$sample_name
K6_eig5_alpha0$loc <- inds$Population
K6_eig5_alpha0 <- K6_eig5_alpha0 %>% gather(key=popGroup,value=prob,1:6) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K6_eig5_alpha0$population <- as.factor(K6_eig5_alpha0$population)

K7_eig6_alpha0$sampleID <- inds$sample_name
K7_eig6_alpha0$loc <- inds$Population
K7_eig6_alpha0 <- K7_eig6_alpha0 %>% gather(key=popGroup,value=prob,1:7) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K7_eig6_alpha0$population <- as.factor(K7_eig6_alpha0$population)

K8_eig7_alpha0$sampleID <- inds$sample_name
K8_eig7_alpha0$loc <- inds$Population
K8_eig7_alpha0 <- K8_eig7_alpha0 %>% gather(key=popGroup,value=prob,1:8) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K8_eig7_alpha0$population <- as.factor(K8_eig7_alpha0$population)

K9_eig8_alpha0$sampleID <- inds$sample_name
K9_eig8_alpha0$loc <- inds$Population
K9_eig8_alpha0 <- K9_eig8_alpha0 %>% gather(key=popGroup,value=prob,1:9) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K9_eig8_alpha0$population <- as.factor(K9_eig8_alpha0$population)

K10_eig9_alpha0$sampleID <- inds$sample_name
K10_eig9_alpha0$loc <- inds$Population
K10_eig9_alpha0 <- K10_eig9_alpha0 %>% gather(key=popGroup,value=prob,1:10) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K10_eig9_alpha0$population <- as.factor(K10_eig9_alpha0$population)

K2_eig1_alpha10000$sampleID <- inds$sample_name
K2_eig1_alpha10000$loc <- inds$Population
K2_eig1_alpha10000 <- K2_eig1_alpha10000 %>% gather(key=popGroup,value=prob,1:2) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K2_eig1_alpha10000$population <- as.factor(K2_eig1_alpha10000$population)

K3_eig2_alpha10000$sampleID <- inds$sample_name
K3_eig2_alpha10000$loc <- inds$Population
K3_eig2_alpha10000 <- K3_eig2_alpha10000 %>% gather(key=popGroup,value=prob,1:3) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K3_eig2_alpha10000$population <- as.factor(K3_eig2_alpha10000$population)

K4_eig3_alpha10000$sampleID <- inds$sample_name
K4_eig3_alpha10000$loc <- inds$Population
K4_eig3_alpha10000 <- K4_eig3_alpha10000 %>% gather(key=popGroup,value=prob,1:4) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K4_eig3_alpha10000$population <- as.factor(K4_eig3_alpha10000$population)

K5_eig4_alpha10000$sampleID <- inds$sample_name
K5_eig4_alpha10000$loc <- inds$Population
K5_eig4_alpha10000 <- K5_eig4_alpha10000 %>% gather(key=popGroup,value=prob,1:5) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K5_eig4_alpha10000$population <- as.factor(K5_eig4_alpha10000$population)

K6_eig5_alpha10000$sampleID <- inds$sample_name
K6_eig5_alpha10000$loc <- inds$Population
K6_eig5_alpha10000 <- K6_eig5_alpha10000 %>% gather(key=popGroup,value=prob,1:6) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K6_eig5_alpha10000$population <- as.factor(K6_eig5_alpha10000$population)

K7_eig6_alpha10000$sampleID <- inds$sample_name
K7_eig6_alpha10000$loc <- inds$Population
K7_eig6_alpha10000 <- K7_eig6_alpha10000 %>% gather(key=popGroup,value=prob,1:7) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K7_eig6_alpha10000$population <- as.factor(K7_eig6_alpha10000$population)

K8_eig7_alpha10000$sampleID <- inds$sample_name
K8_eig7_alpha10000$loc <- inds$Population
K8_eig7_alpha10000 <- K8_eig7_alpha10000 %>% gather(key=popGroup,value=prob,1:8) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K8_eig7_alpha10000$population <- as.factor(K8_eig7_alpha10000$population)

K9_eig8_alpha10000$sampleID <- inds$sample_name
K9_eig8_alpha10000$loc <- inds$Population
K9_eig8_alpha10000 <- K9_eig8_alpha10000 %>% gather(key=popGroup,value=prob,1:9) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K9_eig8_alpha10000$population <- as.factor(K9_eig8_alpha10000$population)

K10_eig9_alpha10000$sampleID <- inds$sample_name
K10_eig9_alpha10000$loc <- inds$Population
K10_eig9_alpha10000 <- K10_eig9_alpha10000 %>% gather(key=popGroup,value=prob,1:10) %>%
  left_join(pop_alpha) %>%
  dplyr::select(sampleID,popGroup,prob,population)

K10_eig9_alpha10000$population <- as.factor(K10_eig9_alpha10000$population)

## plot:
K2_eig1_alpha0 <- K2_eig1_alpha0 %>% dplyr::arrange(population)

K2_eig1_alpha0_plot <-
  ggplot(K2_eig1_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=2, eigenvectors=1, alpha=0", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K2_eig1_alpha0_plot

K3_eig2_alpha0 <- K3_eig2_alpha0 %>% dplyr::arrange(population)

K3_eig2_alpha0_plot <-
  ggplot(K3_eig2_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=3, eigenvectors=2, alpha=0", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K3_eig2_alpha0_plot

K4_eig3_alpha0 <- K4_eig3_alpha0 %>% dplyr::arrange(population)

K4_eig3_alpha0_plot <-
  ggplot(K4_eig3_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=4, eigenvectors=3, alpha=0", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K4_eig3_alpha0_plot

K5_eig4_alpha0 <- K5_eig4_alpha0 %>% dplyr::arrange(population)

K5_eig4_alpha0_plot <-
  ggplot(K5_eig4_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=5, eigenvectors=4, alpha=0", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K5_eig4_alpha0_plot

K6_eig5_alpha0 <- K6_eig5_alpha0 %>% dplyr::arrange(population)

K6_eig5_alpha0_plot <-
  ggplot(K6_eig5_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=6, eigenvectors=5, alpha=0", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K6_eig5_alpha0_plot

K7_eig6_alpha0 <- K7_eig6_alpha0 %>% dplyr::arrange(population)

K7_eig6_alpha0_plot <-
  ggplot(K7_eig6_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=7, eigenvectors=6, alpha=0", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K7_eig6_alpha0_plot

K8_eig7_alpha0 <- K8_eig7_alpha0 %>% dplyr::arrange(population)

K8_eig7_alpha0_plot <-
  ggplot(K8_eig7_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=8, eigenvectors=7, alpha=0", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K8_eig7_alpha0_plot

K9_eig8_alpha0 <- K9_eig8_alpha0 %>% dplyr::arrange(population)

K9_eig8_alpha0_plot <-
  ggplot(K9_eig8_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=9, eigenvectors=8, alpha=0", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K9_eig8_alpha0_plot

K10_eig9_alpha0 <- K10_eig9_alpha0 %>% dplyr::arrange(population)

K10_eig9_alpha0_plot <-
  ggplot(K10_eig9_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=10, eigenvectors=9, alpha=0", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K10_eig9_alpha0_plot

K2_eig1_alpha10000 <- K2_eig1_alpha10000 %>% dplyr::arrange(population)

K2_eig1_alpha10000_plot <-
  ggplot(K2_eig1_alpha10000, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=2, eigenvectors=1, alpha=10000", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K2_eig1_alpha10000_plot

K3_eig2_alpha10000 <- K3_eig2_alpha10000 %>% dplyr::arrange(population)

K3_eig2_alpha10000_plot <-
  ggplot(K3_eig2_alpha10000, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=3, eigenvectors=2, alpha=10000", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K3_eig2_alpha10000_plot

K4_eig3_alpha10000 <- K4_eig3_alpha10000 %>% dplyr::arrange(population)

K4_eig3_alpha10000_plot <-
  ggplot(K4_eig3_alpha10000, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=4, eigenvectors=3, alpha=10000", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K4_eig3_alpha10000_plot

K5_eig4_alpha10000 <- K5_eig4_alpha10000 %>% dplyr::arrange(population)

K5_eig4_alpha10000_plot <-
  ggplot(K5_eig4_alpha10000, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=5, eigenvectors=4, alpha=10000", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K5_eig4_alpha10000_plot

K6_eig5_alpha10000 <- K6_eig5_alpha10000 %>% dplyr::arrange(population)

K6_eig5_alpha10000_plot <-
  ggplot(K6_eig5_alpha10000, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=6, eigenvectors=5, alpha=10000", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K6_eig5_alpha10000_plot

K7_eig6_alpha10000 <- K7_eig6_alpha10000 %>% dplyr::arrange(population)

K7_eig6_alpha10000_plot <-
  ggplot(K7_eig6_alpha10000, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=7, eigenvectors=6, alpha=10000", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K7_eig6_alpha10000_plot

K8_eig7_alpha10000 <- K8_eig7_alpha10000 %>% dplyr::arrange(population)

K8_eig7_alpha10000_plot <-
  ggplot(K8_eig7_alpha10000, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=8, eigenvectors=7, alpha=10000", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K8_eig7_alpha10000_plot

K9_eig8_alpha10000 <- K9_eig8_alpha10000 %>% dplyr::arrange(population)

K9_eig8_alpha10000_plot <-
  ggplot(K9_eig8_alpha10000, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=9, eigenvectors=8, alpha=10000", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K9_eig8_alpha10000_plot

K10_eig9_alpha10000 <- K10_eig9_alpha10000 %>% dplyr::arrange(population)

K10_eig9_alpha10000_plot <-
  ggplot(K10_eig9_alpha10000, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", linewidth = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=10, eigenvectors=9, alpha=10000", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_gdocs(guide = "none")
K10_eig9_alpha10000_plot

all_K <- plot_grid(K2_eig1_alpha0_plot,K2_eig1_alpha10000_plot,
                   K3_eig2_alpha0_plot,K3_eig2_alpha10000_plot,
                   K4_eig3_alpha0_plot,K4_eig3_alpha10000_plot,
                   K5_eig4_alpha0_plot,K5_eig4_alpha10000_plot,
                   K6_eig5_alpha0_plot,K6_eig5_alpha10000_plot,
                   K7_eig6_alpha0_plot,K7_eig6_alpha10000_plot,
                   K8_eig7_alpha0_plot,K8_eig7_alpha10000_plot,
                   K9_eig8_alpha0_plot,K9_eig8_alpha10000_plot,
                   K10_eig9_alpha0_plot,K10_eig9_alpha10000_plot,
                   ncol=2)

all_K

setwd("~/Dropbox/Postdoc_Colorado/convergence_GEA/dataset/BGP_species/COYE/loco-pipe/pcangsd_admix/")
ggsave("admixture_K2-10_diff_eig_K-1_alpha0_10000.pdf", all_K, width = 50, height = 50, device=cairo_pdf, units="cm", limitsize = FALSE)