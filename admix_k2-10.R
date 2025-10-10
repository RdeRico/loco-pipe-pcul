##################################
## plot pcangsd -admix for COYE ##
##################################

#################################################################
# Plotting results until K that separates geography and alpha 0 #
#################################################################

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
library(paletteer)

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

## plot:
### get color palette
colors <- paletteer_d("RColorBrewer::Set2")
colors <- colors[1:7]
k2_colors <- colors[c(3,1)]
k3_colors <- colors[c(2,3,1)]
k4_colors <- colors[c(6,1,2,3)]
k5_colors <- colors[c(1,7,3,2,6)]
k6_colors <- colors[c(3,6,1,2,4,7)]

K2_eig1_alpha0 <- K2_eig1_alpha0 %>% dplyr::arrange(population)

K2_eig1_alpha0_plot <-
  ggplot(K2_eig1_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", size = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=2", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values=k2_colors, guide = FALSE)
K2_eig1_alpha0_plot

K3_eig2_alpha0 <- K3_eig2_alpha0 %>% dplyr::arrange(population)

K3_eig2_alpha0_plot <-
  ggplot(K3_eig2_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", size = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=3", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values=k3_colors, guide = FALSE)
K3_eig2_alpha0_plot

K4_eig3_alpha0 <- K4_eig3_alpha0 %>% dplyr::arrange(population)

K4_eig3_alpha0_plot <-
  ggplot(K4_eig3_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", size = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=4", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values=k4_colors, guide = FALSE)
K4_eig3_alpha0_plot

K5_eig4_alpha0 <- K5_eig4_alpha0 %>% dplyr::arrange(population)

K5_eig4_alpha0_plot <-
  ggplot(K5_eig4_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", size = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=5", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values=k5_colors, guide = FALSE)
K5_eig4_alpha0_plot

K6_eig5_alpha0 <- K6_eig5_alpha0 %>% dplyr::arrange(population)

K6_eig5_alpha0_plot <-
  ggplot(K6_eig5_alpha0, aes(factor(sampleID), prob, fill = factor(popGroup))) +
  geom_col(color = "gray", size = 0.1) +
  facet_grid(~fct_inorder(population), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=6", y = "Ancestry") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    strip.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values=k6_colors, guide = FALSE)
K6_eig5_alpha0_plot

all_K <- plot_grid(K2_eig1_alpha0_plot,K3_eig2_alpha0_plot,K4_eig3_alpha0_plot,
                   K5_eig4_alpha0_plot,K6_eig5_alpha0_plot,
                   ncol=1)

all_K

setwd("~/Dropbox/Postdoc_Colorado/convergence_GEA/dataset/BGP_species/COYE/loco-pipe/pcangsd_admix/")
ggsave("admixture_K2-5_alpha0.pdf", all_K, width = 25, height = 23.4, device=cairo_pdf, units="cm", limitsize = FALSE)