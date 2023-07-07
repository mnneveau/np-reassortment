library(readxl)
library(tidyverse)
library(ggpubr)
library(cowplot)


setwd("//iastate/lss/research/pcgauger-lab/Megan/NP_WGS/pdm_NP_detection_increase/")

#setwd("\\las-dfs-01.las.iastate.edu\lss\research\pcgauger-lab\Megan\NP_WGS\pdm_NP_detection_increase")
min_text_size = 20
#constellation_colors <- c("TTPTPT" = "#3b3b3bFF", "TTTTPT" = "#CD534CFF", "TTTPPT" = "#EFC000FF", "TTPPPT" = "#0073C2FF",
#                                   "Other" = "#868686FF")
constellation_colors <- c( "TTTTPT" = "#4e6eff", "TTTPPT" = "#5fb3f3", "TTPTPT" = "#96c870","TTPPPT" = "#d0d554",
                          "Other" = "#868686FF")

data <- read_excel("2016_to_2022_all.xlsx")

#note we set to remove anything less than 5% of detections that year
df_tidy <- data %>%
  drop_na(Constellation) %>%
  filter(WGS == "TRUE") %>%
  filter(Year < 2022) %>%
  group_by(Year,Constellation) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>%
  mutate(Constellation2 = if_else(freq > 0.07, Constellation, "Other")) %>%
  group_by(Year, Constellation2) %>%
  summarise(freq = sum(freq)) %>%
  ungroup() %>%
  complete(Year, Constellation2, fill=list(freq=0))

df_tidy$Constellation2 <- factor(df_tidy$Constellation2, levels = c("TTTTPT", "TTTPPT", "TTPTPT","TTPPPT", "Other"))

#get plot with 4 constellations - but how to add "other" section for the remainder of small ones
all <- ggplot(df_tidy, aes(x=Year, y=freq, fill=Constellation2)) +
  geom_col(position="fill") +
  scale_fill_manual(values = constellation_colors) +
  labs(x=NULL, y=NULL, fill = "Constellation", title = "All IAV Clades") +
  theme(
    axis.text = element_text(size = min_text_size - 3, face = "bold"),
    axis.title = element_text(size = min_text_size, face = "bold"),
    legend.title = element_text(size = min_text_size -2, face="bold"),
    legend.text = element_text(size=min_text_size -1),
    plot.title = element_text(size = min_text_size, face = "bold", hjust = 0.5)
  )
all

#graph C-IVA data from 2018 to 2023
civa <- read_excel("2018_to_2023_C-IVA_only.xlsx")
civa_tidy <- civa %>%
  drop_na(Constellation) %>%
  group_by(Year,Constellation) %>%
  filter(WGS == "TRUE") %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(Constellation2 = if_else(Constellation %in% df_tidy$Constellation2, Constellation, "Other")) %>%
  group_by(Year, Constellation2) %>%
  summarise(freq = sum(freq)) %>%
  ungroup() %>%
  complete(Year, Constellation2, fill=list(freq=0))
 #%>% filter(Constellation %in% df_tidy$Constellation)

civa_tidy$Constellation2 <- factor(civa_tidy$Constellation2, levels = c( "TTTTPT", "TTTPPT","TTPTPT", "TTPPPT", "Other"))

civa <- ggplot(civa_tidy, aes(x=Year, y=freq, fill=Constellation2)) +
  geom_col(position="fill") +
  scale_fill_manual(values = constellation_colors) 
civa

#graph C-IVA from 2016 to 2021
civa <- read_excel("2015_to_2023_C-IVA_only.xlsx")
civa_tidy <- civa %>%
  drop_na(Constellation) %>%
  group_by(Year,Constellation) %>%
  filter(WGS == "TRUE") %>%
  filter(Year < 2022 & Year > 2015) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(Constellation2 = if_else(Constellation %in% df_tidy$Constellation2, Constellation, "Other")) %>%
  group_by(Year, Constellation2) %>%
  summarise(freq = sum(freq)) %>%
  ungroup() %>%
  complete(Year, Constellation2, fill=list(freq=0))
#%>% filter(Constellation %in% df_tidy$Constellation)

civa_tidy$Constellation2 <- factor(civa_tidy$Constellation2, levels = c("TTTTPT", "TTTPPT", "TTPTPT","TTPPPT", "Other"))

civa <- ggplot(civa_tidy, aes(x=Year, y=freq, fill=Constellation2)) +
  geom_col(position="fill") +
  scale_fill_manual(values = constellation_colors) +
  labs(x=NULL, y="Frequency of Detection", title = "C-IVA Only") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = min_text_size - 3, face = "bold"),
    axis.title = element_text(size = min_text_size, face = "bold"),
    plot.title = element_text(size = min_text_size, face = "bold", hjust = 0.5)
  )
civa

combo <- plot_grid(civa, all, labels = c("B", "C"), label_size = min_text_size, rel_widths = c(1,1.3))
combo

tiff("NP_lineage_colored_same_order_as_nextstrain_2016-2021.tif", units = "in", width = 11, height = 8.5, res = 300, compression = "lzw")
combo
dev.off()