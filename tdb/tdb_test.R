library(tidyverse)
library(plotly)
library(gt)
library(gtsummary)
# Les données sont sous NextCloud accessibles via WebDAV (synchro)
df_nc <- read.csv("/home/alain/Nextcloud_abret/Documents/Data/Cours_Enfants_Présences.csv")

df_cours <- df_nc[,1:4] |>
  dplyr::mutate(DateText=Date)

df_cours$Date <- as.Date(df_cours$Date, format="%Y-%m-%d")
df_cours <- df_cours |>
  dplyr::mutate(Month = format(Date, "%Y-%m"))

bp1 <- ggplot(data = df_cours, aes(x = Month, y = Total.cours)) +
  geom_boxplot() +
  geom_line(color="steelblue") +
  geom_jitter(position=position_jitter(w=0.05, h=0.1), cex=1.5, colour="DeepSkyBlue3") + #, size = 3
  stat_summary(fun=mean, geom="point", colour = "red", shape=10, size = 2) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  xlab("Mois") +
  ylim(0, max(df_cours$Total.cours) + 1) +
  scale_fill_brewer(palette="Greens")

plotly::ggplotly(bp1)

gt(as.data.frame(summary(df_cours$Total.cours)))

df_presences <- df_nc[,5:ncol(df_nc)]
df_presences[is.na(df_presences)] <- 0
presences <- sort(colSums(df_presences), decreasing = TRUE)
names(presences)
tab_presences <- as.data.frame(presences)
tab_presences <- data.frame(Pratiquant=names(presences), tab_presences)
#gt(tab_presences)

gt_summary(df_cours$Total.cours)
