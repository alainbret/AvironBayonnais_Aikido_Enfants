library(tidyverse)
library(plotly)

# Les données sont sous NextCloud accessibles via WebDAV (synchro)
df_nc <- read.csv("/home/alain/Nextcloud_abret/Documents/Data/Cours_Enfants_Présences.csv")

df_cours <- df_nc[,1:4] |>
  dplyr::mutate(DateText=Date) |>
  dplyr::mutate(Date = as.Date(Date, format="%Y-%m-%d")) |>
  dplyr::mutate(Mois = factor(format(Date, "%Y-%m"))) |>
  dplyr::mutate(Enseignant = factor(Enseignant)) |>
  dplyr::rename(Pratiquants = Total.cours) |>
  dplyr::mutate(Jour = factor(Jour , levels=c("samedi","mercredi")))

str(df_cours)
graph3 <- ggplot(data = df_cours, aes(x = Mois, y = Pratiquants)) + # Jour: OK
  geom_boxplot(fill = "skyblue") +
  stat_summary(fun=mean, geom="point", shape=5, size=4)
ggplotly(graph3)



library(plotly)

set.seed(1234)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)), rating = c(rnorm(200),rnorm(200, mean=.8)))
str(dat)
p2 <- ggplot(dat, aes(x=cond, y=rating)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=5, size=4)
ggplotly(p2)

set.seed(1234)
dat <- data.frame(cond = factor(rep(c("A","B", "C"), each=200)), rating = c(rnorm(200),rnorm(200, mean=.8), rnorm(200, mean=0.3)))
str(dat)
p3 <- ggplot(dat, aes(x=cond, y=rating)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=5, size=4)
ggplotly(p3)
