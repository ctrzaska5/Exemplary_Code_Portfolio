library(tidyverse)
library(readxl)
library(patchwork)
library(haven)

la_turnout_basic <- read_dta("PS5/la_turnout_basic.dta")
View(la_turnout_basic)

la_turnout_basic$understandingclause2 <- factor(la_turnout_basic$understandingclause2, labels = c("Control", "Treatment"))

la_breg=
  la_turnout_basic|>
  group_by(year, understandingclause2)|>
  summarise(mean(blackregrate, na.rm = TRUE))

la_wreg=
  la_turnout_basic|>
  group_by(year, understandingclause2)|>
  summarise(mean(whiteregrate, na.rm = TRUE))

la_wreg <- 
  la_wreg|>
  rename(avg_white_reg = `mean(whiteregrate, na.rm = TRUE)`)

la_breg <- 
  la_breg|>
  rename(avg_black_reg = `mean(blackregrate, na.rm = TRUE)`)

la_turnout_avg <- 
  la_turnout_avg|>
  rename(understandingclause_black = understandingclause2.x,
         understandingclause_white = understandingclause2.y)


la_turnout_avg = merge(la_breg, la_wreg, by = "year")

black_plot=
  la_turnout_avg_uf|>
  ggplot(aes(x = year, y = avg_black_reg))+
  geom_line(aes(color = understandingclause_black, "Control" = "darkmagenta", "Treatment" = "gold3"))+
  geom_point(aes(shape = understandingclause_black))+
  scale_x_continuous(limits = c(1950, 1970), breaks = seq(1950, 1970, by = 5))+
  scale_y_continuous(limits = c(0.05, 0.65), breaks = seq(0.2,0.6, by = 0.2))+
  theme_bw()+
  labs(x = "Year", y = "Black Registration Rate")+
  theme(legend.box.margin = margin(0, 0, 0, 0), legend.spacing.y = unit(0.1, "mm"))+
  theme(legend.text = element_text(size = 8), legend.title = element_text(size = 8))+
  theme(legend.title = element_blank(),
        legend.key.width = unit(0.5, "cm"))

white_plot=
  la_turnout_avg_uf|>
  ggplot(aes(x = year, y = avg_white_reg))+
  geom_line(aes(color = understandingclause_white))+
  geom_point(aes(shape = understandingclause_white))+
  scale_x_continuous(limits = c(1950, 1970), breaks = seq(1950, 1970, by = 5))+
  scale_y_continuous(limits = c(0.57, 1.01), breaks = seq(0.6,1.0, by = 0.1))+
  theme_bw()+
  labs(x = "Year", y = "White Registration Rate")+
  theme(legend.box.margin = margin(0, 0, 0, 0), legend.spacing.y = unit(0.1, "mm"))+
  theme(legend.text = element_text(size = 8))+
  theme(legend.title = element_blank(),
        legend.key.width = unit(0.5, "cm"))

la_turnout_avg_uf<-
  na.omit(la_turnout_avg)

la_plot_final <- black_plot + white_plot

la_plot_final

ggsave(
  'la_plot_final.pdf',
  plot = la_plot_final,
  width = 6,
  height = 3
)
