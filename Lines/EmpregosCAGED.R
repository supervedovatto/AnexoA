dados <- Emprego %>%
  filter(
    RPSEGPLAN == LocRef$RPSEGPLAN &
      Ano >= max(Emprego$Ano) - 10 &
      Setor != "Atividade não especificada ou classificada"
  ) %>%
  select(Ano, Admitidos, Desligados, Setor) %>%
  reshape2::melt(
    id.vars = c("Ano", "Setor"),
    variable.name = "Situacao",
    value.name = "Total",
    na.rm = TRUE
  ) %>%
  group_by(Ano, Setor, Situacao) %>%
  summarise(Total = sum(Total))

dados %>%
  ggplot(aes(x = Ano, y = Total)) +
  geom_line(aes(color = Situacao), stat = "identity", size = 1) +
  scale_color_manual(values = mypallete) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90),
    strip.text = element_text(size = 6),
    legend.title = element_blank()
  ) +
  scale_x_continuous(breaks = unique(dados$Ano)) +
  labs(y = "Total", x = NULL,
       caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.") +
  facet_wrap( ~ Setor, ncol = 2, scales = "free_y")