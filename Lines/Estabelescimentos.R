LinesEstabelescimentos <- Estabelescimentos %>%
  filter(Localidade == LocRef$Localidade) %>%
  ggplot(aes(x = Ano, y = Total)) +
  geom_line(aes(color = Rede), size = 1) +
  scale_color_manual(values = mypallete) +
  scale_x_date(date_breaks = "1 years", labels = date_format("%Y")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90)) +
  labs(caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.")