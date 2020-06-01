Taxas %>%
  filter(Localidade %in% MunicipiosSEGPLANRef &
           Ano >= "2013-01-01") %>%
  ggplot(aes(x = Ano, y = Valor)) +
  geom_line(aes(color = Localidade), size = 1) +
  scale_color_manual(values = mypallete) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 years", labels = date_format("%Y")) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90),
    legend.title = element_blank()
  ) +
  labs(y = "Taxas", x = "",
       caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.") +
  facet_wrap(~ Taxa, ncol = 1)