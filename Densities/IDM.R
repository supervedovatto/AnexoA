tabela1 <- IDM %>%
  merge(RegioesGoias) %>%
  filter(!is.na(Valor) &
           RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(Ano)) %>%
  group_by(Localidade, IDM) %>%
  summarise(Valor = mean(Valor, rm.na = TRUE)) %>%
  mutate(Referencia = paste("RP", LocRef$RPSEGPLAN)) %>%
  select(Referencia, Localidade, IDM, Valor)

tabela2 <- IDM %>%
  filter(!is.na(Valor) & Ano == max(Ano)) %>%
  group_by(Localidade, IDM) %>%
  summarise(Valor = mean(Valor, rm.na = TRUE)) %>%
  mutate(Referencia = "Estado de Goiás") %>%
  select(Referencia, Localidade, IDM, Valor)

rbind(tabela1, tabela2) %>%
  ggplot(aes(x = Valor, fill = Referencia)) +
  facet_wrap( ~ IDM, ncol = 2) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = mypallete) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(
    y = "Densidade",
    x = "IDM",
    caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN
         a partir de dados do BDE/IMB, com acesso em 19/03/2020."
  )