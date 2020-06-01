PieChartPopulacaoSEGPLAN <- PopulacaoProjecao %>%
  filter(Ano == DataRef) %>%
  merge(RegioesGoias) %>%
  group_by(RPSEGPLAN) %>%
  summarise(Populacao = sum(Quantidade)) %>%
  arrange(desc(RPSEGPLAN)) %>%
  mutate(freq = Populacao / sum(Populacao),
         ypos = cumsum(freq) - freq / 2) %>%
  ggplot(aes(x = 2, y = freq, fill = RPSEGPLAN)) +
  geom_bar(stat = "identity") +
  geom_text(aes(
    y = ypos,
    label = scales::percent(freq, decimal.mark = ",", accuracy = 0.1)
  ),
  size = 4,
  color = "white") +
  labs(
    x = "",
    y = "",
    fill = paste("Região de Planejamento"),
    caption = "Fonte: BDE/IMB.
                    Acesso: 19/03/2020."
  ) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = mypallete) +
  theme_void() +
  xlim(0.5, 2.5)