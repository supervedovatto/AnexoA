BarplotsPopulacaoMicrorregiao <- PopulacaoProjecao %>%
  merge(RegioesGoias) %>%
  filter(Mesorregiao == LocRef$Mesorregiao &
           Ano == DataRef & !is.na(Quantidade)) %>%
  group_by(Microrregiao) %>%
  summarise(Habitantes = sum(Quantidade)) %>%
  arrange(desc(Microrregiao)) %>%
  mutate(freq = Habitantes / sum(Habitantes)) %>%
  mutate(ypos = cumsum(freq) - freq / 2) %>%
  ggplot(aes(x = 2, y = freq, fill = Microrregiao)) +
  geom_bar(stat = "identity") +
  geom_text(aes(
    y = ypos,
    label = scales::percent(freq, decimal.mark = ",")
  ),
  size = 4,
  color = "white") +
  scale_fill_manual(values = mypallete) +
  theme_void() +
  theme(title = element_text(size = 8),
        axis.title = element_blank()) +
  xlim(0.5, 2.5) +
  labs(
    x = "",
    y = "",
    title = "Percentual da população de cada Microrregião",
    caption = "Fonte: Elaborado pelo OMT/GYN com dados do BDE/IMB. Acesso: 19/03/2020."
  )