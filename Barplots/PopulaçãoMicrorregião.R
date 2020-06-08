BarplotsPopulacaoMicrorregiao <- PopulacaoProjecao %>%
  merge(RegioesGoias) %>%
  filter(Mesorregiao == LocRef$Mesorregiao &
           Ano == DataRef & !is.na(Quantidade)) %>%
  group_by(Microrregiao) %>%
  summarise(Habitantes = sum(Quantidade)) %>%
  ungroup() %>% 
  mutate(freq = Habitantes/sum(Habitantes)) %>% 
  ggplot(aes(x = Microrregiao, y = Habitantes, fill = Microrregiao)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = mypallete) +
  geom_text(aes(
    label = scales::percent(freq, decimal.mark = ","), vjust=-0.5, size=2.5
  ),
  size = 3) +
  theme_bw() +
  theme(title = element_text(size=8),
        axis.text = element_text(size=5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_blank()) +
  labs(
    x = "",
    y = "",
    title = paste("População Projetada por Microrregião (",AnoRef,")",sep=""),
    caption = "Fonte: Elaborado pelo OMT/GYN com dados do BDE/IMB. Acesso: 19/03/2020."
  )