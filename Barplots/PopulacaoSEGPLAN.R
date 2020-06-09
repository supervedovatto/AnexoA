BarplotsPopulacaoSEGPLAN <- PopulacaoProjecao %>%
  merge(RegioesGoias) %>%
  filter(RPSEGPLAN == LocRef$RPSEGPLAN &
           Ano == DataRef & !is.na(Quantidade)) %>%
  group_by(Localidade) %>%
  summarise(Habitantes = sum(Quantidade)/1000) %>%
  ungroup() %>% 
  mutate(freq = Habitantes/sum(Habitantes)) %>% 
  ggplot(aes(x = Localidade, 
             y = Habitantes, 
             fill = Localidade)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = mypallete) +
  geom_text(aes(
    label = scales::percent(freq, decimal.mark = ","), vjust=-0.1, size=2.5
  ),
  size = 3) +
  theme_bw() +
  theme(title = element_text(size=5),
        axis.text = element_text(size=10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_blank()) +
  labs(
    x = "",
    y = "",
    title = paste("População Projetada, em milhares, por Microrregião (",AnoRef,")",sep=""),
    caption = "Fonte: Elaborado pelo OMT/GYN com dados do BDE/IMB. Acesso: 19/03/2020."
  )
BarplotsPopulacaoSEGPLAN