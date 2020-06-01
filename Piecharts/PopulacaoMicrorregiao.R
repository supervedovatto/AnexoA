Micro.Populacao <- PopulacaoProjecao %>%
  merge(RegioesGoias) %>%
  filter(Microrregiao == LocRef$Microrregiao & Ano == DataRef) %>%
  group_by(Localidade) %>%
  summarise(Populacao = sum(Quantidade)) %>%
  arrange(desc(Populacao)) %>%
  mutate(freq = Populacao / sum(Populacao))

#Cria a categoria "Outras para as menores cidades". Estou considerando apenas as 7 maiores.
maiores <- Micro.Populacao %>% head(n = 7)
outras <- Micro.Populacao %>%
  tail(n = -7) %>%
  summarise(
    Localidade = "Outras",
    Populacao = sum(Populacao),
    freq = sum(freq)
  )

rbind(maiores, outras) %>%
  arrange(desc(Localidade)) %>%
  mutate(ypos = cumsum(freq) - freq / 2) %>%
  ggplot(aes(x = 2, y = freq, fill = Localidade)) +
  geom_bar(stat = "identity") +
  geom_text(aes(
    y = ypos,
    label = scales::percent(freq, decimal.mark = ",", accuracy = 0.1)
  ),
  size = 3,
  color = "white") +
  labs(
    x = "",
    y = "",
    fill = paste("Microrregião de", LocRef$Microrregiao),
    caption = "Fonte: BDE/IMB.
                    Acesso: 19/03/2020."
  ) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = mypallete) +
  theme_void() +
  xlim(0.5, 2.5)

rm(maiores, outras, Micro.Populacao)