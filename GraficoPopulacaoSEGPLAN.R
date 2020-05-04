SEGPLAN.Populacao <- PopulacaoProjecao %>% 
  filter(Ano == AnoRef) %>% 
  group_by(RPSEGPLAN) %>% 
  summarise(Populacao = sum(Quantidade)) %>% 
  arrange(desc(RPSEGPLAN)) %>% 
  mutate(freq = Populacao/sum(Populacao)) %>%
  mutate(ypos = cumsum(freq)-freq/2) %>% 
  ggplot(aes(x=2,y=freq,fill=RPSEGPLAN)) +
    geom_bar(stat = "identity") +
    geom_text(aes(y = ypos, label = scales::percent(freq,decimal.mark = ",",accuracy = 0.1)), size=4) +
    labs(x="", y="",fill = paste("Região de Planejamento")) +
    coord_polar("y", start=0) +
    scale_fill_brewer(palette="Set3") +
    theme_void() +
    xlim(0.5, 2.5)