SEGPLAN.Populacao <- PopulacaoProjecao %>% 
  filter(RPSEGPLAN == LocRef$RPSEGPLAN & Ano == AnoRef) %>% 
  group_by(Localidade) %>% 
  summarise(Populacao = sum(Quantidade)) %>% 
  arrange(desc(Populacao)) %>%
  mutate(freq = Populacao/sum(Populacao))

maiores <- SEGPLAN.Populacao %>% head(n = 7)
outras <- SEGPLAN.Populacao %>% 
  tail(n=-7) %>% 
  summarise(Localidade = "Outras",
            Populacao = sum(Populacao),
            freq = sum(freq))

rbind(maiores,outras) %>% 
  arrange(desc(Localidade)) %>%
  mutate(ypos = cumsum(freq)-freq/2) %>% 
  ggplot(aes(x=2,y=freq,fill=Localidade)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = ypos, label = scales::percent(freq,decimal.mark = ",",accuracy = 0.1)), size=3) +
  labs(x="", y="",fill = "Cidade") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Set3") +
  theme_void() +
  xlim(0.5, 2.5)

rm(maiores,outras)
