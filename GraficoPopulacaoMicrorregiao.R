load(file = "Dados/POCV.RData")

Micro.Populacao <- PopulacaoProjecao %>% 
  filter(Microrregiao == LocRef$Microrregiao) %>% 
  filter(Ano == AnoRef) %>%
  group_by(Localidade) %>% 
  summarise(Populacao = sum(Quantidade)) %>% 
  arrange(desc(Populacao)) %>%
  mutate(freq = Populacao/sum(Populacao))

#Cria a categoria "Outras para as menores cidades". Estou considerando apenas as 7 maiores.
maiores <- Micro.Populacao %>% head(n = 7)
outras <- Micro.Populacao %>% 
  tail(n=-7) %>% 
  summarise(Localidade = "Outras",
            Populacao = sum(Populacao),
            freq = sum(freq))

tabelafinal <- rbind(maiores,outras) %>% 
  arrange(desc(Localidade)) %>%
  mutate(ypos = cumsum(freq)-freq/2) %>% 
  ggplot(aes(x=2,y=freq,fill=Localidade)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = ypos, label = scales::percent(freq,decimal.mark = ",",accuracy = 0.1)), size=3) +
  labs(x="", y="",fill = paste("Microrregião de",LocRef$Microrregiao)) +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Set3") +
  theme_void() +
  xlim(0.5,2.5)

rm(maiores,outras,Micro.Populacao)