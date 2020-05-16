load(file = "Dados/POCV.RData")

grafico <- PopulacaoProjecao %>% 
  filter(Mesorregiao == LocRef$Mesorregiao,Ano == AnoRef) %>% 
  group_by(Microrregiao) %>% 
  summarise(Habitantes = sum(Quantidade)) %>%  
  arrange(desc(Microrregiao)) %>%
  mutate(freq = Habitantes / sum(Habitantes)) %>%
  mutate(ypos = cumsum(freq)-freq/2) %>% 
  ggplot(aes(x=2,y=freq,fill=Microrregiao)) +
    geom_bar(stat = "identity") +
    geom_text(aes(y = ypos, label = scales::percent(freq,decimal.mark = ",")), size=4,color = "white") +
    labs(x="", y="",fill = paste("Microrregiões do",LocRef$Mesorregiao)) +
    coord_polar("y", start=0) +
    scale_fill_manual(values = mypallete) +
    theme_void() +
    xlim(0.5, 2.5)