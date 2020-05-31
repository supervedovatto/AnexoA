p1 <- PopulacaoProjecao %>% 
  merge(RegioesGoias) %>% 
  filter(Mesorregiao == LocRef$Mesorregiao & Ano == DataRef & !is.na(Quantidade)) %>% 
  group_by(Microrregiao) %>% 
  summarise(Habitantes = sum(Quantidade)) %>%  
  arrange(desc(Microrregiao)) %>%
  mutate(freq = Habitantes / sum(Habitantes)) %>%
  mutate(ypos = cumsum(freq)-freq/2) %>% 
  ggplot(aes(x = 2,y = freq,fill = Microrregiao)) +
    geom_bar(stat = "identity") +
    geom_text(aes(y = ypos, label = scales::percent(freq,decimal.mark = ",")), size=4, color = "white") +
    coord_polar("y", start=0) +
    scale_fill_manual(values = mypallete) +
    theme_void() +
    theme(title = element_text(size=8),
          axis.title = element_blank()) +
    xlim(0.5, 2.5) +
    labs(x="", y="",title = "Percentual da população de cada Microrregião",
         caption = "Fonte: Elaborado pelo OMT/GYN com dados do BDE/IMB. Acesso: 19/03/2020.")

p2 <- MapaGoias[MapaGoias$Mesorregiao == LocRef$Mesorregiao,] %>% 
  ggplot() + 
  theme_bw() +
  scale_fill_manual(values = mypallete) +
  theme(title = element_text(size=8),
        axis.text = element_text(size=5),
        axis.title = element_blank()) +
  geom_sf(aes(fill= Microrregiao), size = 0.1) +
  labs(caption = "Fonte: Elaborado pelo OMT/GYN com dados do BDE/IMB. Acesso: 19/03/2020.",
       title = paste("Microrregiões do",LocRef$Mesorregiao),
       fill = "Microrregiões")

grafico <- ggarrange(p2, p1,
          common.legend = TRUE,
          align = "v",
          legend = "bottom")
grafico