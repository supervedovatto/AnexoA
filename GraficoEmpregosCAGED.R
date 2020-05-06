load(file = "Dados/POCV.RData")

dados <- EmpregoCAGED %>% 
  filter(!is.na(Quantidade) & Setor != "não classificados" & RPSEGPLAN == LocRef$RPSEGPLAN & Ano >= max(EmpregoCAGED$Ano)-10) %>%
  group_by(Ano,Situacao,Setor) %>%
  summarise(Total = sum(Quantidade))
  
grafico <- dados %>% 
  ggplot(aes(x = Ano,y = Total)) +
    geom_line(aes(color = Situacao), stat="identity") +
    scale_color_brewer(palette="Set2") +
    theme_bw() +
    theme(legend.position="bottom",axis.text.x = element_text(angle = 90)) +
    scale_x_continuous(breaks = unique(dados$Ano)) +
    labs(y = "Total",x="Ano") +
    facet_wrap(~Setor,ncol = 2)