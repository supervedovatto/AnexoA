tabela1 <- IDEB %>% 
  merge(RegioesGoias) %>% 
  filter(!is.na(Valor) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(IDEB$Ano)) %>% 
  mutate(Referencia = paste("RP",LocRef$RPSEGPLAN)) %>% 
  select(Referencia, Anos, Rede, Valor)

tabela2 <- IDEB %>% 
  filter(!is.na(Valor) & Ano == max(IDEB$Ano)) %>% 
  mutate(Referencia = "Estado de Goiás") %>% 
  select(Referencia, Anos, Rede, Valor)

grafico <- rbind(tabela1,tabela2) %>% 
  filter(Rede != "Federal") %>% 
  ggplot(aes(x=Valor, fill=Anos)) +
    geom_density(alpha=0.5)+
    scale_fill_manual(values = mypallete) +
    theme_minimal() +
    theme(legend.position="bottom",legend.title = element_blank())+
    labs(y = "Densidade", x = "IDEB",
         caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.") +
    facet_grid(vars(Rede),vars(Referencia))