IDHGeral <- IDH %>% 
  filter(IDHM == "Geral") %>% 
  mutate(IDH = Valor) %>% 
  select(-c("IDHM","Valor")) %>% 
  rename("IDHM Geral" = "IDH")

BoxPlotsIDHGiniMicrorregiao <- merge(IDHGeral,Gini) %>% 
  merge(RegioesGoias) %>%
  filter(!is.na("IDHM Geral") & !is.na(Gini) & Ano == max(Ano) & Mesorregiao == LocRef$Mesorregiao) %>% 
  select(-c("Ano","Mesorregiao","code_muni","RPSEGPLAN")) %>% 
  gather("Indice","Valor",c("IDHM Geral","Gini")) %>% 
  ggplot(aes(x = Indice, y = Valor, fill = Microrregiao)) +
    geom_boxplot() +
    scale_fill_manual(values = mypallete) +
    theme_bw() +
    theme(title = element_text(size=5),
          axis.text = element_text(size=10),
          axis.ticks.x = element_blank(),
          axis.title = element_blank()) +
    labs(x = "",
         y = "",
         title = paste("IDH e Índices de Gini Municipais (",year(max(IDH$Ano)),")",sep=""),
         caption = "Fonte: Elaborado pelo OMT/GYN com dados do BDE/IMB. Acesso: 01/06/2020.")