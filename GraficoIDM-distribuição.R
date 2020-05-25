load(file = "Dados/POCV.RData")

tabela1 <- IDM %>% 
  merge(RegioesGoias) %>% 
  filter(!is.na(Valor) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(Ano)) %>% 
  group_by(Localidade,IDM) %>% 
  summarise(Valor = mean(Valor,rm.na = TRUE)) %>% 
  mutate(Referencia = paste("RP",LocRef$RPSEGPLAN)) %>% 
  select(Referencia, Localidade, IDM, Valor)

tabela2 <- IDM %>% 
  filter(!is.na(Valor) & Ano == max(Ano)) %>% 
  group_by(Localidade,IDM) %>% 
  summarise(Valor = mean(Valor,rm.na = TRUE)) %>% 
  mutate(Referencia = "Estado de Goiás") %>% 
  select(Referencia, Localidade, IDM, Valor)

grafico <- rbind(tabela1,tabela2) %>% 
  ggplot(aes(x=Valor, fill=Referencia)) +
    geom_density(alpha=0.5)+
    # geom_vline(data=medias, aes(xintercept=IDEBmedio, fill=Anos),linetype="dashed") +
    # geom_text(data=medias, aes(x=IDEBmedio, label=paste("Média:",format(IDEBmedio,decimal.mark = ",")), y=1.2),angle=90, vjust = 1.2, text=element_text(size=8))+
    scale_fill_manual(values = mypallete) +
    theme_bw() +
    theme(legend.position="bottom",legend.title = element_blank()) +
    labs(y = "Densidade", x = "IDM",
         caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.") +
    facet_wrap(~IDM, ncol = 2)