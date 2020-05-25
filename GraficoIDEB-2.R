load(file = "Dados/POCV.RData")

data <- max(IDEB$Ano)

tabela1 <- IDEB %>% 
  merge(RegioesGoias,by = "Localidade") %>% 
  filter(!is.na(IDEB) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano == data) %>% 
  mutate(Referencia = paste("RP",LocRef$RPSEGPLAN)) %>% 
  select(Referencia, Anos, Rede, IDEB)

tabela2 <- IDEB %>% 
  filter(!is.na(IDEB) & Ano == data) %>% 
  mutate(Referencia = "Estado de Goiás") %>% 
  select(Referencia, Anos, Rede, IDEB)

dados <- rbind(tabela1,tabela2)

medias <- dados %>% 
  group_by(Referencia,Anos,Rede) %>% 
  summarise(IDEBmedio = round(mean(IDEB,na.rm = TRUE),digits = 2)) %>% 
  filter(Rede != "Federal")

grafico <- dados %>% 
  filter(Rede != "Federal") %>% 
  ggplot(aes(x=IDEB, fill=Anos)) +
    geom_density(alpha=0.5)+
    geom_vline(data=medias, aes(xintercept=IDEBmedio, fill=Anos),linetype="dashed") +
    geom_text(data=medias, aes(x=IDEBmedio, label=paste("Média:",format(IDEBmedio,decimal.mark = ",")), y=1.2),angle=90, vjust = 1.2, text=element_text(size=8))+
    scale_fill_manual(values = mypallete) +
    theme_minimal() +
    theme(legend.position="bottom",legend.title = element_blank())+
    labs(y = "Densidade", x = "IDEB",
         caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.") +
    facet_grid(vars(Rede),vars(Referencia))