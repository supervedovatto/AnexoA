AnoPIB <- year(max(PIBpc$Ano))
DataPIB <- paste(AnoPIB,"-01-01",sep="")

PopulacaoPIB <- PopulacaoProjecao %>% 
  filter(Ano == DataPIB) %>% 
  group_by(Localidade) %>% 
  summarise(Populacao = sum(Quantidade))

BarplotsPIBpcMesorregiao <- PIBpc %>% 
  merge(RegioesGoias) %>%
  merge(PopulacaoPIB) %>% 
  filter(Mesorregiao == LocRef$Mesorregiao &
           Ano == max(PIBpc$Ano) & !is.na(Valor)) %>%
  group_by(Microrregiao) %>%
  summarise(PIB = sum(Valor), Populacao = sum(Populacao)) %>%
  ungroup() %>% 
  mutate(PIBpercapita = PIB/Populacao) %>% 
  ggplot(aes(x = Microrregiao, y = PIBpercapita, fill = Microrregiao)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = mypallete) +
  geom_text(aes(label = paste("R$",myformat(1000*PIBpercapita,digits = 7))),
  size = 3, angle = 90,color = "white", y=5) +
  theme_bw() +
  theme(title = element_text(size=5),
        axis.text = element_text(size=10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_blank()) +
  labs(
    x = "",
    y = "",
    title = paste("PIB per capita, em milhares de reais, por Microrregião (",AnoPIB,")",sep=""),
    caption = "Fonte: Elaborado pelo OMT/GYN com dados do BDE/IMB. Acesso: 19/03/2020."
  )