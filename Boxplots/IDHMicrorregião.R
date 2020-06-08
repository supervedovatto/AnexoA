IDHMicrorregiao <- IDH %>%
  merge(RegioesGoias) %>%
  filter(Mesorregiao == LocRef$Mesorregiao & Ano == max(Ano) & IDHM == "Geral") %>%
  ggplot(aes(x = Microrregiao, y = Valor, fill = Microrregiao)) +
  geom_boxplot() +
  scale_fill_manual(values = mypallete) +
  theme_bw() +
  theme(title = element_text(size=8),
        axis.text = element_text(size=5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_blank()) +
  labs(
    x = "",
    y = "",
    title = paste("IDHM por Microrregião (",year(max(IDH$Ano)),")",sep=""),
    caption = "Fonte: Elaborado pelo OMT/GYN com dados do BDE/IMB. Acesso: 01/06/2020."
  )