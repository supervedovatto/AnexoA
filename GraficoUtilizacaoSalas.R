grafico <- SalasAula %>% 
  filter(Localidade == LocRef$Localidade) %>%
  spread(Situacao,Total) %>% 
  mutate(TaxaUso = Utilizadas/Existentes) %>% 
  filter(!is.na(TaxaUso)) %>% 
  select(Ano,Rede,TaxaUso) %>%
  ggplot(aes(x = Ano, y = TaxaUso)) + 
    geom_line(aes(color = Rede), size = 1) +
    scale_color_manual(values = mypallete) +
    labs(y="Taxa de Utilização") +
    scale_x_date(date_breaks = "1 years",labels = date_format("%Y")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),limits = c(0,NA)) +
    theme_minimal() +
    labs(caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.") +
    theme(legend.position="bottom",axis.text.x = element_text(angle = 90))