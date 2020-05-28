grafico <- AguaEsgoto %>% 
  filter(Localidade == as.character(LocRef$Localidade) & Ano >= as.Date("2008",format = "%Y")) %>% 
  transmute(Ano,Percentual = Percentual/100,Servico) %>% 
  ggplot(aes(x = Ano, y = Percentual)) + 
    geom_line(aes(color = Servico), size = 1) +
    scale_color_manual(values = mypallete) +
    scale_x_date(date_breaks = "1 years",labels = date_format("%Y")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),limits = c(0,1)) +
    theme_minimal() +
    labs(x="",
         caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.") +
    theme(legend.position="bottom",axis.text.x = element_text(angle = 90))