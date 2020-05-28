grafico <- IDEB %>% 
  filter(!is.na(Valor) & Localidade == LocRef$Localidade & Ano >= "2011-01-01") %>% 
  select(Localidade, Ano, Anos, Rede, Valor) %>% 
  ggplot(aes(x = Ano, y = Valor)) + 
    geom_line(aes(color = Rede), size = 1) +
    scale_color_manual(values = mypallete) +
    theme_minimal() +
    scale_x_date(date_breaks = "1 years",labels = date_format("%Y")) +
    scale_y_continuous(limits = c(0,NA)) +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90),
          legend.title = element_blank())+
    facet_wrap(~Anos,ncol = 1,scales = "free_y")