AnosEscolha <- seq(ymd(max(PopulacaoProjecao$Ano)),ymd(min(PopulacaoProjecao$Ano)), by = '-4 year')

Piramide <- PopulacaoProjecao %>%
  filter(Ano %in% AnosEscolha & Localidade == LocRef$Localidade) %>%
  select(Sexo,Ano,Faixa,Quantidade) %>% 
  mutate(Faixa = fct_collapse(Faixa,
                               '0 a 9' = c("0 a 4","5 a 9"),
                               '10 a 19' = c("10 a 14","15 a 19"))) %>% 
  mutate(Sexo = fct_recode(Sexo,
                           Masculino = "Masculina",
                           Feminino = "Feminina"))

ajuste.coef <- 0.075*max(Piramide$Quantidade)

grafico <- Piramide %>%
  group_by(Ano,Sexo,Faixa) %>%
  summarise(Quantidade = sum(Quantidade)) %>%
  mutate(Proporcao = Quantidade/sum(Quantidade)) %>% 
  ggplot(aes(x = Faixa, y = ifelse(test = Sexo == "Masculino", yes = Quantidade, no = -Quantidade), fill = Sexo)) +
    geom_col() +
    coord_flip() +
    theme_classic() +
    theme(legend.position="bottom") +
    geom_text(aes(y = ifelse(test = Sexo == "Masculino", yes = (Quantidade + ajuste.coef), no = -(Quantidade + ajuste.coef)),
                  label = scales::percent(Proporcao,decimal.mark = ",",accuracy = 0.1)), size=3) +
    scale_y_continuous(labels = abs, limits = 1.1*max(Piramide$Quantidade) * c(-1,1)) +
    scale_fill_manual(values = mypallete) +
    labs(y = "População",x="Faixa etária",
         caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.") +
    facet_wrap(~Ano,ncol = 1,labeller = labeller(Ano = lubridate::year(AnosEscolha)))