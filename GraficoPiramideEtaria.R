load(file = "Dados/POCV.RData")

AnoInicial <- min(PopulacaoProjecao$Ano)
AnosEscolha <- round(seq(from = AnoInicial,to = AnoRef,length.out = 3))

Piramide <- PopulacaoProjecao %>%
  filter(Ano %in% AnosEscolha & Localidade == LocRef$Localidade) %>%
  select(Sexo,Ano,Faixa,Quantidade)

Piramide$Faixa <- forcats::fct_collapse(Piramide$Faixa,
                                        '0 a 9' = c("0 a 4","5 a 9"),
                                        '10 a 19' = c("10 a 14","15 a 19"))

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
    labs(y = "População",x="Faixa etária") +
    facet_wrap(~Ano,ncol = 1)