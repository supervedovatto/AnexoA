RadarIDM <- function(Eixo) {
  
  load(file = "Dados/POCV.RData")
  
  dados <- IDM %>% 
    mutate(Variável = fct_recode(Variável,
                                 "Equilíbrio Orçamentário" = "Equilíbrio Orçamentário do Município",
                                 "Saúde Bucal" = "Cobertura ESF Saúde Bucal",
                                 "Morte causas ext." = "Morte por causas externas",
                                 "Var. dos empregos" = "Variação do numero de empregos formais",
                                 "Escolaridade" = "Nível de escolaridade dos trabalhadores",
                                 "Adeq. ens. infantil" = "Adequação dos professores do ensino infantil",
                                 "Adeq. ens. fundamental" = "Adequação dos professores do ensino fundamental",
                                 "Adeq. ens. médio" = "Adequação dos professores do ensino médio",
                                 "Água tratada" = "Cobertura da rede de água tratada",
                                 "Esgoto" = "Cobertura da rede de esgoto",
                                 "Energia elétrica" = "Cobertura da rede de energia elétrica",
                                 "Tel. fixa e internet" = "Cobertura da rede de telefonia fixa e internet",
                                 "Atend. educ. 4 a 5 anos" = "Atendimento educacional da população de 4 a 5 anos",
                                 "Atend. educ. 6 a 14 anos" = "Atendimento educacional da população de 6 a 14 anos",
                                 "Atend. educ. 15 a 17 anos" = "Atendimento educacional da população de 15 a 17 anos",
                                 "Infraestrutura das esc. públicas" = "Infraestrutura dos prédios das escolas publicas urbanas"
    ))
  
  level1 <- paste("Mun. de",LocRef$Localidade)
  tabela1 <- dados %>% 
    filter(!is.na(Valor) & Localidade == LocRef$Localidade & Ano == max(Ano) & IDM == Eixo) %>% 
    mutate(Referencia = level1) %>% 
    select(Variável,Valor,Referencia)
  
  level2 <- paste("RP",LocRef$RPSEGPLAN)
  tabela2 <- dados %>% 
    merge(RegioesGoias) %>% 
    filter(!is.na(Valor) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(Ano) & IDM == Eixo) %>% 
    group_by(Localidade,Variável) %>% 
    summarise(Valor = mean(Valor,rm.na = TRUE)) %>% 
    group_by(Variável) %>% 
    summarise(Valor = mean(Valor,rm.na = TRUE)) %>% 
    mutate(Referencia = level2)
  
  level3 <- "Estado de Goiás"
  tabela3 <- dados %>% 
    filter(!is.na(Valor) & Ano == max(Ano) & IDM == Eixo) %>% 
    group_by(Localidade,Variável) %>% 
    summarise(Valor = mean(Valor,rm.na = TRUE)) %>% 
    group_by(Variável) %>% 
    summarise(Valor = mean(Valor,rm.na = TRUE)) %>% 
    mutate(Referencia = level3)
  
  rbind(tabela1,tabela2,tabela3) %>% 
    mutate(Referencia = factor(Referencia,ordered = T,levels = c(level1,level2,level3))) %>% 
    dcast(Referencia~Variável,value.var = "Valor") %>% 
    ggradar(grid.max = 10,values.radar = "",
            legend.position = "bottom",
            legend.text.size = 8,group.line.width = 0.5,group.point.size = 3,
            axis.label.size = 3) +
    labs(caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.")
}